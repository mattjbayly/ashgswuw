# Reach-average hydraulics at a stage, and the stage solver.

.g  <- 9.81   # m s^-2
.a1 <- 6.5    # Ferguson (2007) variable-power-law constants as used by ASHGS
.a2 <- 2.5

# Dimensionless resistance (v / sqrt(g R S)) from Ferguson (2007), eq. 4 in
# McParland et al. (2016). Both occurrences use the dimensionless ratio
# R / D84. (The archived Habitat values/FindU.m used R^(5/3) instead of
# (R/D84)^(5/3) in the denominator; that is not reproduced here.)
ferguson_resistance <- function(hydraulic_radius_m, d84_m) {
  ratio <- hydraulic_radius_m / d84_m
  .a1 * .a2 * ratio / sqrt(.a1^2 + .a2^2 * ratio^(5 / 3))
}

hydraulics_at_stage <- function(geom, drawdown_m, gradient_m_per_m, d84_m) {
  gs <- geometry_at_stage(geom, drawdown_m)
  if (gs$flow_area_m2 <= 0) {
    return(c(gs, list(hydraulic_radius_m = 0, mean_velocity_ms = 0,
                      discharge_m3s = 0, froude = NA_real_)))
  }
  R <- gs$flow_area_m2 / gs$wetted_perimeter_m
  v <- ferguson_resistance(R, d84_m) * sqrt(.g * R * gradient_m_per_m)
  c(gs, list(hydraulic_radius_m = R,
             mean_velocity_ms = v,
             discharge_m3s = gs$flow_area_m2 * v,
             froude = v / sqrt(.g * gs$mean_depth_m)))
}

# Find the drawdown at which the section carries `discharge_m3s`.
# Returns list(status, message) on failure, otherwise
# list(status = "ok", bankfull = <hydraulics at drawdown 0>, at_q = <hydraulics>).
solve_stage <- function(geom, discharge_m3s, gradient_m_per_m, d84_m, tol = 1e-10) {
  bf <- hydraulics_at_stage(geom, 0, gradient_m_per_m, d84_m)
  if (discharge_m3s > bf$discharge_m3s) {
    return(list(status = "above_bankfull",
                message = sprintf(paste0(
                  "Discharge %.4g m3/s exceeds the modelled bankfull discharge %.4g m3/s. ",
                  "Flows above bankfull are outside the model domain; no result returned."),
                  discharge_m3s, bf$discharge_m3s)))
  }
  if (discharge_m3s == bf$discharge_m3s) {
    root <- 0
  } else {
    f <- function(dd) {
      hydraulics_at_stage(geom, dd, gradient_m_per_m, d84_m)$discharge_m3s - discharge_m3s
    }
    upper <- geom$dmax_m * (1 - 1e-9)
    if (f(upper) > 0) {
      return(list(status = "solver_failed",
                  message = "Discharge is below the smallest flow the solver can resolve."))
    }
    root <- uniroot(f, lower = 0, upper = upper, tol = tol, maxiter = 1000L)$root
  }
  h <- hydraulics_at_stage(geom, root, gradient_m_per_m, d84_m)
  h$drawdown_m <- root
  h$discharge_residual_m3s <- h$discharge_m3s - discharge_m3s
  list(status = "ok", bankfull = bf, at_q = h)
}
