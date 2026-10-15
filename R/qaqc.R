# QA/QC checks against the tested domain of the method.
#
# Every check returns TRUE (pass), FALSE (fail) or NA (not evaluable, e.g. no
# MAD supplied). Checks never stop a calculation; they are returned with the
# result so users can decide whether to proceed. Thresholds and their sources:
#
#   Gronsdahl et al. (2021), River Res. Applic. 37:1520-1537:
#     - suitable for single-thread gravel-bed rivers with MAD <= ~15 m3/s
#       (approximate and conservative; the 15 m3/s figure follows Saraeva &
#       Hardy 2009 for the velocity distributions);
#     - depth/velocity distributions fitted at 5-100 % of MAD (Schweizer et
#       al. 2007); largest hydraulic errors at the lowest flows;
#     - tested input envelope (Table 3): Wb 1.8-168 m, db 0.19-3.5 m,
#       D84 55-263 mm, gradient 0.0003-0.04 m/m, b 0.06-0.47;
#     - b above 0.7 unlikely in single-thread channels; large Wb/db pushes b
#       toward 1 and indicates a multi-thread or laterally unstable channel
#       (Elbow River could not be run);
#     - benchmark depths below 0.01 m were excluded and the shallowest, lowest
#       flows fit worst.
#   Ferguson (2007), McParland et al. (2016): the resistance law has its
#     largest relative errors under partial submergence, R / D84 < 1.

QAQC_LIMITS <- list(
  mad_max = 15, q_mad_min_frac = 0.05, b_max = 0.7, wd_ratio_max = 70,
  submergence_min = 1,
  width = c(1.8, 168), depth = c(0.19, 3.5), d84 = c(55, 263),
  gradient = c(0.0003, 0.04), mean_depth_min = 0.05)

# Reach-level checks: identical for every discharge.
qaqc_reach <- function(Wb, db, S, d84_mm, b, mad) {
  L <- QAQC_LIMITS
  inr <- function(x, r) x >= r[1] && x <= r[2]
  data.frame(
    check = c("qaqc_mad_ok", "qaqc_b_ok", "qaqc_wd_ratio_ok", "qaqc_width_ok",
              "qaqc_depth_ok", "qaqc_d84_ok", "qaqc_gradient_ok"),
    pass = c(if (is.null(mad)) NA else mad <= L$mad_max,
             b <= L$b_max, Wb / db <= L$wd_ratio_max,
             inr(Wb, L$width), inr(db, L$depth), inr(d84_mm, L$d84), inr(S, L$gradient)),
    value = c(if (is.null(mad)) NA_real_ else mad, b, Wb / db, Wb, db, d84_mm, S),
    limit = c(sprintf("MAD <= %g m3/s", L$mad_max), sprintf("b <= %g", L$b_max),
              sprintf("Wb/db <= %g", L$wd_ratio_max),
              sprintf("%g to %g m", L$width[1], L$width[2]),
              sprintf("%g to %g m", L$depth[1], L$depth[2]),
              sprintf("%g to %g mm", L$d84[1], L$d84[2]),
              sprintf("%g to %g m/m", L$gradient[1], L$gradient[2])),
    explanation = c(
      "Mean annual discharge within the range where the method performed acceptably (single-thread gravel-bed rivers, MAD about 15 m3/s or less; Gronsdahl et al. 2021). Larger rivers gave markedly different habitat-flow curves. NA if no MAD supplied.",
      "Shape parameter b at or below 0.7. Higher values are unlikely in natural single-thread channels; the tested rivers had b of 0.06 to 0.47.",
      "Width-to-depth ratio at or below 70. Larger ratios push b toward 1 and usually indicate a multi-thread or laterally unstable channel, which the method cannot represent.",
      "Bankfull width inside the envelope of the eight evaluated rivers (Gronsdahl et al. 2021, Table 3).",
      "Mean bankfull depth inside the evaluated envelope.",
      "D84 inside the evaluated envelope. The resistance law was developed for gravel and boulder beds.",
      "Gradient inside the evaluated envelope. Steeper reaches are usually step-pool or cascade, where depth and velocity are not independent and the resistance law is less reliable."),
    stringsAsFactors = FALSE)
}

# Discharge-level checks: depend on the solved stage.
qaqc_flow <- function(Q, mad, hydraulic_radius_m, d84_m, mean_depth_m) {
  L <- QAQC_LIMITS
  sub <- hydraulic_radius_m / d84_m
  data.frame(
    check = c("qaqc_q_le_mad_ok", "qaqc_q_ge_5pct_mad_ok", "qaqc_submergence_ok",
              "qaqc_mean_depth_ok"),
    pass = c(if (is.null(mad)) NA else Q <= mad,
             if (is.null(mad)) NA else Q >= L$q_mad_min_frac * mad,
             sub >= L$submergence_min, mean_depth_m >= L$mean_depth_min),
    value = c(if (is.null(mad)) NA_real_ else Q / mad,
              if (is.null(mad)) NA_real_ else Q / mad,
              sub, mean_depth_m),
    limit = c("Q / MAD <= 1", sprintf("Q / MAD >= %g", L$q_mad_min_frac),
              sprintf("R / D84 >= %g", L$submergence_min),
              sprintf("mean depth >= %g m", L$mean_depth_min)),
    explanation = c(
      "Discharge at or below mean annual discharge. The depth and velocity frequency distributions were fitted at flows up to 100 % of MAD; above that they tend to miss the high densities around the mean. NA if no MAD supplied.",
      "Discharge at or above 5 % of MAD, the lower end of the fitted range. The largest depth and velocity errors occurred at the lowest flows, especially in small streams. NA if no MAD supplied.",
      "Relative submergence R / D84 at or above 1. Below 1 the largest grains protrude through the flow and the Ferguson (2007) resistance law has its largest relative errors, so velocity and the stage solution are least reliable.",
      "Mean depth at the discharge at or above 0.05 m. Very shallow results fall where benchmark data were sparse or excluded (depths under 0.01 m) and model fit was poorest."),
    stringsAsFactors = FALSE)
}

qaqc_table <- function(prep, hyd, Q) {
  rbind(qaqc_reach(prep$geom$bankfull_width_m, prep$geom$bankfull_depth_m,
                   prep$gradient_m_per_m, prep$d84_mm, prep$geom$b, prep$mad_m3s),
        qaqc_flow(Q, prep$mad_m3s, hyd$hydraulic_radius_m, prep$d84_m, hyd$mean_depth_m))
}

# One-line summary of failed checks, used for warnings and printing.
qaqc_failures <- function(tbl) {
  tbl$check[!is.na(tbl$pass) & !tbl$pass]
}

#' QA/QC thresholds used by the package
#'
#' Returns the limits applied by the `qaqc_*` checks reported in
#' [wuw_estimate()] and [wuw_curve()], with their sources, so they can be
#' cited or compared with site conditions. Checks are advisory: they never
#' stop a calculation.
#'
#' @return A data frame with one row per check: `check`, `limit`,
#'   `explanation`.
#' @export
#' @examples
#' qaqc_limits()
qaqc_limits <- function() {
  r <- qaqc_reach(10, 0.5, 0.01, 100, 0.2, 1)
  f <- qaqc_flow(1, 1, 0.5, 0.1, 0.3)
  out <- rbind(r, f)[, c("check", "limit", "explanation")]
  rownames(out) <- NULL
  out
}
