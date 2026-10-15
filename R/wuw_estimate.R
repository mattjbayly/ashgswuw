#' Weighted usable width at one discharge
#'
#' Estimates wetted width, weighted usable width (WUW) and, optionally,
#' weighted usable area (WUA) at a single discharge from reach-average bankfull
#' geometry and habitat suitability curves, following the direct-geometry
#' pathway of the At-a-Station Hydraulic Geometry Simulator (ASHGS).
#'
#' @details
#' **Computation.** The bankfull width `Wb` and mean bankfull depth `db` define
#' an equivalent two-segment section with shape parameter
#' `b = (Wb / db) / 100` (McParland et al. 2016, eq. 2; or the value supplied
#' in `b`) and maximum depth `dmax = db / (1 - b)`. The water surface is drawn
#' down below bankfull until the Ferguson (2007) resistance law gives the
#' target discharge. The resulting mean depth, velocity and Froude number set
#' the Schweizer et al. (2007) normal/lognormal mixtures of relative depth and
#' relative velocity, which are integrated into `n_bins` bins over relative
#' values 0 to `rel_max`. Depth and velocity suitability are read from the
#' curves at each bin centre, multiplied, weighted by the joint (independent)
#' bin probability and summed to a combined suitability `CSI`;
#' `WUW = wetted width * CSI` and `WUA = reach_length_m * WUW`.
#'
#' **Returns `NULL` with a message** when: any numeric input is missing or not
#' positive (including `discharge_m3s = 0`); `gradient_m_per_m >= 1`; the shape
#' parameter is outside `0 < b < 1`; the discharge exceeds the modelled
#' bankfull discharge; `n_bins < 2`; or an HSI table cannot be prepared.
#' A warning is issued when `gradient_m_per_m > 0.2`, which usually means a
#' percentage was supplied.
#'
#' **QA/QC checks.** Every result carries a `qaqc` data frame of advisory
#' TRUE/FALSE checks against the domain in which the method was evaluated
#' (Gronsdahl et al. 2021): mean annual discharge at or below 15 m^3/s,
#' discharge between 5 % and 100 % of MAD, `b <= 0.7`, width-to-depth ratio
#' at or below 70, inputs inside the tested envelope (width 1.8-168 m, mean
#' depth 0.19-3.5 m, D84 55-263 mm, gradient 0.0003-0.04), relative
#' submergence `R / D84 >= 1` and mean depth at the discharge at or above
#' 0.05 m. A failed check never stops the calculation; one consolidated
#' warning names the failed checks so users can decide whether to proceed.
#' MAD-based checks are `NA` when `mad_m3s` is not supplied. See
#' [qaqc_limits()].
#'
#' **Assumptions** are listed by [model_assumptions()] and stored in the
#' result.
#'
#' @param bankfull_width_m Bankfull width (m).
#' @param bankfull_depth_m **Mean** bankfull depth (m): bankfull flow area
#'   divided by bankfull width. Not maximum depth.
#' @param gradient_m_per_m Reach gradient as rise over run (m/m), e.g. `0.011`.
#'   Not percent: 1.1 % must be entered as `0.011`.
#' @param d84_mm Bed-surface grain size than which 84 % is finer (mm).
#' @param discharge_m3s Target discharge (m^3/s).
#' @param depth_hsi,velocity_hsi Habitat suitability tables (data frames with
#'   a depth-in-m or velocity-in-m/s column and an `hsi` column), or objects
#'   from [prepare_hsi()].
#' @param reach_length_m Optional reach length (m). When supplied,
#'   `wua_m2 = reach_length_m * wuw_m`; otherwise `wua_m2` is `NA`.
#' @param n_bins Number of equal bins for each of relative depth and relative
#'   velocity (default 50, as in the paper).
#' @param rel_max Upper limit of the relative depth/velocity range that is
#'   binned (default 5, as in the paper). Probability beyond it is reported in
#'   `diagnostics` and dropped.
#' @param b Optional shape parameter overriding the `(Wb / db) / 100`
#'   estimate. Must satisfy `0 < b < 1`.
#' @param joint If `TRUE`, also return the `n_bins x n_bins` matrix of WUW
#'   contributions (rows = depth bins, columns = velocity bins).
#' @param mad_m3s Optional long-term mean annual discharge (m^3/s). Used only
#'   for the QA/QC checks; it never changes the calculation and never causes
#'   a `NULL` result.
#'
#' @return `NULL` on failure (see Details), otherwise a list of class
#'   `wuw_result` with elements:
#' \describe{
#'   \item{wuw_m}{Weighted usable width (m).}
#'   \item{wetted_width_m}{Total wetted width at the discharge (m).}
#'   \item{wuw_fraction}{`wuw_m / wetted_width_m`, in 0-1.}
#'   \item{wua_m2}{Weighted usable area (m^2), or `NA` without a reach length.}
#'   \item{inputs}{Inputs as supplied plus derived SI values.}
#'   \item{geometry}{`b`, `b_source`, `dmax_m`, bankfull area, perimeter,
#'     hydraulic radius, velocity and modelled bankfull discharge, and the
#'     `section` vertices.}
#'   \item{hydraulics}{At the target discharge: drawdown below bankfull, wetted
#'     width, flow area, mean depth, wetted perimeter, hydraulic radius, mean
#'     velocity, Froude number, mixing weight `smix` and the solver residual.}
#'   \item{depth_bins, velocity_bins}{One row per bin: relative bounds and
#'     centre, physical bounds and centre (m or m/s), probability and
#'     suitability.}
#'   \item{joint}{WUW-contribution matrix if `joint = TRUE`, else `NULL`.}
#'   \item{diagnostics}{Probability excluded below 0 and above `rel_max`,
#'     bin settings and solver tolerance.}
#'   \item{qaqc}{Data frame of advisory checks: `check`, `pass` (TRUE, FALSE
#'     or NA), `value`, `limit`, `explanation`.}
#'   \item{qaqc_pass}{Named logical vector, one element per check, for quick
#'     programmatic use.}
#'   \item{assumptions}{Character vector from [model_assumptions()].}
#'   \item{model_version}{Version string.}
#' }
#' @references McParland, D., Eaton, B., & Rosenfeld, J. (2016). At-a-station
#'   hydraulic geometry simulator. *River Research and Applications*, 32(3),
#'   399-410. \doi{10.1002/rra.2851}
#' @seealso [wuw_curve()], [prepare_hsi()], [model_assumptions()]
#' @export
#' @examples
#' depth_hsi <- read.csv(system.file("extdata", "hsi_example_depth.csv",
#'                                   package = "ashgswuw"))
#' velocity_hsi <- read.csv(system.file("extdata", "hsi_example_velocity.csv",
#'                                      package = "ashgswuw"))
#' # Harris Creek, BC (McParland et al. 2016): Wb 14.36 m, db 0.705 m,
#' # S 0.011, D84 135 mm. Illustrative HSI curves only.
#' res <- wuw_estimate(bankfull_width_m = 14.36, bankfull_depth_m = 0.705,
#'                     gradient_m_per_m = 0.011, d84_mm = 135,
#'                     discharge_m3s = 1,
#'                     depth_hsi = depth_hsi, velocity_hsi = velocity_hsi,
#'                     reach_length_m = 150)
#' res
#' res$wuw_m
#' res$qaqc[, c("check", "pass", "value", "limit")]
#' head(res$depth_bins)
wuw_estimate <- function(bankfull_width_m, bankfull_depth_m, gradient_m_per_m,
                         d84_mm, discharge_m3s, depth_hsi, velocity_hsi,
                         reach_length_m = NULL, n_bins = 50, rel_max = 5,
                         b = NULL, joint = FALSE, mad_m3s = NULL) {
  prep <- wuw_prepare(bankfull_width_m, bankfull_depth_m, gradient_m_per_m,
                      d84_mm, depth_hsi, velocity_hsi, reach_length_m,
                      n_bins, rel_max, b, mad_m3s)
  if (prep$status != "ok") return(null_with_message(prep$message))
  out <- wuw_core(prep, discharge_m3s, joint = joint)
  if (out$status != "ok") return(null_with_message(out$message))
  failed <- qaqc_failures(out$result$qaqc)
  if (length(failed)) {
    warning(sprintf("QA/QC: %d check(s) failed (%s). The result is returned; see $qaqc for values and explanations.",
                    length(failed), paste(failed, collapse = ", ")), call. = FALSE)
  }
  out$result
}

# Validate reach-level inputs once and build the objects shared by every
# discharge. Returns list(status, message) or list(status = "ok", ...).
wuw_prepare <- function(bankfull_width_m, bankfull_depth_m, gradient_m_per_m,
                        d84_mm, depth_hsi, velocity_hsi, reach_length_m,
                        n_bins, rel_max, b, mad_m3s = NULL) {
  bad <- validate_reach_inputs(bankfull_width_m, bankfull_depth_m,
                               gradient_m_per_m, d84_mm)
  if (!is.null(bad)) return(bad)
  bad <- validate_bins(n_bins, rel_max)
  if (!is.null(bad)) return(bad)
  # MAD is advisory only: an unusable value is dropped with a message, never
  # a NULL result.
  if (!is.null(mad_m3s) && !is_pos_scalar(mad_m3s)) {
    message("`mad_m3s` ignored: it must be a single positive number. MAD-based QA/QC checks will be NA.")
    mad_m3s <- NULL
  }
  if (!is.null(reach_length_m) && !is_pos_scalar(reach_length_m)) {
    return(list(status = "invalid",
                message = "`reach_length_m` must be NULL or a single positive number."))
  }
  geom <- build_geometry(bankfull_width_m, bankfull_depth_m, b)
  if (geom$status != "ok") return(geom[c("status", "message")])
  dcurve <- prepare_hsi(depth_hsi, "depth")
  if (is.null(dcurve)) return(list(status = "invalid_hsi", message = "`depth_hsi` could not be prepared."))
  vcurve <- prepare_hsi(velocity_hsi, "velocity")
  if (is.null(vcurve)) return(list(status = "invalid_hsi", message = "`velocity_hsi` could not be prepared."))

  list(status = "ok",
       geom = geom,
       gradient_m_per_m = gradient_m_per_m,
       d84_mm = d84_mm,
       d84_m = d84_mm / 1000,
       depth_hsi = dcurve,
       velocity_hsi = vcurve,
       reach_length_m = reach_length_m,
       mad_m3s = mad_m3s,
       n_bins = as.integer(n_bins),
       rel_max = rel_max)
}

# One discharge, given a prepared reach. Returns list(status, message) or
# list(status = "ok", result = <wuw_result>).
wuw_core <- function(prep, discharge_m3s, joint = FALSE, tol = 1e-10) {
  if (!is_pos_scalar(discharge_m3s)) {
    return(list(status = "invalid_discharge",
                message = "`discharge_m3s` must be a single positive number. Zero flow is outside this flowing-channel formulation."))
  }
  geom <- prep$geom
  sol <- solve_stage(geom, discharge_m3s, prep$gradient_m_per_m, prep$d84_m, tol = tol)
  if (sol$status != "ok") return(sol[c("status", "message")])
  h <- sol$at_q
  bf <- sol$bankfull

  smix <- mixture_weight(h$froude)
  pd <- bin_probabilities(prep$n_bins, prep$rel_max, smix, .sdlog_depth)
  pv <- bin_probabilities(prep$n_bins, prep$rel_max, smix, .sdlog_velocity)

  depth_bins <- data.frame(rel_lower = pd$rel_lower, rel_upper = pd$rel_upper,
                           rel_centre = pd$rel_centre,
                           lower_m = pd$rel_lower * h$mean_depth_m,
                           upper_m = pd$rel_upper * h$mean_depth_m,
                           centre_m = pd$rel_centre * h$mean_depth_m,
                           prob = pd$prob)
  depth_bins$hsi <- hsi_at(prep$depth_hsi, depth_bins$centre_m)
  velocity_bins <- data.frame(rel_lower = pv$rel_lower, rel_upper = pv$rel_upper,
                              rel_centre = pv$rel_centre,
                              lower_ms = pv$rel_lower * h$mean_velocity_ms,
                              upper_ms = pv$rel_upper * h$mean_velocity_ms,
                              centre_ms = pv$rel_centre * h$mean_velocity_ms,
                              prob = pv$prob)
  velocity_bins$hsi <- hsi_at(prep$velocity_hsi, velocity_bins$centre_ms)

  # Joint probability p_ij = p_i^d p_j^v (independence), CSI_ij = HSI_d HSI_v.
  contrib <- outer(depth_bins$prob * depth_bins$hsi,
                   velocity_bins$prob * velocity_bins$hsi) * h$wetted_width_m
  wuw <- sum(contrib)
  wua <- if (is.null(prep$reach_length_m)) NA_real_ else prep$reach_length_m * wuw
  qaqc <- qaqc_table(prep, h, discharge_m3s)
  qaqc_pass <- stats::setNames(qaqc$pass, qaqc$check)

  result <- list(
    wuw_m = wuw,
    wetted_width_m = h$wetted_width_m,
    wuw_fraction = wuw / h$wetted_width_m,
    wua_m2 = wua,
    inputs = list(bankfull_width_m = geom$bankfull_width_m,
                  bankfull_depth_m = geom$bankfull_depth_m,
                  gradient_m_per_m = prep$gradient_m_per_m,
                  d84_mm = prep$d84_mm, d84_m = prep$d84_m,
                  discharge_m3s = discharge_m3s,
                  reach_length_m = prep$reach_length_m,
                  mad_m3s = prep$mad_m3s,
                  n_bins = prep$n_bins, rel_max = prep$rel_max,
                  depth_hsi = prep$depth_hsi, velocity_hsi = prep$velocity_hsi),
    geometry = list(b = geom$b, b_source = geom$b_source, dmax_m = geom$dmax_m,
                    bankfull_area_m2 = bf$flow_area_m2,
                    bankfull_perimeter_m = bf$wetted_perimeter_m,
                    bankfull_hydraulic_radius_m = bf$hydraulic_radius_m,
                    bankfull_velocity_ms = bf$mean_velocity_ms,
                    bankfull_discharge_m3s = bf$discharge_m3s,
                    section = geom$section),
    hydraulics = list(drawdown_m = h$drawdown_m,
                      wetted_width_m = h$wetted_width_m,
                      flow_area_m2 = h$flow_area_m2,
                      mean_depth_m = h$mean_depth_m,
                      wetted_perimeter_m = h$wetted_perimeter_m,
                      hydraulic_radius_m = h$hydraulic_radius_m,
                      mean_velocity_ms = h$mean_velocity_ms,
                      froude = h$froude,
                      smix = smix,
                      discharge_residual_m3s = h$discharge_residual_m3s),
    depth_bins = depth_bins,
    velocity_bins = velocity_bins,
    joint = if (joint) contrib else NULL,
    diagnostics = list(depth_prob_excluded_below_zero = pd$excluded_below,
                       depth_prob_excluded_above_rel_max = pd$excluded_above,
                       velocity_prob_excluded_below_zero = pv$excluded_below,
                       velocity_prob_excluded_above_rel_max = pv$excluded_above,
                       n_bins = prep$n_bins, rel_max = prep$rel_max,
                       solver_tol = tol),
    qaqc = qaqc,
    qaqc_pass = qaqc_pass,
    assumptions = model_assumptions(),
    model_version = model_version())
  class(result) <- "wuw_result"
  list(status = "ok", result = result)
}

#' Model version string
#'
#' @return A character string identifying the package version and pathway.
#' @export
#' @examples
#' model_version()
model_version <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("ashgswuw")),
                  error = function(e) "dev")
  paste0("ashgswuw ", ver, " / ASHGS direct-geometry pathway")
}

#' Explicit model assumptions
#'
#' The assumptions built into [wuw_estimate()], returned as text so they can
#' be printed, logged or written alongside results.
#'
#' @return A character vector, one assumption per element.
#' @export
#' @examples
#' cat(model_assumptions(), sep = "\n")
model_assumptions <- function() {
  c("Channel geometry is fixed: no geomorphic adjustment occurs between the flows compared.",
    "Bankfull depth is the MEAN depth across the bankfull section (area / width), not maximum depth.",
    "Reach gradient approximates the energy slope; flow is uniform with no backwater or hydraulic controls.",
    "The channel is represented by a two-segment piecewise-linear section with a vertical closing bank, realising the Ferguson (2003) cumulative depth curve used by ASHGS; it is not a surveyed cross-section. Its bankfull area equals width x mean depth.",
    "Shape parameter b = (Wb/db)/100 (McParland et al. 2016, eq. 2) is an empirical estimate; valid range 0 < b < 1, with 0.1-0.7 typical of fish-bearing BC channels. dmax = db/(1 - b).",
    "Flow resistance follows Ferguson (2007) with a1 = 6.5, a2 = 2.5 and D84 as the only roughness scale; wood, vegetation and form roughness are not represented.",
    "Relative depth and relative velocity follow the Schweizer et al. (2007) normal/lognormal mixtures with one Froude-dependent mixing weight, logit(Smix) = -4.72 - 2.84 ln(Fr), and are treated as independent (joint probability = product of marginals).",
    "Lognormal mixture components have arithmetic mean 1 and SD 1.09 (depth) / 1.19 (velocity), converted to meanlog/sdlog; this differs from the archived MATLAB, which used those values directly as log-scale parameters.",
    "Distributions are integrated into n_bins equal bins over relative values 0 to rel_max (defaults 50 and 5); probability outside that range is dropped, reported in diagnostics, and the remainder renormalised.",
    "HSI curves are interpolated linearly at bin centres and held flat beyond their documented range; combined suitability is the product of depth and velocity HSI.",
    "Tested domain: single-thread, gravel-bed rivers with a mean annual discharge (MAD) of about 15 m3/s or less, in western North America (Gronsdahl et al. 2021, eight rivers, MAD 0.05-87 m3/s). Multi-thread, laterally unstable, step-pool, wood-dominated or anthropogenically modified channels, and regions outside western North America, are untested; apply with caution.",
    "Valid for 0 < Q <= modelled bankfull discharge; zero flow and above-bankfull flow return NULL. The empirical distributions were fitted at 5-100 % of mean annual discharge, so results above MAD, and at the very lowest flows in small streams, are less reliable.",
    "WUW and WUA are indices of physical habitat suitability, not estimates of fish abundance, production or survival.")
}
