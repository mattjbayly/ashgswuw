#' Weighted usable width over a range of discharges
#'
#' Runs [wuw_estimate()] for each value of `discharge_m3s` with the reach
#' geometry and HSI curves prepared once, and returns one row per discharge.
#' Discharges that cannot be evaluated (above bankfull, zero or negative) are
#' kept as rows with a `status` label and `NA` values rather than dropped.
#'
#' @inheritParams wuw_estimate
#' @param discharge_m3s Numeric vector of target discharges (m^3/s).
#' @param keep_results If `TRUE`, the full `wuw_result` objects are attached
#'   to the returned data frame as attribute `"results"` (a list, `NULL` for
#'   failed rows).
#'
#' @return A data frame with columns `discharge_m3s`, `status` (`"ok"`,
#'   `"above_bankfull"`, `"invalid_discharge"`, `"solver_failed"`),
#'   `drawdown_m`, `wetted_width_m`, `mean_depth_m`, `mean_velocity_ms`,
#'   `froude`, `smix`, `wuw_m`, `wuw_fraction`, `wua_m2`, then one logical
#'   `qaqc_*` column per advisory check (TRUE = pass, FALSE = fail, NA = not
#'   evaluable; see [qaqc_limits()]) and `qaqc_all_pass`; attributes
#'   `"bankfull_discharge_m3s"`, `"b"` and `"model_version"`. Returns `NULL`
#'   with a message if the reach-level inputs are invalid. One consolidated
#'   warning summarises any failed checks.
#' @seealso [wuw_estimate()]
#' @export
#' @examples
#' depth_hsi <- read.csv(system.file("extdata", "hsi_example_depth.csv",
#'                                   package = "ashgswuw"))
#' velocity_hsi <- read.csv(system.file("extdata", "hsi_example_velocity.csv",
#'                                      package = "ashgswuw"))
#' wuw_curve(discharge_m3s = c(0.25, 0.5, 1, 2, 5, 10, 25),
#'           bankfull_width_m = 14.36, bankfull_depth_m = 0.705,
#'           gradient_m_per_m = 0.011, d84_mm = 135,
#'           depth_hsi = depth_hsi, velocity_hsi = velocity_hsi)
wuw_curve <- function(discharge_m3s, bankfull_width_m, bankfull_depth_m,
                      gradient_m_per_m, d84_mm, depth_hsi, velocity_hsi,
                      reach_length_m = NULL, n_bins = 50, rel_max = 5,
                      b = NULL, keep_results = FALSE, mad_m3s = NULL) {
  if (!is.numeric(discharge_m3s) || length(discharge_m3s) == 0L) {
    return(null_with_message("`discharge_m3s` must be a non-empty numeric vector."))
  }
  prep <- wuw_prepare(bankfull_width_m, bankfull_depth_m, gradient_m_per_m,
                      d84_mm, depth_hsi, velocity_hsi, reach_length_m,
                      n_bins, rel_max, b, mad_m3s)
  if (prep$status != "ok") return(null_with_message(prep$message))

  cols <- c("drawdown_m", "wetted_width_m", "mean_depth_m", "mean_velocity_ms",
            "froude", "smix", "wuw_m", "wuw_fraction", "wua_m2")
  n <- length(discharge_m3s)
  out <- data.frame(discharge_m3s = discharge_m3s,
                    status = rep(NA_character_, n),
                    stringsAsFactors = FALSE)
  for (cc in cols) out[[cc]] <- NA_real_
  qcols <- qaqc_limits()$check
  for (cc in qcols) out[[cc]] <- NA
  out$qaqc_all_pass <- NA
  results <- vector("list", n)

  for (i in seq_len(n)) {
    r <- wuw_core(prep, discharge_m3s[i])
    out$status[i] <- r$status
    if (r$status != "ok") next
    res <- r$result
    results[[i]] <- res
    out$drawdown_m[i] <- res$hydraulics$drawdown_m
    out$wetted_width_m[i] <- res$wetted_width_m
    out$mean_depth_m[i] <- res$hydraulics$mean_depth_m
    out$mean_velocity_ms[i] <- res$hydraulics$mean_velocity_ms
    out$froude[i] <- res$hydraulics$froude
    out$smix[i] <- res$hydraulics$smix
    out$wuw_m[i] <- res$wuw_m
    out$wuw_fraction[i] <- res$wuw_fraction
    out$wua_m2[i] <- res$wua_m2
    for (cc in qcols) out[[cc]][i] <- res$qaqc_pass[[cc]]
    # All evaluable checks passed (NA checks are ignored).
    out$qaqc_all_pass[i] <- all(res$qaqc_pass[!is.na(res$qaqc_pass)])
  }
  if (any(out$status != "ok")) {
    bad <- table(out$status[out$status != "ok"])
    message(sprintf(paste0("wuw_curve: %d of %d discharge(s) returned no result: %s. ",
                           "Those rows have NA values; the `status` column of the ",
                           "returned data frame says why for each row."),
                    sum(out$status != "ok"), n,
                    paste(sprintf("%d %s", bad, names(bad)), collapse = ", ")))
  }
  fails <- vapply(qcols, function(cc) sum(!out[[cc]], na.rm = TRUE), integer(1))
  if (any(fails > 0)) {
    fails <- fails[fails > 0]
    warning(sprintf(paste0("QA/QC: %d check(s) failed for at least one discharge: %s. ",
                           "Results are returned; see the qaqc_* columns and qaqc_limits()."),
                    length(fails),
                    paste(sprintf("%s (%d row%s)", names(fails), fails, ifelse(fails == 1, "", "s")),
                          collapse = ", ")),
            call. = FALSE)
  }
  bf <- hydraulics_at_stage(prep$geom, 0, prep$gradient_m_per_m, prep$d84_m)
  attr(out, "bankfull_discharge_m3s") <- bf$discharge_m3s
  attr(out, "b") <- prep$geom$b
  attr(out, "model_version") <- model_version()
  if (keep_results) attr(out, "results") <- results
  out
}
