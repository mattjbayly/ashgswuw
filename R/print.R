#' Print a `wuw_result`
#'
#' @param x A `wuw_result` from [wuw_estimate()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.wuw_result <- function(x, ...) {
  g <- x$geometry; h <- x$hydraulics; inp <- x$inputs
  cat(x$model_version, "\n")
  cat(sprintf("Inputs: Wb = %.3g m, db(mean) = %.3g m, S = %.4g m/m, D84 = %.3g mm, Q = %.4g m3/s%s\n",
              inp$bankfull_width_m, inp$bankfull_depth_m, inp$gradient_m_per_m,
              inp$d84_mm, inp$discharge_m3s,
              if (is.null(inp$reach_length_m)) "" else sprintf(", L = %.4g m", inp$reach_length_m)))
  cat(sprintf("Geometry: b = %.4f (%s), dmax = %.3f m, bankfull area = %.3f m2, bankfull Q = %.3f m3/s\n",
              g$b, g$b_source, g$dmax_m, g$bankfull_area_m2, g$bankfull_discharge_m3s))
  cat(sprintf("At Q: drawdown = %.3f m, wetted width = %.3f m, mean depth = %.3f m, velocity = %.3f m/s, Fr = %.3f, Smix = %.3f\n",
              h$drawdown_m, h$wetted_width_m, h$mean_depth_m, h$mean_velocity_ms,
              h$froude, h$smix))
  cat(sprintf("WUW = %.3f m  (fraction of wetted width = %.3f)\n", x$wuw_m, x$wuw_fraction))
  if (is.na(x$wua_m2)) {
    cat("WUA = NA (no reach length supplied)\n")
  } else {
    cat(sprintf("WUA = %.2f m2\n", x$wua_m2))
  }
  cat(sprintf("Bins: %d x %d over relative 0-%g; excluded tail probability (depth, velocity) = %.4f, %.4f\n",
              x$diagnostics$n_bins, x$diagnostics$n_bins, x$diagnostics$rel_max,
              x$diagnostics$depth_prob_excluded_below_zero + x$diagnostics$depth_prob_excluded_above_rel_max,
              x$diagnostics$velocity_prob_excluded_below_zero + x$diagnostics$velocity_prob_excluded_above_rel_max))
  failed <- qaqc_failures(x$qaqc)
  n_eval <- sum(!is.na(x$qaqc$pass))
  if (length(failed)) {
    cat(sprintf("QA/QC: %d of %d evaluable checks FAILED: %s. See $qaqc.\n",
                length(failed), n_eval, paste(failed, collapse = ", ")))
  } else {
    cat(sprintf("QA/QC: all %d evaluable checks passed%s.\n", n_eval,
                if (is.null(x$inputs$mad_m3s)) " (MAD not supplied; MAD checks NA)" else ""))
  }
  cat("See $assumptions for the", length(x$assumptions), "explicit model assumptions.\n")
  invisible(x)
}
