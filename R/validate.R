# Internal validation helpers.
#
# All user-facing failures follow one convention: emit a message() and return
# NULL. Internally, functions return a "status" list so that wuw_curve() can
# label each row rather than losing the information.

null_with_message <- function(...) {
  message(...)
  NULL
}

is_pos_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x > 0
}

# Returns NULL when all reach inputs are acceptable, otherwise a
# list(status, message).
validate_reach_inputs <- function(bankfull_width_m, bankfull_depth_m,
                                  gradient_m_per_m, d84_mm) {
  args <- list(bankfull_width_m = bankfull_width_m,
               bankfull_depth_m = bankfull_depth_m,
               gradient_m_per_m = gradient_m_per_m,
               d84_mm = d84_mm)
  for (nm in names(args)) {
    if (!is_pos_scalar(args[[nm]])) {
      return(list(status = "invalid",
                  message = sprintf("`%s` must be a single positive finite number.", nm)))
    }
  }
  if (gradient_m_per_m >= 1) {
    return(list(status = "invalid",
                message = paste0("`gradient_m_per_m` must be < 1. Gradient is rise/run ",
                                 "(e.g. 0.011), not percent.")))
  }
  if (gradient_m_per_m > 0.2) {
    warning("`gradient_m_per_m` exceeds 0.2 (20 %). Gradient is rise/run, not percent; check the units.",
            call. = FALSE)
  }
  NULL
}

validate_bins <- function(n_bins, rel_max) {
  if (!is.numeric(n_bins) || length(n_bins) != 1L || is.na(n_bins) || n_bins < 2 ||
      n_bins != round(n_bins)) {
    return(list(status = "invalid", message = "`n_bins` must be a single integer >= 2."))
  }
  if (!is_pos_scalar(rel_max)) {
    return(list(status = "invalid", message = "`rel_max` must be a single positive number."))
  }
  NULL
}
