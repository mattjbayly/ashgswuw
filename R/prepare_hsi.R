#' Prepare a habitat suitability index (HSI) curve
#'
#' Cleans a two-column table of habitat suitability against depth (m) or
#' velocity (m/s) so it can be used by [wuw_estimate()].
#'
#' @details
#' Columns are matched by name when the table has a column called `depth` or
#' `velocity` (whichever `variable` is) and a column called `hsi`
#' (case-insensitive). Otherwise the first column is taken as the variable and
#' the second as `hsi`, and a message says so.
#'
#' The following checks and fixes are applied, each reported by a message:
#' * values are coerced to numeric and rows with a missing value are dropped;
#' * `hsi` values outside 0-1 are clamped to that range;
#' * rows are sorted by the variable (ascending; the direction has no effect
#'   on results but is required by [stats::approx()]);
#' * exact duplicate variable values are collapsed to their mean `hsi`.
#'
#' The curve is later interpolated linearly and **held flat beyond its
#' documented range**: depths or velocities below the first point take the
#' first `hsi`, those above the last point take the last `hsi`. Include an
#' explicit end point (for example `hsi = 0` at a large velocity) if that is
#' not the intended behaviour.
#'
#' @param x A data frame with a variable column (depth in m or velocity in
#'   m/s) and an `hsi` column (0-1). An object already of class `hsi_curve`
#'   is returned unchanged if its variable matches.
#' @param variable `"depth"` or `"velocity"`.
#'
#' @return A data frame of class `hsi_curve` with columns `<variable>` and
#'   `hsi`, or `NULL` with a message if the table cannot be used (fewer than
#'   two complete rows, or negative variable values).
#' @export
#' @examples
#' depth_hsi <- data.frame(depth = c(0, 0.06, 0.3, 0.61, 0.91),
#'                         hsi   = c(0, 0.15, 1, 0.7, 0.4))
#' prepare_hsi(depth_hsi, "depth")
prepare_hsi <- function(x, variable = c("depth", "velocity")) {
  variable <- match.arg(variable)
  if (inherits(x, "hsi_curve")) {
    if (!identical(attr(x, "variable"), variable)) {
      return(null_with_message(sprintf("prepare_hsi: curve is for `%s`, not `%s`.",
                          attr(x, "variable"), variable)))
    }
    return(x)
  }
  if (!is.data.frame(x) || ncol(x) < 2L) {
    return(null_with_message("prepare_hsi: `x` must be a data frame with at least two columns."))
  }
  nms <- tolower(names(x))
  vcol <- match(variable, nms)
  hcol <- match("hsi", nms)
  if (is.na(vcol) || is.na(hcol)) {
    message(sprintf("prepare_hsi: columns `%s` and `hsi` not both found; using column 1 as %s and column 2 as hsi.",
                    variable, variable))
    vcol <- 1L; hcol <- 2L
  }
  v <- suppressWarnings(as.numeric(x[[vcol]]))
  h <- suppressWarnings(as.numeric(x[[hcol]]))
  keep <- !is.na(v) & !is.na(h)
  if (any(!keep)) {
    message(sprintf("prepare_hsi: dropped %d row(s) with missing or non-numeric values.", sum(!keep)))
  }
  v <- v[keep]; h <- h[keep]
  if (length(v) < 2L) {
    return(null_with_message("prepare_hsi: at least two complete rows are required."))
  }
  if (any(v < 0)) {
    return(null_with_message(sprintf("prepare_hsi: %s values must be non-negative.", variable)))
  }
  if (any(h < 0 | h > 1)) {
    message("prepare_hsi: hsi values outside [0, 1] were clamped to that range.")
    h <- pmin(pmax(h, 0), 1)
  }
  o <- order(v); v <- v[o]; h <- h[o]
  if (anyDuplicated(v)) {
    message("prepare_hsi: duplicate variable values collapsed to their mean hsi.")
    h <- as.numeric(tapply(h, v, mean))
    v <- sort(unique(v))
    if (length(v) < 2L) {
      return(null_with_message("prepare_hsi: at least two distinct variable values are required."))
    }
  }
  out <- data.frame(v, hsi = h)
  names(out)[1] <- variable
  structure(out, class = c("hsi_curve", "data.frame"), variable = variable)
}

# Linear interpolation, flat beyond the documented range.
hsi_at <- function(curve, values) {
  approx(curve[[1]], curve$hsi, xout = values, rule = 2, ties = mean)$y
}
