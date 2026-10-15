# Equivalent channel section.
#
# The section is a two-segment piecewise-linear realisation of the Ferguson
# (2003) cumulative depth curve used by ASHGS, closed by a vertical bank:
#
#   (0, 0) -> (b * Wb, -db) -> (Wb, -dmax) -> (Wb, 0)
#
# with dmax = db / (1 - b) (McParland et al. 2016, eq. 1 rearranged). Bankfull
# water surface is z = 0. The bankfull flow area of this section equals
# Wb * db exactly, so the supplied width and mean depth are preserved.
#
# NOTE: the archived MATLAB (ASHGS_WUA.m) used dmax = (1 + b) / (1 - b) * db,
# which inflates bankfull area by a factor (1 + b / 2). That form is not used.

build_geometry <- function(bankfull_width_m, bankfull_depth_m, b = NULL) {
  Wb <- bankfull_width_m
  db <- bankfull_depth_m
  b_source <- if (is.null(b)) "estimated" else "user"
  if (is.null(b)) b <- (Wb / db) / 100           # McParland et al. 2016, eq. 2
  if (!is.numeric(b) || length(b) != 1L || is.na(b) || !is.finite(b)) {
    return(list(status = "invalid", message = "`b` must be a single finite number."))
  }
  if (b <= 0 || b >= 1) {
    return(list(status = "b_out_of_range",
                message = sprintf(paste0(
                  "Shape parameter b = %.4f (%s) is outside the valid range 0 < b < 1. ",
                  "With b = (Wb/db)/100 this requires 0 < Wb/db < 100; ",
                  "check the geometry or supply `b` explicitly."), b, b_source)))
  }
  dmax <- db / (1 - b)
  section <- data.frame(x_m = c(0, b * Wb, Wb, Wb),
                        z_m = c(0, -db, -dmax, 0))
  structure(list(bankfull_width_m = Wb,
                 bankfull_depth_m = db,
                 b = b,
                 b_source = b_source,
                 dmax_m = dmax,
                 section = section,
                 status = "ok"),
            class = "ashgs_geometry")
}

# Exact clipping of the section at a water surface drawn down `drawdown_m`
# below bankfull. Each submerged sloping segment contributes its horizontal
# span to wetted width, trapezoidal depth-area to flow area and line length to
# wetted perimeter. The vertical closing bank contributes perimeter only.
geometry_at_stage <- function(geom, drawdown_m) {
  eta <- -drawdown_m
  x <- geom$section$x_m[1:3]
  z <- geom$section$z_m[1:3]
  W <- 0; A <- 0; P <- 0
  for (i in 1:2) {
    x1 <- x[i]; x2 <- x[i + 1]; z1 <- z[i]; z2 <- z[i + 1]
    h1 <- eta - z1; h2 <- eta - z2
    if (h1 <= 0 && h2 <= 0) next
    if (h1 < 0) { t <- h1 / (h1 - h2); x1 <- x1 + t * (x2 - x1); z1 <- eta; h1 <- 0 }
    if (h2 < 0) { t <- h2 / (h2 - h1); x2 <- x2 + t * (x1 - x2); z2 <- eta; h2 <- 0 }
    W <- W + abs(x2 - x1)
    A <- A + abs(x2 - x1) * (h1 + h2) / 2
    P <- P + sqrt((x2 - x1)^2 + (z2 - z1)^2)
  }
  P <- P + max(0, geom$dmax_m - drawdown_m)
  list(wetted_width_m = W,
       flow_area_m2 = A,
       wetted_perimeter_m = P,
       mean_depth_m = if (W > 0) A / W else 0)
}
