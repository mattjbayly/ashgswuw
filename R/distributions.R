# Relative depth and velocity frequency distributions (Schweizer et al. 2007,
# as used by ASHGS: McParland et al. 2016, eqs 8-10).
#
# Each relative variable r = d / dbar or v / vbar follows a mixture
#   f(r) = (1 - Smix) N(r; 1, 0.52^2) + Smix LN(r; meanlog, sdlog)
# with a single Froude-dependent mixing weight
#   logit(Smix) = -4.72 - 2.84 ln(Fr).
#
# The source parameters are ARITHMETIC mean 1 and SD 1.09 (depth) / 1.19
# (velocity) of the unlogged relative variable. plnorm() takes meanlog and
# sdlog, so they are converted:
#   sdlog   = sqrt(log(1 + (SD / mean)^2))
#   meanlog = log(mean) - sdlog^2 / 2     (= -sdlog^2 / 2 for mean 1)
# The archived Preference.m passed 0 and 1.09 straight to lognpdf() as
# log-scale parameters (giving an arithmetic mean of about 1.8), and built the
# velocity mixture from the depth variables. Neither is reproduced here.

.sd_normal      <- 0.52
.sdlog_depth    <- sqrt(log(1 + 1.09^2))
.sdlog_velocity <- sqrt(log(1 + 1.19^2))

mixture_weight <- function(froude) {
  plogis(-4.72 - 2.84 * log(froude))
}

mixture_cdf <- function(r, smix, sdlog) {
  (1 - smix) * pnorm(r, mean = 1, sd = .sd_normal) +
    smix * plnorm(r, meanlog = -sdlog^2 / 2, sdlog = sdlog)
}

# Integrate the mixture into n_bins equal bins over relative values
# [0, rel_max]. Probability below 0 (negative tail of the normal component)
# and above rel_max is dropped and reported; remaining mass is renormalised.
bin_probabilities <- function(n_bins, rel_max, smix, sdlog) {
  edges <- seq(0, rel_max, length.out = n_bins + 1)
  cdf <- mixture_cdf(edges, smix, sdlog)
  raw <- diff(cdf)
  list(rel_lower = edges[-(n_bins + 1)],
       rel_upper = edges[-1],
       rel_centre = (edges[-1] + edges[-(n_bins + 1)]) / 2,
       prob = raw / sum(raw),
       excluded_below = cdf[1],
       excluded_above = 1 - cdf[n_bins + 1])
}
