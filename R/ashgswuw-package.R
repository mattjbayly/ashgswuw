#' ashgswuw: Weighted Usable Width from Reach-Average Channel Geometry
#'
#' An R adaptation of selected functions from the At-a-Station Hydraulic
#' Geometry Simulator (ASHGS), originally written in MATLAB by Dr. Brett Eaton
#' (University of British Columbia) and released under GPL-3 with the
#' UBCRM-ASHGS tools at <https://blogs.ubc.ca/beaton/working-code/>. The
#' method is described in McParland, Eaton and Rosenfeld (2016). Only the
#' hydraulic-geometry and habitat components were adapted, with deliberate
#' corrections listed in the README, so results are not an exact reproduction
#' of the original MATLAB output. Full credit for the method and original code
#' belongs to McParland, Eaton and Rosenfeld; they have not reviewed or
#' endorsed this package, and any errors in the adaptation are the package
#' author's alone.
#'
#' The user supplies bankfull width, mean bankfull depth,
#' reach gradient, D84 and depth/velocity habitat suitability curves; the
#' package returns the wetted width, weighted usable width (WUW) and, when a
#' reach length is given, weighted usable area (WUA) at a chosen discharge.
#'
#' The UBC Regime Model, sediment transport and all plotting in the original
#' MATLAB tools are omitted. Channel geometry is treated as fixed.
#'
#' @section Main functions:
#' * [prepare_hsi()] cleans a two-column habitat suitability table.
#' * [wuw_estimate()] computes WUW at one discharge and returns all
#'   intermediate hydraulic and distribution results.
#' * [wuw_curve()] runs [wuw_estimate()] over a vector of discharges.
#' * [model_assumptions()] lists the explicit model assumptions.
#'
#' @references
#' McParland, D., Eaton, B., & Rosenfeld, J. (2016). At-a-station hydraulic
#' geometry simulator. *River Research and Applications*, 32(3), 399-410.
#' \doi{10.1002/rra.2851}
#'
#' Ferguson, R. (2007). Flow resistance equations for gravel- and boulder-bed
#' streams. *Water Resources Research*, 43, W05427. \doi{10.1029/2006WR005422}
#'
#' Schweizer, S., Borsuk, M. E., Jowett, I., & Reichert, P. (2007). Predicting
#' joint frequency distributions of depth and velocity for instream habitat
#' assessment. *River Research and Applications*, 23(3), 287-302.
#' \doi{10.1002/rra.980}
#'
#' Eaton, B. (n.d.). Working code: UBCRM-ASHGS tools (original MATLAB source
#' of ASHGS, GPL-3). University of British Columbia.
#' <https://blogs.ubc.ca/beaton/working-code/>
#'
#' @keywords internal
#' @importFrom stats approx plnorm plogis pnorm uniroot
"_PACKAGE"
