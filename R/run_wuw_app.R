#' Launch the interactive WUW explorer
#'
#' Opens a Shiny application bundled with the package. Users enter the reach
#' inputs, paste depth and velocity HSI tables as comma-separated text, and
#' view the equivalent cross-section, habitat-flow curves, hydraulics,
#' depth/velocity frequency distributions, bin tables and the model
#' assumptions. Requires the `shiny` package.
#'
#' @param ... Passed to [shiny::runApp()], e.g. `port` or `launch.browser`.
#' @return Called for its side effect; returns whatever [shiny::runApp()]
#'   returns when the app is closed.
#' @export
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   run_wuw_app()
#' }
run_wuw_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the app. Install it with install.packages(\"shiny\").",
         call. = FALSE)
  }
  app_dir <- wuw_app_dir()
  if (app_dir == "") {
    stop("Could not find the bundled app directory. Try re-installing 'ashgswuw'.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal", ...)
}

# Location of the bundled app (exported for tests and curious users).
wuw_app_dir <- function() {
  system.file("shiny-examples", "wuw_app", package = "ashgswuw")
}
