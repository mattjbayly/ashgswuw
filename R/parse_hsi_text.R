#' Parse a pasted HSI table from text
#'
#' Turns a block of delimited text, such as a table copied from a spreadsheet
#' or a CSV file, into a cleaned `hsi_curve` via [prepare_hsi()]. Used by the
#' Shiny app ([run_wuw_app()]) but also handy at the console.
#'
#' @details
#' Commas, semicolons, tabs and runs of spaces are all accepted as delimiters.
#' Blank lines and empty trailing columns (from lines ending in a comma) are
#' ignored. A header row is detected when the first non-blank line contains a
#' letter; otherwise the first column is taken as the variable and the second
#' as `hsi`.
#'
#' @param text A single string (may contain newlines) or a character vector of
#'   lines.
#' @param variable `"depth"` or `"velocity"`.
#' @return An `hsi_curve` data frame, or `NULL` with a message.
#' @export
#' @examples
#' parse_hsi_text("velocity,hsi,
#' 0,0,
#' 0.03,1,
#' 0.06,1,
#' 0.12,0.7,
#' 0.18,0.4", "velocity")
parse_hsi_text <- function(text, variable = c("depth", "velocity")) {
  variable <- match.arg(variable)
  if (!is.character(text) || length(text) == 0L) {
    return(null_with_message("parse_hsi_text: `text` must be character."))
  }
  lines <- unlist(strsplit(paste(text, collapse = "\n"), "\r?\n"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) < 2L) {
    return(null_with_message("parse_hsi_text: need at least two non-blank lines (header optional)."))
  }
  # Normalise delimiters to a comma, then drop empty trailing fields.
  lines <- gsub("[;\t]+", ",", lines)
  lines <- gsub(" {2,}", ",", lines)
  lines <- gsub("^,+|,+$", "", lines)
  lines <- gsub(" *, *", ",", lines)
  header <- grepl("[A-Za-z]", lines[1])
  fields <- strsplit(lines, ",", fixed = TRUE)
  ncol_min <- min(lengths(fields))
  if (ncol_min < 2L) {
    return(null_with_message("parse_hsi_text: every line must have at least two fields."))
  }
  body <- if (header) fields[-1] else fields
  if (length(body) < 2L) {
    return(null_with_message("parse_hsi_text: need at least two data rows."))
  }
  if (header) {
    nms <- tolower(trimws(fields[[1]]))
    vcol <- match(variable, nms)
    hcol <- match("hsi", nms)
    if (is.na(vcol) || is.na(hcol)) { vcol <- 1L; hcol <- 2L }
  } else {
    vcol <- 1L; hcol <- 2L
  }
  get <- function(i) vapply(body, function(f) if (length(f) >= i) f[i] else NA_character_, "")
  df <- data.frame(v = get(vcol), hsi = get(hcol), stringsAsFactors = FALSE)
  names(df)[1] <- variable
  prepare_hsi(df, variable)
}
