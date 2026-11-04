#' @title Engine Metadata
#'
#' @description This function calls the metadata of each filtration
#'
#' @noRd

engine_metadata <- read.delim(
  system.file("engine_metadata.tsv", package = "phreng"),
  stringsAsFactors = FALSE
)

lookup_metadata <- function(filtration = NULL, input = NULL) {
  supported <- engine_metadata
  if (! is.null(filtration)) {
    supported <- supported[supported$filtration == filtration, ]
  }
  if (! is.null(input)) {
    supported <- supported[supported$input == input, ]
  }
  supported
}
