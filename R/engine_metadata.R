#' @title Engine Metadata
#'
#' @description This function calls the metadata of each filtration
#'
#' @noRd

engine_metadata <- read.delim(
  system.file("engine_metadata.tsv", package = "phreng"),
  stringsAsFactors = FALSE
)
