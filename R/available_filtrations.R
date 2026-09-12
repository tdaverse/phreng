#' @title Available Filtrations
#'
#' @description Lists all filtrations supported by each engine
#'
#' @return Dataframe with one row per filtration and a column with supported
#' engines
#'
#' @include engine_metadata.R
#'
#' @export

available_filtrations <- function() {

  df <- unique(engine_metadata[, c("filtration", "engine")])

  df
}
