#' @title Available Engines
#'
#' @description Lists the engines available for a filtration and input type
#'
#' @param filtration Character string specifying filtration
#' @param input Character string specifying input type
#'
#' @include engine_metadata.R
#'
#' @return Character vector that has the available engines
#'
#' @export

available_engines <- function(filtration, input = NULL) {
  supported <- engine_metadata[
    engine_metadata$filtration == filtration,
  ]

  if(!is.null(input)) {
    supported <- supported[
      supported$input == input,
    ]
  }

  if (nrow(supported) == 0) {
    stop("No engine supports the requested filtration and/or input.
         Try available_filtrations() or available_inputs() to see supported options.")
  }
  unique(supported$engine)
}
