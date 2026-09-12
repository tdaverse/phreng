#' @title Available Classes
#'
#' @description Lists the classes supported by a filtration and input type
#'
#' @param filtration Character string specifying filtration
#' @param input Character string specifying input type
#'
#' @include engine_metadata.R
#'
#' @return Character vector that has the available classes
#'
#' @export

available_classes <- function(filtration = NULL, input = NULL) {
  supported <- engine_metadata

  if (!is.null(filtration)) {
    supported <- supported[
      supported$filtration == filtration,
    ]
  }

  if (!is.null(input)) {
    supported <- supported[
      supported$input == input,
    ]
  }

  if (nrow(supported) == 0) {
    stop("No class supports the requested filtration and/or input.
         Try available_filtrations() or available_inputs() to see supported options.")
  }
  unique(supported$class)
}
