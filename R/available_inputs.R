#' @title Available Inputs
#'
#' @description Lists the input types supported by a filtration
#'
#' @param filtration Character string specifying filtration
#'
#' @include engine_metadata.R
#'
#' @return Character vector that has the available input types
#'
#' @export

available_inputs <- function(filtration = NULL) {
  supported <- engine_metadata
  if (!is.null(filtration)) {
    supported <- supported[
      supported$filtration == filtration,
    ]
  }

  if (nrow(supported) == 0) {
    stop("No input supports the requested filtration.
         Try available_filtrations() to see supported filtrations.")
  }
  unique(supported$input)
}
