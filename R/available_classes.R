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
  if (! is.null(filtration)) {
    rlang::arg_match0(filtration, unique(engine_metadata$filtration))
  }
  if (! is.null(input)) {
    rlang::arg_match0(input, unique(engine_metadata$input))
  }

  supported <- lookup_metadata(filtration, input)

  if (nrow(supported) == 0) {
    msg <- "No class is available for the requested filtration and input."
    if (! is.null(filtration)) {
      msg <- c(msg, "x" = "Filtration: {.val {filtration}}.")
    }
    if (! is.null(input)) {
      msg <- c(msg, "x" = "Input: {.val {input}}.")
    }
    msg <- c(
      msg,
      "i" = "See supported options with {.fn available_filtrations} and {.fn available_inputs}."
    )
    cli::cli_abort(msg, class = "phreng_error_no_class")
  }

  unique(supported$class)
}
