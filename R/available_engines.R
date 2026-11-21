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
  rlang::arg_match0(filtration, unique(engine_metadata$filtration))
  if (! is.null(input)) {
    rlang::arg_match0(input, unique(engine_metadata$input))
  }

  engines <- unique(lookup_metadata(filtration, input)$engine)

  if (length(engines) == 0) {
    msg <- c(
      "No engine is available for the requested filtration and input.",
      "x" = "Filtration: {.val {filtration}}."
    )
    if (! is.null(input)) {
      msg <- c(msg, "x" = "Input: {.val {input}}.")
    }
    msg <- c(
      msg,
      "i" = "See supported options with {.fn available_filtrations} and {.fn available_inputs}."
    )
    cli::cli_abort(msg, class = "phreng_error_no_engine")
  }

  engines
}
