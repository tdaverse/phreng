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
  if (! is.null(filtration)) {
    rlang::arg_match0(filtration, unique(engine_metadata$filtration))
  }

  supported <- lookup_metadata(filtration)

  if (nrow(supported) == 0) {
    cli::cli_abort(
      c(
        "No input is available for the requested filtration.",
        "x" = "Filtration: {.val {filtration}}.",
        "i" = "See supported filtrations with {.fn available_filtrations}."
      ),
      class = "phreng_error_no_input"
    )
  }

  unique(supported$input)
}
