#' @title Recommend Engine
#'
#' @description Returns the recommended engine for a filtration and input type
#'
#' @param filtration Character string specifying filtration
#' @param input Character string specifying input type
#'
#' @include engine_metadata.R
#'
#' @return Character string with the recommend engine
#'
#' @export

recommend_engine <- function(filtration, input) {
  rlang::arg_match0(filtration, unique(engine_metadata$filtration))
  rlang::arg_match0(input, unique(engine_metadata$input))

  engines <- unique(lookup_metadata(filtration, input)$engine)

  if (length(engines) == 0) {
    cli::cli_inform(
      c(
        "No engine is available for the requested filtration and input.",
        "x" = "Filtration: {.val {filtration}}.",
        "x" = "Input: {.val {input}}.",
        "i" = "See supported options with {.fn available_filtrations} and {.fn available_inputs}."
      ),
      class = "phreng_message_no_engine"
    )
    return(NULL)
  }

  if (filtration == "vietoris_rips" && "ripserr" %in% engines) {
    return("ripserr")
  }

  if (length(engines) == 1) {
    return(engines)
  }

  cli::cli_inform(
    c(
      "Multiple engines are available for the requested filtration and input: {.val {engines}}.",
      "i" = "Specify an engine explicitly in the specification."
    ),
    class = "phreng_message_no_recommendation"
  )
}
