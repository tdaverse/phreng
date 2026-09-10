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
  engines <- available_engines(
    filtration = filtration,
    input = input
  )

  if (length(engines) == 0) {
    message("No engine exists for the requested filtration and input.
            Try available_filtrations() or available_inputs() to see supported options.")
    return(NULL)
  }

  if (filtration == "vietoris_rips" && "ripserr" %in% engines) {
    return("ripserr")
  }

  if (length(engines) == 1) {
    return(engines)
  }

  message("Multiple engines are available, but no recommended engine exists: ",
          paste(engines, collapse = ", "),
          ".")
}
