#' @title function to check if packages are installed
#'
#' @description This function prints an error when the user does not have the
#'    required packages installed. It directs the user to install the packages.
#'
#' @param object
#' @NoRd



check_packages <- function(object){
  if(object@engine == "ripserr"){
    if (!requireNamespace("ripserr", quietly = TRUE)) {
      stop(
        "Package `ripserr` is required when engine `ripserr` is selected \n",
        "Please install it with install.packages('ripserr')"
      )
    }
  }
  if(object@engine == "TDA"){
    if (!requireNamespace("TDA", quietly = TRUE)) {
      stop(
        "Package `TDA` is required when engine `TDA` is selected \n",
        "Please install it with:\n",
        "install.packages('TDA')"
      )
    }
  }
}
