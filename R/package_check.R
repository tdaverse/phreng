#' @title function to check if packages are installed
#'
#' @description This function prints an error when the user does not have the
#'    required packages installed. It directs the user to install the packages.
#'
#' @param object object to check
#' @noRd


check_packages <- function(object) {
  pkg <- object@engine

  if (!rlang::is_installed(pkg)) {
    cli::cli_abort(
      c(
        "The {.pkg {pkg}} package is required to use the {.val {object@engine}} engine.",
        "i" = "Install it with {.run install.packages(\"{pkg}\")}."
      ),
      class = "phreng_error_missing_package",
      package = pkg,
      call = parent.frame()
    )
  }
}
