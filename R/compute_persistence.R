#' @include aaa.R
#' @include ph_classes.R

class_dist <- new_S3_class("dist")

#' @title Compute Persistent Homology
#'
#' @description This function is an S7 generic which dispatches based on
#'  the user specification and the class of the data. The function standardizes
#'  output by converting it to a `persistence` object.
#'
#' @param object user specification
#' @param data object on which to compute persistent homology
#' @param ... additional engine-specific arguments
#'
#' @return class `persistence` object
#' @export
compute_persistence <- new_generic("compute_persistence", c("object","data"))


# REVIEW: Can we set up conditionals to use an alternative package if the
# requested package is not installed? (Should we do this?)

method(compute_persistence, list(PH_pointcloud, class_dist)) <- function(object, data) {
  check_packages(object)
  res <- NULL
  if (object@engine == "ripserr") {
    res <- ripserr::vietoris_rips(
      data,
      max_dim = object@max_dimension,
      threshold = ifelse(is.na(object@max_diameter), max(data), object@max_diameter)
    )
  }
  else if (object@engine == "TDA") {
    if (object@filtration == "vietoris_rips") {
      res <- TDA::ripsDiag(
        data,
        library = ifelse(is.na(object@library), "GUDHI", object@library),
        maxdimension = object@max_dimension,
        dist = "arbitrary",
        maxscale = ifelse(is.na(object@max_diameter), max(data), object@max_diameter)
      )
    }
    if (object@filtration == "alpha_complex") {
      res<- TDA::alphaComplexDiag(
        data,
        library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
        maxdimension = object@max_dimension
      )
    }
    if (object@filtration == "alpha_shape") {
      res <- TDA::alphaShapeDiag(
        data,
        library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
        maxdimension = object@max_dimension
      )
    }
  }
  res <- as_persistence(res)
  res
}

method(compute_persistence, list(PH_pointcloud, class_double)) <- function(object, data) {
  check_packages(object)
  res <- NULL
  if (is.matrix(data) | is.array(data)) {
    if (object@engine == "ripserr") {
      res <- ripserr::vietoris_rips(
        data,
        max_dim = object@max_dimension,
        threshold = ifelse(is.na(object@max_diameter), max(dist(data)), object@max_diameter)
      )
    }
    else if (object@engine == "TDA") {
      if (object@filtration == "vietoris_rips") {
        res <- TDA::ripsDiag(
          data,
          library = ifelse(is.na(object@library), "GUDHI", object@library),
          maxdimension = object@max_dimension,
          maxscale = ifelse(is.na(object@max_diameter), max(dist(data)), object@max_diameter)
        )
      }
      if (object@filtration == "alpha_complex") {
        res <- TDA::alphaComplexDiag(
          data,
          library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
          maxdimension = object@max_dimension
        )
      }
      if (object@filtration == "alpha_shape") {
        res <- TDA::alphaShapeDiag(
          data,
          library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
          maxdimension = object@max_dimension
        )
      }
    }
    res <- as_persistence(res)
    res
  }
  else {
    stop("Data must be a matrix or an array for PH_pointcloud")
  }
}

method(compute_persistence, list(PH_raster, class_double)) <- function(object, data) {
  check_packages(object)
  res <- NULL
  if (is.matrix(data) | is.array(data)) {
    if (object@engine == "ripserr") {
      res <- ripserr::cubical(
        data,
        threshold = ifelse(is.na(object@max_scale), max(dist(data)), object@max_scale)
      )
    }
    else if (object@engine == "TDA") {
      res <- TDA::gridDiag(
        FUNvalues = data,
        library = ifelse(is.na(object@library), "GUDHI", object@library),
        sublevel = object@sublevel,
        maxdimension = object@max_dimension
      )
    }
    res <- as_persistence(res)
    res
  }
  else {
    stop("Data must be a matrix or an array for PH_pointcloud")
  }
}
