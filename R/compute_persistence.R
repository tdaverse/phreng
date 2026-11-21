#' @include aaa.R
#' @include Persistence_classes.R

class_dist <- new_S3_class("dist")

#' @title Compute Persistent Homology
#'
#' @description This function is an S7 generic which dispatches based on the
#'   user specification and the class of the data. The function standardizes
#'   output by converting it to a `persistence` object.
#'
#' @param object user specification of class [PersistenceDistances],
#'   [PersistenceCoordinates], or [PersistenceGrid]
#' @param data object on which to compute persistent homology, must be of a
#'   class compatible with object
#' @param ... reserved for future engine-specific arguments; currently must be
#'   empty
#'
#' @return class `persistence` object
#' @export
compute_persistence <- new_generic("compute_persistence", c("object", "data"))


# REVIEW: Can we set up conditionals to use an alternative package if the
# requested package is not installed? (Should we do this?)

method(
  compute_persistence,
  list(PersistenceDistances, class_dist)
) <- function(object, data, ...) {
  rlang::check_dots_empty(call = rlang::caller_env(2))
  check_packages(object)
  res <- NULL
  if (object@engine == "ripserr") {
    res <- ripserr::vietoris_rips(
      data,
      max_dim = object@max_dimension,
      threshold = ifelse(is.na(object@max_diameter),
        max(data),
        object@max_diameter
      )
    )
  } else if (object@engine == "TDA") {
    res <- TDA::ripsDiag(
      data,
      library = ifelse(is.na(object@library), "Dionysus", object@library),
      maxdimension = object@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(object@max_diameter),
        max(data),
        object@max_diameter
      )
    )
  }
  res <- as_persistence(res)
  res
}

method(
  compute_persistence,
  list(PersistenceDistances, class_double)
) <- function(object, data, ...) {
  if (! is.matrix(data)) {
    rlang::check_dots_empty(call = rlang::caller_env(2))
    cli::cli_abort(
      c(
        "Data must be a {.cls dist} object or a coordinate {.cls matrix} for {.fn PersistenceDistances}."
      ),
      class = "phreng_error_bad_data"
    )
  }
  if (isSymmetric(data)) {
    cli::cli_warn(
      c(
        "{.arg data} was passed a symmetric matrix.",
        "i" = "Pass {.cls dist} objects directly, not through {.fn as.matrix}."
      )
    )
  }
  compute_persistence(
    object,
    stats::dist(data, method = "euclidean")
  )
}

method(
  compute_persistence,
  list(PersistenceCoordinates, class_double)
) <- function(object, data, ...) {
  rlang::check_dots_empty(call = rlang::caller_env(2))
  check_packages(object)
  res <- NULL
  if (is.matrix(data) || is.array(data)) {
    # TODO: Store a table like `engine_metadata` for engine--filtration distance
    # options.
    if (object@filtration == "alpha" &&
      object@distance_metric != "euclidean") {
      stop(
        "Engines only support alpha filtrations using euclidean distance."
      )
    }
    data_dist <- NULL
    if (object@filtration == "vietoris_rips" &&
      object@distance_metric != "euclidean") {
      data_dist <- stats::dist(data, method = object@distance_metric)
    }
    if (object@engine == "ripserr") {
      res <- ripserr::vietoris_rips(
        if (is.null(data_dist)) data else data_dist,
        max_dim = object@max_dimension,
        threshold = ifelse(
          is.na(object@max_diameter),
          if (is.null(data_dist)) max(dist(data)) else max(data_dist),
          object@max_diameter
        )
      )
    } else if (object@engine == "TDA") {
      if (object@filtration == "vietoris_rips") {
        res <- TDA::ripsDiag(
          if (is.null(data_dist)) data else data_dist,
          dist = if (is.null(data_dist)) "euclidean" else "arbitrary",
          library = ifelse(is.na(object@library), "GUDHI", object@library),
          maxdimension = object@max_dimension,
          maxscale = ifelse(
            is.na(object@max_diameter),
            if (is.null(data_dist)) max(dist(data)) else max(data_dist),
            object@max_diameter
          )
        )
      }
      if (object@filtration == "alpha") {
        if (NCOL(data) == 3) {
          res <- TDA::alphaShapeDiag(
            data,
            library = ifelse(is.na(object@library),
                             c("GUDHI", "Dionysus"),
                             object@library
            ),
            maxdimension = object@max_dimension
          )
        } else {
          res <- TDA::alphaComplexDiag(
            data,
            library = ifelse(is.na(object@library),
                             c("GUDHI", "Dionysus"),
                             object@library
            ),
            maxdimension = object@max_dimension
          )
        }
        if (! is.na(object@max_diameter)) {
          cli::cli_warn(
            c(
              "{.arg max_diameter} is not supported for {.val alpha} filtrations.",
              "i" = "The supplied value will be ignored."
            ),
            class = "phreng_warning_max_diameter_ignored"
          )
        }
      }
    }
    res <- as_persistence(res)
    res
  } else {
    cli::cli_abort(
      "Data must be a matrix or an array for {.fn PersistenceCoordinates}.",
      class = "phreng_error_bad_data"
    )
  }
}

method(
  compute_persistence,
  list(PersistenceGrid, class_double)
) <- function(object, data, ...) {
  rlang::check_dots_empty(call = rlang::caller_env(2))
  check_packages(object)
  res <- NULL
  if (is.matrix(data) || is.array(data)) {
    if (object@engine == "ripserr") {
      if (object@sublevel) {
        res <- ripserr::cubical(
          data,
          threshold = ifelse(is.na(object@max_scale),
            max(dist(data)),
            object@max_scale
          )
        )
      } else {
        res <- ripserr::cubical(
          -data,
          threshold = ifelse(is.na(object@max_scale),
                             max(dist(data)),
                             object@max_scale
          )
        )
        res <- as.data.frame(res)
        res <- transform(
          res,
          birth = -birth,
          death = -death
        )
      }
    } else if (object@engine == "TDA") {
      res <- TDA::gridDiag(
        FUNvalues = data,
        library = ifelse(is.na(object@library), "GUDHI", object@library),
        sublevel = object@sublevel,
        maxdimension = object@max_dimension
      )
    }
    res <- as_persistence(res)
    res
  } else {
    cli::cli_abort(
      "Data must be a matrix or an array for {.fn PersistenceGrid}.",
      class = "phreng_error_bad_data"
    )
  }
}
