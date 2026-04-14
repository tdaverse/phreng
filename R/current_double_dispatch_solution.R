# ADVICE: Break up source code into thematic / coherent chunks.

# REVIEW: Check that `@` is used where needed, e.g. `include`.

#' @include aaa.R

filtration_type <- new_property(
  class = class_character,
  validator = function(value) {
    valid_filtrations <- c("vietoris_rips", "cubical", "alpha_shape", "alpha_complex")
    val <- snakecase::to_snake_case(value)
    if (!(TRUE %in% stringr::str_detect(valid_filtrations, val)))
      "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
  },
  default = "vietoris_rips"
)


max_dimension_type <- new_property(
  class = class_numeric,
  validator = function(value) {
    if (!is.na(value) & value < 0)
      "must be a non-negative integer"
    else if (value %% 1 != 0)
      "must be a non-negative integer"
  },
  default = 1
)

engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("TDA","ripserr")) )
      "must be TDA or ripserr"
  },
  default ="ripserr"
)

library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus", NA_character_)) )
      "must be GUDHI, PHAT, or Dionysus or NA_character_"
  },
  default = NA_character_
)

max_diameter_type <- new_property(
  class = class_numeric,
  default = NA_real_)


max_radius_type <- new_property(
  class = class_numeric,
  getter = function(self) {
    self@max_diameter/2
  }
)

sublevel_type <- new_property(
  class = class_logical,
  validator = function(value) {
    if (length(value) != 1 || is.na(value))
      "sublevel must be either TRUE or FALSE."
  },
  default = TRUE
)

max_scale_type <- new_property(
  class = class_numeric,
  default = NA_real_
)

# ADVICE: Decide whether to include examples in {roxygen2} comments (below) or
# in separate files in 'inst/examples' (not both). Figure out which of
# `@example` or `@examples` to use.

# ADVICE: Specify input type for each argument, e.g.
# #' @param filtration Character; the ....

# ADVICE: In source code, use line breaks to left-justify function bodies.

#' @title Classes for persistent homology specifications
#'
#' @description These class define the parameters needed for persistence
#'  calculations and allow for reuse of specification for different data sets.
#'  The parent class, `PH`, stores core attributes that are common among all
#'  persistence calculations. The point cloud subclass is used to handle
#'  distance matrices and point cloud arrays. The raster subclass is used
#'  to handle grid data encoded as matrices or arrays.
#'
#' @param filtration filtration used in persistence calculation
#' @param engine back-end engine used in persistence calculation
#' @param library c++ library used for TDA engine
#' @param max_dimension maximum homological dimension to compute persistence
#'
#' @return S7 object storing user specification for calculating persistence
#' @rdname ph_classes
#' @export
PH <- new_class(
  "PH",
  properties = list(
    filtration = filtration_type,
    engine = engine_type,
    library = library_type,
    max_dimension = max_dimension_type),
  validator = function(self) {
    if (self@engine == "ripserr" & !is.na(self@library) ){
      paste0("Library is only defined when engine is TDA. Please leave library ",
             "blank or NA_character_ when using the ripserr engine.")
    }
  }
)

#' @param max_diameter maximum threshold for rips filtration (point clouds)
#' @rdname ph_classes
#' @export
PH_pointcloud <-  new_class("PH_pointcloud",
                            parent = PH,
                            properties = list(
                              max_radius = max_radius_type,
                              max_diameter= max_diameter_type),
                            validator = function(self) {
                              if (self@engine == "ripserr" &
                                  (self@filtration == "alpha_complex" ||
                                   self@filtration=="alpha_shape")){
                                paste0(
                                  "Alpha complexes are only defined for the ",
                                  "engine TDA using point clouds. Please use ",
                                  "library TDA for any alpha filtration")
                              }
                            }
)

#' @param max_scale maximum threshold for rips filtration (rasters)
#' @param sublevel specifies sublevel or superlevel filtration
#' @rdname ph_classes
#' @export
PH_raster <- new_class("PH_raster",
                       parent = PH,
                       properties = list(
                         max_scale = max_scale_type,
                         sublevel = sublevel_type),
                       validator = function(self) {
                         if (self@filtration != "cubical") {
                           paste0("Only cubical filtrations are allowed for ",
                                   "raster objects")
                         }
                       }
)

class_dist <- new_S3_class("dist")

#' @title Compute Persistent Homology
#'
#' @description This function is an S7 generic which dispatches based on
#'  the user specification and the class of the data. The function standardizes
#'  output by converting it to a `persistence` object.
#'
#' @param object user specification
#' @param data object on which to compute persistent homology
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
    stop("Data must be a matrix or an array for PH_raster")
  }
}

