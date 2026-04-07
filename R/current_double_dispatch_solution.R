#' include aaa.r

filtration_type <- new_property(
  class = class_character,
  validator = function(value) {
    valid_filtrations <- c("vietoris_rips", "cubical", "alpha_shape", "alpha_complex")
    val <- to_snake_case(value)
    if (!(TRUE %in% str_detect(valid_filtrations, val)))
      "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
  },
  default = "vietoris_rips"
)

max_dimension_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (!is.na(value) & value < 0)
      "must be a non-negative integer"
    if (value %% 1 != 0)
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
  class = class_double,
  default = NA_real_)


max_radius_type <- new_property(
  class = class_double,
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

max_scale_type = new_property(
  class = class_double,
  default = NA_real_
)

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
#'
#' @examples
#'
#' pc <- PH_pointcloud(filtration = "vietoris_rips",
#'               engine = "ripserr",
#'               max_dimension = 1,
#'               max_diameter = 1)
#'
#' r <- PH_raster(filtration = "cubical",
#'           engine = "TDA",
#'           library = "GUDHI",
#'           max_dimension = 2,
#'           max_scale = 10,
#'           sublevel = TRUE)
#' @rdname ph_classes
#' @export
PH <- new_class("PH",
                properties = list(
                  filtration = filtration_type,
                  engine = engine_type,
                  library = library_type,
                  max_dimension = max_dimension_type),
                validator = function(self) {
                  if (self@engine == "ripserr" & !is.na(self@library) ){
                    sprintf("Library is only defined when engine is TDA. Please leave library blank or NA_character_ when using the ripserr engine.")
                  }
                }
)

#' @param max_diameter maximum threshold for rips filtration (point clouds)
#' @rdname ph_classes
#' @export
PH_pointcloud <-  new_class("PH_pointcloud", parent = PH,
                            properties = list(
                              max_radius = max_radius_type,
                              max_diameter= max_diameter_type),
                            validator = function(self) {
                              if (self@engine == "ripserr" & (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                                sprintf("Alpha complexes are only defined for the engine TDA using point clouds. Please use library TDA for any alpha filtration")
                              }
                            }
)

#' @param max_scale maximum threshold for rips filtration (rasters)
#' @param sublevel specifies sublevel or superlevel filtration
#' @rdname ph_classes
#' @export
PH_raster <- new_class("PH_raster", parent = PH,
                       properties = list(
                         max_scale = max_scale_type,
                         sublevel = sublevel_type),
                       validator = function(self) {
                         if (self@filtration != "cubical") {
                           sprintf("Only cubical filtrations are allowed for raster objects")
                         }
                       }
)

dist_class <- new_S3_class("dist")

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
#'
#' @examples
#'
#' obj <- PH_pointcloud(filtration = "vietoris_rips",
#'               engine = "ripserr",
#'               max_dimension = 1,
#'               max_diameter = 1)
#' data <- matrix(rnorm(30), ncol = 3)
#' compute_persistence(obj, data)
#'
#' obj <- PH_raster(filtration = "cubical",
#'           engine = "TDA",
#'           library = "GUDHI",
#'           max_dimension = 2,
#'           max_scale = 10,
#'           sublevel = TRUE)
#' data <- volcano
#' compute_persistence(obj, data)
#' @export
compute_persistence <- new_generic("compute_persistence", c("object","data"))

method(compute_persistence, list(PH_pointcloud, dist_class)) <- function(object, data) {
  res <- NULL
  if (object@engine == "ripserr") {
    res <-  vietoris_rips(
      data,
      max_dim = object@max_dimension,
      threshold = ifelse(is.na(object@max_diameter), max(data), object@max_diameter)
    ) |> as_persistence()
  }
  else if (object@engine == "TDA") {
    if (object@filtration == "vietoris_rips") {
      res<- ripsDiag(
        data,
        library = ifelse(is.na(object@library), "GUDHI", object@library),
        maxdimension = object@max_dimension,
        dist = "arbitrary",
        maxscale = ifelse(is.na(object@max_diameter), max(data), object@max_diameter)
      ) |> as_persistence()
    }
    if (object@filtration == "alpha_complex") {
      res<- alphaComplexDiag(
        data,
        library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
        maxdimension = object@max_dimension
      ) |> as_persistence()
    }
    if (object@filtration == "alpha_shape") {
      res<-alphaShapeDiag(
        data,
        library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
        maxdimension = object@max_dimension
      ) |> as_persistence()
    }
  }
  res
}

method(compute_persistence, list(PH_pointcloud, class_double)) <- function(object, data) {
  res <- NULL
  if (is.matrix(data) | is.array(data)) {
    if (object@engine == "ripserr") {
      res <- vietoris_rips(
        data,
        max_dim = object@max_dimension,
        threshold = ifelse(is.na(object@max_diameter), max(dist(data)), object@max_diameter)
      ) |> as_persistence()
    }
    else if (object@engine == "TDA") {
      if (object@filtration == "vietoris_rips") {
        res <- ripsDiag(
          data,
          library = ifelse(is.na(object@library), "GUDHI", object@library),
          maxdimension = object@max_dimension,
          maxscale = ifelse(is.na(object@max_diameter), max(dist(data)), object@max_diameter)
        ) |> as_persistence()
      }
      if (object@filtration == "alpha_complex") {
        res<- alphaComplexDiag(
          data,
          library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
          maxdimension = object@max_dimension
        ) |> as_persistence()
      }
      if (object@filtration == "alpha_shape") {
        res <- alphaShapeDiag(
          data,
          library = ifelse(is.na(object@library), c("GUDHI", "Dionysus"), object@library),
          maxdimension = object@max_dimension
        ) |> as_persistence()
      }
    }
    res
  }
  else {
    return("Data must be a matrix or an array for PH_pointcloud")
  }
}

method(compute_persistence, list(PH_raster, class_double)) <- function(object, data) {
  res <- NULL
  if (is.matrix(data) | is.array(data)) {
    if (object@engine == "ripserr") {
      res <- cubical(
        data,
        threshold = ifelse(is.na(object@max_scale), max(dist(data)), object@max_scale)
      ) |> as_persistence()
    }
    else if (object@engine == "TDA") {
      res <- gridDiag(
        FUNvalues = data,
        library = ifelse(is.na(object@library), "GUDHI", object@library),
        sublevel = object@sublevel,
        maxdimension = object@max_dimension
      ) |> as_persistence()
    }
    res
  }
  else {
    return("Data must be a matrix or an array for PH_pointcloud")
  }
}
