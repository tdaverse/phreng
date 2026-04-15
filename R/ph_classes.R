#' @include aaa.R
#' @include properties.R

#' @title Classes for persistent homology specifications
#'
#' @description These class define the parameters needed for persistence
#'  calculations and allow for reuse of specification for different data sets.
#'  The parent class, `PH`, stores core attributes that are common among all
#'  persistence calculations. The point cloud subclass is used to handle
#'  distance matrices and point cloud arrays. The raster subclass is used
#'  to handle grid data encoded as matrices or arrays.
#'
#' @param filtration character; filtration used in persistence calculation
#' @param engine character; back-end engine used in persistence calculation
#' @param library character; c++ library used for TDA engine
#' @param max_dimension character; maximum homological dimension to compute persistence
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
      paste0("Library is only defined when engine is `TDA`. Please leave library ",
             "blank or `NA_character_` when using the ripserr engine.")
    }
  }
)

#' @param max_diameter character; maximum threshold for rips filtration (point clouds)
#' @rdname ph_classes
#' @export
PH_pointcloud <- new_class(
  "PH_pointcloud",
  parent = PH,
  properties = list(
    max_radius = max_radius_type,
    max_diameter = max_diameter_type),
  validator = function(self) {
    if (self@engine == "ripserr" &
        (self@filtration == "alpha_complex" ||
         self@filtration=="alpha_shape")){
      paste0(
        "Alpha complexes are only defined for the ",
        "engine `TDA` using point clouds. Please use ",
        "engine `TDA` for any alpha filtration.")
    }
    else if (self@filtration == "cubical"){
      paste0(
        "Cubical filtrations are only defined for ",
        "raster objects. Please select a different ",
        "filtration such as `vietoris_rips`, `alpha_shape` ",
        "or `alpha_complex`.")
    }
  }
)

#' @param max_scale character; maximum threshold for rips filtration (rasters)
#' @param sublevel boolean; specifies sublevel or superlevel filtration
#' @rdname ph_classes
#' @export
PH_raster <- new_class(
  "PH_raster",
  parent = PH,
  properties = list(
    max_scale = max_scale_type,
    sublevel = sublevel_type),
  validator = function(self) {
    if (self@filtration != "cubical") {
      paste0("Only cubical filtrations are allowed for ",
             "raster objects.")
    }
  }
)
