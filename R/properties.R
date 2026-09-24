#' @include aaa.R
#' @importFrom stringr str_detect

distances_filtrations <- c(
  "vietoris_rips"
)

point_cloud_filtrations <- c(
  "cech",
  "alpha",
  distances_filtrations
)

grid_filtrations <- c(
  "cubical",
  "triangulation"
)

valid_filtrations <- c(
  distances_filtrations,
  point_cloud_filtrations,
  grid_filtrations
)

filtration_type_distances <- new_property(
  class = class_character,
  validator = function(value) {
    if (! value %in% distances_filtrations) {
      paste("must be one of", paste(distances_filtrations, collapse = ", "))
    }
  },
  setter = function(self, value) {
    self@filtration <- value
    self
  },

  default = "vietoris_rips"
)

filtration_type_point_cloud <- new_property(
  class = class_character,
  validator = function(value) {
    if (! value %in% point_cloud_filtrations) {
      paste("must be one of", paste(point_cloud_filtrations, collapse = ", "))
    }
  },
  setter = function(self, value) {
    self@filtration <- value
    self
  },

  default = "alpha"
)

filtration_type_grid <- new_property(
  class = class_character,
  validator = function(value) {
    if (! value %in% grid_filtrations) {
      paste("must be one of", paste(grid_filtrations, collapse = ", "))
    }
  },
  setter = function(self, value) {
    self@filtration <- value
    self
  },

  default = "cubical"
)

max_dimension_type <- new_property(
  class = class_numeric,
  validator = function(value) {
    if (!is.na(value) & value < 0) {
      "must be a non-negative integer."
    } else if (value %% 1 != 0) {
      "must be a non-negative integer."
    }
  },
  default = 1
)

engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (!(value %in% c("TDA", "ripserr"))) {
      "must be `TDA` or `ripserr`."
    }
  },
  default = "TDA"
)

library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (!(value %in% c("GUDHI", "PHAT", "Dionysus", NA_character_))) {
      "must be `GUDHI`, `PHAT`, or `Dionysus` or `NA_character_`"
    }
  },
  default = NA_character_
)

max_diameter_type <- new_property(
  class = class_numeric,
  default = NA_real_
)

max_radius_type <- new_property(
  class = class_numeric,
  getter = function(self) {
    self@max_diameter / 2
  }
)

sublevel_type <- new_property(
  class = class_logical,
  validator = function(value) {
    if (length(value) != 1 || is.na(value)) {
      "sublevel must be either TRUE or FALSE."
    }
  },
  default = TRUE
)

max_scale_type <- new_property(
  class = class_numeric,
  default = NA_real_
)
