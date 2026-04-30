#' @include aaa.R
#' @importFrom snakecase to_snake_case
#' @importFrom stringr str_detect


vietoris_rips_variations <- c(
  "vietoris_rips",
  "vietorisrips",
  "vietoris",
  "rips"
)

alpha_shape_variations <- c(
  "alpha_shape",
  "alphashape"
)

alpha_complex_variations <- c(
  "alpha_complex",
  "alphacomplex"
)

valid_filtrations <- c(
  "vietoris_rips",
  "cubical",
  "alpha_shape",
  "alpha_complex"
)

filtration_type_point_cloud <- new_property(
  class = class_character,
  validator = function(value) {
    val <- snakecase::to_snake_case(value)
    if (!(val %in% valid_filtrations ||
          val %in% vietoris_rips_variations ||
          val %in% alpha_complex_variations ||
          val %in% alpha_shape_variations)) {
      "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
    }
  },
  setter = function(self, value) {
    value <- snakecase::to_snake_case(value)
    if (value %in% vietoris_rips_variations) {
      value <- "vietoris_rips"
    } else if (value %in% alpha_complex_variations) {
      value <- "alpha_complex"
    } else if (value %in% alpha_shape_variations) {
      value <- "alpha_shape"
    }
    self@filtration <- value
    self
  },

  default = "vietoris_rips"
)


filtration_type_raster <- new_property(
  class = class_character,
  validator = function(value) {
    val <- snakecase::to_snake_case(value)
    if (!(val %in% valid_filtrations ||
          val %in% vietoris_rips_variations ||
          val %in% alpha_complex_variations ||
          val %in% alpha_shape_variations)) {
      "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
    }
  },
  setter = function(self, value) {
    value <- snakecase::to_snake_case(value)
    if (value %in% vietoris_rips_variations) {
      value <- "vietoris_rips"
    } else if (value %in% alpha_complex_variations) {
      value <- "alpha_complex"
    } else if (value %in% alpha_shape_variations) {
      value <- "alpha_shape"
    }
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
