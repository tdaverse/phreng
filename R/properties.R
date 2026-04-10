#' @include aaa.r

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
