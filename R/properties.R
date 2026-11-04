#' @include aaa.R
#' @importFrom stringr str_detect

distances_filtrations <- c(
  "vietoris_rips"
)

coordinates_filtrations <- c(
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
  coordinates_filtrations,
  grid_filtrations
)

filtration_type_distances <- new_property(
  class = class_character,
  validator = function(value) {
    if (!rlang::is_string(value) || !value %in% distances_filtrations) {
      cli::format_inline(
        "must be one of {.val {distances_filtrations}}, not {.val {value}}."
      )
    }
  },
  setter = function(self, value) {
    self@filtration <- value
    self
  },

  default = "vietoris_rips"
)

filtration_type_coordinates <- new_property(
  class = class_character,
  validator = function(value) {
    if (!rlang::is_string(value) || !value %in% coordinates_filtrations) {
      cli::format_inline(
        "must be one of {.val {coordinates_filtrations}}, not {.val {value}}."
      )
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
    if (!rlang::is_string(value) || !value %in% grid_filtrations) {
      cli::format_inline(
        "must be one of {.val {grid_filtrations}}, not {.val {value}}."
      )
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
    if (!(rlang::is_scalar_integer(value) ||
      rlang::is_scalar_double(value)) ||
      ! is.finite(value) || value < 0 || value %% 1 != 0) {
      cli::format_inline("must be a non-negative integer, not {.val {value}}.")
    }
  },
  default = 1
)

engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (!rlang::is_string(value) || !value %in% c("TDA", "ripserr")) {
      cli::format_inline(
        "must be {.val TDA} or {.val ripserr}, not {.val {value}}."
      )
    }
  },
  default = "TDA"
)

library_type <- new_property(
  class = class_character,
  validator = function(value) {
    valid_na <- length(value) == 1L && is.na(value)
    valid_library <- rlang::is_string(value) &&
      value %in% c("GUDHI", "PHAT", "Dionysus")
    if (!valid_na && !valid_library) {
      cli::format_inline(
        "must be {.val GUDHI}, {.val PHAT}, {.val Dionysus}, or {.val NA}, not {.val {value}}."
      )
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
    if (!rlang::is_bool(value)) {
      cli::format_inline("must be either {.code TRUE} or {.code FALSE}.")
    }
  },
  default = TRUE
)

max_scale_type <- new_property(
  class = class_numeric,
  default = NA_real_
)
