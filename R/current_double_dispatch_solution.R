#' include aaa.r

# allow different spacing, capitalization, etc
# e.g. allow `<S7_obj>(filtration = "Alpha shape")`
# maybe use `tolower()`, `snakecase::to_snake_case()`, etc.
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
