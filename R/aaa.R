#' importFrom S7 new_property
new_property <- S7::new_property
class_character <- S7::class_character
class_double <- S7::class_double
new_class <- S7::new_class
class_logical <- S7::class_logical
new_S3_class <- S7::new_S3_class
new_generic <- S7::new_generic
`method<-` <- S7::`method<-`

# REVIEW: Is it necessary to redefine e.g. `S7::class_double`? (or is it better
# to import them instead?)

# Be sure to prefix tags with `@` where necessary.

# Import all needed for source code; export only those user will need (or those
# necessary to pass tests).
