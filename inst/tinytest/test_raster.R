spec <- PH_raster(
  filtration = "cubical",
  engine = "ripserr",
  max_dimension = 1,
  max_scale = 2,
  sublevel = TRUE
)


# constructor test

expect_inherits(spec, "phreng::PH_raster")
expect_inherits(spec, "phreng::PH")
expect_equal(spec@filtration, "cubical")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_scale, 2)
expect_equal(spec@sublevel, TRUE)


# validator tests

expect_error(
  PH_raster(engine = "bad_engine"),
  "TDA"
)

expect_error(
  PH_raster(engine = "bad_engine"),
  "ripserr"
)

expect_error(
  PH_raster(filtration = "bad_filtration"),
  "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
)

expect_error(
  PH_raster(max_dimension = -1),
  "non-negative"
)

expect_error(
  PH_raster(max_dimension = 1.5),
  "integer"
)

expect_error(
  PH_raster(engine = "ripserr", library = "GUDHI"),
  "TDA"
)

expect_error(
  PH_raster(filtration = "vietoris_rips", engine = "ripserr"),
  "cubical"
)

expect_error(
  PH_raster(sublevel = NA),
  "TRUE or FALSE"
)


# data type test

expect_error(
  compute_persistence(spec, as.double(1:5)),
  "matrix"
)

expect_error(
  compute_persistence(spec, as.double(1:5)),
  "array"
)




# compute test

exit_if_not(
  requireNamespace("ripserr", quietly = TRUE),
  requireNamespace("phutil", quietly = TRUE)
)

data <- volcano

spec <- PH_raster(
  filtration = "cubical",
  engine = "ripserr",
  max_dimension = 1,
  max_scale = 300,
  sublevel = TRUE
)

out <- compute_persistence(spec, data)
expect_inherits(out, "persistence")

