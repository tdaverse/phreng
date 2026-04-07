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
  "must be TDA or ripserr"
)

expect_error(
  PH_raster(filtration = "bad_filtration"),
  "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
)

expect_error(
  PH_raster(max_dimension = -1),
  "must be a non-negative integer"
)

expect_error(
  PH_raster(max_dimension = 1.5),
  "must be a non-negative integer"
)

expect_error(
  PH_raster(engine = "ripserr", library = "GUDHI"),
  "Library is only defined when engine is TDA"
)

expect_error(
  PH_raster(filtration = "vietoris_rips", engine = "ripserr"),
  "Only cubical filtrations are allowed for raster objects"
)

expect_error(
  PH_raster(sublevel = NA),
  "sublevel must be either TRUE or FALSE"
)


# data type test

expect_equal(
  compute_persistence(spec, as.double(1:5)),
  "Data must be a matrix or an array for PH_pointcloud"
)


# compute test

if (requireNamespace("ripserr", quietly = TRUE) &&
    requireNamespace("phutil", quietly = TRUE)) {

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
}
