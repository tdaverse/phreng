spec <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)


# constructor test

expect_inherits(spec, "phreng::PH_pointcloud")
expect_inherits(spec, "phreng::PH")
expect_equal(spec@filtration, "vietoris_rips")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_diameter, 2)
expect_equal(spec@max_radius, 1)


# validator tests
# TODO: should i write TDA|ripserr or keep as two

expect_error(
  PH_pointcloud(engine = "bad_engine"),
  "TDA"
)

expect_error(
  PH_pointcloud(engine = "bad_engine"),
  "ripserr"
)

expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "vietoris_rips"
)

expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "cubical"
)

expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "alpha_shape"
)

expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "alpha_complex"
)

expect_error(
  PH_pointcloud(max_dimension = -1),
  "non-negative"
)

expect_error(
  PH_pointcloud(max_dimension = 1.5),
  "integer"
)

expect_error(
  PH_pointcloud(engine = "ripserr", library = "GUDHI"),
  "TDA"
)

expect_error(
  PH_pointcloud(filtration = "alpha_complex", engine = "ripserr"),
  "TDA"
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

data <- eurodist

spec <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2000
)

out <- compute_persistence(spec, data)

expect_inherits(out, "persistence")
