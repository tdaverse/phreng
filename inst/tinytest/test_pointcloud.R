spec <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)


# constructor test

# REVIEW: Is there a standard way to test S7 class inheritance (different from
# this)?
# expect_true(inherits(spec, "phreng::PH_pointcloud"))
expect_inherits(spec, "phreng::PH_pointcloud")
expect_inherits(spec, "phreng::PH")
expect_equal(spec@filtration, "vietoris_rips")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_diameter, 2)
expect_equal(spec@max_radius, 1)


# validator tests

# ADVICE: Test for short, keyword character strings, in case phrasing changes.

expect_error(
  PH_pointcloud(engine = "bad_engine"),
  "must be TDA or ripserr"
)

expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
)

expect_error(
  PH_pointcloud(max_dimension = -1),
  "must be a non-negative integer"
)

expect_error(
  PH_pointcloud(max_dimension = 1.5),
  "must be a non-negative integer"
)

# ADVICE: Use back-ticks to refer to objects, e.g. `library`.
expect_error(
  PH_pointcloud(engine = "ripserr", library = "GUDHI"),
  "Library is only defined when engine is TDA"
)

expect_error(
  PH_pointcloud(filtration = "alpha_complex", engine = "ripserr"),
  "Alpha complexes are only defined for the engine TDA"
)


# data type test

expect_equal(
  compute_persistence(spec, as.double(1:5)),
  "Data must be a matrix or an array for PH_pointcloud"
)


# compute test

# REVIEW: Is there a standard way to require that a package is installed?
# e.g. with {testthat}, i use `skip_if_not_installed()`.

if (requireNamespace("ripserr", quietly = TRUE) &&
    requireNamespace("phutil", quietly = TRUE)) {

  data <- eurodist

  spec <- PH_pointcloud(
    filtration = "vietoris_rips",
    engine = "ripserr",
    max_dimension = 1,
    max_diameter = 2000
  )

  out <- compute_persistence(spec, data)
  expect_inherits(out, "persistence")
}
