spec <- PersistenceCoordinates(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)

# constructor test
expect_inherits(spec, "phreng::PersistenceCoordinates")
expect_inherits(spec, "phreng::Persistence")
expect_equal(spec@filtration, "vietoris_rips")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_diameter, 2)
expect_equal(spec@max_radius, 1)
expect_equal(spec@distance_metric, "euclidean")


# default argument test
spec <- PersistenceCoordinates()

expect_inherits(spec, "phreng::PersistenceCoordinates")
expect_equal(spec@engine, "TDA")
expect_true(is.na(spec@library))
expect_equal(spec@max_dimension, 1)
expect_equal(spec@filtration, "alpha")
expect_equal(spec@distance_metric, "euclidean")
expect_true(is.na(spec@max_radius))
expect_true(is.na(spec@max_diameter))

# max_diameter and max_radius test
spec <- PersistenceCoordinates(max_diameter = 10)

expect_equal(spec@max_diameter, 10)
expect_equal(spec@max_radius, 5)

# max_diameter boundary test
spec <- PersistenceCoordinates(max_diameter = 0)

expect_equal(spec@max_diameter, 0)
expect_equal(spec@max_radius, 0)

# max_dimension boundary test
spec <- PersistenceCoordinates(max_dimension = 0)

expect_equal(spec@max_dimension, 0)

# `distance_metric` tests
for (metric in c(
  "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
)) {
  spec <- PersistenceCoordinates(distance_metric = metric)
  expect_equal(spec@distance_metric, metric)
}

expect_error(
  PersistenceCoordinates(distance_metric = "unrecognized"),
  "must be one of"
)
expect_error(
  PersistenceCoordinates(distance_metric = c("euclidean", "manhattan")),
  "must be one of"
)

expect_error(
  PersistenceCoordinates(filtration = "unrecognized"),
  "must be one of"
)

# library value tests
spec <- PersistenceCoordinates(library = "GUDHI")
expect_equal(spec@library, "GUDHI")

spec <- PersistenceCoordinates(library = "PHAT")
expect_equal(spec@library, "PHAT")

spec <- PersistenceCoordinates(library = "Dionysus")
expect_equal(spec@library, "Dionysus")

# validator tests
expect_error(
  PersistenceCoordinates(engine = "bad_engine"),
  "TDA"
)
expect_error(
  PersistenceCoordinates(engine = "bad_engine"),
  "ripserr"
)
expect_error(
  PersistenceCoordinates(library = "bad_engine"),
  "GUDHI"
)
expect_error(
  PersistenceCoordinates(filtration = "bad_filtration"),
  "vietoris_rips"
)

expect_error(
  PersistenceCoordinates(filtration = "bad_filtration"),
  "alpha"
)
expect_error(
  PersistenceCoordinates(filtration = "bad_filtration"),
  "cech"
)
expect_error(
  PersistenceCoordinates(max_dimension = -1),
  "non-negative"
)
expect_error(
  PersistenceCoordinates(max_dimension = 1.5),
  "integer"
)
expect_error(
  PersistenceCoordinates(engine = "ripserr", library = "GUDHI"),
  "TDA"
)
expect_error(
  PersistenceCoordinates(filtration = "alpha", engine = "ripserr"),
  "TDA"
)

# engine value tests
spec <- PersistenceCoordinates(engine = "TDA")
expect_equal(spec@engine, "TDA")

spec <- PersistenceCoordinates(filtration = "vietoris_rips", engine = "ripserr")
expect_equal(spec@engine, "ripserr")

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
  requireNamespace("phutil", quietly = TRUE)
)
data <- phutil::noisy_circle_points

# ripserr tests
if (requireNamespace("ripserr", quietly = TRUE)) {
  spec <- PersistenceCoordinates(
    filtration = "vietoris_rips",
    engine = "ripserr",
    max_dimension = 1,
    max_diameter = 3
  )
  out <- compute_persistence(spec, data)
  expect_inherits(out, "persistence")

  # non-euclidean metric
  spec <- PersistenceCoordinates(
    filtration = "vietoris_rips",
    engine = "ripserr",
    distance_metric = "manhattan",
    max_dimension = 1,
    max_diameter = 3
  )
  out <- compute_persistence(spec, data)
  expect_inherits(out, "persistence")
}

# TDA tests
if (requireNamespace("TDA", quietly = TRUE)) {
  spec <- PersistenceCoordinates(
    filtration = "vietoris_rips",
    engine = "TDA",
    max_dimension = 1,
    max_diameter = 3
  )
  out <- compute_persistence(spec, data)
  expect_inherits(out, "persistence")

  spec <- PersistenceCoordinates(
    filtration = "vietoris_rips",
    engine = "TDA",
    distance_metric = "manhattan",
    max_dimension = 1,
    max_diameter = 3
  )
  out <- compute_persistence(spec, data)
  expect_inherits(out, "persistence")
}
