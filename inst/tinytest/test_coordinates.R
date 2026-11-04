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


# default argument test
spec <- PersistenceCoordinates()

expect_inherits(spec, "phreng::PersistenceCoordinates")
expect_equal(spec@engine, "TDA")
expect_true(is.na(spec@library))
expect_equal(spec@max_dimension, 1)
expect_equal(spec@filtration, "alpha")
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

expect_error(
  PersistenceCoordinates(filtration = c("alpha", "cech")),
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

expect_error(
  compute_persistence(
    PersistenceCoordinates(),
    matrix(as.double(1:4), ncol = 2),
    foo = 1
  ),
  "must be empty"
)

# compute test
exit_if_not(
  requireNamespace("ripserr", quietly = TRUE),
  requireNamespace("phutil", quietly = TRUE)
)
data <- phutil::noisy_circle_points
spec <- PersistenceCoordinates(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 3
)
out <- compute_persistence(spec, data)
expect_inherits(out, "persistence")
