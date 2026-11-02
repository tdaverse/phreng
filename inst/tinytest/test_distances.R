spec <- PersistenceDistances(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)

# constructor test
expect_inherits(spec, "phreng::PersistenceDistances")
expect_inherits(spec, "phreng::Persistence")
expect_equal(spec@filtration, "vietoris_rips")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_diameter, 2)

# default argument test
spec <- PersistenceDistances()

expect_inherits(spec, "phreng::PersistenceDistances")
expect_equal(spec@engine, "TDA")
expect_true(is.na(spec@library))
expect_equal(spec@max_dimension, 1)
expect_equal(spec@filtration, "vietoris_rips")
expect_true(is.na(spec@max_diameter))

# max_diameter test
spec <- PersistenceDistances(max_diameter = 10)

expect_equal(spec@max_diameter, 10)

# max_diameter boundary test
spec <- PersistenceDistances(max_diameter = 0)

expect_equal(spec@max_diameter, 0)

# max_dimension boundary test
spec <- PersistenceDistances(max_dimension = 0)

expect_equal(spec@max_dimension, 0)

# filtration validator tests
expect_error(
  PersistenceDistances(filtration = "alpha"),
  "must be one of"
)
expect_error(
  PersistenceDistances(filtration = "unrecognized"),
  "must be one of"
)

# library value tests
spec <- PersistenceDistances(library = "GUDHI")
expect_equal(spec@library, "GUDHI")

spec <- PersistenceDistances(library = "PHAT")
expect_equal(spec@library, "PHAT")

spec <- PersistenceDistances(library = "Dionysus")
expect_equal(spec@library, "Dionysus")

# validator tests
expect_error(
  PersistenceDistances(engine = "bad_engine"),
  "TDA"
)
expect_error(
  PersistenceDistances(engine = "bad_engine"),
  "ripserr"
)
expect_error(
  PersistenceDistances(library = "bad_engine"),
  "GUDHI"
)
expect_error(
  PersistenceDistances(max_dimension = -1),
  "non-negative"
)
expect_error(
  PersistenceDistances(max_dimension = 1.5),
  "integer"
)
expect_error(
  PersistenceDistances(engine = "ripserr", library = "GUDHI"),
  "TDA"
)

# engine value tests
spec <- PersistenceDistances(engine = "TDA")
expect_equal(spec@engine, "TDA")

spec <- PersistenceDistances(filtration = "vietoris_rips", engine = "ripserr")
expect_equal(spec@engine, "ripserr")

# compute test
exit_if_not(
  requireNamespace("ripserr", quietly = TRUE),
  requireNamespace("phutil", quietly = TRUE)
)
data <- eurodist
spec <- PersistenceDistances(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2000
)
out <- compute_persistence(spec, data)
expect_inherits(out, "persistence")

# coordinate matrix
coords <- matrix(
  c(0, 0, 1, 0, 0, 1, 1, 1),
  ncol = 2,
  byrow = TRUE
)
spec <- PersistenceDistances(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 3
)
out <- compute_persistence(spec, coords)
expect_inherits(out, "persistence")
# numeric vectors are not coordinate matrices
expect_error(
  compute_persistence(spec, as.double(1:5)),
  "matrix"
)

# compute test with TDA
if (requireNamespace("TDA", quietly = TRUE)) {
  spec <- PersistenceDistances(
    filtration = "vietoris_rips",
    engine = "TDA",
    library = "Dionysus",
    max_dimension = 1,
    max_diameter = 2000
  )
  out <- compute_persistence(spec, data)
  expect_inherits(out, "persistence")
}
