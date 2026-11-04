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

expect_error(
  PersistenceDistances(max_dimension = NA_real_),
  "non-negative"
)

expect_error(
  PersistenceDistances(max_dimension = Inf),
  "non-negative"
)

expect_error(
  PersistenceDistances(filtration = c("vietoris_rips", "vietoris_rips")),
  "must be one of"
)

expect_error(
  PersistenceDistances(engine = c("TDA", "ripserr")),
  "TDA"
)

expect_error(
  PersistenceDistances(library = c("GUDHI", "PHAT")),
  "GUDHI"
)

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

# numeric matrices are not distances
spec <- PersistenceDistances(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)
expect_error(
  compute_persistence(spec, as.matrix(eurodist)),
  "dist"
)

expect_error(
  compute_persistence(PersistenceDistances(), eurodist, foo = 1),
  "must be empty"
)

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
