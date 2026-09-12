spec <- PersistenceGrid(
  filtration = "cubical",
  engine = "ripserr",
  max_dimension = 1,
  max_scale = 2,
  sublevel = TRUE
)

# constructor test
expect_inherits(spec, "phreng::PersistenceGrid")
expect_inherits(spec, "phreng::Persistence")
expect_equal(spec@filtration, "cubical")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_scale, 2)
expect_equal(spec@sublevel, TRUE)

# default argument test
spec <- PersistenceGrid()

expect_inherits(spec, "phreng::PersistenceGrid")
expect_equal(spec@engine, "TDA")
expect_true(is.na(spec@library))
expect_equal(spec@max_dimension, 1)
expect_equal(spec@filtration, "cubical")
expect_true(is.na(spec@max_scale))
expect_equal(spec@sublevel, TRUE)

# sublevel value tests
spec <- PersistenceGrid(sublevel = TRUE)
expect_equal(spec@sublevel, TRUE)

spec <- PersistenceGrid(sublevel = FALSE)
expect_equal(spec@sublevel, FALSE)

# max_scale test
spec <- PersistenceGrid(max_scale = 10)

expect_equal(spec@max_scale, 10)

# max_scale boundary test
spec <- PersistenceGrid(max_scale = 0)

expect_equal(spec@max_scale, 0)

# validator tests
expect_error(
  PersistenceGrid(engine = "bad_engine"),
  "TDA"
)
expect_error(
  PersistenceGrid(engine = "bad_engine"),
  "ripserr"
)
expect_error(
  PersistenceGrid(library = "bad_engine"),
  "GUDHI"
)
expect_error(
  PersistenceGrid(filtration = "bad_filtration"),
  "cubical"
)
expect_error(
  PersistenceGrid(max_dimension = -1),
  "non-negative"
)
expect_error(
  PersistenceGrid(max_dimension = 1.5),
  "integer"
)
expect_error(
  PersistenceGrid(engine = "ripserr", library = "GUDHI"),
  "TDA"
)
expect_error(
  PersistenceGrid(filtration = "vietoris_rips", engine = "ripserr"),
  "cubical"
)
expect_error(
  PersistenceGrid(sublevel = NA),
  "TRUE or FALSE"
)

expect_error(
  PersistenceGrid(sublevel = "TRUE"),
  "sublevel"
)

expect_error(
  PersistenceGrid(sublevel = 1),
  "sublevel"
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
spec <- PersistenceGrid(
  filtration = "cubical",
  engine = "ripserr",
  max_dimension = 1,
  max_scale = 300,
  sublevel = TRUE
)
out <- compute_persistence(spec, data)
expect_inherits(out, "persistence")
