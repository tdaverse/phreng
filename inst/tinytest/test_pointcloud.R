spec <- PersistencePointCloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)

# constructor test
expect_inherits(spec, "phreng::PersistencePointCloud")
expect_inherits(spec, "phreng::Persistence")
expect_equal(spec@filtration, "vietoris_rips")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_diameter, 2)
expect_equal(spec@max_radius, 1)


# default argument test
spec <- PersistencePointCloud()

expect_inherits(spec, "phreng::PersistencePointCloud")
expect_equal(spec@engine, "TDA")
expect_true(is.na(spec@library))
expect_equal(spec@max_dimension, 1)
expect_equal(spec@filtration, "vietoris_rips")
expect_true(is.na(spec@max_radius))
expect_true(is.na(spec@max_diameter))

# max_diameter and max_radius test
spec <- PersistencePointCloud(max_diameter = 10)

expect_equal(spec@max_diameter, 10)
expect_equal(spec@max_radius, 5)

# max_diameter boundary test
spec <- PersistencePointCloud(max_diameter = 0)

expect_equal(spec@max_diameter, 0)
expect_equal(spec@max_radius, 0)

# max_dimension boundary test
spec <- PersistencePointCloud(max_dimension = 0)

expect_equal(spec@max_dimension, 0)

# filtration alias tests
spec <- PersistencePointCloud(filtration = "alphacomplex")
expect_equal(spec@filtration, "alpha_complex")

spec <- PersistencePointCloud(filtration = "alphashape")
expect_equal(spec@filtration, "alpha_shape")

rips_aliases <- c(
  "vietorisrips",
  "vietoris",
  "rips",
  "rips_vietoris",
  "ripsvietoris"
)

for (alias in rips_aliases) {
  spec <- PersistencePointCloud(filtration = alias)
  expect_equal(spec@filtration, "vietoris_rips")
}

# library value tests
spec <- PersistencePointCloud(library = "GUDHI")
expect_equal(spec@library, "GUDHI")

spec <- PersistencePointCloud(library = "PHAT")
expect_equal(spec@library, "PHAT")

spec <- PersistencePointCloud(library = "Dionysus")
expect_equal(spec@library, "Dionysus")

# validator tests
expect_error(
  PersistencePointCloud(engine = "bad_engine"),
  "TDA"
)
expect_error(
  PersistencePointCloud(engine = "bad_engine"),
  "ripserr"
)
expect_error(
  PersistencePointCloud(library = "bad_engine"),
  "GUDHI"
)
expect_error(
  PersistencePointCloud(filtration = "bad_filtration"),
  "vietoris_rips"
)

expect_error(
  PersistencePointCloud(filtration = "bad_filtration"),
  "cubical"
)
expect_error(
  PersistencePointCloud(filtration = "bad_filtration"),
  "alpha_shape"
)
expect_error(
  PersistencePointCloud(filtration = "bad_filtration"),
  "alpha_complex"
)
expect_error(
  PersistencePointCloud(max_dimension = -1),
  "non-negative"
)
expect_error(
  PersistencePointCloud(max_dimension = 1.5),
  "integer"
)
expect_error(
  PersistencePointCloud(engine = "ripserr", library = "GUDHI"),
  "TDA"
)
expect_error(
  PersistencePointCloud(filtration = "alpha_complex", engine = "ripserr"),
  "TDA"
)

# engine value tests
spec <- PersistencePointCloud(engine = "TDA")
expect_equal(spec@engine, "TDA")

spec <- PersistencePointCloud(engine = "ripserr")
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
  requireNamespace("ripserr", quietly = TRUE),
  requireNamespace("phutil", quietly = TRUE)
)
data <- eurodist
spec <- PersistencePointCloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2000
)
out <- compute_persistence(spec, data)
expect_inherits(out, "persistence")
