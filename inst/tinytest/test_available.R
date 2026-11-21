# available_filtrations tests

filtrations <- available_filtrations()

expect_true(is.data.frame(filtrations))
expect_equal(
  names(filtrations),
  c("filtration", "engine")
)

expect_true(
  any(
    filtrations$filtration == "vietoris_rips" &
      filtrations$engine == "ripserr"
  )
)

expect_true(
  any(
    filtrations$filtration == "vietoris_rips" &
      filtrations$engine == "TDA"
  )
)

expect_true(
  any(
    filtrations$filtration == "cubical" &
      filtrations$engine == "ripserr"
  )
)

expect_true(
  any(
    filtrations$filtration == "triangulation" &
      filtrations$engine == "TDA"
  )
)

expect_true(
  any(
    filtrations$filtration == "alpha" &
      filtrations$engine == "TDA"
  )
)

# available_engines tests

engines <- available_engines("vietoris_rips")

expect_true("ripserr" %in% engines)
expect_true("TDA" %in% engines)

expect_equal(
  available_engines("cubical"),
  "ripserr"
)

expect_equal(
  available_engines("triangulation"),
  "TDA"
)

expect_equal(
  available_engines("alpha"),
  "TDA"
)

expect_equal(
  available_engines(
    filtration = "vietoris_rips",
    input = "distances"
  ),
  c("ripserr", "TDA")
)

expect_equal(
  available_engines(
    filtration = "vietoris_rips",
    input = "coordinates"
  ),
  c("ripserr", "TDA")
)

expect_error(
  available_engines("bad_filtration"),
  "must be one of"
)

expect_error(
  available_engines("vietoris_rips", input = "bad_input"),
  "must be one of"
)

expect_error(
  available_engines(
    filtration = "cubical",
    input = "coordinates"
  ),
  "No engine is available"
)


# available_inputs tests

inputs <- available_inputs()

expect_true("distances" %in% inputs)
expect_true("coordinates" %in% inputs)
expect_true("grid" %in% inputs)

expect_equal(
  available_inputs("vietoris_rips"),
  c("distances", "coordinates")
)

expect_equal(
  available_inputs("cubical"),
  "grid"
)

expect_equal(
  available_inputs("triangulation"),
  "grid"
)

expect_equal(
  available_inputs("alpha"),
  "coordinates"
)

expect_error(
  available_inputs("bad_filtration"),
  "must be one of"
)


# available_classes tests

classes <- available_classes()

expect_true("double" %in% classes)
expect_true("dist" %in% classes)

expect_equal(
  available_classes(
    filtration = "vietoris_rips",
    input = "distances"
  ),
  "dist"
)

expect_equal(
  available_classes(
    filtration = "vietoris_rips",
    input = "coordinates"
  ),
  "double"
)

expect_equal(
  available_classes(
    filtration = "cubical",
    input = "grid"
  ),
  "double"
)

expect_equal(
  available_classes(
    filtration = "triangulation",
    input = "grid"
  ),
  "double"
)

expect_equal(
  available_classes(
    filtration = "alpha",
    input = "coordinates"
  ),
  "double"
)

expect_error(
  available_classes(
    filtration = "bad_filtration",
    input = "coordinates"
  ),
  "must be one of"
)

expect_error(
  available_classes(input = "bad_input"),
  "must be one of"
)

expect_error(
  available_classes(
    filtration = "vietoris_rips",
    input = "grid"
  ),
  "No class is available"
)


# recommend_engine tests

expect_equal(
  recommend_engine(
    filtration = "vietoris_rips",
    input = "distances"
  ),
  "ripserr"
)

expect_equal(
  recommend_engine(
    filtration = "vietoris_rips",
    input = "coordinates"
  ),
  "ripserr"
)

expect_equal(
  recommend_engine(
    filtration = "cubical",
    input = "grid"
  ),
  "ripserr"
)

expect_equal(
  recommend_engine(
    filtration = "triangulation",
    input = "grid"
  ),
  "TDA"
)

expect_equal(
  recommend_engine(
    filtration = "alpha",
    input = "coordinates"
  ),
  "TDA"
)

expect_error(
  recommend_engine(
    filtration = "bad_filtration",
    input = "coordinates"
  ),
  "must be one of"
)

recommendation <- NULL
expect_message(
  recommendation <- recommend_engine(
    filtration = "cubical",
    input = "coordinates"
  ),
  "No engine is available",
  class = "phreng_message_no_engine"
)
expect_null(recommendation)

expect_error(
  available_engines(
    filtration = "cubical",
    input = "coordinates"
  ),
  class = "phreng_error_no_engine"
)

expect_error(
  available_classes(
    filtration = "vietoris_rips",
    input = "grid"
  ),
  class = "phreng_error_no_class"
)
