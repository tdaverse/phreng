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
    input = "point_cloud"
  ),
  c("ripserr", "TDA")
)

expect_error(
  available_engines("bad_filtration"),
  "No engine supports"
)

expect_error(
  available_engines(
    filtration = "cubical",
    input = "point_cloud"
  ),
  "No engine supports"
)


# available_inputs tests

inputs <- available_inputs()

expect_true("point_cloud" %in% inputs)
expect_true("raster" %in% inputs)

expect_equal(
  available_inputs("vietoris_rips"),
  "point_cloud"
)

expect_equal(
  available_inputs("cubical"),
  "raster"
)

expect_equal(
  available_inputs("triangulation"),
  "raster"
)

expect_equal(
  available_inputs("alpha"),
  "point_cloud"
)

expect_error(
  available_inputs("bad_filtration"),
  "No input supports"
)


# available_classes tests

classes <- available_classes()

expect_true("double" %in% classes)
expect_true("dist" %in% classes)

expect_equal(
  available_classes(
    filtration = "vietoris_rips",
    input = "point_cloud"
  ),
  c("double", "dist")
)

expect_equal(
  available_classes(
    filtration = "cubical",
    input = "raster"
  ),
  "double"
)

expect_equal(
  available_classes(
    filtration = "triangulation",
    input = "raster"
  ),
  "double"
)

expect_equal(
  available_classes(
    filtration = "alpha",
    input = "point_cloud"
  ),
  "double"
)

expect_error(
  available_classes(
    filtration = "bad_filtration",
    input = "point_cloud"
  ),
  "No class supports"
)

expect_error(
  available_classes(
    filtration = "vietoris_rips",
    input = "raster"
  ),
  "No class supports"
)


# recommend_engine tests

expect_equal(
  recommend_engine(
    filtration = "vietoris_rips",
    input = "point_cloud"
  ),
  "ripserr"
)

expect_equal(
  recommend_engine(
    filtration = "cubical",
    input = "raster"
  ),
  "ripserr"
)

expect_equal(
  recommend_engine(
    filtration = "triangulation",
    input = "raster"
  ),
  "TDA"
)

expect_equal(
  recommend_engine(
    filtration = "alpha",
    input = "point_cloud"
  ),
  "TDA"
)
