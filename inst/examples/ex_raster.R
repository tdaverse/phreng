# using volcano dataset
data <- volcano

# with TDA 
## cubical filtration
x <- PH_raster(
  filtration = "cubical", 
  engine = "TDA", 
  library = "GUDHI", 
  max_dimension = 1,
  max_scale = 1000,
  sublevel = TRUE
  )
result <- compute_persistence(x, data)
as.data.frame(result)

# with ripserr
## cubical filtration 
x <- PH_raster(
  filtration = "cubical", 
  engine = "ripserr",
  library = ,
  max_dimension = 1,
  max_scale = 1000,
  sublevel = TRUE
  )
result <- compute_persistence(x, data)
as.data.frame(result)
