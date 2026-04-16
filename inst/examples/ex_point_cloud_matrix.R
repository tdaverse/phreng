# generating point cloud data
data <- matrix(rnorm(100), ncol = 4)

# with TDA
## vietoris_rips filtration
x <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "TDA",
  library = "GUDHI",
  max_dimension = 1,
  max_diameter = 1000
)
result <- compute_persistence(x, data)
as.data.frame(result)

# with ripserr
## vietoris_rips filtration
x <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  library = ,
  max_dimension = 1,
  max_diameter = 1000
)
result <- compute_persistence(x, data)
as.data.frame(result)
