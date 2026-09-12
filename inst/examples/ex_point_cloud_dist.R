# using eurodist dataset
data <- eurodist

# with TDA
## vietoris_rips filtration
x <- PersistencePointCloud(
  engine = "TDA",
  library = "GUDHI",
  filtration = "vietoris_rips",
  max_dimension = 1,
  max_diameter = 1000
)
result <- compute_persistence(x, data)
as.data.frame(result)

# with ripserr
## vietoris_rips filtration
## library specification not necessary when engine is ripserr
x <- PersistencePointCloud(
  engine = "ripserr",
  library = ,
  filtration = "vietoris_rips",
  max_dimension = 1,
  max_diameter = 1000
)
result <- compute_persistence(x, data)
as.data.frame(result)
