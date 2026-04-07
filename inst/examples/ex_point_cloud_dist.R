# using eurodist dataset
data <- eurodist 

# with TDA
## vietoris_rips filtration
x <- PH_pointcloud(
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
## library, max_dimension specifications not necessary when engine is ripserr 
x <- PH_pointcloud(
  engine = "ripserr", 
  library = ,
  filtration = "vietoris_rips",
  max_dimension = 1,
  max_diameter = 1000
  )
result <- compute_persistence(x, data)
as.data.frame(result) 
