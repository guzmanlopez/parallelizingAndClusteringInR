# Parallel computing and clusters with R
This script is an example of performing parallel computing on clusters with R language using `parallel` package. The example is based on `krige` function of the `gstat` package with `meuse` dataset.

The aim of this script is that you can customite it to your needs! Also the aim is to speed up time consuming functions using all your cores of your computers or other computers processors.

**Note:** to make a cluster, you have to be able to access the other PCs via SSH.

```r

################################################################################
############### R routine for cluster and parallel computations ################
################################################################################

# Example for parallel computing on cluster for Kriging function using 'gstat' package and 'meuse' dataset

# Load data ---------------------------------------------------------------

# Load libraries
library('sp')
library('gstat')

# Load example data: meuse dataset
data("meuse") # data
data("meuse.grid") # 40m x 40m prediction grid for meuse dataset

# Create SpatialPointsDataFrame from meuse
coordinates(meuse) = ~x+y

# Create SpatialPixelsDataFrame from meuse.grid
gridded(meuse.grid) = ~x+y

# Create Spheric Variogram model with predefined parameters (psill, range and nugget)
m <- vgm(psill = 0.59, model = "Sph", range = 874, nugget = 0.04)


# Start Cluster -----------------------------------------------------------

# Configure the machines, IPs, numeber of cores of every machine used here

# Machines used
numberOfMachines <- 3

# Vector of hosts, usernames and number of cores
# E.g.: Machine1 = host: localhost, user: user1 and nCores: 7
# It's recommended to use number of total cores -1, let one core be free if you need to run something
hosts <- c('localhost', '10.5.1.1', '10.5.1.2')
users <- c('guzman', 'user2', 'user3')
nCores <- c(7, 8, 4)

# Machines's Adresses
machineAddresses <- lapply(X = 1:numberOfMachines, FUN = function(x) list(host = hosts[x], user = users[x], ncore = nCores[x]))

# Specifications
spec <- lapply(machineAddresses, function(machine) {
  rep(list(list(host = machine$host, user = machine$user)), machine$ncore)
})

spec <- unlist(spec, recursive = FALSE)

# Calculate the number of cores
no_cores <- sum(nCores[1:numberOfMachines])

# Split prediction grid
parts <- split(x = 1:length(meuse.grid), f = 1:no_cores)

# Function to parallelize
FunctionParallelKrige <- function(x) {

  # Load required libraries on clusters
  library('gstat')

  # Function
  message(paste("Process number ", x), appendLF = TRUE)
  output <- krige(formula = log(zinc)~1, locations = meuse, newdata = meuse.grid[parts[[x]],], model = m)

  # Return
  return(output)

}

# All the objects requiered to run the function
varlist <- c("meuse", "meuse.grid", "m", "parts", "FunctionParallelKrige")

# Write Parallelized function
RunFunctionOnParallelizedCluster <- function(master, spec, varlist, parts, functionParallel) {

  message("\n", appendLF = TRUE)

  # Load library
  library('parallel')

  # START CLUSTER
  message("Making cluster...")
  (parallelCluster <- makeCluster(type = 'PSOCK', master = master, spec = spec, homogeneous = FALSE, port = 58178, outfile = ""))
  message("done! \n", appendLF = TRUE)

  # Objects to export to clusters
  message("Exporting required objects to clusters...")
  clusterExport(cl = parallelCluster, varlist = varlist)
  message("done! \n", appendLF = TRUE)

  # Parallel Cluster Function
  message("Running parallel function...")
  system.time(

    functionOut <- parLapplyLB(cl = parallelCluster,
                               X = 1:parts,
                               fun = function(x) functionParallel(x = x))
  )
  message("done! \n", appendLF = TRUE)

  # FINISH CLUSTER
  message("Stopping cluster...")
  stopCluster(parallelCluster)
  message("Finish! \n", appendLF = TRUE)
  message("=) \n", appendLF = TRUE)

  # return
  return(functionOut)

}

# Run Parallelized function
outputParallel <- RunFunctionOnParallelizedCluster(master = "localhost", spec = spec, varlist = varlist, parts = length(parts), functionParallel = FunctionParallelKrige)

# Merge all the predictions

mergeParts <- raster::bind(outputParallel[[1]], outputParallel[[2]])

for (j in 3:length(outputParallel)) {
  mergeParts <- raster::bind(mergeParts, outputParallel[[j]])
}

# Plot
spplot(mergeParts, zcol = "var1.pred")


```
