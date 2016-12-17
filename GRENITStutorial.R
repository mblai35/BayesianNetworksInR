# GRENITStutorial.R
# R version 3.2.2 (2015-08-14)
# December 12, 2016. Mallory B. Lai.
# Walk-through of GRENITS vignette. 
#-----------------------------------------------------------------------
source("https://bioconductor.org/biocLite.R")
biocLite("GRENITS")
library(GRENITS)
#-----------------------------------------------------------------------

# Load the pre-loaded data set Athaliana_ODE.
data(Athaliana_ODE)

# Inspect the dimensions of the data set. 
dim(Athaliana_ODE)

# Create tempdir folder to hold MCMC files in output.folder. 
output.folder <- paste(tempdir(), "/Example_LinearNet", sep = "")

# Run MCMC function; 2 chains, default parameters
LinearNet(output.folder, Athaliana_ODE)

# Analys raw results, place analysis plots and files in output.folder
analyse.output(output.folder)

# View contents of output.folder
dir(output.folder)

# Load inferred network probabilities.
# Create prob.file to store full path name for 
# NetworkProbability_Matrix.txt.
prob.file <- paste(output.folder, "/NetworkProbability_Matrix.txt", 
                   sep = "")

# Read in the NetworkProbability_Matrix.txt to R as a data.frame.
prob.mat <- read.table(prob.file)

# Apply a threshold of 0.8 on the link probability for a network
# prediction. 
inferred.net <- 1 * (prob.mat > 0.8)

# View inferred network. 
print(inferred.net)

# For information on direction of the interaction, see 
# NetworkProbability_List.txt
prob.list.file <- paste(output.folder, "/NetworkProbability_List.txt",
                        sep = "")

# Read in NetworkProbability_List.txt.
prob.list <- read.table(prob.list.file, header = T)





















