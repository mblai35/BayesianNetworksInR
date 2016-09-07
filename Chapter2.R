# Chapter2.R
# R version 3.2.2 (2015-08-14)
# September 6, 2016. Mallory B. Lai.
# Reviewed by: TODO (Mallory B. Lai) : Find reviewer to proofread
# Code following Chapter 2 from Bayesian Networks in R.

# Sources: Bayesian Networks in R
# By: Radhakrishnan Nagarajan, Marco Scutari, & Sophie Lebre

# Install appropriate packages.
install.packages("bnlearn")
library(bnlearn)
# Other packages include: deal, pcalg, catnet, mugnet, gRbase, gRain

#------------------------------------------------------------------------------

# 2.3.2 Creating and Manipulating Network Structures

# Load the data set "marks". 
data("marks")

# Look at the structure of the data. 
str(marks)

# Create empty network with nodes corresponding to variables in marks using
# empty.graph(). Creates a bn object. 
ug = empty.graph(names(marks))

# View structure of ug. 
str(ug)

# Add arcs present in original network from Whittaker (1990) by assigning
# two-column matrix containing the labels of their end-nodes. Undirected arcs
# are represented by their two possible orientations. 
arcs(ug) = matrix(c('MECH', 'VECT', 'MECH', 'ALG', 'VECT', 'MECH',
                    'VECT', 'ALG', 'ALG', 'MECH', 'ALG', 'VECT', 
                    'ALG', 'ANL', 'ALG', 'STAT', 'ANL', 'ALG', 
                    'ANL', 'STAT', 'STAT', 'ALG', 'STAT', 'ANL'),
                    ncol = 2, byrow = T, dimnames = list(c(), c("from", "to")))
# NOTE: ignore.cycles = TRUE produces an 'unused argument' error

# Visual inspection of arc matrix:
arcmatrix <- matrix(c('MECH', 'VECT', 'MECH', 'ALG', 'VECT', 'MECH',
         'VECT', 'ALG', 'ALG', 'MECH', 'ALG', 'VECT', 
         'ALG', 'ANL', 'ALG', 'STAT', 'ANL', 'ALG', 
         'ANL', 'STAT', 'STAT', 'ALG', 'STAT', 'ANL'),
         ncol = 2, byrow = T, dimnames = list(c(), c("from", "to")))

# Create bn object for DAG.
# Initialize empty network.
dag = empty.graph(names(marks))

# Create directed arcs. 
arcs(dag) = matrix(c('VECT', 'MECH', 'ALG', 'MECH', 'ALG', 'VECT', 
                     'ANL', 'ALG', 'STAT', 'ALG', 'STAT', 'ANL'),
                    ncol = 2, byrow = T, dimnames = list(c(), c("from", "to")))

# Visual inspection of DAG arc matrix. 
dagarcmatrix <- matrix(c('VECT', 'MECH', 'ALG', 'MECH', 'ALG', 'VECT', 
                         'ANL', 'ALG', 'STAT', 'ALG', 'STAT', 'ANL'),
                       ncol = 2, byrow = T, dimnames = list(c(), c("from", "to")))
# Notice it's half the size of the arc matrix from ug.

# Now, create ug from adjacency matrix. 
# Initialize the adjacency matrix. 
mat <- matrix(c(0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 
              nrow = 5, dimnames = list(nodes(dag), nodes(dag)))

# Initialize the graph. 
dag2 <- empty.graph(nodes(dag))

# Create arcs using adjacency matrix function amat().
amat(dag2) <- mat

# Visual inspection of dag2
dag2

# Check to see if dag and dag2 are equal. 
all.equal(dag, dag2)

# Create DAG using set.arc.
# Initialize empty network "dag3".
dag3 <- empty.graph(nodes(dag))

# Set arcs.
dag3 <- set.arc(dag3, 'VECT', 'MECH')
dag3 <- set.arc(dag3, 'ALG', 'MECH')
dag3 <- set.arc(dag3, 'ALG', 'VECT')
dag3 <- set.arc(dag3, 'ANL', 'ALG')
dag3 <- set.arc(dag3, 'STAT', 'ALG')
dag3 <- set.arc(dag3, 'STAT', 'ANL')
 
# Check equivalence. 
all.equal(dag, dag3)

?moral














