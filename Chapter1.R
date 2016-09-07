# Chapter1.R
# R version 3.2.2 (2015-08-14)
# September 6, 2016. Mallory B. Lai.
# Reviewed by: TODO (Mallory B. Lai) : Find reviewer to proofread
# Code following Chapter 1 from Bayesian Networks in R.

# Sources: Bayesian Networks in R
# By: Radhakrishnan Nagarajan, Marco Scutari, & Sophie Lebre

#install appropriate packages
install.packages("bnlearn")
library(bnlearn)

#------------------------------------------------------------------------------

# Exercises

# 1.4

# Load iris data set. 
data(iris)
# Look at data structure. 
str(iris)

# Write iris data frame to space-separated .txt file named iris.txt.
write.table(iris, file = "iris.txt", sep = " ")

# Read iris.txt in as iris2.
iris2 <- read.table(file.choose())

# Check to see that iris and iris2 are identical.
sum(iris != iris2)
# Sum of zero indicates all values of iris are equal to iris2.

# Back of books says: 
identical(iris, iris2)
# This yields the output FALSE.
# However, column-wise comparisons yield TRUE.
for (i in 1:dim(iris)[2])
{
  print(identical(iris[,i], iris2[,i]))
}

# Skipping c and d.

# List objects in global environment.
ls()

# Remove all but iris.
rm(iris2)

# 1.5

# Load data set guassian.test. 
data("gaussian.test")

# Print column names.
colnames(gaussian.test)

# Print range and quartile for each variable
for (i in 1:dim(gaussian.test)[2])
{
  print(range(gaussian.test[, i])) # Print range.
  print(quantile(gaussian.test[,i])[c(2,4)]) # Prints the 25% and 75% quantile.
  
}












