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

# Print values of A between [3,4]
subA <- (gaussian.test[, "A"] >= 3) & (gaussian.test[, "A"] <= 4)
trueValues <- which(subA == T)
gaussian.test$A[trueValues]

# Sample 50 rows without replacement. 
gaussian.test[sample(50, replace = F), ] 

# Bootstrap sample w/5,000 observations w/replacement and compute mean of 
# variable. 
colMeans(gaussian.test[sample(5000, replace = F), ])

# Standardize each column. 
scale(gaussian.test)

# 1.6

# Generate a data frame with 100 observations:
# 50 "low" then 50 "high"
# 25 "good", 25 "bad", 25 "good", 25 "bad"
# if "low": sample from Gaussian w/mean of 2 and var. of 4
# if "high": sample from Gaussian w/mean of 4 and var. of 1

a <- factor(c(rep("low", 50), rep("high", 50)), levels = c("low", "high"))
b <- factor(rep(c(rep("good", 25), rep("bad", 25)), 2, levels = c("good", "bad")))
df <- data.frame(a, b, c = c(rnorm(50, mean = 2, sd = 2), 
                             rnorm(50, mean = 4, sd = 1)))









