# Chapter3.R
# R version 3.3.1 (2016-06-21)
# January 13, 2017. Mallory B. Lai.
# Code following Chapter 3 from Bayesian Networks in R.

# Sources: Bayesian Networks in R
# By: Radhakrishnan Nagarajan, Marco Scutari, & Sophie Lebre

#install appropriate packages
install.packages("vars")
library(vars)
install.packages("lars")
library(lars)
install.packages("GeneNet")
library(GeneNet)

#-----------------------------------------------------------------------

# 3.5.1 Multivariate Time Series Analysis

# Load data set Canada with 4 macroeconomic indicators: 
# prod, e, U, rw
data("Canada")

# Examine the data.
str(Canada)
dim(Canada)
summary(Canada)

# Vector auto-regressive process VAR(p) can be fitted with VAR fcn
# p is the integer for the lag order. Default is p = 1.
VAR(Canada, p = 2)

# Summarize returned object of class varest returned by VAR.
summary(VAR(Canada, p = 2))

# Note: Can change type of deterministic regressor. 

# Estimate optimal lag order for the VAR process w/information criteria.
# Must set teh upper bound for the lag order. 
VAR(Canada, lag.max = 4, ic = "AIC")
VAR(Canada, lag.max = 4, ic = "SC")

# Tests:
# Verify covariance stationarity
var.2c <- VAR(Canada, p = 2, type = "const")
stab <- stability(var.2c, type = "OLS-CUSUM")
plot(stab)
# Normality test.
normality.test(var.2c)
# Portmanteau test.
serial.test(var.2c, lags.pt = 16, type = "PT.adjusted")
# Breusch-Godfrey test.
serial.test(var.2c, lags.pt = 16, type = "BG")
# Heteroscedasticity test. 
arch.test(var.2c)

# 3.5.2 LASSO learning: lars and simone

# Load data.
data(arth800)

# Subset the data. 
subset <- c(60, 141, 260, 333, 365, 424, 441, 512, 521, 789, 799)
arth12 <- arth800.expr[, subset]


str(arth12)







# Exercises: 
data("Canada")




