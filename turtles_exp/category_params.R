# Source this file to get distribution parameters for experiment 2
# Sean Conway
# Last modified July 2022

# category parameters 
am <- 25 # angle mean
rm <- 75 # radius mean

# out of category parameters
# just limits on a bivariate uniform
alim <- c(10, 40) # limits for angle out distribution
rlim <- c(30, 120) # limits for radius out distribution

# variance parameters
prop_sd <- .1
rsd <- prop_sd*(diff(rlim))
asd <- prop_sd*(diff(alim))
rv <- rsd^2
av <- asd^2
cv <- 0 # covariance of 0
cv_mat <- matrix(c(av, cv,cv, rv),2,2) # covariance matrix
