# Code from Andrew Cohen
# assumes (you can mess with these values)
#   low physical value = 150
#   high physical value = 450 = 150 + 300
#   reverse engineered n for this physical range by log(450/150)/log(1.05) = 22.52
#   physical normal mean centered in space = 259.83 = 150*1.05^(22.52/2), where n = 22.52/2
#   jnd for normal sd = n = 2.252 = 22.52/10, just made up the 10 here

# clear environment
rm(list=ls())

# Psychological to physical mapping
# n = number of jnd
psy2phy <- function(n) {
  return(150*1.05^n)
}

# Draw from a uniform
r <- runif(1, 0, 22.52)
c(r, psy2phy(r))

# Draw from a normal
r <- rnorm(1, 22.52/2, 22.52/10)
c(r, psy2phy(r))

# Plots for fun
par(mfrow=c(2,3))

# The mapping
x <- seq(0, 22.52)
plot(x, psy2phy(x), ty='l', xlab='psychological', ylab='physical', main='mapping')

# Uniform
x <- runif(10000, 0, 22.52)
hist(psy2phy(x), xlab='physical', main='uniform')

# Normal
x <- rnorm(10000, 22.52/2, 22.52/10)
hist(psy2phy(x), xlab='physical', main='normal')

# A few examples of equal psych intervals
x <- seq(0, 22.52, length.out=10)
n_x <- length(x)
plot(NA, ty='n', xlim=c(1,n_x), ylim=c(0, 450), main='equal psych intervals')
for (i in 1:n_x) {
  points(c(i,i), c(0,psy2phy(x[i])), ty='l', lwd=2)
}
# Question: Do these look equal interval to you? I'm not 100%.

# A few examples of equal physical intervals
x <- seq(150, 450, length.out=10)
n_x <- length(x)
plot(NA, ty='n', xlim=c(1,n_x), ylim=c(0, 450), main='equal physical intervals')
for (i in 1:n_x) {
  points(c(i,i), c(0,x[i]), ty='l', lwd=2)
}
# Do the equal psych intervals look better than the equal physical intervals for this purpose?