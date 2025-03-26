psy2phy <- function(n) {
  return(150*1.05^n)
}
phy2psy <- function(a) {
  log(a/150,1.05)
}
dens_in <- function(x){ # x should be a jnd
  jnd_min <- 0
  jnd_max <- 22.5
  in_mean_jnds <- jnd_max/2
  in_sd_jnds <- jnd_max/6
  
  # normalizing constant
  in_nc <- pnorm(jnd_max, in_mean_jnds, in_sd_jnds)-pnorm(jnd_min, in_mean_jnds, in_sd_jnds)
  
  dens <- dnorm(x, in_mean_jnds, in_sd_jnds)/in_nc
  
  return(dens)
}

dens_out <- function(x){
  jnd_min <- 0
  jnd_max <- 22.5
  
  dens <- dunif(x, jnd_min, jnd_max)
  return(dens)
}

# Probability a stimilus is in/out of category based on JND values
prob_in <- function(x) dens_in(x)/(dens_in(x)+dens_out(x))
prob_out <- function(x) dens_out(x)/(dens_in(x)+dens_out(x))

# code from jeff starns!!!
# modified w/ actual parameter values
dens_in_actual <- function(a) dnorm(phy2psy(a), 11.25, 3.75)/(.05*a)
dens_out_actual <- function(a) dunif(phy2psy(a), 0, 22.5)/(.05*a)
prob_in_actual <- function(a) dens_in_actual(a)/(dens_in_actual(a)+dens_out_actual(a))
prob_out_actual <- function(a) dens_out_actual(a)/(dens_in_actual(a)+dens_out_actual(a))
