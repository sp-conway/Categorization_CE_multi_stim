psy2phy <- function(n) {
  return(150*1.05^n)
}
phy2psy <- function(a) {
  log(a/150,1.05)
}
dens_out <- function(x){ # x should be a jnd
  jnd_min <- 0
  jnd_max <- 22.5
  
  # Category properties
  out_mean_jnds <- jnd_max/2
  out_sd_jnds <- jnd_max/10
  
  # normalizing constant
  out_nc <- pnorm(jnd_max, out_mean_jnds, out_sd_jnds) - pnorm(jnd_min, out_mean_jnds, out_sd_jnds)
  
  dens <- dnorm(x, out_mean_jnds, out_sd_jnds)/out_nc
  
  return(dens)
}

dens_in <- function(x){
  jnd_min <- 0
  jnd_max <- 22.5
  
  dens <- dunif(x, jnd_min, jnd_max)
  return(dens)
}

# Probability a stimulus is in/out of category based on JND values
prob_in <- function(x) {
  dens_in(x)/(dens_in(x)+dens_out(x))
}
prob_out <- function(x) {
  dens_out(x)/(dens_in(x)+dens_out(x))
}

# function for computing the log likelihood for a given choice
stim_ll <- function(x) log(dens_in(x)/dens_out(x))

# code from jeff starns!!!
# modified w/ actual parameter values
dens_out_actual <- function(a) dnorm(phy2psy(a), 11.25, 2.25)/(.05*a)
dens_in_actual <- function(a) dunif(phy2psy(a), 0, 22.5)/(.05*a)
prob_out_actual <- function(a) (dens_in_actual(a))/(dens_in_actual(a)+dens_out_actual(a))
prob_in_actual <- function(a) (dens_out_actual(a))/(dens_in_actual(a)+dens_out_actual(a))