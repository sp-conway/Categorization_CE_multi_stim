dbvn <- function(x, y, mu_x, mu_y, sig_x, sig_y, rho){
  a <- 1/(2*pi*sig_x*sig_y*sqrt(1-rho^2))
  b <- exp( -(1/(2*(1-rho^2)))*( ((x-mu_x)/sig_x)^2 + ((y-mu_y)/sig_y)^2 - (2*rho)* ( ( (x-mu_x)*(y-mu_y) )/(sig_x*sig_y))) )
  c <- a*b
  return(c)
}

dbvu <- function(xlim, ylim){
  a <- diff(xlim)*diff(ylim)
  d <- 1/a
  return(d)
}

sample_category <- function(n_draw, mu, cv_mat){
  x <- MASS::mvrnorm(n=n_draw, mu=mu,Sigma=cv_mat)
  tibble(
    angle=x[,1],
    radius=x[,2],
    distribution="in"
  )
}
