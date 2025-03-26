# function to compute normalizing constants
compute_nc <- function(lwr, upr, m, s) pnorm(upr, m, s)-pnorm(lwr, m,s)

# psychological to physical mapping
psy2phy <- function(min_len,n) dplyr::if_else(n==0, 0, min_len*1.05^n)

# compute probability density for the category
compute_density <- function(x, m, s, nc) dnorm(x, m, s)/ nc