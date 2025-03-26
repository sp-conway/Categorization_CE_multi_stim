rm(list=ls())
# transform psych to phys values
# assumes min line length is 150 pixels, weber coefficient is 1.05
psy2phy <- function(n) {
  trans <- ifelse(n==0, 0, 150*1.05^n)
  return(trans)
}

# min psych value
min_psy <- 0
# max psych value
max_psy <- 22.5

# mean & sd for out distribution
out_mean_psy <- max_psy/2
out_sd_psy <- 2.5

# n per category
n <- 100

# sample psych values
in_cat_psy <- runif(n,min_psy,max_psy)
out_cat_psy <- rnorm(n, out_mean_psy, out_sd_psy)

# transform to physical values
in_cat_phy <- psy2phy(in_cat_psy)
out_cat_phy <- psy2phy(out_cat_psy)

# plotting
par(mfrow=c(3,2))
plot(in_cat_psy, in_cat_phy)
plot(out_cat_psy, out_cat_phy)
hist(in_cat_psy); hist(out_cat_psy)
hist(in_cat_phy); hist(out_cat_phy)
