# SOURCE THIS FILE TO LOAD CATEGORY DISTRIBUTION PARAMS
# NOTE THAT THIS ALSO REQUIRES SOURCING multi_stim/line_exp_3/dist_funcs.R 
jnd_min <- 0
jnd_max <- 22.5
jnd_mdpt <- jnd_max/2
jnds_1 <- seq(0,jnd_max, .001)


# Category properties
in_mean_jnds_centered <- jnd_max/2
in_mean_jnds_left <- unname(quantile(jnds_1, 1/3))
in_mean_jnds_right <- unname(quantile(jnds_1, 2/3))

in_sd_jnds <- floor((jnd_max - in_mean_jnds_right)/3.5)

# normalizing constants
in_centered_nc <- compute_nc(jnd_min, jnd_max, in_mean_jnds_centered, in_sd_jnds)
in_left_nc <- compute_nc(jnd_min, jnd_max, in_mean_jnds_left, in_sd_jnds)
in_right_nc <- compute_nc(jnd_min, jnd_max, in_mean_jnds_right, in_sd_jnds)
