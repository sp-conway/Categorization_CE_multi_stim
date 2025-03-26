library(tidyverse)
library(here)

# clear environment
rm(list=ls())

# learning trials
learn_trials <- read_csv(here("trials","learn_trials.csv"))

source(here("R","functions","density_functions.R"))

optimal <- learn_trials %>%
  mutate(prob_in=prob_in(jnds),
         optimal_rsp=case_when(
           prob_in > .5 ~ "in",
           prob_in < .5 ~ "out"
         )
  )

optimal_prob <- sum(optimal$distribution==optimal$optimal_rsp)/nrow(optimal)

optimal$optimal_prob <- optimal_prob
write_csv(optimal, here("R","optimality","optimal.csv"))
