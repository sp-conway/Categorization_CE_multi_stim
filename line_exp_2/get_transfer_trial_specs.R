rm(list=ls())
library(tidyverse)
library(here)

trials <- here("line_exp_2","trials","transfer_trials_1b.csv") %>%
  read_csv()

trials %>%
  count(trial_choice_set, transfer_trial_type, tc_correct, difficulty)

trials %>%
  count(trial_choice_set, transfer_trial_type)

trials %>%
  count(transfer_trial_type)
