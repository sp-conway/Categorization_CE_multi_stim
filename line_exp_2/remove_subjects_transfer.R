# Sean Conway
# Last modified Mar. 2023

# compute ll of each choice option
# calc correctness as selection of choice with highest ll
# remove subjects who don't get at least 60% correct on filler trials

# Setup ==============================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(stringr)

# read in data
dat <- read_csv(here("line_exp_2","data","cleaned","dataset_transfer.csv"))

# density functions
source(here("line_exp_2","functions","density_functions_2.R"))

dat <- dat %>%
  mutate(stim_1_ll = stim_ll(psy_1_in_order),
         stim_2_ll = stim_ll(psy_2_in_order),
         stim_3_ll = ifelse(is.na(psy_3_in_order),NA_real_, stim_ll(psy_3_in_order)),
         choice_ll = stim_ll(choice_psy)) %>%
  rowwise() %>%
  mutate(max_ll=max(c(stim_1_ll,stim_2_ll,stim_3_ll),na.rm=T)) %>%
  ungroup() %>%
  mutate(
    correct_ll=case_when(
      choice_ll==max_ll~T,
      choice_ll!=max_ll~F
    )
  )


# data filtering ==============================================================================
# get filler proportion correct
fill_prop_corr <- dat %>% 
  filter(transfer_trial_type=="filler_easy") %>%
  mutate(correct_ll=as.factor(correct_ll)) %>%
  count(sub_n,correct_ll,.drop=F) %>%
  group_by(sub_n) %>%
  mutate(prop_correct=n/sum(n)) %>%
  ungroup() %>%
  filter(correct_ll==T) %>%
  arrange(prop_correct) %>%
  select(-n)

# get subject numbers for ppts. whose filler easy performance >= 60%
sub_n_keep <- fill_prop_corr %>%
  filter(prop_correct>=.6) %>%
  pull(sub_n)

# filter the data
dat_filtered <- dat %>%
  filter(sub_n %in% sub_n_keep)

# overwrite original data (raw files are left as is) ===================================================
write_csv(dat_filtered, here("line_exp_2","data","cleaned","dataset_transfer.csv"))
