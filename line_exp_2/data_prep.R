# data_prep.R
# prepping & cleaning data from line experiment 2
# Sean Conway
# last modified Mar. 2023

# Setup ================================================================
# clear environment
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(here)
library(glue)
library(purrr)
library(readr)

# source psy2phy 
source(here("line_exp_2","functions","psy2phy.R"))

# logical - should we write dataset(s) to file?
write_data <- T

# Getting directory/file names
data_dir_raw <- here("line_exp_2","data","raw_data")
data_dir_cleaned <- here("line_exp_2","data","cleaned")
data_files <- list.files(data_dir_raw) %>%
  map(.x=., ~glue(data_dir_raw,"/",.x)) %>%
  as_vector()

# Columns to keep / ordering
cols_keep_all <- c("screen_id",
               "sub_n",
               "trial_type",
               "trial_index",
               "time_elapsed",
               "date",
               "stim",
               "stim_vals",
               "line_lens_order",
               "stim_vals_order_psy",
               "choices",
               "stim_names",
               "corr_ans",
               "give_fb",
               "in_cat",
               "rt",
               "key_choice",
               "choice",
               "choice_psy",
               "correct",
               "fb_in_cat",
               "line_1_in_order",
               "line_2_in_order",
               "line_3_in_order",
               "psy_1_in_order",
               "psy_2_in_order",
               "psy_3_in_order",
               "line_length",
               "exp_trial_type",
               "dens",
               "jnds",
               "distribution",
               "correct_1_psy",
               "correct_1_phy",
               "correct_2_psy",
               "transfer_trial_type",
               "trial_choice_set",
               "tc_correct",
               "side_correct",
               "difficulty",
               "target_side",
               "competitor_side",
               "stim_1_phy",
               "stim_1_psy",
               "stim_2_phy",
               "stim_2_psy",
               "stim_3_phy",
               "stim_3_psy",
               "target_phy",
               "competitor_phy",
               "decoy_phy",
               "target_psy",
               "competitor_psy",
               "decoy_psy",
               "attr_choice",
               "response")

cols_keep_learn <- c("date",
                     "sub_n",
                     "screen_id",
                     "trial_type",
                     "exp_trial_type",
                     "trial_index",
                     "time_elapsed",
                     "give_fb",
                     "fb_in_cat",
                     "in_cat",
                     "distribution",
                     "stim",
                     "stim_vals",
                     "line_length",
                     "jnds",
                     "dens",
                     "rt",
                     "key_choice",
                     "choice",
                     "correct")

cols_keep_transfer <- c("date",
                        "sub_n",
                        "screen_id",
                        "trial_type",
                        "trial_index",
                        "time_elapsed",
                        "exp_trial_type",
                        "transfer_trial_type",
                        "trial_choice_set",
                        "stim_vals",
                        "line_lens_order",
                        "stim_vals_order_psy",
                        "choices",
                        "stim_names",
                        "corr_ans",
                        "line_1_in_order",
                        "line_2_in_order",
                        "line_3_in_order",
                        "psy_1_in_order",
                        "psy_2_in_order",
                        "psy_3_in_order",
                        "correct_1_psy",
                        "correct_1_phy",
                        "correct_2_psy",
                        "correct_2_phy",
                        "stim_1_phy",
                        "stim_2_phy",
                        "stim_3_phy",
                        "target_phy",
                        "competitor_phy",
                        "decoy_phy",
                        "target_psy",
                        "competitor_psy",
                        "decoy_psy",
                        "side_correct",
                        "difficulty",
                        "tc_correct",
                        "target_side",
                        "competitor_side",
                        "give_fb",
                        "key_choice",
                        "choice",
                        "choice_psy",
                        "correct",
                        "attr_choice",
                        "rt"
                        )

cols_keep_survey <- c("date",
                      "sub_n",
                      "screen_id",
                      "trial_type",
                      "response",
                      "rt")

# Reading / cleaning functions ================================================================
prep_data <- function(file_name, cols_keep){
  currfile <- file_name
  orig_names <- c("T_psy","C_psy","D_psy","T_phy","C_phy","D_phy")
  new_names <- c("target_psy","competitor_psy","decoy_psy","target_phy","competitor_phy","decoy_phy")
  
  d <- data.table::fread(file_name) %>%
    as_tibble()
  check_orig <- sum(orig_names %in% names(d))>1
  check_new <- sum(new_names %in% names(d))>1
  if(check_orig & check_new){
    d <- d %>%
      select(-orig_names)
  }else if(check_orig & !check_new){
    d <- d %>%
      rename(
        target_phy=T_phy,
        competitor_phy=C_phy,
        decoy_phy=D_phy,
        target_psy=T_psy,
        competitor_psy=C_psy,
        decoy_psy=D_psy
      ) 
  }
    
  d <- d %>%
    mutate(target_psy=if(exists("target_psy",d)) target_psy else phy2psy(target_phy)) %>%
    mutate(competitor_psy=if(exists("competitor_psy",d)) competitor_psy else phy2psy(competitor_phy)) %>%
    mutate(decoy_psy=if(exists("decoy_psy",d)) decoy_psy else phy2psy(decoy_phy)) %>%
    mutate(across(c(stim_3_psy, stim_3_phy),~na_if(., 0))) %>%
    select({cols_keep}) %>%
    mutate(across(matches("rt", "choice_psy"),as.double)) %>%
    mutate(
      stim_1_phy=case_when(
      transfer_trial_type=="filler_sample"~line_1_in_order,
      TRUE~stim_1_phy
    ),
    stim_2_phy=case_when(
      transfer_trial_type=="filler_sample"~line_2_in_order,
      TRUE~stim_2_phy
    ),
    stim_3_phy=case_when(
      transfer_trial_type=="filler_sample"~line_3_in_order,
      TRUE~stim_3_phy
    ))
    
  return(d)
}

split_data <- function(dataset, cols_keep_learn, cols_keep_transfer,cols_keep_survey){
  learn <- dataset %>%
    filter(screen_id=="learn_trial") %>%
    select(all_of({{cols_keep_learn}})) %>%
    group_by(sub_n) %>%
    arrange(trial_index) %>%
    mutate(trial_num=1:n()) %>%
    relocate(trial_num,.after = trial_type)
  transfer <- dataset %>%
    filter(screen_id=="transfer_trial") %>%
    mutate(correct_2_phy=phy2psy(correct_2_psy)) %>%
    select(all_of({{cols_keep_transfer}})) %>%
    group_by(sub_n) %>%
    arrange(trial_index) %>%
    mutate(trial_num=1:n()) %>%
    relocate(c(transfer_trial_type,trial_num), .after=trial_type) %>%
    mutate(attr_choice=case_when(
      transfer_trial_type=="attraction" & attr_choice=="ERROR-t/c/d could not be determined" & choice==target_phy ~ "target",
      transfer_trial_type=="attraction" & attr_choice=="ERROR-t/c/d could not be determined" & choice==competitor_phy ~ "competitor",
      transfer_trial_type=="attraction" & attr_choice=="ERROR-t/c/d could not be determined" & choice==decoy_phy ~ "decoy",
      TRUE~attr_choice
    ),
    across(c(target_psy,competitor_psy,decoy_psy),~round(.x,digits = 2))
    )
  survey <- dataset %>%
    filter(trial_type=="survey-html-form"|trial_type=="survey-text"|trial_type=="survey-multi-choice") %>%
    select(all_of({{cols_keep_survey}}))

  return(list("learn"=learn, "transfer"=transfer, "survey"=survey))
}

# Doing the reading/cleaning ================================================================
# read in data
dataset <- map_dfr(data_files, 
               ~prep_data(.x, cols_keep_all) %>%
                 mutate(across(c(`rt`,`choice_psy`),as.numeric))
                 ) 

# Split into learn vs transfer
dataset_split <- split_data(dataset, cols_keep_learn, cols_keep_transfer, cols_keep_survey)
learn <- dataset_split[["learn"]]
transfer <- dataset_split[["transfer"]] %>%
  select(-c(date,screen_id,
            choices,stim_names,give_fb),contains("in_order"))
survey <- dataset_split[["survey"]]

# Check number of subjects
n_subs <- length(unique(dataset$sub_n)); n_subs

# Write cleaned datasets to file ================================================================
if(write_data){
  map2(list(dataset,learn,transfer,survey),
       list(glue("{data_dir_cleaned}/dataset_all.csv"),
            glue("{data_dir_cleaned}/dataset_learn.csv"),
            glue("{data_dir_cleaned}/dataset_transfer.csv"),
            glue("{data_dir_cleaned}/dataset_survey.csv")),
       ~write_csv(.x, .y))
}

