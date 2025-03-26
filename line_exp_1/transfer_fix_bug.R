# transfer_fix_bug.R
# Sean Conway
# Last modified Mar. 2023

# clear environment 
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(scales)

# Source functions for converting psy2phy and phy2psy
source(here("line_exp_1","functions","psy2phy.R"))

# Source plotting functions
source(here("line_exp_1","functions","plotting_functions.R"))

# read in the transfer data
transfer <- read_csv(here("line_exp_1","data","cleaned","dataset_transfer.csv"))

phy_mean <- 272.9321

corr_choice <- function(...){
  cat_mean <- 272.9321 # defining manually here
  vec <- c(...)
  dists <- abs(vec-cat_mean)
  if(length(dists[dists==min(dists)])>1){ #If there"s more than one right answer, correct is NA
    correct <- 999
  }
  else {
    correct<-vec[which.min(dists)]
  }
  return(correct)
}


transfer_fixed <- transfer %>%
  mutate(line_lens=str_replace_all(line_lens, c("\\["="","\\]"=""))) %>%
  separate(line_lens, sep=",", into = c("line_1", "line_2", "line_3"), fill="right") %>%
  select(-c(line_lens_order,
            choice_line,
            stim_1, 
            stim_2, 
            stim_3,
            stim_1_psy, 
            stim_2_psy,
            stim_3_psy, 
            stim_1_in_order, 
            stim_2_in_order, 
            stim_3_in_order,
            attr_choice, 
            correct)) %>%
  rowwise() %>%
  mutate(across(c(line_1, line_2, line_3),as.numeric),
         line_1_psy=phy2psy(line_1),
         line_2_psy=phy2psy(line_2),
         line_3_psy=phy2psy(line_3),
         line_choice=case_when(
           key_choice=="j" ~ line_1,
           key_choice=="k" ~ line_2,
           key_choice=="l" ~ line_3
         ),
         jnd_choice=case_when(
           key_choice=="j" ~ line_1_psy,
           key_choice=="k" ~ line_2_psy,
           key_choice=="l" ~ line_3_psy
         ),
         correct=case_when(
           line_choice==corr_choice & (transfer_trial_type =="filler" | tc_correct != "both") ~ 1,
           line_choice!=corr_choice & (transfer_trial_type =="filler" | tc_correct != "both") ~ 0
         ),
         target_line=case_when(
           transfer_trial_type=="attraction" & trial_choice_set=="trinary" & target_side=="right" ~ median(c(line_1, line_2, line_3),na.rm=T),
           transfer_trial_type=="attraction" & trial_choice_set=="binary" & target_side=="right" ~ max(c(line_1,line_2)),
           transfer_trial_type=="attraction" & trial_choice_set=="binary" & target_side=="left" ~ min(c(line_1,line_2)),
           transfer_trial_type=="attraction" & trial_choice_set=="trinary" & target_side=="left" ~ median(c(line_1, line_2, line_3),na.rm=T)
         ),
         competitor_line=case_when(
           transfer_trial_type=="attraction" & competitor_side =="right" ~ max(c(line_1, line_2, line_3),na.rm=T),
           transfer_trial_type=="attraction" & competitor_side =="left" ~  min(c(line_1, line_2, line_3),na.rm=T),
         ),
         decoy_line=case_when(
           transfer_trial_type=="attraction" & trial_choice_set=="trinary" & target_side=="right" ~ max(c(line_1, line_2,line_3)),
           transfer_trial_type=="attraction" & trial_choice_set=="trinary" & target_side=="left" ~ min(c(line_1, line_2,line_3))
         ),
         target_jnd=phy2psy(target_line),
         competitor_jnd=phy2psy(competitor_line),
         decoy_jnd=phy2psy(decoy_line),
         attr_choice=case_when(
           line_choice==target_line~"target",
           line_choice==competitor_line~"competitor",
           line_choice==decoy_line~"decoy"
         ),
         corr_choice_optimal=case_when(
           trial_choice_set=="binary" & tc_correct!="both" ~ corr_choice(c(line_1,line_2)),
           trial_choice_set=="trinary"& tc_correct!="both" ~ corr_choice(c(line_1,line_2,line_3))
         )
  ) 



write_csv(transfer_fixed, file=here("line_exp_1","data","cleaned","dataset_transfer_FIXED.csv"))




