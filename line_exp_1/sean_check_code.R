rm(list=ls())
files <- "/Users/seanconway/Downloads/line_cat_multi_stim_Sean_test_1.csv"

# libraries
library(here)
library(tidyverse)
library(glue)

# source psy2phy function
source(here(,"line_exp_1","functions","psy2phy.R"))
# data reading/cleaning function
# hard-coded in the columns I want
read_clean_data <- function(file){
  my_df <- data.table::fread(file) # read in individual subject data files
  my_df_clean <- my_df %>% # clean the dataset
    as_tibble() %>% # convert to tibble
    filter(trial_type=="categorize-lines") %>% # only need categorization trials 
    select(date, # hard coded in columns needed (jspsych saves other unnecessary variables)
           time_elapsed, 
           screen_id, 
           trial_type, 
           line_lens, 
           line_lens_order,
           in_cat, 
           stim_1, 
           stim_2, 
           stim_3,
           stim_1_psy, 
           stim_2_psy, 
           stim_3_psy,
           stim_1_in_order, 
           stim_2_in_order, 
           stim_3_in_order,
           transfer_trial_type, 
           trial_choice_set,
           difficulty_level, 
           target_side, 
           competitor_side, 
           corr_choice,
           tc_correct, 
           correct_dist_to_mean, 
           side_correct, 
           key_choice,
           choice, 
           attr_choice, 
           correct,
           rt) %>%
    mutate(
      across(matches("rt"), as.numeric), # convert rt to numeric
      across(c(difficulty_level, # empty values sometimes saved as "", convert to NA
               target_side, 
               competitor_side,
               tc_correct),
             ~na_if(x=., y="")),
      correct=case_when( # if both options were correct, correct needs to be NA
        tc_correct=="both" ~ NA,
        TRUE ~ as.logical(`correct`)),
      across(c(stim_3,
               stim_3_psy), # if there was no 3rd option (line_len=0), needs to be NA
             ~na_if(.,0))
    ) %>%
    rename(plugin=trial_type, # renaming some variables 
           trial_type=screen_id) %>%
    mutate(trial_type=str_replace(trial_type, "_trial","")) %>% # cleaning up this column
    group_by(trial_type) %>% # need an actual trial number variable for each subject, trial type
    arrange(time_elapsed) %>% # can use time elapsed to order rows
    mutate(trial_num=1:n()) %>% # create trial number
    relocate(trial_num, .after=plugin) %>% # move this column to where I want it
    select(-time_elapsed) # no longer need this column
  
  return(my_df_clean)
}

# Actually do the reading
transfer <- map_dfr(files, read_clean_data) 
# Source functions for converting psy2phy and phy2psy
source(here(,"line_exp_1","functions","psy2phy.R"))

# Source plotting functions
source(here(,"line_exp_1","functions","plotting_functions.R"))

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

transfer_fixed %>%
  count(trial_choice_set, attr_choice)