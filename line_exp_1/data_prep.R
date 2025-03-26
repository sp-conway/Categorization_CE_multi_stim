# data_prep.R
# wrangling/cleaning data from line categorization experiment 1
# Sean Conway
# Last modified Mar. 2023

# clear environment
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(glue)

# source psy2phy function
source(here("line_exp_1","functions","psy2phy.R"))

# Reading in the data =================================================================

# get file names with extension
files <- list.files(here("line_exp_1","data","raw_data")) %>%
  map(., ~glue("line_exp_1/data/raw_data/{.x}"))

# data reading/cleaning function
# hard-coded in the columns I want
read_clean_data <- function(file){
  print(file)
  my_df <- data.table::fread(file) # read in individual subject data files
  my_df_clean <- my_df %>% # clean the dataset
    as_tibble() %>% # convert to tibble
    filter(trial_type=="categorize-lines") %>% # only need categorization trials 
    select(date, # hard coded in columns needed (jspsych saves other unnecessary variables)
           sub_n, 
           time_elapsed, 
           screen_id, 
           trial_type, 
           line_length,
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
           jnds, 
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
           yn_choice, 
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
           trial_type=screen_id,
           line_len=line_length) %>%
    mutate(trial_type=str_replace(trial_type, "_trial","")) %>% # cleaning up this column
    group_by(sub_n, trial_type) %>% # need an actual trial number variable for each subject, trial type
    arrange(time_elapsed) %>% # can use time elapsed to order rows
    mutate(trial_num=1:n()) %>% # create trial number
    relocate(trial_num, .after=plugin) %>% # move this column to where I want it
    select(-time_elapsed) # no longer need this column
  
  return(my_df_clean)
}

# Actually do the reading
dataset <- map_dfr(files, read_clean_data)

# More data cleaning =================================================================
# filter out stuff for learning
learn <- dataset %>% 
  filter(trial_type=="learn") %>%
  select(-c(plugin,
            line_lens, 
            line_lens_order,
            stim_1, 
            stim_2, 
            stim_3,
            stim_1_psy, 
            stim_2_psy, 
            stim_3_psy,
            stim_1_in_order, 
            stim_2_in_order, 
            stim_3_in_order, 
            difficulty_level, 
            target_side, 
            competitor_side, 
            corr_choice,
            tc_correct, 
            correct_dist_to_mean, 
            side_correct,
            attr_choice,
            transfer_trial_type,
            trial_choice_set,
            choice)) %>%
  rename(distribution=in_cat) %>%
  mutate(rt=as.numeric(rt)) %>%
  filter(rt>=100 & rt<30000) # IMPORTANT - filter out RTs

# Filter out stuff for transfer
transfer <- dataset %>%
  filter(trial_type=="transfer") %>%
  select(-c(plugin, line_len, yn_choice, jnds, in_cat),key_choice) %>% # these columns aren"t needed
  rename(choice_line=choice) %>%
  mutate(rt=as.numeric(rt)) %>%
  filter(rt>=100 & rt<30000) # IMPORTANT - filter out RTs

# transfer - fixed bug in code =================================================================

# Source plotting functions
source(here("line_exp_1","functions","plotting_functions.R"))

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

# bug in exp. code caused the stimuli positions to not be randomized on a trial-by-trial basis
# fixing this here
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


# Write data to files =================================================================

# Learning + transfer combined
write_csv(dataset,here("line_exp_1","data","cleaned", "dataset_all.csv"))

# Learning data
write_csv(learn, 
          file=here("line_exp_1","data","cleaned", "dataset_learn.csv"))

# Transfer data
write_csv(transfer_fixed, 
          file=here("line_exp_1","data","cleaned", "dataset_transfer.csv"))

# Keeping track of number of subjects run =================================================================
n_sub <- length(unique(dataset$sub_n))
subn_mssg <- glue("There have been {n_sub} subjects run as of {td_date}.",
                  td_date=lubridate::today())
cat(subn_mssg)
write_lines(subn_mssg, file=here("line_exp_1","notes","n_subs.txt"))
