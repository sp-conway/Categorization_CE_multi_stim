# Sean Conway
# this code is EXTREMELY slow to run, because I interpolate psychological values for transfer stimuli in a for loop
# Really only needs to be run once (clean data and put it in a single csv for transfer & a single csv for learn)
# Last modified Mar. 2023

# setup =====================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(dplyr)
library(tidyr)
library(fs)
library(purrr)
library(readr)

# controls
save_files <- T

# read in data =====================================================================
raw_files <- dir_ls(here(,"turtles_exp","data","raw"))
dat <- map_dfr(raw_files, 
               ~data.table::fread(.x) %>% mutate(age=as.character(age))) 

# clean data =====================================================================
learn <- dat %>% 
  filter(exp_trial_type=="learn") %>%
  select(-c(success,
            timeout,
            failed_images,
            failed_audio,
            failed_video,
            internal_node_id,
            stimulus,
            view_history,
            order,
            angle_a,
            angle_b,
            angle_c,
            radius_a,
            radius_b,
            radius_c,
            jkl_correct,
            abc_correct,
            transfer_trial_type,
            tc_correct,
            choice_set,
            tc_angle,
            tdc_a,
            tdc_b,
            tdc_c,
            pair,
            category_a,
            category_b,
            category_c,
            question_order)) %>%
  mutate(across(c(rt,angle,radius),as.numeric))

transfer <- dat %>%
  filter(exp_trial_type=="transfer" & screen_id=="transfer_trial") %>%
  mutate(tc_angle_new=case_when( # in the first few subjects tc_angle wasn't recorded, so have to figure it out manually
    transfer_trial_type=="attraction" & choice_set=="binary" & (radius_a==radius_b | angle_a==angle_b) & is.na(tc_angle) ~ 180L,
    transfer_trial_type=="attraction" & choice_set=="trinary" & (radius_a==radius_c | angle_a==angle_c)& is.na(tc_angle)  ~ 180L,
    transfer_trial_type=="attraction" & choice_set=="binary" & (radius_a!=radius_b & angle_a!=angle_b) & is.na(tc_angle) ~ 90L,
    transfer_trial_type=="attraction" & choice_set=="trinary" & (radius_a!=radius_c & angle_a!=angle_c)& is.na(tc_angle)  ~ 90L,
    !is.na(tc_angle)~tc_angle,
    TRUE ~ NA_integer_
  )) %>%
  mutate(across(c(angle_c,radius_c),
                ~na_if(., "null"))) %>%
  select(-c(success,
            timeout,
            failed_images,
            failed_audio,
            failed_video,
            internal_node_id,
            stimulus,
            view_history,
            tc_angle,
            angle,
            radius,
            distribution,
            question_order)) %>%
  rename(tc_angle=tc_angle_new)

# Interpolate transfer trials ====================================================================================
source(here(,"turtles_exp","functions","mds_interp.R"))
mds_res <- read_csv(here(,"turtles_exp","mds","mds_turtles.csv")) 
phys_stim <- read_csv(here(,"turtles_exp","mds","orig_turtles.csv"))
mds <- as.matrix(mds_res[,c(1:2)])
phys <- as.matrix(phys_stim[,c(1:2)])
transfer$angle_a_psy <- numeric(nrow(transfer))
transfer$angle_b_psy <- numeric(nrow(transfer))
transfer$angle_c_psy <- numeric(nrow(transfer))
transfer$radius_a_psy <- numeric(nrow(transfer))
transfer$radius_b_psy <- numeric(nrow(transfer))
transfer$radius_c_psy <- numeric(nrow(transfer))

for(i in 1:nrow(transfer)){
  x <- interp(as.matrix(transfer[i,c("angle_a","radius_a")]),
              mds=mds,
              phys=phys)
  y <- interp(as.matrix(transfer[i,c("angle_b","radius_b")]),
              mds=mds,
              phys=phys)
  transfer[i,]$angle_a_psy <- x[1]
  transfer[i,]$radius_a_psy <- x[2]
  transfer[i,]$angle_b_psy <- y[1]
  transfer[i,]$radius_b_psy <- y[2]
  if(transfer$choice_set[i]=="trinary"){
    z <- interp(as.matrix(as.numeric(transfer[i,c("angle_c","radius_c")])),
                mds=mds,
                phys=phys)
    transfer[i,]$angle_c_psy <- z[1]
    transfer[i,]$radius_c_psy <- z[2]
  }else{
    transfer[i,]$angle_c_psy <- NA_real_
    transfer[i,]$radius_c_psy <- NA_real_
  }
}


# write to files =====================================================================
if(save_files){
  write_csv(learn,here(,"turtles_exp","data","cleaned","exp2_learn.csv"))
  write_csv(transfer,here(,"turtles_exp","data","cleaned","exp2_transfer.csv"))
}
