rm(list=ls())
# clean data
# separate out learning and transfer
# filter out RTs < .1 sec or >10 sec
library(tidyverse)
library(fs)
library(glue)
library(here)

data_files_raw <- here("line_exp_3","data","raw") %>%
    dir_ls(regexp = ".csv")
data_dir_cleaned <- here("line_exp_3","data","clean")
dir_create(data_dir_cleaned)

clean_learn <- function(f){
  print(f)
  d <- data.table::fread(f) %>%
    as_tibble()
  if(nrow(d)==0 | !any(str_detect("exp_trial_type",colnames(d)))) return(NULL)
  dd <- d %>%
    filter(str_detect(exp_trial_type,"transfer",negate=T) & !is.na(jnds)) %>% # didn't store exp trial type for learning, this just removes transfer trials and instructions/debrief, since only learning had a jnds value
    select(participant, computer_number,exp_condition, learn_trials.thisTrialN, dens, jnds, line_len, distribution, 
           learn_trials.key_resp.keys, learn_trials.key_resp.rt,learn_trials.key_resp.corr) %>%
    rename(trial_n=learn_trials.thisTrialN,
           rt=learn_trials.key_resp.rt,
           key_resp=learn_trials.key_resp.keys,
           correct=learn_trials.key_resp.corr) %>%
    mutate(resp=str_replace_all(key_resp,c("^u$"="in","^i$"="out")),
           trial_n=trial_n+1,
           participant=case_when(
             participant==61~as.numeric(paste0(as.numeric(participant),as.numeric(computer_number))), # this number used twice
             T~participant
           )) %>%
    relocate(resp,.before=correct) 
  if(nrow(dd)!=200){
    return(NULL)
  }else{
    dd <- dd %>%
      filter(rt>=.1 & rt<=10)
    return(dd)
  }
}

check_learn_correct <- function(d){
  dcheck <- d %>%
    mutate(c=case_when(
      key_resp=="u" & distribution=="in" & correct==1 ~ T,
      key_resp=="i" & distribution=="out" & correct==1 ~T,
      key_resp=="u" & distribution=="out" & correct==0 ~ T,
      key_resp=="i" & distribution=="in" & correct==0 ~T))
  if(all(dcheck$c)){
    message("Passed learning check :)")
  }else{
    warning("!!!!!Failed learning check!!!!!!")
  }
}

learn <- map(data_files_raw, clean_learn) %>%
  list_rbind()
check_learn_correct(learn)


clean_transfer <- function(f){
  print(f)
  # browser()
  d <- data.table::fread(f) %>%
    as_tibble()
  if(nrow(d)==0 | !any(str_detect("exp_trial_type",colnames(d)))) return(NULL)
  dd <- d %>%
    filter(!is.na(transfer_binary_dummy.key_resp_transfer_binary.rt)|
           !is.na(transfer_trinary_dummy.key_resp_transfer_trinary.rt)) %>%
    mutate(rt=if_else(set_type=="binary",transfer_binary_dummy.key_resp_transfer_binary.rt,transfer_trinary_dummy.key_resp_transfer_trinary.rt), # renaming variables, cleaning things up
           key_correct_1=if_else(set_type=="binary",key_correct_transfer_binary_1,key_correct_transfer_trinary_1),
           key_correct_2=if_else(set_type=="binary",key_correct_transfer_binary_2,key_correct_transfer_trinary_2),
           correct=if_else(set_type=="binary",key_resp_transfer_binary.corr,key_resp_transfer_trinary.corr),
           key_resp=if_else(set_type=="binary",transfer_binary_dummy.key_resp_transfer_binary.keys,transfer_trinary_dummy.key_resp_transfer_trinary.keys),
           across(c(set,set_type,difficulty_level,stim_3_psy,stim_3_phy),~na_if(.x,"None")),
           across(c(stim_1_psy_shuffled,stim_2_psy_shuffled,stim_3_psy_shuffled,
                    stim_1_phy_shuffled,stim_2_phy_shuffled,stim_3_phy_shuffled),~na_if(.x,0)),
           trial_n=transfer_trials.thisTrialN+1,
           block=transfer_binary_dummy.thisRepN+1,
           choice_num=case_when(
             key_resp=="j"~1,
             key_resp=="k"~2,
             key_resp=="l"~3
           ),
           choice_stim_psy=case_when(
             choice_num==1~stim_1_psy_shuffled,
             choice_num==2~stim_2_psy_shuffled,
             choice_num==3~stim_3_psy_shuffled
           ),
           choice_stim_phy=case_when(
             choice_num==1~stim_1_phy_shuffled,
             choice_num==2~stim_2_phy_shuffled,
             choice_num==3~stim_3_phy_shuffled
           )) %>%
    rowwise() %>%
    mutate(stim_min=case_when(
      trial_type=="filler"~NA_integer_,
      set_type=="trinary"~which.min(c(stim_1_psy_shuffled,stim_2_psy_shuffled,stim_3_psy_shuffled)),
      set_type=="binary"~which.min(c(stim_1_psy_shuffled,stim_2_psy_shuffled))),
      stim_max=case_when(
        trial_type=="filler"~NA_integer_,
        set_type=="trinary"~which.max(c(stim_1_psy_shuffled,stim_2_psy_shuffled,stim_3_psy_shuffled)),
        set_type=="binary"~which.max(c(stim_1_psy_shuffled,stim_2_psy_shuffled))),
      stim_med=case_when(
        trial_type=="filler"~NA_integer_,
        set_type=="trinary" & stim_min==1 & stim_max==3 ~ 2,
        set_type=="trinary" & stim_min==1 & stim_max==2 ~ 3,
        set_type=="trinary" & stim_min==2 & stim_max==1 ~ 3,
        set_type=="trinary" & stim_min==2 & stim_max==3 ~ 1,
        set_type=="trinary" & stim_min==3 & stim_max==1 ~ 2,
        set_type=="trinary" & stim_min==3 & stim_max==2 ~ 1,
        set_type=="binary"~NA_integer_
      )) %>%
    ungroup()
  if(any(dd$stim_min==dd$stim_max,na.rm = T)) stop()
  if(any(dd$stim_1_psy_shuffled==dd$stim_2_psy_shuffled & dd$stim_1_psy_shuffled==dd$stim_3_psy_shuffled,na.rm = T)) stop()
  if(any(dd$stim_1_psy_shuffled==dd$stim_2_psy_shuffled | dd$stim_1_psy_shuffled==dd$stim_3_psy_shuffled,na.rm = T)) stop()
  ddd <- dd %>% 
    mutate(choice_name=case_when(
      set_type=="filler"~NA_character_,
      # choice set l-s
      set=="l-s" & choice_num==stim_min ~ "s",
      set=="l-s" & choice_num==stim_max ~ "l",
      
      # Choice set l-s-ds
      set=="l-s-ds" & choice_num==stim_min ~ "ds",
      set=="l-s-ds" & choice_num==stim_med ~ "s",
      set=="l-s-ds" & choice_num==stim_max ~ "l",
      
      # Choice set l-s-dl
      set=="l-s-dl" & choice_num==stim_min ~ "s",
      set=="l-s-dl" & choice_num==stim_med ~ "l",
      set=="l-s-dl" & choice_num==stim_max ~ "dl",
    )) %>%
    select(participant,computer_number,exp_condition,block,trial_n,
           trial_type,set,set_type,difficulty_level,stim_1_psy_shuffled,stim_2_psy_shuffled,stim_3_psy_shuffled,
           stim_1_phy_shuffled,stim_2_phy_shuffled,stim_3_phy_shuffled,choice_stim_psy,key_resp,choice_name,correct,rt)
    if(nrow(ddd)!=288){
    return(NULL)
  }else{
    dddd <- ddd %>%
      filter(rt>=.1 & rt<=10)
    return(dddd)
  }
}

transfer <- map_dfr(data_files_raw, clean_transfer)
check_transfer <- function(d){
  # browser()
  dc <- d %>%
    mutate(check1=str_detect(key_resp,"d",negate=T) | set_type!="binary")
  if(all(dc$check1)){
    message("Passed transfer check :)")
  }else{
    warning("!!!!!Failed transfer check!!!!!!")
  }
}
check_transfer(transfer)

write_csv(learn,path(data_dir_cleaned,"learn_data.csv"))
write_csv(transfer,path(data_dir_cleaned,"transfer_data.csv"))

learn %>%
  distinct(participant,exp_condition) %>%
  group_by(exp_condition) %>%
  summarise(n=n())
