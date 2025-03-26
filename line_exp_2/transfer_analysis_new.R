# transfer_analysis.R
# analyzing transfer data from 2nd line categorization experiment
# Sean Conway
# Last modified June 2024

# Setup ==============================================================================
# clear environment
rm(list=ls())
 
# libraries
library(tidyverse)
library(here)
library(glue)
library(patchwork)

# Source plotting functions
source("line_exp_2","functions","plotting_functions.R"))
source("line_exp_2","functions","density_functions_2.R"))

# read in data
transfer <- data.table::fread("line_exp_2","data","cleaned","dataset_transfer.csv")) %>%
  as_tibble() %>%
  mutate(across(c(attr_choice,
                  sub_n,
                  transfer_trial_type,
                  trial_choice_set,
                  side_correct,
                  tc_correct,
                  choice,
                  key_choice),
                as.factor))
# Number of subjects
n_subs <- length(unique(transfer$sub_n))

# Attraction effect trial specs
attr_eff_specs <- read_csv("line_exp_2","trials","attr_eff_trials_long_1b.csv"))

# Function for computing 95% CI
compute_ci <- function(mean,sd, n_subs,side){
  x <- switch(as.character(side),
              "lower"=mean-qt(.025, n_subs-1,lower.tail=F)*(sd/sqrt(n_subs)),
              "upper"=mean+qt(.025, n_subs-1,lower.tail=F)*(sd/sqrt(n_subs))
  )
  return(x)
} 

attraction <- transfer %>%
  filter(str_detect(transfer_trial_type,"attraction")) %>%
  mutate(choice=case_when(
    attr_choice=="competitor" & competitor_side=="left"~"s",
    attr_choice=="competitor" & competitor_side=="right"~"l",
    attr_choice=="target" & target_side=="left"~"s",
    attr_choice=="target" & target_side=="right"~"l",
    attr_choice=="decoy"~"d"
  ),
  set=case_when(
    competitor_side=="left"~"[s,l,dl]",
    competitor_side=="right"~"[s,l,ds]"
  ),
  side_correct=str_replace_all(side_correct,
                               c("left"="short",
                                 "right"="long",
                                 "equal"="equal"))) %>%
  mutate(across(c(choice,set,side_correct),as.factor))

# trinary minus trinary analysis ============================================================
tri_diff1 <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  group_by(sub_n, side_correct,set, choice) %>%
  summarise(n=n()) %>%
  group_by(sub_n, side_correct,set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = c(set, choice), values_from = prop,
              values_fill = 0,
              names_sep = "_") %>%
  mutate(s_diff=`[s,l,ds]_s`-`[s,l,dl]_s`,
         l_diff=`[s,l,dl]_l`-`[s,l,ds]_l`) %>%
  select(sub_n,side_correct,s_diff,l_diff)

tridiff1_avg <- tri_diff1 %>%
  group_by(side_correct) %>%
  summarise(sdiff_mean=mean(s_diff),
            sdiff_se = sd(s_diff)/sqrt(n()-1),
            ldiff_mean=mean(l_diff),
            ldiff_se = sd(l_diff)/sqrt(n()-1),
            sdiff_cilower=sdiff_mean-qt(.025,n()-1,lower.tail = F)*sdiff_se,
            sdiff_ciupper=sdiff_mean+qt(.025,n()-1,lower.tail = F)*sdiff_se,
            ldiff_cilower=ldiff_mean-qt(.025,n()-1,lower.tail = F)*ldiff_se,
            ldiff_ciupper=ldiff_mean+qt(.025,n()-1,lower.tail = F)*ldiff_se) %>%
  ungroup() %>%
  select(-contains("se")) %>%
  pivot_longer(-side_correct) %>%
  separate(name,into=c("effect","stat"),sep="_") %>%
  pivot_wider(names_from = stat, values_from=value) %>%
  mutate(effect=case_when(
    str_detect(effect,"sdiff")~"short effect",
    str_detect(effect,"ldiff")~"long effect",
  ))
tridiff1_avg %>%
  ggplot(aes(effect,mean))+
  geom_col(position="dodge",fill="lightblue",col="black")+
  geom_errorbar(aes(ymin=cilower,ymax=ciupper),width=.2)+
  facet_wrap(vars(side_correct),labeller = label_both)+
  ggthemes::theme_few()+
  theme(text=element_text(size=10))
ggsave("line_exp_2","plots","tri_effect_by_sidecorrect_avg.jpg"),
       width=5,height=4)

tri_diff1 %>%
  pivot_longer(-c(sub_n,side_correct), values_to = "effect") %>%
  mutate(name=str_replace_all(name,c("s_diff"="short effect","l_diff"="long effect"))) %>%
  ggplot(aes(effect))+
  geom_histogram(fill="lightblue",col="black")+
  facet_grid(side_correct~name)+
  ggthemes::theme_few()
ggsave("line_exp_2","plots","tri_effect_by_sidecorrect_hist.jpg"),
       width=5,height=4)

tri_diff2 <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  group_by(sub_n, set, choice) %>%
  summarise(n=n()) %>%
  group_by(sub_n,set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = c(set, choice), values_from = prop,
              values_fill = 0,
              names_sep = "_") %>%
  mutate(s_diff=`[s,l,ds]_s`-`[s,l,dl]_s`,
         l_diff=`[s,l,dl]_l`-`[s,l,ds]_l`) %>%
  select(sub_n,s_diff,l_diff) 

tridiff2_avg <- tri_diff2 %>%
  summarise(sdiff_mean=mean(s_diff),
            sdiff_se = sd(s_diff)/sqrt(n()-1),
            ldiff_mean=mean(l_diff),
            ldiff_se = sd(l_diff)/sqrt(n()-1),
            sdiff_cilower=sdiff_mean-qt(.025,n()-1,lower.tail = F)*sdiff_se,
            sdiff_ciupper=sdiff_mean+qt(.025,n()-1,lower.tail = F)*sdiff_se,
            ldiff_cilower=ldiff_mean-qt(.025,n()-1,lower.tail = F)*ldiff_se,
            ldiff_ciupper=ldiff_mean+qt(.025,n()-1,lower.tail = F)*ldiff_se) %>%
  select(-contains("se")) %>%
  pivot_longer(everything()) %>%
  separate(name,into=c("effect","stat"),sep="_") %>%
  pivot_wider(names_from = stat, values_from=value) %>%
  mutate(effect=case_when(
    str_detect(effect,"sdiff")~"short effect",
    str_detect(effect,"ldiff")~"long effect",
  ))
tri_diff2 %>%
  pivot_longer(-sub_n, values_to = "effect") %>%
  mutate(name=str_replace_all(name,c("s_diff"="short effect","l_diff"="long effect"))) %>%
  ggplot(aes(effect))+
  geom_histogram(fill="lightblue",col="black")+
  facet_wrap(vars(name))+
  ggthemes::theme_few()
ggsave("line_exp_2","plots","tri_effect_collapsed_hist.jpg"),
       width=5,height=4)
tridiff2_avg %>%
  ggplot(aes(effect,mean))+
  geom_col(position="dodge",fill="lightblue",col="black")+
  geom_errorbar(aes(ymin=cilower,ymax=ciupper),width=.2)+
  ggthemes::theme_few()
ggsave("line_exp_2","plots","tri_effect_collapsed_avg.jpg"),
       width=5,height=4)

   