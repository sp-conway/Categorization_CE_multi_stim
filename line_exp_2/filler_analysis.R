# filler_analysis.R
# analyzing filler trial data from line exp 2
# Sean Conway
# last modified Feb. 2022

# Setup ==============================================================================
# clear environment
rm(list=ls())

# libraries
library(tidyverse)
library(here)
library(BayesFactor)
library(glue)
library(patchwork)
library(tidybayes)

# Source plotting functions
source(here("line_exp_2","functions","plotting_functions.R"))

# source category probability functions
source(here("line_exp_2","functions","density_functions_2.R"))

# controls
save_plots <- T

# read in data & filter  ==============================================================================
fillers <- data.table::fread(here("line_exp_2","data","cleaned","dataset_transfer.csv")) %>%
  as_tibble() %>%
  filter(str_detect(transfer_trial_type,"filler")) %>%
  select(sub_n, transfer_trial_type, trial_num,
         trial_choice_set, line_1_in_order,
         line_2_in_order, line_3_in_order,
         psy_1_in_order, psy_2_in_order,
         psy_3_in_order,
         correct_1_psy,correct_2_psy,
         choice, choice_psy, correct, rt) %>%
  filter(str_detect(transfer_trial_type,"filler")) %>%
  mutate(across(c(psy_1_in_order, psy_2_in_order, 
                  psy_3_in_order),.fns=list(prob_in),
                  .names="{.col}_prob_in"))

# analysis  ==============================================================================
# calculating relative likelihood of choice
fillers$choice_lik <- map_dbl(fillers$choice_psy, prob_in)
fillers <- fillers %>%
  rowwise() %>%
  mutate(choice_rel_lik=log(choice_lik/max(c(psy_1_in_order_prob_in,
                                             psy_2_in_order_prob_in,
                                             psy_3_in_order_prob_in),
                                           na.rm=T))) %>%
  ungroup() 
fillers %>% 
  group_by(transfer_trial_type, trial_choice_set) %>%
  summarise(med_rel_lik=median(choice_rel_lik)) 

# plotting ==============================================================================

rel_lik_hist <- fillers %>%
  ggplot(aes(choice_rel_lik))+
  geom_histogram(fill="lightblue",binwidth=.25,aes(y=..density..))+
  facet_grid(trial_choice_set~.)+
  labs(x="relative likelihood of choice",
       y="count")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
rel_lik_hist

rel_lik_bplot <- fillers %>%
  group_by(sub_n, trial_choice_set, transfer_trial_type) %>%
  summarise(md_rel_lik=median(choice_rel_lik)) %>%
  group_by(trial_choice_set, transfer_trial_type) %>%
  summarise(mean_md_rel_lik=mean(md_rel_lik)) %>%
  ungroup() %>%
  ggplot(aes(x=trial_choice_set,y=mean_md_rel_lik,fill=transfer_trial_type))+
  geom_col(position="dodge")+
  scale_fill_discrete(name="trial type")+
  labs(y="log(lik/max(lik))",x="choice set")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
rel_lik_bplot

# save figures  ==============================================================================
if(save_plots){
  pdf(here("line_exp_2",'plots','fillers_rel_like_plots.pdf'))
  rel_lik_hist
  rel_lik_bplot
  dev.off()
}

