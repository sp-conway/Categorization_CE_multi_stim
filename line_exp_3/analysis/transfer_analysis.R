rm(list=ls())
library(here)
library(tidyverse)
library(fs)


# grab distribution params ============================================================================================================================
source(here("line_exp_3","dist_funcs.R"))
source(here("line_exp_3","category_params.R"))


d <- here("multi_stim/line_exp_3/data/clean/transfer_data.csv") %>%
  read_csv() %>%
  mutate(correct=as.factor(correct),
         dens_in_1=case_when(
           exp_condition=="in"~
         ))

crit <- d %>%
  filter(trial_type=="attraction")
fill <- d %>%
  filter(trial_type=="filler")
# prop correct ==================================================================================
fill_corr <- fill %>%
  group_by(participant,set_type) %>%
  summarise(prop_corr=mean(correct==1)) %>%
  ungroup() %>%
  left_join(distinct(fill, participant,exp_condition))
fill_corr %>%
  ggplot(aes(prop_corr))+
  geom_histogram(fill="lightblue")+
  scale_x_continuous(limits=c(0,1))+
  facet_grid(exp_condition~set_type)+
  ggthemes::theme_few()
