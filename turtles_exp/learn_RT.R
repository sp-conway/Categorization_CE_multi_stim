# Sean Conway
# Last modified Mar. 2023

# setup =====================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(ggsci)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rtdists)

# controls
save_plots <- T

# read in data =====================================================================
learn <- data.table::fread(here(,"turtles_exp","data","cleaned","exp2_learn.csv")) %>%
  as_tibble()

# Quantizing =====================================================================
qts <- c(.1,.3,.5,.7,.9)

rt_quants_all_indiv <- learn %>%
  group_by(sub_n, distribution, choice) %>%
  summarise(rt=quantile(rt, probs = qts)) %>%
  mutate(quantile=qts) %>%
  ungroup()

mean_rt_all <- rt_quants_all_indiv %>%
  group_by(distribution,choice,quantile) %>%
  summarise(mean_rt=mean(rt)) %>%
  ungroup() %>%
  mutate(choice=case_when(
    choice==0~"no",
    choice==1~"yes"
  ))
ggplot(mean_rt_all, aes(quantile,mean_rt,col=choice))+
  geom_point(size=3.5,alpha=.5)+
  scale_x_continuous(breaks=qts)+
  scale_color_tron()+
  facet_grid(vars(distribution),scales="free_x")+
  ggthemes::theme_few()
  
  