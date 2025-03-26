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
source(here("line_exp_2","functions","density_functions_2.R"))

# read in data
transfer <- data.table::fread(here("line_exp_2","data","cleaned","dataset_transfer.csv")) %>%
  as_tibble() %>%
  mutate(across(c(attr_choice,
                  sub_n,
                  transfer_trial_type,
                  trial_choice_set,
                  side_correct,
                  tc_correct,
                  choice_psy,
                  key_choice),
                as.factor))

# attraction effect trial data
attraction <- transfer %>%
  filter(transfer_trial_type=="attraction")

trial_count <- transfer %>%
  count(sub_n) %>%
  rename(trial_count=n)

binary <- attraction %>% 
  filter(trial_choice_set=="binary") %>%
  left_join(., trial_count)

binary %>% 
  count(sub_n,attr_choice) %>%
  group_by(sub_n) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(attr_choice) %>%
  summarise(mean=mean(prop),
            median=median(prop))

binary %>% 
  group_by(trial_count, target_side) %>%
  summarise(mean_tar=mean(target_phy,na.rm=T),
            mean_comp=mean(competitor_phy,na.rm=T))

bi_count <- binary %>%
  count(attr_choice)

binom.test(bi_count$n, n=sum(bi_count$n))

proportionBF(bi_count$n[1], N=sum(bi_count$n),p =.5)


binary_new <- binary %>%
  rowwise() %>%
  mutate(
    target_key=case_when(
      target_side=="left" & line_1_in_order==min(c(line_1_in_order,line_2_in_order)) ~ "j",
      target_side=="left" & line_1_in_order==max(c(line_1_in_order,line_2_in_order)) ~ "k",
      target_side=="right" & line_1_in_order==min(c(line_1_in_order,line_2_in_order)) ~ "k",
      target_side=="right" & line_1_in_order==max(c(line_1_in_order,line_2_in_order)) ~ "j",
    ),
    competitor_key=case_when(
      target_side=="left" & line_1_in_order==min(c(line_1_in_order,line_2_in_order)) ~ "k",
      target_side=="left" & line_1_in_order==max(c(line_1_in_order,line_2_in_order)) ~ "j",
      target_side=="right" & line_1_in_order==min(c(line_1_in_order,line_2_in_order)) ~ "j",
      target_side=="right" & line_1_in_order==max(c(line_1_in_order,line_2_in_order)) ~ "k"
    ),
    tc_choice=case_when(
      key_choice==target_key~"target",
      key_choice==competitor_key~"competitor"
    ),
    tar_phy_2=case_when(
      target_side=="left" ~ min(c(line_1_in_order,line_2_in_order)),
      target_side=="right" ~ max(c(line_1_in_order,line_2_in_order))
    ),
    comp_phy_2=case_when(
      target_side=="left" ~ max(c(line_1_in_order,line_2_in_order)),
      target_side=="right" ~ min(c(line_1_in_order,line_2_in_order))
    ),
    tc_choice_2=case_when(
      choice==tar_phy_2 ~ "target",
      choice==comp_phy_2 ~ "competitor"
    )
  ) %>%
  ungroup()

binary_new %>%
  count(tc_choice)

binary_new %>%
  count(tc_choice_2)

binary %>%
  group_by(tc_correct, target_side) %>%
  summarise(mean(trial_num))


tc_sub_props <- binary %>%
  mutate(attr_choice=factor(attr_choice,levels=c("target","competitor"))) %>%
  count(sub_n, attr_choice) %>%
  group_by(sub_n) %>%
  mutate(prop=n/sum(n))

tc_sub_props %>%
  group_by(attr_choice) %>%
  summarise(mean(prop))

ggplot(tc_sub_props, aes(prop))+
  geom_histogram(fill="lightblue",col="black")+
  facet_grid(attr_choice~.)+
  ggthemes::theme_few()


binary %>%
  filter(transfer_trial_type=="attraction") %>%
  count(sub_n, choice) %>%
  group_by(sub_n) %>%
  mutate(prop=n/sum(n)) %>%
  ggplot(aes(choice,prop))+
  geom_jitter(alpha=.75)

