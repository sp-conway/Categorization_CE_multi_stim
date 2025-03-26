rm(list=ls())
library(here)
library(tidyverse)
library(fs)

# grab distribution params ============================================================================================================================
source(here("line_exp_3","dist_funcs.R"))
source(here("line_exp_3","category_params.R"))

# optimal responding just means responding in when the likelihood of in is higher than that of out
# and vice versa
compute_llikr <- function(jnds, m, s, nc, min, max) log(compute_density(jnds, m, s, nc)/dunif(jnds,min, max))

# read in data ================================================================================================
d <- here("multi_stim/line_exp_3/data/clean/learn_data.csv") %>%
  read_csv() %>%
  mutate(llikr=case_when(
           exp_condition=="left"~compute_llikr(jnds, in_mean_jnds_left, in_sd_jnds, in_left_nc, jnd_min, jnd_max),
           exp_condition=="right"~compute_llikr(jnds, in_mean_jnds_right, in_sd_jnds, in_right_nc, jnd_min, jnd_max),
           exp_condition=="centered"~compute_llikr(jnds, in_mean_jnds_centered, in_sd_jnds, in_centered_nc, jnd_min, jnd_max),
         ),
         resp_opt=case_when(
           llikr<0 & resp=="out"~1,
           llikr>0 & resp=="in"~1,
           T~0
         ),
         across(c(resp_opt,correct)))

# check stimuli  ========================================================================
stim <- d %>%
  distinct(exp_condition, jnds, line_len, distribution)
stim %>%
  ggplot(aes(jnds))+
  geom_histogram(fill="lightblue")+
  facet_grid(exp_condition~distribution)+
  ggthemes::theme_few()

stim %>%
  ggplot(aes(line_len))+
  geom_histogram(fill="lightblue")+
  facet_grid(exp_condition~distribution)+
  ggthemes::theme_few()
# check prop correct ========================================================================
prop_corr <- d %>%
  group_by(participant) %>%
  summarise(prop_correct=mean(correct==1)) %>%
  ungroup() %>%
  left_join(distinct(d,participant,exp_condition))
ggplot(prop_corr,aes(prop_correct))+
  geom_histogram(fill="lightblue")+
  facet_grid(exp_condition~.)+
  ggthemes::theme_few()

# check prop optimal ========================================================================
prop_opt <- d %>%
  group_by(participant) %>%
  summarise(prop_optimal=mean(resp_opt==1)) %>%
  ungroup() %>%
  left_join(distinct(d,participant,exp_condition))
ggplot(prop_opt,aes(prop_optimal))+
  geom_histogram(fill="lightblue")+
  facet_grid(exp_condition~.)+
  ggthemes::theme_few()

# check response props ========================================================================
resp_props <- d %>%
  group_by(participant) %>%
  summarise(p_in=mean(resp=="in")) %>%
  ungroup() %>%
  left_join(distinct(d,participant,exp_condition))
ggplot(resp_props,aes(p_in))+
  geom_histogram(fill="lightblue")+
  facet_grid(exp_condition~.)+
  ggthemes::theme_few()

# RTs ================================================================================
rt_mean_quants <- d %>%
  group_by(participant, correct) %>%
  reframe(rt=quantile(rt, probs = seq(0,1,.2)),
          q=seq(0,1,.2)) %>%
  ungroup() %>%
  left_join(distinct(d,participant,exp_condition)) %>%
  group_by(exp_condition, correct, q) %>%
  summarise(m_rt=mean(rt)) %>%
  ungroup()
rt_mean_quants %>%
  ggplot(aes(q,m_rt,col=correct))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks=seq(0,1,.2))+
  facet_grid(exp_condition~.)+
  ggthemes::theme_few()

glm(d)