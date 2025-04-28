rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(lemon)

# grab distribution params ============================================================================================================================
source(here("line_exp_3","dist_funcs.R"))
source(here("line_exp_3","category_params.R"))


d <- here("line_exp_3/data/clean/transfer_data.csv") %>%
  read_csv() %>%
  mutate(correct=as.factor(correct))
crit <- d %>%
  filter(trial_type=="attraction") %>%
  mutate(choice_name=factor(choice_name,levels=c("ds","s","l","dl")))
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

# filter participants based on filler performance ==================================================
fill_opt <- fill %>%
  rowwise() %>%
  mutate(dens_1=case_when(
    exp_condition=="left"~compute_density(stim_1_psy_shuffled, in_mean_jnds_left, in_sd_jnds, in_left_nc),
    exp_condition=="centered"~compute_density(stim_1_psy_shuffled, in_mean_jnds_centered, in_sd_jnds, in_centered_nc),
    exp_condition=="right"~compute_density(stim_1_psy_shuffled, in_mean_jnds_right, in_sd_jnds, in_right_nc),
  ),
  dens_2=case_when(
    exp_condition=="left"~compute_density(stim_2_psy_shuffled, in_mean_jnds_left, in_sd_jnds, in_left_nc),
    exp_condition=="centered"~compute_density(stim_2_psy_shuffled, in_mean_jnds_centered, in_sd_jnds, in_centered_nc),
    exp_condition=="right"~compute_density(stim_2_psy_shuffled, in_mean_jnds_right, in_sd_jnds, in_right_nc),
  ),
  dens_3=case_when(
    exp_condition=="left"~compute_density(stim_3_psy_shuffled, in_mean_jnds_left, in_sd_jnds, in_left_nc),
    exp_condition=="centered"~compute_density(stim_3_psy_shuffled, in_mean_jnds_centered, in_sd_jnds, in_centered_nc),
    exp_condition=="right"~compute_density(stim_3_psy_shuffled, in_mean_jnds_right, in_sd_jnds, in_right_nc),
  ),
  dens_choice=case_when(
    exp_condition=="left"~compute_density(choice_stim_psy, in_mean_jnds_left, in_sd_jnds, in_left_nc),
    exp_condition=="centered"~compute_density(choice_stim_psy, in_mean_jnds_centered, in_sd_jnds, in_centered_nc),
    exp_condition=="right"~compute_density(choice_stim_psy, in_mean_jnds_right, in_sd_jnds, in_right_nc),
  ),
  dens_max=max(c(dens_1,dens_2,dens_3),na.rm=T),
  choice_optimal=if_else(dens_choice==dens_max,1,0)) %>%
  ungroup()

fill_opt_props <- fill_opt %>%
  group_by(participant) %>%
  summarise(opt=mean(choice_optimal)) %>%
  ungroup() 

fill_opt_props %>%
  mutate(corr=opt>=.6) %>%
  group_by(corr) %>%
  summarise(n=n())

# ONLY KEEPING IF THEY PERFORM OPTIMALLY ON AT LEAST 60% OF ALL TRIALS
ppt_keep <- fill_opt_props %>%
  mutate(corr=opt>=.6) %>%
  filter(corr) %>%
  pull(participant)

# filter critical trials ==================================================================================
crit_filtered <- crit %>%
  filter(participant %in% ppt_keep)
crit_filtered %>%
  distinct(participant,exp_condition) %>%
  group_by(exp_condition) %>%
  summarise(n=n())
# transfer analysis ==================================================================================
crit_m_prop <- crit_filtered %>%
  group_by(participant, set,choice_name) %>%
  summarise(n=n()) %>%
  group_by(participant, set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  left_join(distinct(crit,participant,exp_condition),by = c("participant")) %>%
  group_by(exp_condition, set, choice_name) %>%
  summarise(m=mean(prop),
            s=sd(prop),
            se=sd(prop)/n(),
            se_lower=m-se,
            se_upper=m+se) %>%
  ungroup()
crit_m_prop %>%
  ggplot(aes(set, m, fill=choice_name))+
  geom_col(position = position_dodge(width = .9))+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),
                position = position_dodge(width = .9),
                width=.25)+
  scale_y_continuous(limits=c(0,1))+
  labs(y="mean choice prop.")+
  facet_rep_grid(exp_condition~.,repeat.tick.labels = T)+
  ggsci::scale_fill_startrek(name="choice")+
  ggthemes::theme_few()+
  theme(legend.position="inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95))

crit %>%
  group_by(exp_condition, set, difficulty_level, stim_1_psy_shuffled, stim_2_psy_shuffled, stim_3_psy_shuffled) %>%
  summarise(n=n()) %>%
  ungroup()
  

crit_filtered %>%
  filter(set!="l-s") %>%
  group_by(set,participant,difchoice) %>%
  summarise(n=n()) %>%
  group_by(set,participant) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  left_join(distinct(crit,participant,exp_condition),by = c("participant"))  %>%
  group_by(set,exp_condition,choice) %>%
  summarise(m=mean(prop)) %>%
  ungroup() %>%
  arrange(choice,exp_condition)

fill %>%
  filter(participant %in% ppt_keep) %>%
  rowwise() %>%
  mutate(choose_min=isTRUE(all.equal(choice_stim_psy,stim_min)),
         choose_max=isTRUE(all.equal(choice_stim_psy,stim_min)))%>%
  group_by(participant,set_type) %>%
  summarise(min_prop=mean(choose_min),
            max_prop=mean(choose_min)) %>%
  ungroup() %>%
  left_join(distinct(crit,participant,exp_condition)) %>%
  group_by(exp_condition,set_type) %>%
  summarise(m_min=mean(min_prop),
            m_max=mean(max_prop))

