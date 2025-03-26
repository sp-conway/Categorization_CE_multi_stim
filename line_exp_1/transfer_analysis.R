# transfer_analysis.R
# analyzing transfer trials from line categorization experiment 1
# Sean Conway

# SETUP ======================================================
# clear environment 
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(scales)
library(BayesFactor)

# Source functions for converting psy2phy and phy2psy
source(here("line_exp_1","functions","psy2phy.R"))

# Source plotting functions
source(here("line_exp_1","functions","plotting_functions.R"))

# Source prob density functions
source(here("line_exp_1","functions","density_functions.R"))

# function for calculating distance to mean
dist_to_mean <- function(x) abs(11.25-x)

# attraction effect trial specs
attr_trial_specs_long <- read_csv(here("line_exp_1","trials","attr_eff_trials_long.csv")) 

# read in the transfer data
# and get some more columns ready for analysis
transfer <- here("line_exp_1","data","cleaned","dataset_transfer.csv") %>%
  read_csv() %>% 
  filter(rt >= 100 & rt <30000) %>% # filtering out trials with rt < 100 msec and greater than 20 seconds
  rowwise() %>% # This makes it take a long time to run bc of the number of rows
                # Should probably substitute w/ apply statement
                # Though this code is easy to read?
  mutate(median_line=median(c(line_1, line_2, line_3),na.rm=T),
         max_line=max(c(line_1, line_2, line_3), na.rm=T),
         min_line=min(c(line_1, line_2, line_3),na.rm=T)) %>%
  ungroup() %>%
  mutate(
    choice_by_rel_val=
      case_when(
        line_choice==median_line~"median",
        line_choice==min_line~"min",
        line_choice==max_line~"max"
      ),
    corr_choice_by_rel_val=
      case_when(
        corr_choice_optimal==median_line~"median",
        corr_choice_optimal==min_line~"min",
        corr_choice_optimal==max_line~"max"
      ),
    correct=case_when(
      (transfer_trial_type=="attraction" & tc_correct=="both") & (attr_choice=="target"|attr_choice=="competitor")~1,
      (transfer_trial_type=="attraction" & tc_correct=="both") & (attr_choice=="decoy")~0,
      TRUE~correct
    )) %>%
  rowwise() %>%
  mutate(
    local_mean=mean(c(line_1_psy,line_2_psy,line_3_psy),na.rm=T),
    line_1_psy_lm_dist = abs(line_1_psy-local_mean),
    line_2_psy_lm_dist = abs(line_2_psy-local_mean),
    line_3_psy_lm_dist = abs(line_3_psy-local_mean),
    choice_psy_lm_dist = abs(jnd_choice-local_mean),
    target_psy_lm_dist = abs(target_jnd-local_mean),
    competitor_psy_lm_dist = abs(competitor_jnd-local_mean),
    decoy_psy_lm_dist = abs(decoy_jnd-local_mean),
    local_mean_to_actual_mean=abs(local_mean-11.25)
  ) %>%
  ungroup()

# Number of subjects (needed later)
n_subs <- length(unique(transfer$sub_n))

# Make sure a few important variables are factors
transfer$transfer_trial_type <- factor(transfer$transfer_trial_type, 
                                       levels=c("attraction","filler"))
transfer$sub_n <- factor(transfer$sub_n, levels=unique(transfer$sub_n))
transfer$choice_by_rel_val <- factor(transfer$choice_by_rel_val, 
                                     levels=c("median","min","max"))

# get filler and attraction effect trials in separate dfs
filler <- transfer %>%
  filter(transfer_trial_type=="filler")
attraction <- transfer %>%
  filter(transfer_trial_type=="attraction")

# RT ======================================================
new_labs <- c(`1`="correct",`0`="incorrect")

transfer_rt_trinary_hists <- transfer %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(correct=as.factor(correct)) %>%
  ggplot(aes(rt))+
  geom_histogram(col="black",fill="lightblue",binwidth = 500)+
  facet_grid(correct~transfer_trial_type, scales="free",
             labeller = labeller(correct=new_labs))+
  ggthemes::theme_few()
transfer_rt_trinary_hists
  
mean_rt_trial_plot <- transfer %>% 
  group_by(trial_num) %>% 
  summarise(mean_rt=mean(rt)) %>% 
  ggplot(aes(trial_num, mean_rt))+
  geom_point(alpha=.8)+
  scale_x_continuous(breaks=breaks_width(50))+
  scale_y_continuous(breaks=breaks_width(1000))+
  labs(x="trial number",
       y="mean rt",
       title="mean rt by trial number \ntransfer trials")+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5))
mean_rt_trial_plot

save_rt_plots <- F
if(save_rt_plots){
  ggsave(mean_rt_trial_plot, filename=here("line_exp_1","plots","transfer_mean_rt_trial_plot.png"))
  ggsave(transfer_rt_trinary_hists, filename=here("line_exp_1","plots","transfer_rt_trinary_hists.png"))
}

# attraction effect trials ======================================================
compute_avg_attr_eff_grouped <- function(df, return_avg=T){
  df_new <- df %>%
    mutate(across(c(sub_n,target_side, tc_correct, difficulty_level, trial_choice_set, attr_choice),
                  as.factor)) %>%
    count(sub_n,target_side, tc_correct, difficulty_level, trial_choice_set, attr_choice) %>%
    group_by(sub_n,tc_correct, target_side, difficulty_level, trial_choice_set) %>%
    mutate(choice_prop=n/sum(n)) 
  
  if(return_avg){
    df_new <- df_new %>%
      group_by(tc_correct, target_side, difficulty_level, attr_choice, trial_choice_set) %>%
      summarise(avg_choice_prop=mean(choice_prop), 
                sd_choice_prop=sd(choice_prop),
                total_n=sum(n)) %>%
      group_by(tc_correct, target_side, difficulty_level, trial_choice_set) %>%
      mutate(avg_choice_prop=avg_choice_prop/sum(avg_choice_prop)) %>%
      group_by(tc_correct, target_side, difficulty_level, attr_choice, trial_choice_set) %>%
      summarise(avg_choice_prop=avg_choice_prop,
                sd_choice_prop=sd_choice_prop,
                total_n=total_n,
                upper_ci_bar=avg_choice_prop+(qt(.025, n_subs-1,lower.tail=F))*(sd_choice_prop/sqrt(n_subs)),
                lower_ci_bar=avg_choice_prop-(qt(.025, n_subs-1,lower.tail=F))*(sd_choice_prop/sqrt(n_subs))) %>%
      rename(option=attr_choice) %>%
      left_join(.,
                select(attr_trial_specs_long, 
                       c(target_side, 
                         tc_correct, 
                         difficulty_level, 
                         trial_choice_set, 
                         option, 
                         jnds)
                )
      ) %>%
      mutate(option=toupper(str_sub(option, 1, 1)), 
             prob_in=prob_in(jnds)) %>%
      ungroup()
  }
    
  return(df_new)
}
attraction_props <- compute_avg_attr_eff_grouped(attraction)
attraction_props_1_2 <- attraction %>%
  filter(trial_num <= 192) %>%
  compute_avg_attr_eff_grouped(.)

vis_attr_eff_grouped <- function(df, type, blocks){
  pl <- plot_optimal()+
    geom_col(data=df,aes(x=jnds, y=avg_choice_prop, fill=option),alpha=1,
             inherit.aes=F)+
    geom_errorbar(data=df,aes(x=jnds, ymin=lower_ci_bar, ymax=upper_ci_bar),width=.5,alpha=.5,
                  inherit.aes=F)+
    geom_vline(xintercept=11.25,linetype="dashed",alpha=.5)+
    facet_wrap(tc_correct~target_side+difficulty_level,labeller = function(labs)
    {label_both(labs)})+
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
    labs(x="JNDs from min",
         y="mean choice proportion",
         title=glue::glue("Attraction effect {type} trial choice props\n{blocks}"))+
    ggthemes::theme_few()+
    theme(plot.title = element_text(hjust=0.5))
  return(pl)
}

tcd_plot_blocks_1_2_split_trinary <- vis_attr_eff_grouped(filter(attraction_props_1_2, trial_choice_set=="trinary"),
                                                          type="trinary", blocks="blocks 1 and 2")
  
tcd_plot_blocks_1_2_split_trinary 

tcd_plot_blocks_all_split_trinary <- vis_attr_eff_grouped(filter(attraction_props, trial_choice_set=="trinary"),
                                                          type="trinary", blocks="all transfer trials")

tcd_plot_blocks_all_split_trinary

tcd_plot_blocks_1_2_split_binary <-  vis_attr_eff_grouped(filter(attraction_props_1_2, trial_choice_set=="binary"),
                                                          type="binary", blocks="blocks 1 and 2")

tcd_plot_blocks_all_split_binary <-  vis_attr_eff_grouped(filter(attraction_props_1_2, trial_choice_set=="binary"),
                                                          type="trinary", blocks="all blocks")

save_tcd_plots <- T
if(save_tcd_plots){
  ggsave(tcd_plot_blocks_all_split_trinary, filename = here("line_exp_1","plots","tcd_all_split_trinary.pdf"),
         height=8,width=12,units="in")
  ggsave(tcd_plot_blocks_1_2_split_trinary, filename = here("line_exp_1","plots","tcd_blks_1_2_split_trinary.pdf"),
         height=8,width=12,units="in")
  ggsave(tcd_plot_blocks_all_split_binary, filename = here("line_exp_1","plots","tcd_all_split_binary.pdf"),
         height=8,width=12,units="in")
  ggsave(tcd_plot_blocks_1_2_split_binary, filename = here("line_exp_1","plots","tcd_blks_1_2_split_binary.pdf"),
         height=8,width=12,units="in")
}

# Attraction effect aggregate
compute_agg_attr_eff <- function(df, return_avg=T){
  df_new <- df %>%
    mutate(across(c(sub_n, trial_choice_set, attr_choice),as.factor)) %>%
    count(sub_n, trial_choice_set, attr_choice,.drop=F) %>%
    group_by(sub_n, trial_choice_set) %>%
    mutate(choice_prop=n/sum(n)) %>%
    ungroup()
  
  if(return_avg){
    df_new <- df %>%
      mutate(across(c(sub_n,trial_choice_set,attr_choice))) %>%
      count(sub_n,trial_choice_set, attr_choice) %>%
      group_by(sub_n, trial_choice_set) %>%
      mutate(choice_prop=n/sum(n)) %>%
      group_by(trial_choice_set, attr_choice) %>%
      summarise(avg_choice_prop=mean(choice_prop), 
                sd_choice_prop=sd(choice_prop),
                total_n=sum(n)) %>%
      group_by(trial_choice_set) %>%
      mutate(avg_choice_prop=avg_choice_prop/sum(avg_choice_prop)) %>%
      group_by(trial_choice_set,attr_choice) %>%
      summarise(
        avg_choice_prop=avg_choice_prop,
        sd_choice_prop=sd_choice_prop,
        total_n=total_n,
        upper_ci_bar=avg_choice_prop+(qt(.025, n_subs-1,lower.tail=F))*(sd_choice_prop/sqrt(n_subs)),
        lower_ci_bar=avg_choice_prop-(qt(.025, n_subs-1,lower.tail=F))*(sd_choice_prop/sqrt(n_subs))
      ) %>%
       ungroup()
  }
  return(df_new)
}
attr_eff_agg_all <- compute_agg_attr_eff(attraction)
attr_eff_agg_blks_1_2 <- attraction %>%
  filter(trial_num<=192) %>%
  compute_agg_attr_eff()
  
agg_attr_eff_plot_all <- ggplot(attr_eff_agg_all, aes(attr_choice,avg_choice_prop))+
  geom_col(fill="lightblue")+
  geom_errorbar(aes(ymin=lower_ci_bar,ymax=upper_ci_bar),width=.25)+
  facet_wrap(vars(trial_choice_set),scales="free_x")+
  scale_fill_discrete(name="choice")+
  scale_y_continuous(limit=c(0,1),breaks=seq(0,1,.1))+
  labs(x="choice",
       y="mean choice proportion",
       title="avg tcd choice props \nall trials")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,size=20),
        strip.text=element_text(size=20))
agg_attr_eff_plot_all

agg_attr_eff_plot_blks_1_2 <- ggplot(attr_eff_agg_blks_1_2, aes(attr_choice,avg_choice_prop))+
  geom_col(fill="lightblue")+
  geom_errorbar(aes(ymin=lower_ci_bar,ymax=upper_ci_bar),width=.25)+
  facet_wrap(vars(trial_choice_set),scales="free_x")+
  scale_fill_discrete(name="choice")+
  scale_y_continuous(limit=c(0,1),breaks=seq(0,1,.1))+
  labs(x="choice",
       y="mean choice proportion",
       title="avg tcd choice props \nblocks 1 & 2")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,size=20),
        strip.text=element_text(size=20))
agg_attr_eff_plot_blks_1_2

save_agg_ae_plots <- T
if(save_agg_ae_plots){
  ggsave(agg_attr_eff_plot_all, filename=here("line_exp_1","plots","agg_attr_eff_all.pdf"),
         width=8,height=8)
  ggsave(agg_attr_eff_plot_blks_1_2, filename=here("line_exp_1","plots","agg_attr_eff_blks_1_2.pdf"),
         width=8,height=8)
}

# ANOVA on avg T/C/D proportions
attr_eff_agg_indiv <- compute_agg_attr_eff(attraction, return_avg = F) %>%
  mutate(trial_choice_set=as.factor(trial_choice_set),
         attr_choice=as.factor(attr_choice))

# boxplot
tcd_boxplot <- attr_eff_agg_indiv %>%
  ggplot(aes(x=attr_choice,y=choice_prop,fill=attr_choice))+
  geom_boxplot()+
  facet_wrap(vars(trial_choice_set),scales="free_x")

save_tcd_boxplot <- T
if(save_tcd_boxplot){
  ggsave(tcd_boxplot, filename = here("line_exp_1","plots","tcd_boxplot.png"))
}

aov_res_1 <- aov(choice_prop~attr_choice*trial_choice_set+Error(sub_n/(attr_choice*trial_choice_set)),
                 data=attr_eff_agg_indiv)
summary(aov_res_1)

# rst
rst_indiv <- attr_eff_agg_indiv %>%
  filter(attr_choice!="decoy") %>%
  select(-n) %>%
  pivot_wider(names_from = attr_choice, 
              values_from = choice_prop) %>%
  mutate(rst=target/(target+competitor),
         trial_choice_set=as.factor(trial_choice_set))

bf_rst <- rst_indiv %>%
  filter(trial_choice_set=="trinary") %>%
  select(rst) %>%
  as_vector() %>%
  ttestBF(mu=0.5)
summary(bf_rst)

# choice by optimal answer & how difficult 
# collapsed over side
tcd_choice_opt_ans <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(across(c(difficulty_level,tc_correct,attr_choice),as.factor),
         difficulty_level=str_remove(difficulty_level,"_chance")) %>%
  count(sub_n, difficulty_level, tc_correct, attr_choice) %>%
  group_by(sub_n, difficulty_level, tc_correct) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  group_by(difficulty_level, tc_correct, attr_choice) %>%
  summarise(mean_prop=mean(prop),
            sd_prop=sd(prop),
            ci_lower=mean_prop-qt(.025,n_subs-1,lower.tail = F)*(sd_prop/sqrt(n_subs)),
            ci_upper=mean_prop+qt(.025,n_subs-1,lower.tail = F)*(sd_prop/sqrt(n_subs))) %>%
  ungroup() 

tcd_choice_opt_ans_plot <- tcd_choice_opt_ans %>%
  ggplot(aes(attr_choice,mean_prop,fill=attr_choice))+
  geom_col()+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.1)+
  facet_wrap(vars(difficulty_level,tc_correct),labeller=label_both)+
  ggthemes::theme_few()
tcd_choice_opt_ans_plot
ggsave(here("line_exp_1","plots","tdc_choice_diff_opt.pdf"),width=8,height=8)


# filler trials ======================================================
filler_prop_corr <- filler %>%
  mutate(across(c(correct_dist_to_mean, correct, trial_choice_set),as.factor)) %>%
  count(correct_dist_to_mean, correct, trial_choice_set) %>%
  group_by(correct_dist_to_mean, trial_choice_set) %>%
  mutate(prop_corr=n/sum(n)) 

# analysis based on local mean ========================================================
opt_choice_props_condition <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(difficulty_level=str_remove(difficulty_level,"_chance")) %>%
  unite("condition",difficulty_level,tc_correct,sep="/") %>%
  mutate(condition=as.factor(condition)) %>% 
  count(sub_n, condition, attr_choice) %>%
  group_by(sub_n, condition) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(condition, attr_choice) %>%
  summarise(mean_prop=mean(prop),
            sd_prop=sd(prop),
            ci_lower=mean_prop-qt(.025,n_subs-1,lower.tail = F)*(sd_prop/sqrt(n_subs)),
            ci_upper=mean_prop+qt(.025,n_subs-1,lower.tail = F)*(sd_prop/sqrt(n_subs))) %>%
  ungroup() 

lmdist_ll_choice <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(difficulty_level=str_remove(difficulty_level,"_chance")) %>%
  unite("condition",difficulty_level,tc_correct,sep="/") %>%
  mutate(condition=as.factor(condition)) %>%
  group_by(condition) %>%
  summarise(
    target_dist=mean(target_psy_lm_dist,na.rm=T),
    competitor_dist=mean(competitor_psy_lm_dist,na.rm=T),
    decoy_dist=mean(decoy_psy_lm_dist,na.rm=T),
    target_llr=mean(dens_in(target_jnd)/dens_out(target_jnd)),
    competitor_llr=mean(dens_in(competitor_jnd)/dens_out(competitor_jnd)),
    decoy_llr=mean(dens_in(decoy_jnd)/dens_out(decoy_jnd)),
  ) %>%
  ungroup() %>%
  pivot_longer(c(target_dist,competitor_dist,decoy_dist,target_llr,competitor_llr,decoy_llr)) %>%
  separate(name, into = c("attr_choice","name")) %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  right_join(opt_choice_props_condition)%>%
  mutate(condition=glue::glue("{condition} correct"))

lm_dist_to_choice_pl <- lmdist_ll_choice %>%
  ggplot(aes(dist,llr,size=mean_prop,col=attr_choice))+
  geom_point()+
  labs(x="avg. distance to local mean of choice set",
       y="Likelihood of being in category")+
  ggsci::scale_color_futurama(name="choice")+
  # scale_shape_manual(values=c(0,2,4,15,17,23))+
  facet_wrap(vars(condition))+
  scale_x_continuous(limits=c(0,4))+
  scale_y_continuous(limits=c(0,3))+
  ggthemes::theme_few()
lm_dist_to_choice_pl
ggsave(here("line_exp_1","plots","lm_dist_to_choice_pl.pdf"),
       width=8,height=6)

filler %>%
  ggplot(aes(choice_psy_lm_dist))+
  geom_histogram(bins=15,fill="lightblue",col="black")+
  facet_wrap(vars(trial_choice_set))+
  labs(title='filler trials',
       x='choice distance to local mean')+
  ggthemes::theme_few()
ggsave(here("line_exp_1","plots","filler_lm_dist_hist.pdf"),width=5,height=5)

transfer %>%
  ggplot(aes(local_mean_to_actual_mean))+
  geom_histogram(fill="lightblue",col="black",bins=10)+
  facet_wrap(vars(trial_choice_set,transfer_trial_type))+
  labs(x='local mean distance to actual category mean')+
  ggthemes::theme_few()
ggsave(here("line_exp_1","plots","attr_filler_lm_dist_hist.pdf"),width=5,height=5)

attraction_lmdist_lr_long <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(target_lr=dens_in(target_jnd)/dens_out(target_jnd),
         competitor_lr=dens_in(competitor_jnd)/dens_out(competitor_jnd),
         decoy_lr=dens_in(decoy_jnd)/dens_out(decoy_jnd),
         trial=1:n()) %>%
  select(trial,
         target_psy_lm_dist,
         competitor_psy_lm_dist,
         decoy_psy_lm_dist,
         target_lr,
         competitor_lr,
         decoy_lr,
         attr_choice) %>%
  pivot_longer(-c(attr_choice,trial)) %>%
  mutate(name=str_replace(name,"psy_lm_dist","lmdist")) %>%
  separate(name, into=c("tcd","variable")) %>%
  mutate(choice=case_when(
    tcd==attr_choice~1,
    TRUE~0
  )) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  mutate(tcd=forcats::as_factor(tcd),
         tcd=forcats::fct_relevel(tcd,c("competitor","target","decoy")))
# m <- glm(choice~lmdist*lr+tcd,data=attraction_lmdist_lr_long,family=binomial)
# summary(m)

if(!file.exists(here("line_exp_1","lr_lmdist_multinom_model.RData"))){
  multinom_mod <- attraction_lmdist_lr_long %>%
    filter(choice==1) %>%
    brm(attr_choice~lmdist*lr, family="categorical",data=.,chains=4,cores=4)
  save(multinom_mod, file=here("line_exp_1","lr_lmdist_multinom_model.RData"))
}else{
  load(here("line_exp_1","lr_lmdist_multinom_model.RData"))
}

summary(multinom_mod)



