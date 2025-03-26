# learn_analysis_1a.R
# Analyzing learning data from experiment 1a
# Sean Conway
# Last modified Mar. 2023

# clear environment
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(glue)
library(patchwork)

# read in data
learn <- read_csv(here('line_exp_1',"data","cleaned","dataset_learn.csv"))
optimal <- read_csv(here('line_exp_1',"optimality","optimal.csv"))

# optimal probability
# proportion correct if you respond optimally on every trial
# That is, if p(cat) > .5 respond yes, if p(cat) < .5 respond no.
optimal_prob <- unique(optimal$optimal_prob)

# source plotting functions
source(here('line_exp_1',"functions","plotting_functions.R"))

# source probability density functions
source(here('line_exp_1',"functions","density_functions.R"))

# source psy2phy and phy2psy functions
source(here('line_exp_1',"functions","psy2phy.R"))

# Number of subjects
n_subs <- length(unique(learn$sub_n))

# All subject numbers
sub_nums <- distinct(learn, sub_n) %>%
  as_vector()

# Number of learning trials 
n_learn_trials <- max(learn$trial_num)

# How many times did they say "yes" on average per stimulus
learn_choice_props_agg_yes <- learn %>%
  mutate(yn_choice=as.factor(yn_choice)) %>%
  count(jnds, yn_choice,.drop=F) %>%
  group_by(jnds) %>%
  mutate(prop=n/sum(n),
         total=sum(prop)) %>%
  filter(yn_choice=="YES") %>%
  select(-total) %>%
  mutate(prob_in=prob_in(jnds),
         line_len=psy2phy(jnds), 
         prob_in_actual=prob_in_actual(line_len))

# Same as above but only second half of learning
learn_choice_props_agg_yes_2nd_half <- learn %>%
  filter(trial_num > 100) %>%
  mutate(yn_choice=as.factor(yn_choice)) %>%
  count(jnds, yn_choice,.drop=F) %>%
  group_by(jnds) %>%
  mutate(prop=n/sum(n),
         total=sum(prop)) %>%
  filter(yn_choice=="YES") %>%
  select(-total) %>%
  mutate(prob_in=prob_in(jnds),
         line_len=psy2phy(jnds), 
         prob_in_actual=prob_in_actual(line_len))

# proportion correct by subject
learn_correct_prop <- learn %>% 
  group_by(sub_n) %>% 
  summarise(prop_corr=sum(correct)/n()) %>%
  arrange(desc(prop_corr))

# how much did individual subjects respond optimally?
learn_w_optimal <- learn %>% 
  mutate(prob_in=prob_in(jnds)) %>%
  mutate(
    rsp_optimal=case_when(
      prob_in > .5 & yn_choice=="YES" ~ 1,
      prob_in > .5 & yn_choice=="NO" ~ 0,
      prob_in < .5 & yn_choice=="YES" ~ 0,
      prob_in < .5 & yn_choice=="NO" ~ 1,
    )
  )

# PLOTS ================================================================================
# plotting avg. learning categorization prop per stimulus
learn_prop_plot_jnds<- plot_optimal()+
  geom_point(data=learn_choice_props_agg_yes,aes(jnds,prop),
             alpha=.4, col="darkblue")+
  scale_y_continuous(limits=c(0,1,0),breaks=seq(0,1,.2))+
  labs(caption="Curve shows the probability a stimulus is actually in the category.",
       title="JNDs & Choice Prop",
       x="JNDs from Minimum Stimulus Value",
       y="Proportion 'Yes' Responses")+
  theme_classic()+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(hjust=0.5))
learn_prop_plot_jnds

# plotting avg. learning categorization prop per stimulus 2nd half of trials
learn_prop_plot_jnds_2nd_half<- plot_optimal()+
  geom_point(data=learn_choice_props_agg_yes_2nd_half,aes(jnds,prop),
             alpha=.4, col="darkblue")+
  scale_y_continuous(limits=c(0,1,0),breaks=seq(0,1,.2))+
  labs(caption="Curve shows the probability a stimulus is actually in the category.",
       title="JNDs & Choice Prop - 2nd half of learning",
       x="JNDs from Minimum Stimulus Value",
       y="Proportion 'Yes' Responses")+
  theme_classic()+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(hjust=0.5))
learn_prop_plot_jnds

# relationship between line length and categorization probability
learn_prop_plot_line_len <- ggplot(learn_choice_props_agg_yes, aes(line_len, prop))+
  geom_point(alpha=.6, col="darkblue")+
  geom_line(aes(x=line_len, y=prob_in_actual),col="darkgreen")+
  scale_x_continuous(breaks=seq(150,450,50))+
  scale_y_continuous(breaks=seq(0,1,.2))+
  coord_cartesian(xlim=c(150,450),ylim=c(0,1))+
  labs(caption="Green curve shows the probability a stimulus is actually in the category.",
       title="Line Lengths & Choice Prop",
       x="Line Length",
       y="Proportion 'Yes' Responses")+
  theme_classic()+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(hjust=0.5))
learn_prop_plot_line_len

learn_prop_plots <- learn_prop_plot_jnds/learn_prop_plot_line_len
learn_prop_plots

# relationship between stimulus probability and categorization probability
capt_1 <- paste0("Points on the line are unbiased.",
"\nPoints above the line indicate bias to say yes.",
"\nPoints below the line indicate bias to say no.")

learn_prob_to_prop_plot <- ggplot(learn_choice_props_agg_yes, aes(prob_in, prop))+
  geom_point(alpha=.4,col="darkblue")+
  geom_abline(intercept=0, slope=1, linetype="dashed",col="gray4")+
  #stat_smooth(method="lm")+
  scale_x_continuous(breaks=seq(0,1,.1))+
  scale_y_continuous(breaks=seq(0,1,.1))+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  labs(caption=capt_1,
       title="Category Probability & Choice Proportion",
       x="Probability in Category",
       y="Choice Proportion")+
  theme_classic()+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(hjust=0.5))
learn_prob_to_prop_plot

# Plot prop optimal responses by trial number
learn_w_optimal %>%
  group_by(sub_n) %>%
  nest() %>%
  mutate(total_rsp_optimal=map(data, function(df) accumulate(df$rsp_optimal, `+`))) %>%
  unnest(cols=c(data,total_rsp_optimal)) %>%
  mutate(prop_rsp_optimal=total_rsp_optimal/trial_num) %>%
  group_by(trial_num) %>%
  summarise(mean_opt_rsp=mean(prop_rsp_optimal)) %>%
  ggplot(aes(trial_num, mean_opt_rsp))+
  geom_point()

# plot prop correct by subject
learn_corr_prop_plot <- ggplot(learn_correct_prop, aes(x=prop_corr))+
  geom_histogram(binwidth=.05,col="black", fill="lightblue",alpha=.75)+
  geom_vline(xintercept=optimal_prob,linetype="dashed",alpha=.75,col="red")+
  scale_x_continuous(breaks=seq(0,1,.1))+
  scale_y_continuous(limits=c(0,30))+
  coord_cartesian(xlim=c(0,1))+
  theme_classic()+
  labs(
    x="Proportion Correct",
    y="Count",
    title="Learning Proportion Correct",
    caption="Red line marks maximum prop correct when responding optimally on every trial."
  )+
  theme(plot.caption=element_text(hjust=0))
learn_corr_prop_plot

# plot how much they responded optimally 
capt_2 <- paste0("Optimal performance is categorized by the following rule:",
"\nSubject responds YES if P(Category) > .5",
"\nSubject responds NO if P(Category) < .5")

learn_optimal_plot <- learn_w_optimal %>%
  group_by(sub_n) %>%
  summarise(prop_optimal=sum(rsp_optimal)/n()) %>%
  ggplot(aes(x=prop_optimal))+
  geom_histogram(binwidth=.05, fill="lightblue",col="black",na.rm=T)+
  scale_x_continuous(limits=c(0,1))+
  scale_y_continuous(limits=c(0,20))+
  theme_classic()+
  labs(caption=capt_2)+
  theme(plot.caption=element_text(hjust=0))
learn_optimal_plot

# save figures ================================================================================
save_plots <- T
if(save_plots){
  ggsave(learn_prop_plots, filename=here('line_exp_1',"plots","learn_prop_plots.png"),
         height=5, width=5, units="in")
  ggsave(learn_prob_to_prop_plot, filename=here('line_exp_1',"plots","learn_prob_to_prop_plot.png"))
  ggsave(learn_corr_prop_plot, filename=here('line_exp_1',"plots","learn_corr_prop_plot.png"))
  ggsave(learn_optimal_plot, filename=here('line_exp_1',"plots","learn_optimal_plot.png"))
  ggsave(learn_prop_plot_jnds_2nd_half, filename=here('line_exp_1',"plots","learn_prop_plots_2nd_half.png"))
  ggsave(learn_prop_plot_jnds, width=5,height=5,units="in",filename=here('line_exp_1',"plots","learn_prop_plot_jnds.png"))
}

