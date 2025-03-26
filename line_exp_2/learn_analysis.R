# learn_analysis.R
# Analyzing learning data from line categorization exp 2
# Sean Conway
# Last modified Mar. 2023

# Setup =====================================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(glue)
library(patchwork)

# Functions needed
source(here("line_exp_2","functions","plotting_functions.R"))
source(here("line_exp_2","functions","density_functions_2.R"))

# read in data
learn <- read_csv(here("line_exp_2","data","cleaned","dataset_learn.csv")) %>%
  mutate(prob_in=prob_in(jnds),
         choice=as_factor(choice),
         rsp_optimal=case_when(
            prob_in > .5 & choice=="YES" ~ 1,
            prob_in > .5 & choice=="NO" ~ 0,
            prob_in < .5 & choice=="YES" ~ 0,
            prob_in < .5 & choice=="NO" ~ 1,
    )
  )

learn_trials <- read_csv(here("line_exp_2","trials","learn_trials_1b.csv"))

# save plots?
save_plots <- T

# functions =====================================================================================
compute_agg_choice <- function(d,.choice_var, ...){
  grouped_var_all <- enquos(.choice_var,...)
  grouped_var <- enquos(...)
  c_props <- d %>%
      count(!!!grouped_var_all,.drop=F) %>%
      group_by(!!!grouped_var) %>%
      mutate(prop=n/sum(n),
             total=sum(n)) %>%
      ungroup()
  return(c_props)
}

compute_sub_choice <- function(d,.choice_var, .sub_var, ...){
  grouped_var_all <- enquos(.choice_var,.sub_var,...)
  .sub_var <- enquo(.sub_var)
  d %>%
    group_by(!!!grouped_var_all) %>%
    tally() %>%
    group_by(!!.sub_var) %>%
    mutate(prop=n/sum(n))
}

# Analysis =====================================================================================
learn_agg_choice_yes <- compute_agg_choice(learn, choice, jnds, distribution) %>%
  filter(choice=="YES") %>%
  mutate(distribution=paste0(distribution, " category"),
         ll=log(dens_in(jnds)/dens_out(jnds))) %>%
  mutate(distribution=factor(distribution,levels=c("in category","out category")),
         prob_in=prob_in(jnds))
learn_optimal_subs <- compute_sub_choice(learn,rsp_optimal, sub_n)
learn_optimal_subs_2h <- compute_sub_choice(filter(learn,trial_num > 100),rsp_optimal, sub_n)

# RTs =====================================================================================
rt_hists <- ggplot(learn, aes(rt))+
  geom_histogram(col="black",fill="lightblue")+
  facet_wrap(vars(choice))+
  ggthemes::theme_few()

which_quants <- c(.1, .3, .5, .7, .9)

# RT quants by choice set
rt_quants <- learn %>%
  group_by(sub_n, choice, distribution) %>%
  summarise(rt=quantile(rt, probs = which_quants),
            quant=which_quants) %>%
  group_by(choice, quant, distribution) %>%
  summarise(rt=mean(rt)) %>%
  ungroup() %>%
  mutate(distribution=paste0(distribution, " category"))

# Plots =====================================================================================
optimal_choice_agg_plot <- plot_optimal_1b()+
  geom_point(data=learn_agg_choice_yes,aes(jnds, prop),
             alpha=.6,size=2)+
  labs(title="learning trial choice proportions\ncollapsed across subjects")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
optimal_choice_agg_plot

ll_choice_plot <- ggplot(learn_agg_choice_yes, aes(ll,prop,col=distribution))+
  geom_jitter(alpha=.6,size=3.5)+
  geom_hline(yintercept=.5,linetype="dashed",alpha=.75)+
  geom_vline(xintercept=0,linetype="dashed",alpha=.75)+
  scale_color_manual(name="trial type",
                       values=c("darkgreen", "red"))+
  scale_x_continuous(limits=c(-2,12))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='LL in category',
       y='proportion "yes" responses',
       title="learning trial choice proportions\ncollapsed across subjects")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
ll_choice_plot

choice_prob_to_prop_agg_plot <- ggplot(learn_agg_choice_yes, aes(prob_in, prop,
                                                                 col=distribution))+
  geom_point(size=2, alpha=.5)+
  geom_abline(slope=1,intercept=0,linetype='dashed')+
  geom_hline(yintercept=.5,linetype="dashed")+
  scale_color_manual(name="trial type",
                     values=c("darkgreen", "red"))+
  scale_x_continuous(breaks = seq(0,1,.2),limits = c(0,1))+
  scale_y_continuous(breaks = seq(0,1,.2),limits = c(0,1))+
  coord_fixed()+
  labs(x="optimal category probability",
       y="proportion 'yes' responses",
       title="learning trial choice proportions\ncollapsed across subjects")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
choice_prob_to_prop_agg_plot

optimal_choice_sub_hist <- ggplot(filter(learn_optimal_subs,rsp_optimal==1))+
  geom_histogram(aes(prop),binwidth = .05,col="black",fill="lightblue")+
  labs(x="proportion optimal responses")+
  scale_x_continuous(limits=c(0,1))+
  ggthemes::theme_few()
optimal_choice_sub_hist

rt_quant_plot <- ggplot(rt_quants, aes(rt, quant,col=choice))+
  geom_point(alpha=.5,size=2.5)+
  geom_line(alpha=.5)+
  facet_grid(vars(distribution))+
  scale_color_manual(name="choice",
                     values=c("red","darkgreen"))+
  scale_y_continuous(limits=c(0,1),breaks = which_quants)+
  labs(x="RT (msec)", 
       y="Quantile",
       title="Learning Trials\nAverage RT quantiles",
       subtitle = "\"Is this line in the category?\"")+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.title = element_text(size=14))
rt_quant_plot

# saving plots =====================================================================================
if(save_plots){
  ggsave(here("line_exp_2","plots","learn_choice_prop_to_prop_agg.png"),
         choice_prob_to_prop_agg_plot,
         height=4,width=4,units="in")
  ggsave(here("line_exp_2","plots","learn_choice_ll_to_prop_agg.png"),
         ll_choice_plot)
  ggsave(here("line_exp_2","plots","learn_optimal_choice_agg_plot.png"),
         optimal_choice_agg_plot,
         height=4,width=4,units="in")
  ggsave(here("line_exp_2","plots","learn_optimal_choice_sub_hist.png"),
         optimal_choice_sub_hist,
         height=4,width=4,units="in")
  ggsave(here("line_exp_2","plots","learn_rt_hists.png"),
         rt_hists, 
         height = 4,width=5,units="in")
  ggsave(here("line_exp_2","plots","learn_rt_quant_plot.png"),
         rt_quant_plot, 
         height = 4,width=5,units="in")
}

