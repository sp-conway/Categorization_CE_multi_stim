# transfer_analysis.R
# analyzing transfer data from 2nd line categorization experiment
# Sean Conway
# Last modified Mar. 2023

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
                  choice,
                  key_choice),
                as.factor))

# Number of subjects
n_subs <- length(unique(transfer$sub_n))

# Attraction effect trial specs
attr_eff_specs <- read_csv(here("line_exp_2","trials","attr_eff_trials_long_1b.csv"))

# Function for computing 95% CI
compute_ci <- function(mean,sd, n_subs,side){
  x <- switch(as.character(side),
              "lower"=mean-qt(.025, n_subs-1,lower.tail=F)*(sd/sqrt(n_subs)),
              "upper"=mean+qt(.025, n_subs-1,lower.tail=F)*(sd/sqrt(n_subs))
  )
  return(x)
} 

# function for computing the log likelihood for a given choice
stim_ll <- function(x) log(dens_in(x)/dens_out(x))

# compute correctness as a function of stimulus log likelihood
transfer <- transfer %>%
  mutate(stim_1_ll = stim_ll(psy_1_in_order),
         stim_2_ll = stim_ll(psy_2_in_order),
         stim_3_ll = ifelse(is.na(psy_3_in_order),NA_real_, stim_ll(psy_3_in_order)),
         choice_ll = stim_ll(choice_psy)) %>%
  rowwise() %>%
  mutate(max_ll=max(c(stim_1_ll,stim_2_ll,stim_3_ll),na.rm=T)) %>%
  ungroup() %>%
  mutate(
    correct_ll=case_when(
      choice_ll==max_ll~T,
      choice_ll!=max_ll~F
    )
  )
        

# split data
# attraction effect trial data
attraction <- transfer %>%
  filter(transfer_trial_type=="attraction")

# filler trial data
filler <- transfer %>%
  filter(transfer_trial_type=="filler_sample"|transfer_trial_type=="filler_easy")

# controls ==============================================================================
save_plots <- T # Should plots be saved as image files?

# checks ==============================================================================
check_ntrial <- function(d){
  d %>%
    group_by(sub_n) %>%
    summarise(n_trial=max(trial_num)) %>%
    group_by(n_trial) %>%
    tally() %>%
    rename(count=n)
}

n_trials <- check_ntrial(transfer)
cat("These are the numbers of transfer trials participants saw:\n");n_trials
transfer %>%
  count(sub_n, trial_choice_set, transfer_trial_type, target_side, tc_correct)

# prop correct ==============================================================================
prop_corr <- transfer %>%
  count(sub_n, transfer_trial_type, trial_choice_set, correct_ll) %>%
  group_by(sub_n,transfer_trial_type,trial_choice_set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  filter(correct_ll)

prop_corr_summaries <- prop_corr %>%
  group_by(transfer_trial_type,trial_choice_set) %>%
  summarise(mean_prop_correct=mean(prop),
            se_corrected=sqrt(var(prop)*(5/6))/sqrt(n_subs),
            ci_lower=mean_prop_correct-qt(.025,n_subs-1,lower.tail=F)*se_corrected,
            ci_upper=mean_prop_correct+qt(.025,n_subs-1,lower.tail=F)*se_corrected) %>%
  ungroup() %>%
  mutate(transfer_trial_type=str_replace(transfer_trial_type,"_","-"))

ggplot(prop_corr_summaries,aes(trial_choice_set,mean_prop_correct))+
  geom_col(fill="gray")+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2)+
  facet_wrap(vars(transfer_trial_type))+
  scale_y_continuous(limits=c(0,1))+
  labs(x="choice set",y="mean prop correct")+
  ggthemes::theme_few()

prop_corr_aov <- anovaBF(prop~transfer_trial_type*trial_choice_set, data=prop_corr, whichRandom = "sub_n")
summary(prop_corr_aov)
prop_corr_aov@bayesFactor$error
plot(prop_corr_aov)

# Attraction effect trials ==============================================================================
compute_attr_eff <- function(d, by_sub,...){
  grouping_vars <- enquos(...)
  d_props <- d %>%
    group_by(sub_n, attr_choice, !!!grouping_vars) %>%
    tally() %>%
    group_by(sub_n, !!!grouping_vars) %>%
    mutate(prop=n/sum(n)) %>%
    group_by(attr_choice,!!!grouping_vars) 
  if(by_sub){
    return(d_props)
  }else{
    d_props_avg <- d_props %>%
      summarise(mean_prop=mean(prop),
                total_n=sum(n), 
                sd_prop=sd(prop)) %>%
      group_by(!!!grouping_vars) %>%
      mutate(mean_prop=mean_prop/sum(mean_prop)) %>% # normalize proportions to sum to 1
      group_by(attr_choice,!!!grouping_vars) %>%
      summarise(mean_prop=mean_prop,
                ci_lower=compute_ci(mean_prop,sd_prop,n_subs,"lower"),
                ci_upper=compute_ci(mean_prop,sd_prop,n_subs,"upper")) %>%
      ungroup()
    return(d_props_avg)
  }
  
}

join_trial_specs <- function(df, specs){
  df %>% left_join(.,
                   select(specs, 
                          c(option,
                            trial_choice_set,
                            tc_correct,
                            difficulty,
                            target_side,
                            jnds)
                   )
  ) %>%
    ungroup()
}
tcd_avg <- compute_attr_eff(attraction, by_sub=F, trial_choice_set)
tcd_avg_f_100 <- attraction %>%
  filter(trial_num<100) %>%
  compute_attr_eff(., by_sub = F, trial_choice_set)

tcd_grouped_trinary <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  compute_attr_eff(.,by_sub=F,
                   trial_choice_set,
                   difficulty, 
                   target_side,
                   tc_correct) %>%
  rename(option=attr_choice) %>%
  join_trial_specs(., filter(attr_eff_specs,trial_choice_set=="trinary")) 

tcd_grouped_binary <- attraction %>%
  filter(trial_choice_set=="binary") %>%
  compute_attr_eff(.,by_sub=F,
                   trial_choice_set,
                   difficulty, 
                   target_side,
                   tc_correct) %>%
  rename(option=attr_choice) %>%
  join_trial_specs(., filter(attr_eff_specs,trial_choice_set=="binary")) 
  
tcd_avg_plot <- ggplot(tcd_avg,aes(attr_choice,mean_prop,fill=attr_choice))+
  geom_col(alpha=.75)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2)+
  facet_wrap(vars(trial_choice_set),scales="free_x")+
  scale_x_discrete(name="choice")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.1))+
  labs(y="mean choice proportion",
       title="Experiment 1b mean t/c/d choice proportions")+
  ggthemes::theme_few()+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        strip.text = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=15))
tcd_avg_plot

tcd_avg_plot_f_100 <- ggplot(tcd_avg_f_100,aes(attr_choice,mean_prop,fill=attr_choice))+
  geom_col(alpha=.75)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2)+
  facet_wrap(vars(trial_choice_set),scales="free_x")+
  scale_x_discrete(name="choice")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.1))+
  labs(y="mean choice proportion",
       title="Experiment 1b mean t/c/d choice proportions\nfirst 100 trials")+
  ggthemes::theme_few()+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        strip.text = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=15))

plot_tcd_grouped <- function(df, set){
  plot_optimal_1b()+
    geom_col(data=df, aes(jnds, mean_prop, fill=option),
             alpha=.75,inherit.aes=F)+
    geom_errorbar(data=df,aes(x=jnds,ymin=ci_lower,ymax=ci_upper),
                  width=.4,inherit.aes=F)+
    geom_vline(xintercept = 11.25,linetype="dashed",alpha=.6)+
    facet_wrap(tc_correct~target_side+difficulty,
               scales="free_x",
               labeller = function(labs)
               {label_both(labs)})+
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
    labs(x="JNDs from min",
         y="mean choice proportion",
         title=glue("Experiment 1b - Testing the attraction effect \n{set} trials"))+
    ggthemes::theme_few()+
    theme(plot.title = element_text(hjust=0.5))
}

tcd_grouped_plot_tri <- plot_tcd_grouped(tcd_grouped_trinary, "trinary")
tcd_grouped_plot_bi <- plot_tcd_grouped(tcd_grouped_binary, "binary")
tcd_grouped_plot_tri
tcd_grouped_plot_bi

tcd_by_sub <- compute_attr_eff(attraction, by_sub = T, trial_choice_set)
tcd_boxplot <- ggplot(tcd_by_sub,aes(attr_choice, prop,fill=attr_choice))+
  geom_boxplot()+
  facet_wrap(vars(trial_choice_set),scales="free_x")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.1))+
  scale_fill_discrete(name="choice")+
  labs(x="choice",
       title="Experiment 1b - t/c/d choice proportions")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5)); tcd_boxplot

comp_choice <- tcd_by_sub %>%
  filter(attr_choice=="competitor") 
comp_bf <- ttestBF(y=comp_choice$prop[comp_choice$trial_choice_set=="binary"],
        x=comp_choice$prop[comp_choice$trial_choice_set=="trinary"],
        paired=T)
comp_bf_post <- posterior(comp_bf, iterations=10000) %>%
  as_tibble()
comp_hdi <- ggdist::hdi(comp_bf_post$mu)
comp_posterior_plot <- ggplot(comp_bf_post, aes(mu))+
  geom_histogram(binwidth = .005,col="white",fill="lightblue")+
  geom_vline(xintercept = median(comp_bf_post$mu),size=1)+
  geom_vline(xintercept = comp_hdi[,1],size=1,col="red",linetype="dashed")+
  geom_vline(xintercept = comp_hdi[,2],size=1,col="red",linetype="dashed")+
  labs(x="mean competitor choice prop\ntrinary minus binary",y="count",
       title=glue("mean competitor choice\ntrinary minus binary\nposterior distribution samples (N=10000)"),
       caption="Black line indicates the median of the distribution.\nRed dashed lines mark the 95% HDI.")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0))
comp_posterior_plot


rst_by_sub <- tcd_by_sub %>%
  filter(attr_choice!="decoy") %>%
  group_by(sub_n, trial_choice_set) %>%
  mutate(rst=prop/sum(prop)) %>%
  filter(attr_choice=="target") %>%
  select(-c(attr_choice, prop, n)) %>%
  ungroup() 

rst_hist <- ggplot(rst_by_sub, aes(rst))+
  geom_histogram(binwidth = .05, fill="lightblue",col="black")+
  facet_grid(vars(trial_choice_set))+
  ggthemes::theme_few()

rst_summaries <- rst_by_sub %>%
  group_by(trial_choice_set) %>%
  summarise(mean_rst=mean(rst),
            median_rst=median(rst),
            sd_rst=sd(rst),
            min_rst=min(rst),
            max_rst=max(rst), 
            ci_lower=compute_ci(mean_rst,sd_rst,n_subs,"lower"),
            ci_upper=compute_ci(mean_rst,sd_rst,n_subs,"upper")) %>%
  ungroup()
rst_summaries 

rst_barplot <- ggplot(rst_summaries, aes(trial_choice_set, mean_rst,fill=trial_choice_set))+
  geom_col(width = .5)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.10)+
  scale_fill_discrete(name="choice set")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="choice set",
       title="mean rst by choice set")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
rst_barplot  

# computing a bayes factor for RST - one sample. trinary trials only
bf_rst_tri <- rst_by_sub %>%
  filter(trial_choice_set=="trinary") %>%
  select(rst) %>%
  as_vector() %>%
  ttestBF(mu=.5)
bf_rst_tri

# computing a bayes factor for RST - one sample. binary trials only
bf_rst_bin <- rst_by_sub %>%
  filter(trial_choice_set=="binary") %>%
  select(rst) %>%
  as_vector() %>%
  ttestBF(mu=.5)
bf_rst_bin

# sample from the posterior
n_post_samps <- 50000
rst_posterior <- posterior(bf_rst_tri,iterations = n_post_samps) %>%
  as_tibble()

rst_hdi <- ggdist::hdi(rst_posterior$mu)

# plot posterior
rst_posterior_plot <- ggplot(rst_posterior, aes(mu))+
  geom_histogram(binwidth = .005,col="white",fill="lightblue")+
  geom_vline(xintercept = median(rst_posterior$mu),size=1)+
  geom_vline(xintercept = rst_hdi[,1],size=1,col="red",linetype="dashed")+
  geom_vline(xintercept = rst_hdi[,2],size=1,col="red",linetype="dashed")+
  scale_x_continuous(limits=c(.34,.5),breaks=seq(.34,.5,.02))+
  labs(x="mean rst",y="count",
       title=glue("mean rst - trinary trials\nposterior distribution samples (N={n_post_samps})"),
       caption="Black line indicates the median of the distribution.\nRed dashed lines mark the 95% HDI.")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0))
rst_posterior_plot

# Also compute bayes factor difference in RST for paired samples
bf_rst_ps <- ttestBF(x=rst_by_sub[rst_by_sub$trial_choice_set=="binary",]$rst,
                     y=rst_by_sub[rst_by_sub$trial_choice_set=="trinary",]$rst,
                     paired=T)
bf_rst_ps

# Trinary vs. Binary ==============================================================================
tri_v_bi <- attraction %>%
  filter(!is.na(attr_choice)) %>%
  mutate(attr_choice=factor(attr_choice,level=c("target","decoy","competitor"))) %>%
  count(sub_n, trial_choice_set, attr_choice, .drop=F) %>%
  group_by(sub_n, trial_choice_set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  filter(attr_choice=="target") %>%
  select(-c(attr_choice,n)) %>%
  pivot_wider(id_cols = sub_n, 
              values_from = prop, 
              names_from = trial_choice_set) %>%
  mutate(tri_min_bi=trinary-binary)

tri_v_bi_hist <- ggplot(tri_v_bi, aes(tri_min_bi))+
  geom_histogram(fill="lightblue",col="black",binwidth = .025)+
  geom_vline(xintercept = mean(tri_v_bi$tri_min_bi),col="red")+
  scale_x_continuous(limits=c(-.5,.5),breaks=seq(-.5,.5,.1))+
  labs(x="P(T|T,C,D) - P(T|T,C)",
       y="Count",
       title="Trinary Minus Binary Choice Proportion")+
  ggthemes::theme_few()
tri_v_bi_hist


bf_tri_v_bi <- ttestBF(tri_v_bi$binary,
                       tri_v_bi$trinary, 
                       mu=0,
                       paired=T)
t.test(tri_v_bi$binary,
       tri_v_bi$trinary,
       paired=T,
       mu = 0)
bf_tri_v_bi

tri_v_bi %>%
  summarise(mean(tri_min_bi),
            sd(tri_min_bi))

tri_v_bi_post <- posterior(bf_tri_v_bi,iterations = n_post_samps) %>%
  as_tibble()

tri_v_bi_hdi <- ggdist::hdi(tri_v_bi_post$mu)

tri_v_bi_post_plot <- ggplot(tri_v_bi_post, aes(mu))+
  geom_histogram(col="black",fill="lightblue")+
  geom_vline(xintercept=tri_v_bi_hdi[1],col="red")+
  geom_vline(xintercept=tri_v_bi_hdi[2],col="red")+
  labs(title="trinary minus binary\n mean target choice proportion\nposterior distribution",
       caption="Red lines mark 95% HDI.")+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0))
tri_v_bi_post_plot
  

# Trinary vs. Trinary ==============================================================================
tri_v_tri <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(across(c(target_side,attr_choice),as.character)) %>%
  mutate(side_choice=factor(case_when(
    target_side=="right" & attr_choice=="target" ~ "L",
    target_side=="right" & attr_choice=="competitor" ~ "S",
    target_side=="left" & attr_choice=="target" ~ "S",
    target_side=="left" & attr_choice=="competitor" ~ "L",
    target_side=="left" & attr_choice=="decoy" ~ "DS",
    target_side=="right" & attr_choice=="decoy" ~ "DL",
  ),levels=c("L","S","DS","DL")),
    tri_set=factor(recode(target_side,"left"="L,S,DS","right"="L,S,DL"),
                   levels=c("L,S,DS","L,S,DL"))
  ) %>%
  count(sub_n,tri_set,side_choice,.drop=F) %>%
  group_by(sub_n,tri_set) %>%
  mutate(prop=n/sum(n)) %>%
  select(-n) %>%
  ungroup() %>%
  pivot_wider(id_cols = sub_n,
              names_from = c(side_choice,tri_set),
              values_from = prop,
              names_glue="P({side_choice}|{tri_set})") %>%
  mutate(`P(L|L,S,DL) - P(L|L,S,DS)`=`P(L|L,S,DL)`-`P(L|L,S,DS)`,
         `P(S|L,S,DS) - P(S|L,S,DL)`=`P(S|L,S,DS)`-`P(S|L,S,DL)`) %>%
  select(sub_n,`P(L|L,S,DL) - P(L|L,S,DS)`,`P(S|L,S,DS) - P(S|L,S,DL)`) %>%
  pivot_longer(cols = c(`P(L|L,S,DL) - P(L|L,S,DS)`,
                        `P(S|L,S,DS) - P(S|L,S,DL)`),
               values_to = "choice_prop",
               names_to = "set") 

tri_v_tri_hists <- ggplot(tri_v_tri, aes(choice_prop))+
  geom_histogram(col="black",fill="lightblue",binwidth=.1)+
  facet_wrap(vars(set))+
  scale_x_continuous(limits=c(-.8,.8),breaks=seq(-.8,.8,.2))+
  labs(x="",
       y="Count",
       title="Trinary minus Trinary Choice Proportion")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0),
        strip.text=element_text(size=10))
tri_v_tri_hists

tri_v_tri %>%
  group_by(set) %>%
  summarise(m=mean(choice_prop))

bf_tri_v_tri_L <- ttestBF(tri_v_tri[tri_v_tri$set=="P(L|L,S,DL) - P(L|L,S,DS)",]$choice_prop,
        mu=0)
bf_tri_v_tri_L
bf_tri_v_tri_S <- ttestBF(tri_v_tri[tri_v_tri$set=="P(S|L,S,DS) - P(S|L,S,DL)",]$choice_prop,
        mu=0)
bf_tri_v_tri_S

# Is decoy choice driving the repulsion effect? ==============================================================================
rst_decoy_choice <- tcd_by_sub %>%
  filter(attr_choice=="decoy") %>%
  select(sub_n, trial_choice_set, prop) %>%
  rename(decoy_prop=prop) %>%
  left_join(., rst_by_sub)

rst_decoy_choice_plot <- ggplot(rst_decoy_choice, aes(decoy_prop, rst))+
  geom_point(alpha=0.5, size=2.5, col="darkblue")+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  ggthemes::theme_few()
rst_decoy_choice_plot

# Position effects ==============================================================================
tcd_by_pos <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(
    pos_1=case_when(
      target_phy==line_1_in_order ~ "T",
      decoy_phy==line_1_in_order ~ "D",
      competitor_phy==line_1_in_order ~ "C",
    ),
    pos_2=case_when(
      target_phy==line_2_in_order ~ "T",
      decoy_phy==line_2_in_order ~ "D",
      competitor_phy==line_2_in_order ~ "C",
    ),
    pos_3=case_when(
      target_phy==line_3_in_order ~ "T",
      decoy_phy==line_3_in_order ~ "D",
      competitor_phy==line_3_in_order ~ "C",
      TRUE~""
    )
  ) %>%
  unite("tcd_order",c("pos_1","pos_2","pos_3"),sep="") %>%
  group_by(sub_n,tcd_order,attr_choice) %>%
  tally() %>%
  group_by(sub_n,tcd_order) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(tcd_order, attr_choice) %>%
  summarise(mean_prop=mean(prop),
            sd_prop=sd(prop),
            n=sum(n),
            ci_lower=compute_ci(mean_prop,sd_prop,n_subs,"lower"),
            ci_upper=compute_ci(mean_prop,sd_prop,n_subs,"upper"))


rst_by_pos <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(
    pos_1=case_when(
      target_phy==line_1_in_order ~ "T",
      decoy_phy==line_1_in_order ~ "D",
      competitor_phy==line_1_in_order ~ "C",
    ),
    pos_2=case_when(
      target_phy==line_2_in_order ~ "T",
      decoy_phy==line_2_in_order ~ "D",
      competitor_phy==line_2_in_order ~ "C",
    ),
    pos_3=case_when(
      target_phy==line_3_in_order ~ "T",
      decoy_phy==line_3_in_order ~ "D",
      competitor_phy==line_3_in_order ~ "C",
      TRUE~""
    )
  ) %>%
  unite("tcd_order",c("pos_1","pos_2","pos_3"),sep="") %>%
  mutate(tcd_order=as.factor(tcd_order)) %>%
  count(sub_n,tcd_order,attr_choice,.drop=F) %>%
  filter(attr_choice!="NA") %>%
  group_by(sub_n, tcd_order) %>%
  mutate(prop=n/sum(n)) %>%
  filter(attr_choice!="decoy") %>%
  select(-n) %>%
  pivot_wider(names_from = attr_choice,
              values_from = prop) %>%
  ungroup() %>%
  mutate(rst=target/(target+competitor)) %>%
  group_by(tcd_order) %>%
  summarise(mean_rst=mean(rst),
            median_rst=median(rst),
            sd_rst=sd(rst),
            ci_lower=compute_ci(mean_rst,sd_rst,n_subs,"lower"),
            ci_upper=compute_ci(mean_rst,sd_rst,n_subs,"upper"))
  
rst_by_pos_plot <- ggplot(rst_by_pos, aes(x=reorder(tcd_order,-mean_rst),mean_rst))+
  geom_col(fill="lightblue")+
  geom_hline(yintercept = .5,linetype="dashed")+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.15)+
  scale_y_continuous(limits=c(0,.6),breaks=seq(0,.6,.1))+
  labs(x="order",y="mean rst")+
  ggthemes::theme_few()
rst_by_pos_plot
  
tcd_by_pos_plot <- ggplot(tcd_by_pos, aes(attr_choice,mean_prop,fill=attr_choice)) +
  geom_col(width=.5)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.15)+
  facet_wrap(vars(tcd_order),scales="free_x")+
  scale_fill_discrete(name="choice")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
  labs(x="choice",y="mean choice proportion")+
  ggthemes::theme_few()
tcd_by_pos_plot

# Other - Is target/competitor more likely to be large/small ==============================================================================
tc_max <- function(target, competitor) ifelse(target>competitor, "target", "competitor")
tc_min <- function(target, competitor) ifelse(target<competitor, "target", "competitor")

transfer$tc_max <- map2_chr(as.character(transfer$target_phy), as.character(transfer$competitor_phy), tc_max) 
transfer$tc_min <- map2_chr(as.character(transfer$target_phy), as.character(transfer$competitor_phy), tc_min)

transfer %>%
  filter(transfer_trial_type=="attraction") %>%
  count(sub_n, trial_choice_set, tc_max) %>%
  rename(option=tc_max) %>%
  ggplot(aes(option, n, fill=option))+
  geom_col()+
  facet_wrap(vars(trial_choice_set))+
  ggthemes::theme_few()

# Saving plots ==============================================================================
if(save_plots){
  ggsave(filename=here("line_exp_2","plots","tcd_avg_plot.png"),
         tcd_avg_plot,
         width=5,height=4,units="in")
  ggsave(filename=here("line_exp_2","plots","tcd_grouped_plot_binary.pdf"),
         tcd_grouped_plot_bi,
         width=12,height=9,units="in")
  ggsave(filename=here("line_exp_2","plots","tcd_grouped_plot_trinary.pdf"),
         tcd_grouped_plot_tri,
         width=12,height=9,units="in")
  ggsave(filename=here("line_exp_2","plots","rst_hist.png"),
         rst_hist,
         width=5,height=4,units="in")
  ggsave(filename=here("line_exp_2","plots","tcd_boxplot.png"),
         tcd_boxplot,
         width=6,height=5,units="in")
  ggsave(filename=here("line_exp_2","plots","rst_barplot.png"),
         rst_barplot,
         width=4,height=4,units="in")
  ggsave(filename=here("line_exp_2","plots","tcd_avg_plot_first_100_trials.png"),
         tcd_avg_plot_f_100,
         width=4,height=4,units="in")
  ggsave(filename=here("line_exp_2","plots","mean_rst_posterior_hist.png"),
         rst_posterior_plot,
         width=5,height=5,units="in")
  ggsave(filename=here("line_exp_2","plots","rst_by_pos_plot.png"),
         rst_by_pos_plot,
         width=5,height=4,units="in")
  ggsave(filename=here("line_exp_2","plots","tcd_by_pos_plot.png"),
         tcd_by_pos_plot,
         width=7,height =4,units="in")
  ggsave(filename = here("line_exp_2","plots","tri_v_bi_hist.png"),
         tri_v_bi_hist,
         width=5,height=4,units="in")
  ggsave(filename = here("line_exp_2","plots","tri_v_bi_post_dist.png"),
         tri_v_bi_post_plot,
         width=5,height=4,units="in")
  ggsave(filename = here("line_exp_2","plots","tri_v_tri_hist.png"),
         tri_v_tri_hists,
         width=5,height=4,units="in")
}

   