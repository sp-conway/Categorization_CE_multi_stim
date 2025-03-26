# Sean Conway
# Last modified Mar. 2023

# setup =====================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(fs)
library(glue)
library(ggsci)
library(patchwork)
library(BayesFactor)
library(rlang)
library(geometry)

# save plots?
save_plots <- T

# source category plotting function
source(here(,"turtles_exp","functions","plot_category.R"))
# source category params
source(here(,"turtles_exp","category_params.R"))

# functions =====================================================================
# function for computing summary statistic for a dv 
# grouped by 0 or more grouping variables
summary_stats <- function(dat, dv, n_subs,...){
  grouped_vars <- enquos(...)
  summ <- dat %>%
    group_by(!!!grouped_vars) %>%
    summarise("mean"=mean({{dv}}),
              "median"=median({{dv}}),
              "sd"=sd({{dv}}),
              "se"=sd/sqrt(n_subs),
              "se_lower"=mean-se,
              "se_upper"=mean+se) %>%
    ungroup()
  return(summ)
}

# count choice proportion based on 0 or more grouping variables
grouped_props <- function(dat, choice_var, ...){
  all_vars <- enquos(choice_var, ...)
  grouping <- enquos(...)
  props <- dat %>%
    mutate(across(c(!!!all_vars),as.factor)) %>%
    count(!!!all_vars,.drop=F) %>%
    group_by(!!!grouping) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup()
  return(props)
}

# read in data =====================================================================
# transfer data
transfer <- data.table::fread(here(,"turtles_exp","data","cleaned","exp2_transfer.csv"))
n_subs <- length(unique(transfer$sub_n))

# attraction trial specs & interpolate
source(here(,"turtles_exp","functions","mds_interp.R"))
mds_res <- read_csv(here(,"turtles_exp","mds","mds_turtles.csv")) 
phys_stim <- read_csv(here(,"turtles_exp","mds","orig_turtles.csv"))
mds <- as.matrix(mds_res[,c(1:2)])
phys <- as.matrix(phys_stim[,c(1:2)])
attraction_trials <- readr::read_csv(here(,"turtles_exp","trials","exp2_attraction_long.csv"))
attraction_trials$angle_psy <- numeric(nrow(attraction_trials))
attraction_trials$radius_psy <- numeric(nrow(attraction_trials))

for(i in 1:nrow(attraction_trials)){
  x <- interp(as.matrix(attraction_trials[i,c("angle","radius")]),
              mds=mds,
              phys=phys)
  attraction_trials$angle_psy[i] <- x[1]
  attraction_trials$radius_psy[i] <- x[2]
}

# Interpolate category prototype
proto <- interp(c(am,rm),phys,mds)

# filter data =====================================================================
attraction <- transfer %>%
  filter(transfer_trial_type=="attraction") %>%
  mutate(tdc_choice=case_when(
    choice=="a" ~ tdc_a,
    choice=="b" ~ tdc_b,
    choice=="c" ~ tdc_c
  ),
  across(c(choice_set,tc_angle,tdc_choice),as.factor))

filler_sample <- transfer %>%
  filter(transfer_trial_type=="filler_sample")

filler_easy <- transfer %>%
  filter(transfer_trial_type=="filler_easy")

# analyze fillers =====================================================================
filler_easy_correct <- grouped_props(filler_easy, correct, choice_set, sub_n) %>%
  select(-n) %>%
  filter(correct==1) 

filler_easy_correct_avg <- filler_easy_correct %>%
  summary_stats(., prop, n_subs, choice_set) %>%
  mutate(type="filler-easy")

filler_sample_correct <- grouped_props(filler_sample, correct, choice_set, sub_n) %>%
  select(-n) %>%
  filter(correct==1) 

filler_sample_correct_avg <- filler_sample_correct %>%
  summary_stats(., prop, n_subs, choice_set) %>%
  mutate(type="filler-sample")

filler_correct_all_avg <- bind_rows(filler_easy_correct_avg,
                                    filler_sample_correct_avg)

# plot filler data =====================================================================
filler_easy_hist <- ggplot(filler_easy_correct,aes(prop))+
  geom_histogram(binwidth=.1,fill="dodgerblue1",col="dodgerblue2",alpha=.5)+
  facet_wrap(vars(choice_set))+
  scale_x_continuous(breaks=c(0,.5,1))+
  labs(title="proportion correct - filler easy trials")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

filler_sample_hist <- ggplot(filler_sample_correct,aes(prop))+
  geom_histogram(binwidth=.1,fill="dodgerblue1",col="black",alpha=.5)+
  facet_wrap(vars(choice_set))+
  #scale_x_continuous(limits=c(0,1),breaks=c(0,.5,1))+
  labs(title="proportion correct - filler sample trials")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

bplot_filler_correct_all_avg <- ggplot(filler_correct_all_avg, aes(choice_set,mean))+
  geom_col(fill="dodgerblue2")+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.7)+
  facet_wrap(vars(type))+
  labs(x="choice set",
       y="mean prop. correct",
       caption="Error bars are +- 1 SE.",
       title="filler trial mean prop. correct")+
  scale_y_continuous(limits=c(0,1))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        strip.text=element_text(size=15))

# analyze attraction trials =====================================================================
compute_rst <- function(dat){
  dd <- dat %>%
    select(-n) %>%
    pivot_wider(names_from = tdc_choice, values_from=prop) %>%
    mutate(rst=t/(t+c))
  return(dd)
}

# Average T/D/C choices
# collapsed across/not collapsed across various variables
tdc_by_set_ang_indiv <- grouped_props(attraction, tdc_choice, choice_set,tc_angle,sub_n) %>%
  filter( choice_set!="binary" | tdc_choice!="d")
tdc_by_set_indiv <- grouped_props(attraction, tdc_choice, choice_set,sub_n) %>%
  filter( choice_set!="binary" | tdc_choice!="d")
tdc_by_set_ang_pair_indiv <- grouped_props(attraction, tdc_choice, tc_angle, choice_set, sub_n, pair) %>%
  filter( choice_set!="binary" | tdc_choice!="d")
tdc_by_set_ang_avg <- summary_stats(tdc_by_set_ang_indiv, prop, n_subs, choice_set, tc_angle, tdc_choice)
tdc_by_set_ang_pair_avg <- summary_stats(tdc_by_set_ang_pair_indiv, prop, n_subs, choice_set, tc_angle, tdc_choice, pair)
tdc_by_set_avg <- summary_stats(tdc_by_set_indiv, prop, n_subs, choice_set, tdc_choice)

# RSTs
rst_by_set_ang_indiv <- compute_rst(tdc_by_set_ang_indiv )
rst_by_set_indiv <- compute_rst(tdc_by_set_indiv)
rst_by_set_ang_avg <- summary_stats(rst_by_set_ang_indiv, rst, n_subs, choice_set, tc_angle)
rst_by_set_avg <- summary_stats(rst_by_set_indiv, rst, n_subs, choice_set)

# trinary minus binary indiv
tri_min_bi_ang_pair_indiv <- tdc_by_set_ang_pair_indiv %>%
  select(-n) %>%
  pivot_wider(names_from = choice_set, values_from = prop) %>%
  mutate(trinary_minus_binary=trinary-binary) %>%
  filter(tdc_choice!="d")

tri_min_bi_ang_indiv <- tdc_by_set_ang_indiv %>%
  select(-n) %>%
  pivot_wider(names_from = choice_set, values_from = prop) %>%
  mutate(trinary_minus_binary=trinary-binary) %>%
  filter(tdc_choice!="d") %>%
  mutate(across(c(tdc_choice,tc_angle),as.factor))

tmb_t_ang <- tdc_by_set_ang_indiv %>%
  select(-n) %>%
  filter(tdc_choice=="t") %>%
  pivot_wider(names_from = choice_set,
              values_from = prop) %>%
  mutate(tmb=trinary-binary)
tmb_t_ang_pair <- tdc_by_set_ang_pair_indiv %>%
  select(-n) %>%
  filter(tdc_choice=="t") %>%
  pivot_wider(names_from = choice_set,
              values_from = prop) %>%
  mutate(tmb=trinary-binary)

# trinary minus binary avg
tri_min_bi_ang_pair_avg <- summary_stats(tri_min_bi_ang_pair_indiv, trinary_minus_binary, n_subs,tc_angle, tdc_choice, pair)
tri_min_bi_ang_avg <- summary_stats(tri_min_bi_ang_indiv, trinary_minus_binary, n_subs,tc_angle, tdc_choice)
tmb_t_ang_pair_avg <- summary_stats(tmb_t_ang_pair, tmb, n_subs, tc_angle, pair)
tmb_t_ang_avg <- summary_stats(tmb_t_ang_pair, tmb, n_subs, tc_angle)

# Plotting attraction effect data  =====================================================================
bplot_tdc_by_set_ang_avg <- tdc_by_set_ang_avg %>%
  filter(choice_set!="binary") %>%
  ggplot(aes(tdc_choice,mean))+
  geom_col(fill="dodgerblue2")+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.75)+
  facet_wrap(vars(tc_angle,choice_set))+
  labs(x="choice",y="mean choice proportion",
       caption="Error bars are +- 1 SE.",
       title="avg. t/d/c choice proportion by t/c angle",
       subtitle=glue("N={n_subs}"))+
  scale_y_continuous(limits=c(0,1))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        plot.subtitle=element_text(hjust=0,size=14),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        strip.text=element_text(size=15))

bplot_tdc_avg <- tdc_by_set_avg %>%
  filter(choice_set!="binary") %>%
  ggplot(aes(tdc_choice,mean))+
  geom_col(fill="dodgerblue2")+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.75)+
  labs(x="choice",y="mean choice proportion",
       caption="Error bars are +- 1 SE.",
       title="avg. t/d/c choice proportion (trinary)",
       subtitle=glue("N={n_subs}"))+
  scale_y_continuous(limits=c(0,1))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        plot.subtitle=element_text(hjust=0,size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

bplot_tri_min_bi <- tri_min_bi_ang_avg %>%
  ggplot(aes(tdc_choice,mean,fill=tc_angle))+
  geom_col(position = position_dodge())+
  geom_errorbar(aes(ymin=se_lower,
                    ymax=se_upper),
                position=position_dodge(.9),
                width=.2)+
  scale_fill_tron()+
  geom_hline(yintercept=0,linetype="dashed",alpha=.4)+
  labs(x="choice",y="diff in mean choice prop",
       caption="Error bars are +- 1 SE.",
       title="avg. tri min bi choice proportion",
       subtitle=glue("N={n_subs}"))+
  scale_y_continuous(limits=c(-.5,.5))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        plot.subtitle = element_text(hjust=0.5,size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

bplot_tmb_ang_avg <- tmb_t_ang_avg %>%
  ggplot(aes(tc_angle,mean))+
  geom_col(fill="purple")+
  geom_errorbar(aes(ymin=se_lower,
                    ymax=se_upper),
                position=position_dodge(.9),
                width=.2)+
  geom_hline(yintercept=0,linetype="dashed",alpha=.4)+
  labs(x="choice",y="tri - bi",
       caption="Error bars are +- 1 SE.",
       title="avg. tri min bi t choice prop",
       subtitle=glue("N={n_subs}"))+
  scale_y_continuous(limits=c(-.5,.5))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        plot.subtitle = element_text(hjust=0.5,size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))
bplot_tmb_ang_avg

hist_rst_by_ang <- rst_by_set_ang_indiv %>%
  filter(choice_set!="binary") %>%
  ggplot(aes(rst))+
  geom_histogram(binwidth=.1,fill="dodgerblue1",col="black",alpha=.5)+
  facet_wrap(vars(tc_angle))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

bplot_rst_by_ang <- rst_by_set_ang_avg %>%
  filter(choice_set!="binary") %>%
  ggplot(aes(tc_angle,mean))+
  geom_col()+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25)+
  scale_y_continuous(limits=c(0,1))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=13),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))



# Plotting various pairs 
pair_plots_90 <- list()
pair_plots_180 <- list()
pair_plots_180_tmb <- list()
pair_plots_90_tmb <- list()
pair_plots_180_tmc <- list()
pair_plots_90_tmc <- list()

for(i in 1:4){
  pair_plots_90[[i]] <-  ( 
    (plot_category_psy()+
       geom_text(data=filter(attraction_trials,choice_set=="trinary" & pair==i & tc_angle==90),
                 aes(x=angle_psy,y=radius_psy,label=stimulus),size=6,col="black")+
       # geom_point(aes(proto[1],proto[2]),inherit.aes=F,
       #            size=8,col="red",shape="X")+
       # labs(caption="X indicates category prototype")+
       scale_color_simpsons()) |
    (tdc_by_set_ang_pair_avg %>%
         filter(tc_angle==90 & pair==i) %>%
         ggplot(aes(tdc_choice,mean))+
         facet_wrap(vars(choice_set),scales="free_x")+
         geom_col(fill="dodgerblue3")+
         geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.75)+
         labs(x="choice",y="mean choice proportion",
              title="avg. t/d/c choice prop")+
         scale_y_continuous(limits=c(0,1))+
         ggthemes::theme_few()+
         theme(plot.title=element_text(hjust=0.5,size=16),
               axis.text=element_text(size=15),
               axis.title=element_text(size=15)) 
    
  )
  )
  
  pair_plots_180[[i]] <- ( 
    (plot_category_psy()+
       geom_text(data=filter(attraction_trials,choice_set=="trinary" & pair==i & tc_angle==180),
                 aes(x=angle_psy,y=radius_psy,label=stimulus),size=6,col="black")+
       # geom_point(aes(proto[1],proto[2]),inherit.aes=F,
       #            size=8,col="red",shape="X")+
       # labs(caption="X indicates category prototype")+
       scale_color_simpsons()) |
      (tdc_by_set_ang_pair_avg %>%
         filter(tc_angle==180 & pair==i) %>%
         ggplot(aes(tdc_choice,mean))+
         facet_wrap(vars(choice_set),scales="free_x")+
         geom_col(fill="dodgerblue3")+
         geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.75)+
         labs(x="choice",y="mean choice proportion",
              title="avg. t/d/c choice prop")+
         scale_y_continuous(limits=c(0,1))+
         ggthemes::theme_few()+
         theme(plot.title=element_text(hjust=0.5,size=16),
               axis.text=element_text(size=15),
               axis.title=element_text(size=15)) 
       
      )
  )
  
  pair_plots_90_tmb[[i]] <-  ( 
    (plot_category_psy()+
       geom_text(data=filter(attraction_trials,choice_set=="trinary" & pair==i & tc_angle==90),
                 aes(x=angle_psy,y=radius_psy,label=stimulus),size=6,col="black")+
       scale_color_simpsons())|
       # geom_point(aes(proto[1],proto[2]),inherit.aes=F,
       #            size=4,col="red",shape="X",alpha=.5)+
       # labs(caption="X indicates category prototype")) |
      (tri_min_bi_ang_pair_avg %>%
         filter(tc_angle==90 & pair==i) %>%
         ggplot(aes(tdc_choice,mean))+
         geom_col(fill="dodgerblue3")+
         geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.75)+
         labs(x="choice",y="mean choice proportion",
              title="avg. trinary minus binary")+
         scale_y_continuous(limits=c(-1,1))+
         ggthemes::theme_few()+
         theme(plot.title=element_text(hjust=0.5,size=16),
               axis.text=element_text(size=15),
               axis.title=element_text(size=15))) 
    
  )
  
  pair_plots_180_tmb[[i]] <- ( 
    (plot_category_psy()+
       geom_text(data=filter(attraction_trials,choice_set=="trinary" & pair==i & tc_angle==180),
                 aes(x=angle_psy,y=radius_psy,label=stimulus),size=6,col="black")+
       scale_color_simpsons())|
       # geom_point(aes(proto[1],proto[2]),inherit.aes=F,
       #            size=4,col="red",shape="X",alpha=.5)+
       # labs(caption="X indicates category prototype")) |
      (tri_min_bi_ang_pair_avg %>%
         filter(tc_angle==180 & pair==i) %>%
         ggplot(aes(tdc_choice,mean))+
         geom_col(fill="dodgerblue3")+
         geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,alpha=.75)+
         labs(x="choice",y="mean choice proportion",
              title="avg. trinary minus binary")+
         scale_y_continuous(limits=c(-1,1))+
         ggthemes::theme_few()+
         theme(plot.title=element_text(hjust=0.5,size=16),
               axis.text=element_text(size=15),
               axis.title=element_text(size=15))) 
    
  )
}

# stats ==============================================================================
ttestBF(rst_by_set_ang_indiv[rst_by_set_ang_indiv$choice_set=="trinary" & rst_by_set_ang_indiv$tc_angle==90,]$rst,
        mu=.5)
ttestBF(rst_by_set_ang_indiv[rst_by_set_ang_indiv$choice_set=="trinary" & rst_by_set_ang_indiv$tc_angle==180,]$rst,
        mu=.5)


# ANOVA
aov_tmb_bayes <- anovaBF(trinary_minus_binary~ (tc_angle) + (tdc_choice) + (tc_angle*tdc_choice),
                         data=tri_min_bi_ang_indiv,whichRandom = "sub_n")
summary(aov_tmb_bayes)
aov_tmb_freq <- aov(trinary_minus_binary~ (tc_angle) + (tdc_choice) + (tc_angle*tdc_choice),
                    data=tri_min_bi_ang_indiv,whichRandom = "sub_n")
summary(aov_tmb_freq)



# save plots =====================================================================
if(save_plots){
  ggsave(bplot_tdc_avg,filename=here(,"turtles_exp","plots","turtles_exp_barplot_tdc_avg.pdf"),
         width=8,height=8)
  ggsave(bplot_tdc_by_set_ang_avg,filename=here(,"turtles_exp","plots","turtles_exp_barplot_tdc_by_set_ang_avg.pdf"),
         width=8,height=8)
  ggsave(filler_easy_hist,filename=here(,"turtles_exp","turtles_exp_filler_easy_hist.pdf"),
         width=8,height=8)
  ggsave(hist_rst_by_ang,filename=here(,"turtles_exp","turtles_exp_hist_rst_by_ang.pdf"),
         width=8,height=6)
  ggsave(filler_sample_hist,filename=here(,"turtles_exp","turtles_exp_filler_correct_hist.pdf"),
         width=8,height=6)
  ggsave(bplot_filler_correct_all_avg,filename=here(,"turtles_exp","plots","turtles_exp_barplot_filler_correct_all_avg.pdf"),
         width=6,height=8)
  ggsave(bplot_tri_min_bi,filename=here(,"turtles_exp","turtles_exp_tri_min_bi_by_ang_barplot.pdf"),
         width=5,height=5)
  pdf(here(,"turtles_exp","plots","turtles_exp_tdc_avg_by_pair_180.pdf"))
  for(i in 1:4){
    plot(pair_plots_180[[i]])
  }
  dev.off()
  
  pdf(here(,"turtles_exp","plots","turtles_exp_tdc_avg_by_pair_90.pdf"))
  for(i in 1:4){
    plot(pair_plots_90[[i]])
  }
  dev.off()
  
  pdf(here(,"turtles_exp","plots","turtles_exp_tri_min_bi_avg_by_pair_180.pdf"))
  for(i in 1:4){
    plot(pair_plots_180_tmb[[i]])
  }
  dev.off()
  
  pdf(here(,"turtles_exp","plots","turtles_exp_tri_min_bi_avg_by_pair_90.pdf"))
  for(i in 1:4){
    plot(pair_plots_90_tmb[[i]])
  }
  dev.off()

}


