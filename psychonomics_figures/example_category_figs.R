rm(list=ls())
library(tidyverse)
library(here)
library(patchwork)

# experiment 1 ============================================================
exp1b_attr_trials <- here("exp1b","trials","attr_eff_trials_long_1b.csv") %>%
  read_csv()
source(here("exp1b","R","functions", "density_functions_2.R"))
source(here("exp1b","R","functions", "plotting_functions.R"))

plot_categories_psych_1b()+
  labs(x="line length")+
  theme(text=element_text(size=32),axis.text = element_blank(),
        axis.ticks=element_blank())
ggsave(here("psychonomics_figures","plots","exp1dist_plot.pdf"),
       width=7,height=5)

exp1b_trials_tri <- exp1b_attr_trials %>%
  slice(1:3) %>%
  mutate(p_in=prob_in(jnds),
         `target type`=case_when(
           target_side=="left"~"short",
           target_side=="right"~"long"
         ))

exp1b_trials_bi <- exp1b_attr_trials %>%
  slice(1:2) %>%
  mutate(p_in=prob_in(jnds),
         `target type`=case_when(
           target_side=="left"~"short",
           target_side=="right"~"long"
         ))

exp1dist <- plot_optimal_1b()+
  labs(x="line length",
       y="p(in category)",
       title="category distribution")+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=23))

exp1_examp_plot_tri <- plot_optimal_1b()+
  geom_text(data=exp1b_trials_tri,aes(jnds,p_in,label=str_sub(option,1,1)),
            inherit.aes=F,col='red',size=16)+
  labs(x="line length",
       y="p(in category)")+
  scale_y_continuous(limits=c(0,1),breaks=c(0,.5,1))+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=30))
exp1_examp_plot_bi <- plot_optimal_1b()+
  geom_text(data=exp1b_trials_bi,aes(jnds,p_in,label=str_sub(option,1,1)),
            inherit.aes=F,col='red',size=16)+
  labs(x="line length",
       y="p(in category)")+
  scale_y_continuous(limits=c(0,1),breaks=c(0,.5,1))+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=30))
# experiment 2 ============================================================
exp2_attr_trials <- here("exp2","trials","exp2_attraction_long.csv") %>%
  read_csv()

exp2_trials_tri <- slice(exp2_attr_trials, c(1:3)) %>%
  rename(`target type`=tc_angle)
exp2_trials_bi <- slice(exp2_attr_trials, c(1,3)) %>%
  rename(`target type`=tc_angle)

source(here("exp2","R","functions","exp2_plot_category.R"))
source(here("exp2","R","functions","distribution_functions.R"))
source(here("exp2","R","exp2_category_params.R"))

exp2_category_normed <- sample_category(50000,c(0,0),matrix(c(1,0,0,1),nrow=2,byrow=T))

exp2_category_normed_plot <- ggplot(exp2_category_normed)+
  stat_density_2d_filled(aes(angle,radius,fill=..level..),bins=5)+
  coord_fixed(xlim=c(-3,3),ylim=c(-3,3))+
  scale_x_continuous(breaks=c(-3,0,3))+
  scale_y_continuous(breaks=c(-3,0,3))+
  labs(x="angle",
       y="radius")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),
        text =  element_text(size=32),
        axis.ticks = element_blank(),
        axis.text = element_blank())
exp2_category_normed_plot
exp2_examp_plot_tri <- exp2_category_normed_plot+
  geom_text(data=exp2_trials_tri,aes(x=(angle-am)/asd,
                                 y=(radius-rm)/rsd,
                                 label=stimulus),
            inherit.aes = F,
            size=16,
            col='red')+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size=32))
exp2_examp_plot_tri

exp2_examp_plot_bi <- exp2_category_normed_plot+
  geom_text(data=exp2_trials_bi,aes(x=(angle-am)/asd,
                                     y=(radius-rm)/rsd,
                                     label=stimulus),
            inherit.aes = F,
            size=16,
            col='red')+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size=32))
exp2_examp_plot_bi

# save figs =======================================================
ggsave(filename = here("psychonomics_figures","plots","exp1_example_plot_tri.pdf"),
       width=4,height=4,plot=exp1_examp_plot_tri)
ggsave(filename = here("psychonomics_figures","plots","exp1_example_plot_bi.pdf"),
       width=4,height=4,plot=exp1_examp_plot_bi)
ggsave(filename = here("psychonomics_figures","plots","exp2_example_plot_tri.pdf"),
       width=6,height=4,plot=exp2_examp_plot_tri)
ggsave(filename = here("psychonomics_figures","plots","exp2_example_plot_bi.pdf"),
       width=6,height=4,plot=exp2_examp_plot_bi)
ggsave(filename = here("psychonomics_figures","plots","exp1_example_dist_plot.pdf"),
       width=6,height=4,plot=exp1dist)
ggsave(filename = here("psychonomics_figures","plots","exp2_example_dist_plot.pdf"),
       width=6,height=4,plot=exp2_category_normed_plot)
