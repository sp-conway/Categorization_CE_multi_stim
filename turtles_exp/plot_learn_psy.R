# Plotting MDS-interpolated learning trial stimuli for exp2
# Sean Conway
# Last modified July 2022

# Setup ========================================================================================
# clear environment
rm(list=ls())

# libraries
library(readr)
library(ggplot2)
library(dplyr)
library(ggsci)
library(here)
library(patchwork)

# read in stim
exemplars_psy <- read_csv(here("exp2","mds","exp2_learning_stim_psy.csv"))
exemplars_phy <- read_csv(here("exp2","trials","exp2_learn.csv"))

# mds res
mds <- read_csv(here("exp2","mds","mds_turtles.csv")) %>%
  mutate(source="similarity study")

turtles <- read_csv(here("exp2","mds","orig_turtles.csv")) %>%
  mutate(source="similarity study")

all_psy <- bind_rows(
  select(exemplars_psy, c(angle,radius)) %>%
    mutate(source="categorization experiment"),
  mds
)

all_phy <- bind_rows(
  select(exemplars_phy, c(angle,radius)) %>%
    mutate(source="categorization experiment"),
  turtles
)

# plotting ========================================================================================
psy_plot <- ggplot(exemplars_psy, aes(angle,radius,col=distribution))+
  geom_point(alpha=.6,size=4)+
  coord_fixed(xlim=range(exemplars_psy$angle),
              ylim=range(exemplars_psy$radius))+
  scale_color_tron()+
  labs(title="Psychological")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=14))

all_psy_plot <- ggplot(all_psy, aes(angle,radius,col=source))+
  geom_point(alpha=.6,size=4)+
  coord_fixed(xlim=range(all_psy$angle),
              ylim=range(all_psy$radius))+
  #scale_color_tron()+
  labs(title="Psychological")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=14))

phy_plot <- ggplot(exemplars_phy,aes(angle,radius,col=distribution))+
  geom_point(alpha=.6,size=4)+
  scale_x_continuous(limits=c(10,40))+
  scale_y_continuous(limits=c(30,120))+
  scale_color_tron()+
  labs(title="Physical")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=14))

all_phy_plot <-ggplot(all_phy,aes(angle,radius,col=source))+
  geom_point(alpha=.6,size=4)+
  # scale_x_continuous(limits=c(10,40))+
  # scale_y_continuous(limits=c(30,120))+
  #ale_color_tron()+
  labs(title="Physical stimuli")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=14))

both_plot <- (phy_plot|psy_plot)+
  plot_annotation(title="Learning trial stimuli",
                  theme = theme(plot.title=element_text(hjust=0.5,size=18)))

all_plot <- (all_phy_plot|all_psy_plot)

ggsave(both_plot,filename=here("exp2","R","plots","exp2_learn_exemplars_phy_psy.pdf"),
       width=10,height=6)
ggsave(all_plot,filename=here("exp2","R","plots","exp2_sim_learn_COMBINED_stim_phy_psy.pdf"),
       width=10,height=6)
