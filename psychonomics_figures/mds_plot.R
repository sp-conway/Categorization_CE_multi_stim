rm(list=ls())
library(here)
library(tidyverse)
library(latex2exp)
library(patchwork)

learning <- here("exp2","mds","exp2_learning_stim_psy.csv") %>%
  read_csv()

learning %>%
  ggplot(aes(angle,radius,col=distribution))+
  geom_point(size=6,alpha=.6)+
  scale_color_startrek(name="category")+
  coord_fixed(xlim=c(-30,50),ylim=c(-50,40))+
  labs(title="exp 2 exemplars\npsychological space")+
  ggthemes::theme_few()+
  theme(text = element_text(size=32),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))
ggsave(filename=here("psychonomics_figures","plots","exp2mds.png"),
       width=7,height=5) 
