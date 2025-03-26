rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(latex2exp)

tibble(
  d = seq(0,5,.05),
  s = exp(-.5*d)
) %>%
  ggplot(aes(d,s))+
  geom_path()+
  geom_text(aes(x=3,y=.8),
            label=TeX("$s_{ij}=e^{-c*d_{ij}}$",output = "character"),
            inherit.aes = F,parse=T,size=14)+
  ggthemes::theme_few()+
  labs(x=TeX("$d_{ij}$"),
       y=TeX("$s_{ij}$"))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=36))

ggsave(here("psychonomics_figures","plots","sim_dist_plot.png"),
       width=4,height=4)
