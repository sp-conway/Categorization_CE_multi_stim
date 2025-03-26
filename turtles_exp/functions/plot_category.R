library(dplyr)
library(ggsci)
library(tidyr)
library(ggplot2)
plot_category <- function(title=""){
  d <- data.table::fread(here(,"turtles_exp","categories","exp2_category_sampled.csv")) %>%
    as_tibble()
  bv_dens_plot <- ggplot(d)+
    geom_density_2d_filled(aes(angle, radius))+
    coord_fixed(xlim=c(10,40),
                ylim=c(30,120))+
    labs(x="angle",y="radius",title=title)+
    ggthemes::theme_few()+
    theme(plot.title=element_text(hjust=0.5,size=20),
          strip.text.x=element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          legend.position="none")
  return(bv_dens_plot)
}

plot_category_psy <- function(title=""){
  d <- data.table::fread(here(,"turtles_exp","stimuli","exp2_learning_stim_psy_phy.csv")) %>%
    as_tibble() 
  # am <- mean(d$angle_psy)
  # asd <- sd(d$angle_psy)
  # rm <- mean(d$radius_psy)
  # rsd <- sd(d$radius_psy)
  # d <- d %>%
  #   mutate(angle_psy_z=(angle_psy-am)/asd,
  #          radius_psy_z=(radius_psy-rm)/rsd)
  pl <- ggplot(d)+
    geom_point(aes(angle_psy, radius_psy,col=distribution),size=3,alpha=.3)+
    coord_fixed(xlim=c(-30,30),
                ylim=c(-30,30))+
    labs(x="angle",y="radius",title="")+
    scale_color_tron()+
    ggthemes::theme_few()+
    theme(plot.title=element_text(hjust=0.5,size=20),
          strip.text.x=element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12))
  return(pl)
}

plot_category_phy <- function(title=""){
  d <- data.table::fread(here(,"turtles_exp","stimuli","exp2_learning_stim_psy_phy.csv")) %>%
    as_tibble() 
  am <- mean(d$angle)
  asd <- sd(d$angle)
  rm <- mean(d$radius)
  rsd <- sd(d$radius)
  d <- d %>%
    mutate(angle_z=(angle-am)/asd,
           radius_z=(radius-rm)/rsd)
  pl <- ggplot(d)+
    geom_point(aes(angle_z, radius_z,col=distribution),size=3,alpha=.3)+
    coord_fixed(xlim=c(-3,3),
                ylim=c(-3,3))+
    labs(x="angle",y="radius",title="")+
    scale_color_tron()+
    ggthemes::theme_few()+
    theme(plot.title=element_text(hjust=0.5,size=20),
          strip.text.x=element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12))
  return(pl)
}
