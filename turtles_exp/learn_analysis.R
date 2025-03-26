# Sean Conway
# Last modified July 2022

# setup =====================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(readr)
library(fs)
library(purrr)
library(patchwork)
library(glue)
library(khroma)
library(ggsci)

# LaX participants
# some participants were participating while UMass held a loud Lacrosse match right outside
# checking on whether or not these subjects need to be removed  
lax_ppts <- c(6,7,8,21,22,23,40,41,42)

# source distribution functions
source(here(,"turtles_exp","functions","distribution_functions.R"))

# source category parameters
source(here(,"turtles_exp","category_params.R"))

# read in learning stim psy-phy
learn_stim <- read_csv(here(,"turtles_exp","stimuli","exp2_learning_stim_psy_phy.csv")) %>%
  mutate(across(c(angle,radius),round,digits=2))

# controls
save_plots <- T

# read in data =====================================================================
get_dat <- function(f,am,rm,asd,rsd){
  d <- data.table::fread(f) %>%
    as_tibble() 
  dd <- d %>%
    mutate(dens_in=dbvn(angle,radius,am,rm,asd,rsd,0),
           dens_out=dbvu(alim,rlim),
           prob_in=dens_in/(dens_in+dens_out),
           ll_in=log(dens_in/dens_out),
           turtle=paste(angle,radius,sep="_"),
           angle_z=(angle-am)/asd,
           radius_z=(radius-rm)/rsd,
           angle_z_abs=abs(angle-am)/asd,
           radius_z_abs=abs(radius-rm)/rsd,
           choice=as.factor(choice))
  return(dd)
}

# read in learning data & join psych dim values
learn <- get_dat(here(,"turtles_exp","data","cleaned","exp2_learn.csv"),
                 am,rm,asd,rsd) %>%
  mutate(across(c(angle,radius),round,digits=2)) %>%
  left_join(., select(learn_stim,
                      -distribution), 
            by=c("angle","radius")
            ) %>% # join psych values 
  mutate(turtle_psy=paste(angle_psy,radius_psy,sep="_"))

n_subs <- length(unique(learn$sub_n))
cat("\n==================\nN=",n_subs,"\n==================\n",sep="")

# proportion correct  =====================================================================
prop_correct <- learn %>%
  count(sub_n,correct) %>%
  group_by(sub_n) %>%
  mutate(prop=n/sum(n)) %>%
  filter(correct==1) %>%
  ungroup()

prop_correct_by_dist <- learn %>%
  count(sub_n,correct,distribution) %>%
  group_by(sub_n,distribution) %>%
  mutate(prop=n/sum(n)) %>%
  filter(correct==1) %>%
  ungroup()

prop_correct %>%
  summarise(mean=mean(prop),
            median=median(prop),
            sd=sd(prop),
            min=min(prop),
            max=max(prop))

prop_correct_by_dist %>%
  group_by(distribution) %>%
  summarise(mean=mean(prop),
            median=median(prop),
            sd=sd(prop),
            min=min(prop),
            max=max(prop))

# check lax ppts
prop_correct %>%
  filter(sub_n %in% lax_ppts) %>%
  select(sub_n,prop)

learn_prop_corr_plot <- ggplot(prop_correct,aes(prop))+
  geom_histogram(col="black",fill="lightblue",binwidth=.05)+
  scale_x_continuous(limits=c(0,1))+
  labs(x="proportion correct",
       title=glue("learning trial\naccuracy data\nN={n_subs}"),
       subtitle=glue("Median = {round(median(prop_correct$prop),digits=2)}"))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.subtitle=element_text(hjust=0.5,size=15),
        axis.text = element_text(size=14),
        axis.title= element_text(size=14))

learn_prop_corr_by_dist_plot <- ggplot(prop_correct_by_dist,aes(prop))+
  geom_histogram(col="black",fill="lightblue",binwidth=.05)+
  facet_wrap(vars(distribution))+
  scale_x_continuous(limits=c(0,1))+
  labs(x="proportion correct",
       title=glue("learning trial\naccuracy data\nN={n_subs}"))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.subtitle=element_text(hjust=0.5,size=15),
        axis.text = element_text(size=14),
        axis.title= element_text(size=14))

# aggregate choice  =====================================================================
# Unique turtle log likelihoods
unique_ll_phy <- learn %>%
  distinct(turtle,ll_in)

unique_ll_psy <- learn %>%
  distinct(turtle_psy,ll_in)

agg_choice <- function(dat,turtle_var,unique_ll){
  turtle_var <- enquo(turtle_var)
  dat %>%
    mutate(across(c(!!turtle_var,choice),as.factor)) %>%
    count(!!turtle_var,choice,.drop=F) %>%
    group_by(!!turtle_var) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    mutate(turtle=as.character(!!turtle_var)) %>%
    filter(choice==1) %>%
    left_join(., unique_ll) %>%
    separate(!!turtle_var,into=c("angle","radius"),sep="_",remove=F)
}

choice_agg_phy <- agg_choice(learn,turtle,unique_ll_phy)  %>%
  mutate(across(c(angle,radius),as.numeric),
         angle_z=(angle-am)/asd,
         radius_z=(radius-rm)/rsd)

choice_agg_psy <- agg_choice(learn,turtle_psy,unique_ll_psy) %>%
  mutate(across(c(angle,radius,prop),as.numeric))

make_grid <- function(angle,radius,prop){
  grid <- akima::interp(angle,
                        radius,
                        prop)
  grid_df <- tibble(
    angle=rep(grid$x,ncol(grid$z)),
    radius=rep(grid$y,each=nrow(grid$z)),
    prop=as.numeric(grid$z)
  )
  return(grid_df)
}

choice_agg_forHM_phy <- make_grid(choice_agg_phy$angle_z,
                              choice_agg_phy$radius_z,
                              choice_agg_phy$prop)

# learning heatmap - aggregate-physical
learn_hm_phy <- ggplot(choice_agg_forHM_phy, aes(angle,radius,fill=prop))+
  geom_tile()+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_devon(name="'in category'\nchoice proportion")+
  coord_fixed(xlim=c(-3,3),ylim=c(-3,3))+
  labs(x="angle z",y="radius z",
       title=glue("learning trial choice proportion\nphysical dimension values\nN={n_subs}"))+
       #caption="Dashed lines indicate angle and radius means, respectively.")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        plot.caption=element_text(hjust=0,size=12),
        axis.text = element_text(size=14),
        axis.title= element_text(size=14))
learn_hm_phy

# learning LL plot - aggregate
learn_ll_plot <- choice_agg_phy %>%
  ggplot(aes(ll_in,prop))+
  geom_point(size=6,alpha=.5,col="dodgerblue1")+
  geom_hline(yintercept=.5,alpha=.5,linetype="dashed")+
  geom_vline(xintercept=0,alpha=.5,linetype="dashed")+
  labs(x="LL in category",
       y="'In Category' Choice Proportion",
       title=glue("Learning trial choice proportion by stimulus log likelihood",
                  "\nN={n_subs}"))+
  scale_x_continuous(limits=c(-25,5),breaks=seq(-25,5,5))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=16),
        axis.text = element_text(size=14),
        axis.title= element_text(size=14))
learn_ll_plot

# Stats - OLD ====================================================================================
# learn_logit <- stan_glm(
#   choice~angle_z_abs+radius_z_abs,
#   data=learn,
#   family=binomial(link="logit"),
#   cores=parallel::detectCores()-1
#   )
# summary(learn_logit)
# posterior_interval(learn_logit,prob=.95)
# 
# mod_pred <- function(a,r=0,betas) plogis(betas[1] + betas[2]*a + betas[3]*r)
# 
# pal <- pal_simpsons("springfield")(4)
# 
# logit_ppc_angle <- ggplot(learn, aes(angle_z_abs,as.integer(levels(choice))[choice]))+
#   scale_y_continuous(breaks=c(0,.5,1))+
#   geom_point(position = position_jitter(height = 0.05, width = 0.1), 
#              size = 2, shape = 21, stroke = 0.2)+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             r=0), 
#                   size = 2, color = pal[1])+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             r=1),
#                 size = 2, color = pal[2])+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             r=2),
#                 size = 2, color = pal[3])+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             r=3),
#                 size = 2, color = pal[4])+
#   ggthemes::theme_few()+
#   labs(x="abs angle z score",
#        y="choice",
#        title="radius held constant",
#        caption="yellow,blue,gray,and brown curves have \nabs. radius z of 0,1,2,& 3 respectively")+
#   theme(plot.title=element_text(hjust=0.5,size=16),
#         plot.subtitle=element_text(hjust=0.5,size=15),
#         plot.caption=element_text(hjust=0,size=10),
#         axis.text = element_text(size=14),
#         axis.title= element_text(size=14))
# 
# logit_ppc_radius <- ggplot(learn, aes(radius_z_abs,as.integer(levels(choice))[choice]))+
#   scale_y_continuous(breaks=c(0,.5,1))+
#   geom_point(position = position_jitter(height = 0.05, width = 0.1), 
#              size = 2, shape = 21, stroke = 0.2)+  
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             a=0), 
#                 size = 2, color = pal[1])+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             a=1),
#                 size = 2, color = pal[2])+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             a=2),
#                 size = 2, color = pal[3])+
#   stat_function(fun = mod_pred, args = list(betas=coef(learn_logit),
#                                             a=3),
#                 size = 2, color = pal[4])+
#   ggthemes::theme_few()+
#   labs(x="abs radius z score",
#        y="choice",
#        title="angle held constant",
#        caption="yellow,blue,gray,and brown curves have \nabs. angle z of 0,1,2,& 3 respectively")+
#   theme(plot.title=element_text(hjust=0.5,size=16),
#         plot.subtitle=element_text(hjust=0.5,size=15),
#         plot.caption=element_text(hjust=0,size=10),
#         axis.text = element_text(size=14),
#         axis.title= element_text(size=14))
# 
# logit_ppc_all <- (logit_ppc_angle|logit_ppc_radius)+
#   plot_annotation(title="logistic regression posterior predictive check",
#                   theme=theme(plot.title=element_text(hjust=0.5,size=18)))
# 
# post_slope_plot <- mcmc_areas(learn_logit, prob=.95)+
#   scale_x_continuous(breaks=seq(-1,3,by=.5))+
#   labs(title="learning trial bayesian logistic regression",
#        subtitle="95% credible intervals")+
#   ggthemes::theme_few()+
#   theme(plot.title=element_text(hjust=0.5,size=16),
#         plot.subtitle=element_text(hjust=0.5,size=15),
#         plot.caption=element_text(hjust=0,size=14),
#         axis.text = element_text(size=14),
#         axis.title= element_text(size=14))
# 
# traceplots <- plot(learn_logit, "trace")

# Save plots ====================================================================================
if(save_plots){
  ggsave(learn_prop_corr_plot,file=here(,"turtles_exp","plots","turtle_exp_learn_prop_corr_plot.pdf"),
         width=8,height=8)
  ggsave(learn_prop_corr_by_dist_plot,file=here(,"turtles_exp","plots","turtle_exp_learn_prop_corr_by_dist_plot.pdf"),
         width=8,height=8)
  ggsave(learn_hm_phy,file=here(,"turtles_exp","plots","turtle_exp_learn_HM_phy.pdf"),
         width=8,height=8)
  ggsave(learn_ll_plot,file=here(,"turtles_exp","plots","turtle_exp_learn_ll_choice_plot.pdf"),
         width=8,height=8)
}

