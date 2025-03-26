# two-dimensional, probabilistic category structure for turtle exp
# Last modified Mar. 2023
# Sean Conway

# setup ================================================================================================================

# clear environment
rm(list=ls())

# libraries
library(dplyr)
library(ggplot2)
library(here)
library(purrr)
library(glue)
library(extrafont)
library(ggrepel)
library(readr)
library(tidyr)

# control parameters
save_plots <- T
save_samples <- F
n_draw <- 50000 # number to draw to plot mv distribution
n_learn <- 100 # number to sample per category for learning trials

# Category Structure ===========================================================================
# category parameters 
am <- 25 # angle mean
rm <- 75 # radius mean

# out of category parameters
# just limits on a bivariate uniform
alim <- c(10, 40) # limits for angle out distribution
rlim <- c(30, 120) # limits for radius out distribution

# variance parameters
prop_sd <- .1
rsd <- prop_sd*(diff(rlim))
asd <- prop_sd*(diff(alim))
rv <- rsd^2
av <- asd^2
cv <- 0 # covariance of 0
cv_mat <- matrix(c(av, cv,cv, rv),2,2) # covariance matrix

# Sampling from the category ===========================================================================
sample_category <- function(n_draw, mu, cv_mat){
  x <- MASS::mvrnorm(n=n_draw, mu=mu,Sigma=cv_mat)
  tibble(
    angle=x[,1],
    radius=x[,2],
    distribution="in"
  )
}

sample_noncategory <- function(n_draw,alim,rlim){
  tibble(
    angle=runif(n_draw, alim[1], alim[2]),
    radius=runif(n_draw, rlim[1], rlim[2]),
    distribution="out"
  )
}

# large sample category distribution for plotting
category <- sample_category(n_draw, mu=c(am,rm), cv_mat)


# Learning trials =================================================================================
# "in" category learning trials
# draw_circle <- function(n, am,rm,r){
#   #r <- radius#*sqrt(runif(n))
#   theta <- seq(0,1,length.out=n+1)*2*pi
#   angle <- am+r*cos(theta)
#   radius <- rm+r*sin(theta)
#   dat <- tibble(angle,radius) %>%
#     slice(-(n+1))
#   return(dat)
# }

rotate_ellipse <- function(n,am,rm,asd,rsd){
  if(n==0) return(tibble())
  print(n)
  theta <- seq(0,360,length.out=(n+1))
  angle <- am+asd*cos(theta*(pi/180))[1:n]
  radius <- rm+rsd*sin(theta*(pi/180))[1:n]
  return(tibble(angle,radius))
}

ra <- c(0, asd, 2*asd, 3*asd)
rr <- c(0, rsd, 2*rsd, 3*rsd)
probs <- dnorm(am-ra, mean=am, sd=asd)/sum(dnorm(am-ra, mean=am, sd=asd))
counts <- round((n_learn)*probs)
counts[4] <- 0

learn_in <- map_dfr(1:length(ra), ~rotate_ellipse(counts[.x],am,rm,ra[.x],rr[.x])) %>%
    mutate(distribution="in")

# # "out" category learning trials
learn_out_a <- seq(alim[1],alim[2],length.out=10)
learn_out_r <- seq(rlim[1],rlim[2],length.out=10)
learn_out <- crossing(learn_out_a, learn_out_r) %>%
  mutate(distribution="out")
colnames(learn_out) <- c("angle","radius","distribution")

# all learning
learn <- bind_rows(learn_in,
                   learn_out) %>%
  mutate(distribution=factor(distribution, levels=c("in","out"),ordered=T))

learn_test <- learn %>%
  mutate(category=case_when(
    distribution=="in"~1,
    distribution=="out"~0
  ),
  ang_diff=abs(angle-am)/asd,
  rad_diff=abs(radius-rm)/rsd) 

learn_logit <- glm(category~ang_diff+rad_diff, data=learn_test, family=binomial)
summary(learn_logit)


# Demo stim =================================================================================
demo_in_r <- c(rm-rsd,rm+rsd,rm)
demo_in_a <- c(am-asd,am+asd,am)
demo_in <- crossing(demo_in_a, demo_in_r) %>%
  mutate(type="in")
colnames(demo_in) <- c("angle","radius","type")

# # "out" category learning trials
demo_out_r <- c(rm-3*rsd, rm+3*rsd, rm)
demo_out_a <- c(am-3*asd, am+3*asd, am)
demo_out <- crossing(demo_out_a, demo_out_r) %>%
  mutate(type="out") 
colnames(demo_out) <- c("angle","radius","type")
demo_out <- demo_out %>%
  filter(angle!=am | radius!=rm)

demo <- bind_rows(demo_in, demo_out)

# attraction effect ==========================================================================

# attraction effect tc correct
attraction_eq_180_1 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am+asd,am+1.75*asd,am-asd), 
  radius=c(rm,rm,rm),
  pair=1
)

attraction_eq_180_2 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am-asd,am-1.75*asd,am+asd), 
  radius=c(rm,rm,rm),
  pair=2
)

attraction_eq_180_3 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am,am,am), 
  radius=c(rm+rsd,rm+1.75*rsd,rm-rsd),
  pair=3
)

attraction_eq_180_4 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am,am,am), 
  radius=c(rm-rsd,rm-1.75*rsd,rm+rsd),
  pair=4
)

attraction_eq_180_tri <- bind_rows(
  attraction_eq_180_1,
  attraction_eq_180_2,
  attraction_eq_180_3,
  attraction_eq_180_4
) %>%
  mutate(choice_set="trinary",
         abc_correct="ac",
         tc_correct="both",
         tc_angle=180)

attraction_eq_180_bi <- attraction_eq_180_tri %>%
  filter(stimulus!="d") %>%
  mutate(choice_set="binary",
         abc_correct="ab")

# attraction effect tc correct
attraction_eq_90_1 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am+asd,am+1.75*asd,am), 
  radius=c(rm,rm,rm-rsd),
  pair=1
)

attraction_eq_90_2 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am-asd,am-1.75*asd,am), 
  radius=c(rm,rm,rm+rsd),
  pair=2
)

attraction_eq_90_3 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am,am,am+asd), 
  radius=c(rm-rsd,rm-1.75*rsd,rm),
  pair=3
)

attraction_eq_90_4 <- tibble(
  stimulus=c("t","d","c"),
  angle=c(am,am,am-asd), 
  radius=c(rm+rsd,rm+1.75*rsd,rm),
  pair=4
)

attraction_eq_90_tri <- bind_rows(
  attraction_eq_90_1,
  attraction_eq_90_2,
  attraction_eq_90_3,
  attraction_eq_90_4
) %>%
  mutate(choice_set="trinary",
         abc_correct="ac",
         tc_correct="both",
         tc_angle=90)

attraction_eq_90_bi <- attraction_eq_90_tri %>%
  filter(stimulus!="d") %>%
  mutate(choice_set="binary",
         abc_correct="ab")

# all attraction effect trials
attraction_all_long <- bind_rows(attraction_eq_180_tri,
                            attraction_eq_180_bi,
                            # attraction_tar_corr_180_tri,
                            # attraction_tar_corr_180_bi,
                            # attraction_comp_corr_180_tri,
                            # attraction_comp_corr_180_bi,
                            attraction_eq_90_tri,
                            attraction_eq_90_bi) %>%
                            # attraction_tar_corr_90_tri,
                            # attraction_tar_corr_90_bi,
                            # attraction_comp_corr_90_tri,
                            # attraction_comp_corr_90_bi,) %>%
  mutate(trial_type="attraction") 

attraction_all <- attraction_all_long %>%
  rename(tdc=stimulus) %>%
  mutate(stimulus=case_when(
    choice_set=="trinary"~stringr::str_replace_all(tdc,
                                                   c("t"="a",
                                                     "d"="b",
                                                     "c"="c")),
    choice_set=="binary"~stringr::str_replace_all(tdc,
                                                  c("t"="a",
                                                    "c"="b"))
  )) %>%
  pivot_wider(names_from = stimulus,
              values_from = c(angle,radius,tdc)) %>%
  mutate(exp_trial_type="transfer") %>%
  relocate(exp_trial_type,
           trial_type,
           choice_set,
           angle_a,
           radius_a,
           angle_b,
           radius_b,
           angle_c,
           radius_c,
           abc_correct,
           tc_correct,
           tc_angle,
           tdc_a,
           tdc_b,
           tdc_c)
           
           

# filler trials ====================================================================================
# note - any n indicates per block
# number of filler easy trials (includes BOTH binary and trinary)
# that is, n filler easy trinary = n filler easy binary = n_filler_easy/2
n_filler_easy <- 16

# number of filler sample trials (includes BOTH binary and trinary)
# that is, n filler sample trinary = n filler sample binary = n_filler_sample/2
n_filler_sample <- 20 

# number of total fillers (to keep track of it)
n_fillers <- n_filler_easy+n_filler_sample

filler_easy_seed_tri <- 818
filler_easy_seed_bi <- 183

# function for sampling filler easy trials
sample_filler_easy <- function(n, am, asd, rm, rsd, bi=F, seed){
  set.seed(seed)
  if(bi){
    samp <- tibble(
      set=rep(1:n,2),
      trial_type="filler_easy",
      stimulus=c(rep("a",n),rep("b",n)),
      category=c(rep("in",n),rep("out1",n)),
      angle=c(am+rnorm(n), am+(sample(c(3,-3),n,T)*asd)+runif(n)),
      radius=c(rm+rnorm(n),rm+(sample(c(3,-3),n,T)*rsd)+runif(n)),
      choice_set="binary"
    ) %>%
      arrange(set) %>%
      mutate(across(c(angle,radius),round))
  } else{
    samp <- tibble(
      set=rep(1:n,3),
      trial_type="filler_easy",
      stimulus=c(rep("a",n),rep("b",n),rep("c",n)),
      category=c(rep("in",n),rep("out1",n),rep("out2",n)),
      angle=c(am+rnorm(n), am+(sample(c(3,-3),n*2,T)*asd)+rnorm(n*2)),
      radius=c(rm+rnorm(n),rm+(sample(c(3,-3),n*2,T)*rsd)+rnorm(n*2)),
      choice_set="trinary"
    ) %>%
    arrange(set) %>%
    mutate(across(c(angle,radius),round))
  }
  return(samp)
}

# filler easy trinary
filler_easy_tri <- sample_filler_easy(n_filler_easy/2, am, asd, rm, rsd, seed=filler_easy_seed_tri)

# filler easy binary
filler_easy_bi <- sample_filler_easy(n_filler_easy/2, am, asd, rm, rsd,T,seed=filler_easy_seed_bi)

# all filler easy
filler_easy_all_long <- bind_rows(
  filler_easy_bi, 
  filler_easy_tri
) %>%
  mutate(exp_trial_type="transfer",
         abc_correct="a") 

filler_easy_all <- filler_easy_all_long %>%
  pivot_wider(names_from=stimulus,
              values_from=c(angle,radius,category)) %>%
  select(-set)


# filler sample 
filler_sample_all <- tibble(
  exp_trial_type="transfer",
  trial_type="filler_sample",
  choice_set=c(rep("binary",n_filler_sample/2),rep("trinary",n_filler_sample/2)),
  category_a=rep("in",n_filler_sample),
  category_b=rep("out1",n_filler_sample),
  category_c=c(rep("out2",n_filler_sample/2),rep(NA_character_,n_filler_sample/2)),
  abc_correct="a"
)

# combine transfer =====================================================================================
transfer <- bind_rows(
  attraction_all, # repeat attraction effect trials 3x per block
  attraction_all,
  attraction_all,
  filler_easy_all,
  filler_sample_all
)
  
# plotting =====================================================================================
# bivariate density category plot
bv_dens_plot <- ggplot()+
  geom_density_2d_filled(data=category,aes(angle, radius),bins=5)+
  coord_fixed(xlim=alim,
              ylim=rlim)+
  labs(x="angle",y="radius",title="Category Distribution")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=24),
        strip.text.x=element_text(size=18),
        axis.text.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.text=element_text(size=14),
        legend.title=element_text(size=18),
        axis.title = element_text(size=18))

# add learning trials to density plot
learn_plot <- bv_dens_plot + 
  geom_point(data=learn,aes(angle,radius,col=distribution),
             size=2.5,inherit.aes=F,alpha=.8)+
  labs(title="learning trial stimuli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_manual(values=c("#E69F00","#56B4E9"))

# demo trial plot
demo_plot <- ggplot(demo, aes(angle,radius))+
  geom_point(aes(shape=type,col=type),size=4)+
  facet_grid(vars(type))+
  labs(title="demo stimuli")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=20))
#demo_plot

# function to plot context effect trials
plot_CE_trials <- function(bv_dens_plot, trials){
  trials <- trials %>%
    mutate(stimulus=stringr::str_replace_all(stimulus,
                                             c("target"="T",
                                               "competitor"="C",
                                               "decoy"="D")))
  pl <- bv_dens_plot + 
    geom_text(data=trials, aes(angle,radius,label=stimulus),size=6,alpha=.75,col="black")+
    facet_wrap(vars(pair))+
    theme(plot.title=element_blank(),
          legend.position="none")
  return(pl)
}

attraction_plot_eq_180 <- plot_CE_trials(bv_dens_plot,attraction_eq_180_tri) +
  labs(title="Attraction Effect Trials\nTC Equal\nTrinary\nTC 180 Degrees Apart")+
  theme(plot.title=element_text(hjust=0.5))
attraction_plot_eq_180_bi <- plot_CE_trials(bv_dens_plot,attraction_eq_180_bi) +
  labs(title="Attraction Effect Trials\nTC Equal\nBinary\nTC 180 Degrees Apart")+
  theme(plot.title=element_text(hjust=0.5))
attraction_plot_eq_90 <- plot_CE_trials(bv_dens_plot,attraction_eq_90_tri) +
  labs(title="Attraction Effect Trials\nTC Equal\nTrinary\nTC 90 Degrees Apart")+
  theme(plot.title=element_text(hjust=0.5))
attraction_plot_eq_90_bi <- plot_CE_trials(bv_dens_plot,attraction_eq_90_bi) +
  labs(title="Attraction Effect Trials\nTC Equal\nBinary\nTC 90 Degrees Apart")+
  theme(plot.title=element_text(hjust=0.5))



# saving output ===========================================================================
if(save_plots){
  ggsave(attraction_plot_eq_180, filename=here(,"turtles_exp","plots","turtle_exp_attraction_plot_eq_180_trinary.pdf"),
         width=8,height=8)
  ggsave(attraction_plot_eq_180_bi, filename=here(,"turtles_exp","plots","turtle_exp_attraction_plot_eq_180_binary.pdf"),
         width=8,height=8)
  ggsave(attraction_plot_eq_90, filename=here(,"turtles_exp","plots","turtle_exp_attraction_plot_eq_90_trinary.pdf"),
         width=8,height=8)
  ggsave(attraction_plot_eq_90_bi, filename=here(,"turtles_exp","plots","turtle_exp_attraction_plot_eq_90_binary.pdf"),
         width=8,height=8)
  ggsave(bv_dens_plot,filename=here(,"turtles_exp","plots","turtle_exp_bv_dens_plot.png"))
  ggsave(learn_plot,filename=here(,"turtles_exp","plots","turtle_exp_learn_plot.pdf"),
         width=8,height=8)
  ggsave(demo_plot,filename=here(,"turtles_exp","plots","turtle_exp_demo_plot.pdf"),
         width=8,height=8)
  save(bv_dens_plot,file=here(,"turtles_exp","plots","turtle_exp_bv_dens_plot.RData"))
}

if(save_samples){
  write_csv(learn,file = here(,"turtles_exp","trials","exp2_learn.csv"))
  jsonlite::write_json(learn, path=here(,"turtles_exp","trials","exp2_learn.json"))
  write_csv(category,file=here(,"turtles_exp","categories","exp2_category_sampled.csv"))
  write_csv(attraction_all, file=here(,"turtles_exp","trials","exp2_attraction.csv"))
  write_csv(attraction_all_long, file=here(,"turtles_exp","trials","exp2_attraction_long.csv"))
  write_csv(filler_easy_all, file=here(,"turtles_exp","trials","exp2_fillers.csv"))
  write_csv(transfer, file=here(,"turtles_exp","trials","exp2_transfer.csv"))
  jsonlite::write_json(transfer, path=here(,"turtles_exp","trials","exp2_transfer.json"))
  write_csv(demo, file=here(,"turtles_exp","trials","exp2_demo.csv"))
  jsonlite::write_json(demo, path=here(,"turtles_exp","trials","exp2_demo.json"))
} 

# Figures for math psych =================================================
attraction_sample <- transfer %>%
  filter( (pair==1 & choice_set=="trinary" & trial_type=="attraction") ) %>%
  slice(c(1,2)) %>%
  mutate(type=glue("attraction \n{tc_angle}")) %>%
  pivot_longer(cols=c(angle_a,
                      angle_b,
                      angle_c,
                      radius_a,
                      radius_b,
                      radius_c)) %>%
  separate(name,into=c("dimension","option")) %>%
  pivot_wider(names_from = dimension,
              values_from = value) %>%
  mutate(option=stringr::str_replace_all(option,
                                         c("a"="T",
                                           "b"="D",
                                           "c"="C")))


filler_easy_sample <- tibble(
  angle=c(26,am+asd*2.5,am+asd*-2.5),
  radius=c(74,rm+rsd*-2.5,rm+rsd*2.5),
  option=c("1","2","3"),
  type="easy"
)
filler_sample <- tibble(
    angle=c(23,18,28),
    radius=c(76,79,66),
    distribution=c("in","out","out")
  ) %>%
  mutate(type="sampled",
         option=c("1","2","3"))

all_sample_trials <- bind_rows(
  filler_easy_sample,
  attraction_sample,
  filler_sample
) %>%
  mutate(angle=(angle-am)/asd,
         radius=(radius-rm)/rsd)

category_normed <- sample_category(50000,c(0,0),matrix(c(1,0,0,1),nrow=2,byrow=T))

category_normed_plot <- ggplot(category_normed)+
  stat_density_2d_filled(aes(angle,radius,fill=..level..),bins=5)+
  coord_fixed(xlim=c(-3,3),ylim=c(-3,3))+
  scale_x_continuous(breaks=c(-3,0,3))+
  scale_y_continuous(breaks=c(-3,0,3))+
  labs(x="angle z score",
       y="radius z score",
       title="Category distribution")+
  ggthemes::theme_few()
  # theme(text=element_text(family="Tahoma"),
  #       axis.text = element_text(size=14),
  #       axis.title = element_text(size=14),
  #       legend.text=element_text(size=14),
  #       legend.title = element_text(size=14),
  #       plot.title=element_text(size=18,hjust=0.5))

math_psych_plot <- category_normed_plot+
  geom_text(data=all_sample_trials,
            aes(angle,radius,label=option),
            inherit.aes=F,size=6,col="red") +
  facet_wrap(vars(type),
             labeller = labeller(groupwrap = label_wrap_gen(width=20)))+
  labs(title="Transfer trials")
save(category_normed_plot, file=here(,"turtles_exp","plots","category_normed_plot.RData"))

ggsave(filename = here(,"turtles_exp","math_psych_transfPlot.png"),
       plot=math_psych_plot,
       width=8,height=8)
ggsave(filename = here(,"turtles_exp","math_psych_distPlot.png"),
       plot=category_normed_plot,
       width=8,height=8)



