# category_structure.R
# design for 1 dimension, 1 category, context effect/categorization experiment
# follow up experiment 
# Sean Conway
# Last modified Mar. 2023

# clear environment
rm(list=ls())

# Libraries
library(tidyverse)
library(here)

# Source density functions 
source(here("line_exp_2","functions","density_functions_2.R"))
# source plotting functions
source(here("line_exp_2","functions","plotting_functions.R"))

# Constants
min_len <- 150
max_len <- 450

# controls
save_plots <- T

# Psychological mapping  ============================================================
# Psychological to physical mapping
# n = number of jnd
psy2phy <- function(n) {
  trans <- ifelse(n==0, 0, 150*1.05^n)
  return(trans)
}

# Defining the categories  ============================================================

# Large sequence of jnds
jnd_min <- 0
jnd_max <- 22.5
jnds_1 <- seq(0,jnd_max, .001)

# Category properties
out_mean_jnds <- jnd_max/2
out_sd_jnds <- jnd_max/10

# normalizing constant
out_nc <- pnorm(jnd_max, out_mean_jnds, out_sd_jnds)-pnorm(jnd_min, out_mean_jnds, out_sd_jnds)

in_category <- tibble(
  distribution="in",
  jnds=jnds_1,
  dens=dunif(jnds_1, jnd_min, jnd_max)
)

out_category <- tibble(
  distribution="out",
  jnds=jnds_1,
  dens=dnorm(jnds_1, out_mean_jnds, out_sd_jnds)/out_nc
)

# Combine category/non-category distributions
all_category <- bind_rows(in_category, out_category)

# Learning Trial Stimuli ============================================================

n_per_category <- 100

# set seeds
# Same as first experiment but flipped
# Do not change 
in_seed <- 432
out_seed <- 850

# write learning seeds to file
write_lines(paste0("Seeds for randomly sampling learning stimuli\nin_seed=",
                   in_seed, "\nout_seed=",out_seed),
            here("line_exp_2","notes","seeds_for_learning_stim_exp1b.txt"))

# Learning trial stimuli - out of the category
bins_out <- seq(jnd_min, jnd_max, length.out=11)
probs_out <- pnorm(bins_out[2:length(bins_out)], out_mean_jnds, out_sd_jnds)-
  pnorm(bins_out[1:length(bins_out)-1],out_mean_jnds, out_sd_jnds)

# Normalize
probs_out <- probs_out/sum(probs_out)
n_per_bin_out <- n_per_category*probs_out
n_per_bin_out <- map_dbl(n_per_bin_out, .f=~ifelse(.x>1, round(.x), floor(.x))) 

# Sampling learning stimuli - out of category
learn_out <- c()
set.seed(out_seed)
for(i in 2:length(bins_out)){
  learn_out <- c(learn_out,sample(seq(bins_out[i-1], bins_out[i],by=.01),size=n_per_bin_out[i-1],replace = T))
}

# Learning trial stimuli - in the category
bins_in <- seq(jnd_min, jnd_max, length.out=11)
probs_in <- rep(.1, 10)
n_per_bin_in <- n_per_category*probs_in

# Sampling learning stimuli - out of the category
learn_in <- c()
set.seed(in_seed)
for(i in 2:length(bins_in)){
  learn_in <- c(learn_in, sample(seq(bins_in[i-1], bins_in[i],by=.01),size=n_per_bin_in[i-1],replace = T))
}

# Combine all learning stimuli
learn <- tibble(
  `in`= learn_in,
  `out`=learn_out
) %>%
  pivot_longer(
    cols=c(`in`, `out`),names_to='distribution',
    values_to = 'jnds'
  ) %>%
  mutate(
    distribution=as_factor(distribution),
    dens=case_when(
      distribution=='out'~dnorm(jnds, out_mean_jnds, out_sd_jnds)/out_nc,
      distribution=='in'~dunif(jnds, jnd_min, jnd_max)
    )
  )

# Learning Demo Stimuli  ============================================================
# Stim to show to try and aid learning

# Number of demo stimuli
n_demo <- 40

# Sampling learn-out stimuli for demo
# Basically sampling from a truncated normal
learn_out_demo <- rep(-99, n_demo)
for(i in 1:n_demo){
  while(learn_out_demo[i] < jnd_min | learn_out_demo[i] > jnd_max){
    learn_out_demo[i] <- rnorm(1, out_mean_jnds, out_sd_jnds)
  }
}

# Sampling learn-in stimuli for demo
# Sample from uniform range outside 3 SDs of mean
learn_in_demo <- sample(c(seq(jnd_min,out_mean_jnds-2*out_sd_jnds,length.out=100),
                          seq(out_mean_jnds+2*out_sd_jnds,jnd_max,length.out=100)),
                        size=n_demo, replace=T)

learn_demo <- tibble(
  in_category=learn_in_demo,
  out_category=learn_out_demo
) %>%
  pivot_longer(
    cols=everything(),
    names_to='category',
    values_to = 'jnds'
  ) %>%
  mutate(
    dens=case_when(
      category=='in_category'~dunif(jnds, jnd_min, jnd_max),
      category=='out_category'~dnorm(jnds, out_mean_jnds, out_sd_jnds)/out_nc
    )
  )

# make into two separate vectors
learn_demo_in_cat <- learn_demo %>%
  filter(category=='in_category') %>%
  select(jnds) %>%
  as_vector() %>%
  unname()

learn_demo_out_cat <- learn_demo %>%
  filter(category=='out_category') %>%
  select(jnds) %>%
  as_vector() %>%
  unname()

# Compute optimal probabilities ============================================================
optimal <- tibble(
  jnds=jnds_1,
  prob_in=prob_in(jnds),
  prob_out=prob_out(jnds)
)

# Transfer Trial Stimuli  ============================================================

# Attraction effect stimuli parameters
td_dist <- 1 # JNDs between target and decoy

# jnd distance to mean for equally correct options
jnds_equal_1 <- 4.5
jnds_equal_2 <- 5.5
jnds_correct_hard <- 5.5
jnds_incorrect_hard <- 4.5
jnds_correct_easy <- 6.5
jnds_incorrect_easy <- 4.5

# Original (from first experiment)
# jnds_equal_1 <- 1
# jnds_equal_2 <- 2
# jnds_correct_hard <- 1
# jnds_incorrect_hard <- 2
# jnds_correct_easy <- 1
# jnds_incorrect_easy <- 3

# Pair where either is equally correct (i.e., equidistant to min/max)
pair_equal_1 <- tibble(
  side_correct="equal",
  stim_1_psy=out_mean_jnds+jnds_equal_1,
  stim_2_psy=out_mean_jnds-jnds_equal_1,
  difficulty="equal_chance_4.5_JND"
)

pair_equal_2 <- tibble(
  side_correct="equal",
  stim_1_psy=out_mean_jnds+jnds_equal_2,
  stim_2_psy=out_mean_jnds-jnds_equal_2,
  difficulty="equal_chance_5.5_JND"
)

# Pair where left is correct, but it's easy
pair_left_correct_1 <- tibble(
  side_correct="left",
  difficulty="easy",
  stim_1_psy=out_mean_jnds-jnds_correct_easy,
  stim_2_psy=out_mean_jnds+jnds_incorrect_easy
)

# Pair where left is correct, and it's difficult
pair_left_correct_2 <- tibble(
  side_correct="left",
  difficulty="difficult",
  stim_1_psy=out_mean_jnds-jnds_correct_hard,
  stim_2_psy=out_mean_jnds+jnds_incorrect_hard
)

# Pair where right is correct, but it's difficult
pair_right_correct_1 <- tibble(
  side_correct="right",
  difficulty="difficult",
  stim_1_psy=out_mean_jnds+jnds_correct_hard,
  stim_2_psy=out_mean_jnds-jnds_incorrect_hard
)

# Pair where right is correct, and it's easy
pair_right_correct_2 <- tibble(
  side_correct="right",
  difficulty="easy",
  stim_1_psy=out_mean_jnds+jnds_correct_easy,
  stim_2_psy=out_mean_jnds-jnds_incorrect_easy
)

# Bind all together
attr_stim_init <- bind_rows(
  pair_equal_1,
  pair_equal_2,
  pair_left_correct_1,
  pair_left_correct_2,
  pair_right_correct_1,
  pair_right_correct_2
) 

# double them so each trial type can have target and competitor be both options
attr_stim <- attr_stim_init %>%
  bind_rows(attr_stim_init) %>%
  arrange(side_correct,stim_1_psy, stim_2_psy, difficulty)

attr_stim$trial_type <- rep(c("Target","Competitor"),nrow(attr_stim)/2)
attr_stim <- attr_stim %>%
  mutate(
    target=case_when(
      trial_type=="Target"~stim_1_psy,
      trial_type=="Competitor"~stim_2_psy
    ),
    competitor=case_when(
      trial_type=="Target"~stim_2_psy,
      trial_type=="Competitor"~stim_1_psy
    )
  ) %>%
  select(-trial_type)


# Create decoy by subtracting or adding from the target
attr_stim <- attr_stim %>%
  mutate(
    decoy=case_when(
      target < out_mean_jnds ~ target+td_dist,
      target > out_mean_jnds ~ target-td_dist
    ),
    stim_3_psy=decoy,
    trial_choice_set="trinary",
    transfer_trial_type="attraction"
  )

# Binary trials (no decoy)
attr_stim_binary <- attr_stim %>%
  mutate(
    trial_choice_set="binary",
    decoy=NA
  ) %>%
  mutate(
    stim_1_psy=case_when(
      stim_2_psy==target & stim_3_psy==competitor~0,
      stim_3_psy==target & stim_2_psy==competitor~0,
      TRUE~stim_1_psy
    ),
    stim_2_psy=case_when(
      stim_1_psy==target & stim_3_psy==competitor~0,
      stim_3_psy==target & stim_1_psy==competitor~0,
      TRUE~stim_2_psy
    ),
    stim_3_psy=case_when(
      stim_1_psy==target & stim_2_psy==competitor~0,
      stim_2_psy==target & stim_1_psy==competitor~0,
      TRUE~stim_3_psy
    )
  )

# Bind trinary to binary
attr_stim <- full_join(attr_stim,attr_stim_binary) %>%
  relocate(trial_choice_set, 
           side_correct, 
           difficulty, 
           stim_1_psy, 
           stim_2_psy, 
           stim_3_psy,
           target, 
           competitor, 
           decoy)

# function to get correct choice for attraction effect trials
# Probably could have done this above, but it works fine
corr_choice <- function(...){
  cat_mean <- 11.25 # defining manually here
  
  # Get input into vector
  vec <- c(...)
  
  # Compute distances
  dists <- abs(vec-cat_mean)
  
  #If there's more than one right answer, correct is NA
  correct <- ifelse(length(dists[dists==max(dists)])>1, NA, vec[which.max(dists)])
  
  return(correct)
}

# Find out correct option based on which is closest to cat mean
# Assign tc_correct (whether target or competitor is correct)
attr_stim_w_corr <- attr_stim %>%
  rowwise() %>%
  mutate(
    correct_1_psy=case_when(
      trial_choice_set=="trinary" & side_correct=="left"~min(c(stim_1_psy,stim_2_psy,stim_3_psy)),
      trial_choice_set=="binary" & side_correct=="left"~min(c(stim_1_psy,stim_2_psy)),
      trial_choice_set=="trinary" & side_correct=="right"~max(c(stim_1_psy,stim_2_psy,stim_3_psy)),
      trial_choice_set=="binary" & side_correct=="right"~max(c(stim_1_psy,stim_2_psy)),
      side_correct=="equal" ~ target
    ),
    correct_2_psy=case_when(
      trial_choice_set=="trinary" & side_correct=="left"~min(c(stim_1_psy,stim_2_psy,stim_3_psy)),
      trial_choice_set=="binary" & side_correct=="left"~min(c(stim_1_psy,stim_2_psy)),
      trial_choice_set=="trinary" & side_correct=="right"~max(c(stim_1_psy,stim_2_psy,stim_3_psy)),
      trial_choice_set=="binary" & side_correct=="right"~max(c(stim_1_psy,stim_2_psy)),
      side_correct=="equal" ~ competitor
    ),
    tc_correct=case_when(
      side_correct=="equal"~"both",
      (side_correct=="left" | side_correct=="right") & correct_1_psy==target ~"target",
      (side_correct=="left" | side_correct=="right") & correct_2_psy==competitor ~"competitor"
    ),
    target_side=case_when(
      target<out_mean_jnds~"left",
      target>out_mean_jnds~"right",
    ),
    competitor_side=case_when(
      competitor<out_mean_jnds~"left",
      competitor>out_mean_jnds~"right"
    )
  )

# Making attraction effect stimuli long to make plotting easier
attr_stim_long <- attr_stim_w_corr %>%
  pivot_longer(cols=c(target,competitor, decoy),names_to = "option",
               values_to = "jnds") %>%
  mutate(
    dens=dunif(jnds, jnd_min, jnd_max),
    option_abbrev=case_when(
      option=="target"~"T",
      option=="competitor"~"C",
      option=="decoy"~"D"
    )
  )

# Repeating attraction effect trials 3 times in a block!
attr_stim_all <- bind_rows(attr_stim_w_corr, 
                       attr_stim_w_corr,
                       attr_stim_w_corr) %>%
  mutate(transfer_trial_type="attraction")

# Filler trial stimuli ============================================================
n_easy_filler_per_side <- 3
n_sample_filler_per_side <- 6

fillers_easy_left_correct_tri <- tibble(
  stim_1_psy = jnd_min+runif(n_easy_filler_per_side,0,2),
  stim_2_psy = out_mean_jnds+runif(n_easy_filler_per_side,-2,2),
  stim_3_psy = out_mean_jnds+runif(n_easy_filler_per_side,-2,2),
  trial_choice_set = "trinary",
  side_correct = "left",
  transfer_trial_type =  "filler_easy"
)

fillers_easy_right_correct_tri <- tibble(
  stim_1_psy = jnd_max+runif(n_easy_filler_per_side,-2,0),
  stim_2_psy = out_mean_jnds+runif(n_easy_filler_per_side,-2,2),
  stim_3_psy = out_mean_jnds+runif(n_easy_filler_per_side,-2,2),
  trial_choice_set = "trinary",
  side_correct = "right",
  transfer_trial_type =  "filler_easy"
)

fillers_easy_left_correct_bi <- tibble(
  stim_1_psy = jnd_min+runif(n_easy_filler_per_side,0,2),
  stim_2_psy = out_mean_jnds+runif(n_easy_filler_per_side,-2,2),
  stim_3_psy = rep(0,n_easy_filler_per_side),
  trial_choice_set = "binary",
  side_correct = "left",
  transfer_trial_type =  "filler_easy"
)

fillers_easy_right_correct_bi <- tibble(
  stim_1_psy = jnd_max+runif(n_easy_filler_per_side,-2,0),
  stim_2_psy = out_mean_jnds+runif(n_easy_filler_per_side,-2,2),
  stim_3_psy = rep(0,n_easy_filler_per_side),
  trial_choice_set="binary",
  side_correct="right",
  transfer_trial_type =  "filler_easy"
)

fillers_sample_left_correct_tri <- tibble(
  stim_1_psy = rep(-1,n_sample_filler_per_side),
  stim_2_psy = rep(1,n_sample_filler_per_side),
  stim_3_psy = rep(1,n_sample_filler_per_side),
  trial_choice_set = "trinary",
  side_correct = "left",
  transfer_trial_type =  "filler_sample"
)

fillers_sample_right_correct_tri <- tibble(
  stim_1_psy = rep(-1,n_sample_filler_per_side),
  stim_2_psy = rep(1,n_sample_filler_per_side),
  stim_3_psy = rep(1,n_sample_filler_per_side),
  trial_choice_set = "trinary",
  side_correct = "right",
  transfer_trial_type =  "filler_sample"
)

fillers_sample_left_correct_bi <- tibble(
  stim_1_psy = rep(-1,n_sample_filler_per_side),
  stim_2_psy = rep(1,n_sample_filler_per_side),
  stim_3_psy = rep(0,n_sample_filler_per_side),
  trial_choice_set = "binary",
  side_correct = "left",
  transfer_trial_type =  "filler_sample"
)

fillers_sample_right_correct_bi <- tibble(
  stim_1_psy = rep(-1,n_sample_filler_per_side),
  stim_2_psy = rep(1,n_sample_filler_per_side),
  stim_3_psy = rep(0,n_sample_filler_per_side),
  trial_choice_set = "binary",
  side_correct = "right",
  transfer_trial_type =  "filler_sample"
)

fillers <- bind_rows(
  fillers_easy_left_correct_tri,
  fillers_easy_left_correct_bi,
  fillers_easy_right_correct_tri,
  fillers_easy_right_correct_bi,
  fillers_sample_left_correct_tri,
  fillers_sample_left_correct_bi,
  fillers_sample_right_correct_tri,
  fillers_sample_right_correct_bi
) %>%
  rowwise() %>%
  mutate(
    correct_1_psy =case_when(
      transfer_trial_type=="filler_easy" & trial_choice_set=="trinary"~ corr_choice(stim_1_psy, stim_2_psy, stim_3_psy),
      transfer_trial_type=="filler_easy" & trial_choice_set=="binary"~ corr_choice(stim_1_psy, stim_2_psy)
    ),
    correct_2_psy=correct_1_psy
  ) %>%
  ungroup()
fillers

# All transfer trial stimuli ============================================================
transfer <- bind_rows(attr_stim_all,
                      fillers)

# psy2phy ============================================================
learn$line_len <- psy2phy(learn$jnds)
attr_stim_all$stim_1_phy <- psy2phy(attr_stim_all$stim_1_psy)
attr_stim_all$stim_2_phy <- psy2phy(attr_stim_all$stim_2_psy)
attr_stim_all$stim_3_phy <- psy2phy(attr_stim_all$stim_3_psy)
transfer_w_phy <- transfer %>%
  mutate(
    stim_1_phy=case_when(
      transfer_trial_type!="filler_sample" ~ psy2phy(stim_1_psy)
    ),
    stim_2_phy=case_when(
      transfer_trial_type!="filler_sample" ~ psy2phy(stim_2_psy)
    ),
    stim_3_phy=case_when(
      transfer_trial_type!="filler_sample" ~ psy2phy(stim_3_psy)
    ),
    correct_1_phy=psy2phy(correct_1_psy),
    correct_2_phy=psy2phy(correct_2_psy),
    target_phy=psy2phy(target),
    competitor_phy=psy2phy(competitor),
    decoy_phy=psy2phy(decoy)
  )

# Plots  ============================================================

# plot category distributions
cat_dists_plot <- all_category %>%
  ggplot(aes(jnds, dens, col=distribution))+
  geom_line()+
  scale_color_manual(labels=c('In Category','Out of Category'),
                    values=c('dark green','red'),
                    name='Category')+
  labs(x="Line Length",
       y="Probability Density",
       title="Category Distributions")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5))
cat_dists_plot

# Plot learning trials
learn_plot <- learn %>%
  ggplot(aes(jnds, dens, color=distribution))+
  geom_point(alpha=0.6,size=3,shape='X')+
  geom_line(data=filter(out_category, jnds>=jnd_min & jnds<=jnd_max),
            aes(jnds, dens), color='red',alpha=0.2)+
  geom_line(data=filter(in_category, jnds>=jnd_min & jnds<=jnd_max ),
            aes(jnds, dens),color='dark green',alpha=0.2)+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=c('dark green','red'),
                     name='Category')+
  labs(
    x='Line length',
    y='Probability Density',
    title='Learning Trial Stimuli'
  )+
  scale_x_continuous(limits=c(jnd_min, jnd_max))+
  ggthemes::theme_few()+
  theme(plot.title = (element_text(hjust=0.5)))
learn_plot

# Histogram of learning trial stimuli
learn_hist <- learn %>%
  ggplot(aes(jnds,fill=distribution))+
  geom_histogram(alpha=0.6,size=3,binwidth = 2)+
  scale_fill_manual(labels=c('In Category','Out of Category'),
                    values=c('dark green','red'),
                    name='Category')+
  facet_grid(vars(distribution))+
  labs(
    x='JNDS from minimum stimulus',
    y='Frequency',
    title='Learning Trial Stimuli'
  )+
  scale_x_continuous(breaks=seq(jnd_min, jnd_max, 4))+
  ggthemes::theme_few()+
  theme(plot.title = (element_text(hjust=0.5)))
learn_hist

# Plot stimuli for demonstration prior to learning trials
learn_demo_plot <- learn_demo %>%
  ggplot() +
  geom_point(aes(x=jnds,y=dens,color=category),alpha=0.25)+
  geom_line(data=filter(in_category, jnds >= jnd_min & jnds <= jnd_max),
            aes(x=jnds, y=dens),color='dark green',alpha=0.3)+
  geom_line(data=filter(out_category, jnds >= jnd_min & jnds <= jnd_max),
            aes(x=jnds, y=dens),color='red', alpha=0.3)+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=c('dark green','red'),
                     name='Category')+
  labs(
    x='JNDS from minimum stimulus',
    y='Density',
    title='Learning Demo Stimuli'
  )+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5))
learn_demo_plot

lcr_plot <- ggplot()+
  geom_line(data=optimal, aes(jnds, prob_in), col="darkgreen")+
  labs(x="Line length",
       y="Probability in category")+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),text = element_text(size=20))
lcr_plot

# recode attr stim for paper figure
attr_stim_long_relabeled <- attr_stim_long %>%
  mutate(side_correct=recode(side_correct, equal="equal", left="short", right="long"),
         target_side=recode(target_side,left="short", right="long"),
         difficulty=recode(difficulty, equal_chance_5.5_JND="equal - 5.5 JFM", equal_chance_4.5_JND="equal - 4.5 JFM", difficult="difficult",easy="easy")) %>%
  rename(`which correct`=side_correct,
         `target side`=target_side)

trinary_ae_plot <- ggplot(filter(attr_stim_long, trial_choice_set=="trinary"))+
  geom_text(aes(jnds,prob_in(jnds),label=option_abbrev,col=option_abbrev),size=5.5)+
  facet_wrap(side_correct~target_side+difficulty,labeller = function(labs)
  {label_both(labs)})+
  geom_line(data=optimal, aes(jnds, prob_in), col="darkgreen")+
  geom_vline(xintercept = out_mean_jnds, linetype="dashed")+
  scale_y_continuous(limits=c(0,1.2))+
  labs(x='JNDs from minimum stimulus',y='Density',title='Trinary Attraction Effect Trials')+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5,size = 25)); trinary_ae_plot

trinary_ae_plot_ll <- plot_optimal_1b(ll=T)+
  geom_text(data=filter(attr_stim_long_relabeled,trial_choice_set=="trinary"),
            aes(jnds, log(dens_in(jnds)/dens_out(jnds)),label=option_abbrev,col=option_abbrev),size=4)+
  lemon::facet_rep_wrap(`which correct`~`target side`+difficulty,labeller = function(labs)
    {label_both(labs)},repeat.tick.labels = T)+
  labs(x='JNDs from minimum stimulus',y='LL in category')+
  ggthemes::theme_few()+
  ggsci::scale_color_jama(name="option")+
  theme(plot.title = element_text(hjust=0.5,size = 25))
trinary_ae_plot_ll 

plot_optimal_1b(ll=T)+
  geom_text(data=slice(attr_stim_long,4:6),aes(jnds, log(dens_in(jnds)/dens_out(jnds)),label=option_abbrev,col=option_abbrev),size=5)+
  # lemon::facet_rep_wrap(side_correct~target_side+difficulty,labeller = function(labs)
  # {label_both(labs)},repeat.tick.labels = T)+
  labs(x='Line Length',y='LL in category')+#+,title='Trinary Attraction Effect Trials')+
  ggthemes::theme_few()+
  theme(text=element_text(size=18))
ggsave(here("psychonomics_figures","plots","example_attraction.pdf"))
plot_optimal_1b(ll=T)+
  labs(x='Line Length',y='LL in category')+#+,title='Trinary Attraction Effect Trials')+
  ggthemes::theme_few()+
  coord_fixed()+
  theme(text=element_text(size=18),
        axis.text=element_blank(),
        axis.ticks=element_blank())
ggsave(here("line_exp_2","plots","cat_line_ll.pdf"))


binary_ae_plot <- ggplot(filter(attr_stim_long, trial_choice_set=="binary"))+
  geom_text(aes(jnds,prob_in(jnds),label=option_abbrev,col=option_abbrev),size=5.5)+
  facet_wrap(side_correct~target_side+difficulty,labeller = function(labs)
  {label_both(labs)})+
  geom_line(data=optimal, aes(jnds, prob_in), col="darkgreen")+
  geom_vline(xintercept = out_mean_jnds, linetype="dashed")+
  scale_y_continuous(limits=c(0,1.2))+
  labs(x='JNDS from minimum stimulus',y='Density',title='Binary Attraction Effect Trials')+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5,size = 20))

psypsy_pl <- tibble(
  jnds=jnds_1[jnds_1>0],
  len=psy2phy(jnds)
) %>%
  ggplot(aes(jnds,len))+
  geom_line()+
  scale_y_continuous(limits=c(150,450),breaks=seq(150,450,50))+
  labs(x="JNDs from minimum line length",
       y="Line length in pixels")+
  ggthemes::theme_few()+
  theme(text=element_text(size=15))


if(save_plots){
  ggsave(cat_dists_plot,
         filename = here("line_exp_2","plots","cat_dist_plots_1b.png"),
         width=5,height=5,units="in")
  ggsave(learn_plot,
         filename = here("line_exp_2","plots","learn_plot_1b.png"),
         width=5,height=5,units="in")
  ggsave(learn_hist,
         filename = here("line_exp_2","plots","learn_hist_1b.png"),
         width=5,height=5,units="in")
  ggsave(learn_demo_plot,
         filename = here("line_exp_2","plots","learn_demo_plot_1b.png"),
         width=5,height=5,units="in")
  ggsave(trinary_ae_plot,
         filename = here("line_exp_2","plots","trinary_attr_eff_plot_1b.png"),
         width=13, height=8, units="in")
  ggsave(trinary_ae_plot_ll,
         filename = here("line_exp_2","plots","trinary_attr_eff_plot_1b_ll.pdf"),
         width=12, height=8)
  ggsave(binary_ae_plot,
         filename = here("line_exp_2","plots","binary_attr_eff_plot_1b.png"),
         width=13, height=8, units="in")
  ggsave(lcr_plot,
         filename = here("line_exp_2","plots","lcr_plot_1b.png"),
         width=5, height=5, units="in")
  ggsave(psypsy_pl,
         filename = here("line_exp_2","plots","psy_psy_1b.png"),
  width=5,height=5,units="in")
}

# 
# # Write trial / stimuli info to files ============================================================
# # # # # # Save learning trials
# readr::write_csv(learn, here('trials','learn_trials_1b.csv'))
# jsonlite::write_json(learn, here('trials','learn_trials_1b.json'))
# jsonlite::write_json(transfer_w_phy, here('trials','transfer_trials_1b.json'))
# # # # # # # #
# # # # # # # # # Save learning demo stim
# # # # # # # # DO NOT UNCOMMENT THIS
# readr::write_csv(learn_demo, here('stimuli','learn_demo_stim_1b.csv'))
# jsonlite::write_json(learn_demo_in_cat,here('stimuli','learn_demo_in_cat_1b.json'))
# jsonlite::write_json(learn_demo_out_cat,here('stimuli','learn_demo_out_cat_1b.json'))
# # # # # # # # #
# # # # # # # # # Save transfer trials
# readr::write_csv(transfer_w_phy, here('trials','transfer_trials_1b.csv'))
# readr::write_csv(attr_stim_all, here('trials','attr_eff_trials_1b.csv'))
# readr::write_csv(fillers, here('trials','filler_trials_1b.csv'))
# readr::write_csv(attr_stim_long, here("trials","attr_eff_trials_long_1b.csv"))
# # # # # # # # #
# # # # # # # #

# Checks ============================================================
transfer_w_phy_checks <- transfer_w_phy %>%
  mutate(
    check_correct_1_psy=case_when(
     (transfer_trial_type=="attraction") & (tc_correct=="target" | tc_correct=="both") ~ target==correct_1_psy,
     (transfer_trial_type=="attraction") & (tc_correct=="competitor") ~ competitor==correct_1_psy,
     (transfer_trial_type=="filler_easy" & trial_choice_set=="trinary" & side_correct=="left") ~ 
       correct_1_psy==min(c(stim_1_psy, stim_2_psy,stim_3_psy)),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="binary" & side_correct=="left") ~ 
       correct_1_psy==min(c(stim_1_psy, stim_2_psy)),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="trinary" & side_correct=="right") ~ 
       correct_1_psy==max(c(stim_1_psy, stim_2_psy,stim_3_psy)),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="binary" & side_correct=="right") ~ 
       correct_1_psy==max(c(stim_1_psy, stim_2_psy)),
     transfer_trial_type=="filler_sample"~is.na(correct_1_psy)
    ),
    check_correct_2_psy=case_when(
     (transfer_trial_type=="attraction") & (tc_correct=="competitor" | tc_correct=="both") ~ competitor==correct_2_psy,
     (transfer_trial_type=="attraction") & (tc_correct=="target") ~ target==correct_2_psy,
     transfer_trial_type=="filler_sample"~is.na(correct_2_psy),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="trinary" & side_correct=="left") ~ 
       correct_2_psy==min(c(stim_1_psy, stim_2_psy,stim_3_psy)),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="binary" & side_correct=="left") ~ 
       correct_2_psy==min(c(stim_1_psy, stim_2_psy)),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="trinary" & side_correct=="right") ~ 
       correct_2_psy==max(c(stim_1_psy, stim_2_psy,stim_3_psy)),
     (transfer_trial_type=="filler_easy" & trial_choice_set=="binary" & side_correct=="right") ~ 
       correct_2_psy==max(c(stim_1_psy, stim_2_psy))
    )
  )

transfer_w_phy_checks$check_correct_1_psy 
transfer_w_phy_checks$check_correct_2_psy
