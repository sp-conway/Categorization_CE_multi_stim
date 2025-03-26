# category_structure.R
# design for 1 dimension, 1 category, context effect/categorization experiment
# Initial experiment
# Sean Conway
# Last modified Mar. 2023

# clear environment
rm(list=ls())

# Libraries
library(tidyverse)
library(here)

# Constants
min_len <- 150
max_len <- 450

# Psychological mapping  ============================================================
# Psychological to physical mapping
# n = number of jnd
psy2phy <- function(n) {
  trans <- ifelse(n==0, 0, 150*1.05^n)
  return(trans)
}

jnd_min <- 0
jnd_max <- 22.5
jnds_1 <- seq(0,jnd_max, .001)

all_stim <- tibble(
  psych=jnds_1,
  phys=psy2phy(jnds_1)
)

psy_to_phys_plot <- ggplot(all_stim, aes(psych, phys))+
  geom_line()+
  scale_y_continuous(limits=c(min_len, max_len),
                     breaks = seq(min_len, max_len,50))+
  labs(
    x="Psychological Value\n(JNDs)",
    y="Physical Value \n(Line Lengths)",
    title="Psychological to Physical Mapping"
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5));psy_to_phys_plot

# Defining the categories  ============================================================

# Category properties
in_mean_jnds <- jnd_max/2
in_sd_jnds <- jnd_max/6

# normalizing constant
in_nc <- pnorm(jnd_max, in_mean_jnds, in_sd_jnds)-pnorm(jnd_min, in_mean_jnds, in_sd_jnds)

in_category <- tibble(
  distribution="in",
  jnds=jnds_1,
  dens=dnorm(jnds_1, in_mean_jnds, in_sd_jnds)/in_nc
)

out_category <- tibble(
  distribution="out",
  jnds=jnds_1,
  dens=dunif(jnds_1, jnd_min, jnd_max)
)

# Combine category/non-category distributions
all_category <- full_join(in_category, out_category)

# Learning Trial Stimuli ============================================================

n_per_category <- 100

# set seeds
# Do not change!
in_seed <-850
out_seed <- 432
write_lines(paste0("Seeds for randomly sampling learning stim\nin_seed=",
                   in_seed, "\nout_seed=",out_seed),
            here("line_exp_1","notes","seeds_for_learning_stim.txt"))

# Learning trial stimuli - in the category
bins_in <- seq(jnd_min, jnd_max, length.out=11)
probs_in <- pnorm(bins_in[2:length(bins_in)], in_mean_jnds, in_sd_jnds)-pnorm(bins_in[1:length(bins_in)-1],in_mean_jnds, in_sd_jnds)

# Normalize
probs_in <- probs_in/sum(probs_in)
n_per_bin_in <- n_per_category*probs_in
n_per_bin_in <- map(n_per_bin_in, .f=~ifelse(.x>1, round(.x), floor(.x))) %>%
  as_vector()

# Sampling learning stimuli - in category
learn_in <- c()
set.seed(in_seed)
for(i in 2:length(bins_in)){
  learn_in <- c(learn_in,sample(seq(bins_in[i-1], bins_in[i],by=.01),size=n_per_bin_in[i-1],replace = T))
}

# Learning trial stimuli - out of the category
bins_out <- seq(jnd_min, jnd_max, length.out=11)
probs_out <- rep(.1, 10)
n_per_bin_out <- n_per_category*probs_out

# Sampling learning stimuli - out of the category
learn_out <- c()
set.seed(out_seed)
for(i in 2:length(bins_out)){
  learn_out <- c(learn_out, sample(seq(bins_out[i-1], bins_out[i],by=.01),size=n_per_bin_out[i-1],replace = T))
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
      distribution=='in'~dnorm(jnds, in_mean_jnds, in_sd_jnds),
      distribution=='out'~dunif(jnds, jnd_min, jnd_max)
    )
  )

# Learning Demo Stimuli  ============================================================
# Stim to show to try and aid learning
#

n_demo <- 40

# Sampling learn-in stimuli for demo
# Basically sampling from a truncated normal
learn_in_demo <- rep(-99, n_demo)
for(i in 1:n_demo){
  while(learn_in_demo[i] < jnd_min | learn_in_demo[i] > jnd_max){
    learn_in_demo[i] <- rnorm(1, in_mean_jnds, in_sd_jnds)
  }
}

# Sampling learn-out stimuli for demo
# Sample from uniform range outside 3 SDs of mean
learn_out_demo <- sample(c(seq(in_mean_jnds-3*in_sd_jnds,in_mean_jnds-2*in_sd_jnds),
                           seq(in_mean_jnds+2*in_sd_jnds, in_mean_jnds+3*in_sd_jnds)),
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
      category=='in_category'~dnorm(jnds, in_mean_jnds, in_sd_jnds),
      category=='out_category'~dunif(jnds, jnd_min, jnd_max)
    )
  )

# make into two data frames
learn_demo_in_cat <- learn_demo %>%
  filter(category=='in_category') %>%
  select(jnds) %>%
  as_vector(.)
learn_demo_out_cat <- learn_demo %>%
  filter(category=='out_category') %>%
  select(jnds) %>%
  as_vector(.)


# Transfer Trial Stimuli  ============================================================

# # Attraction effect stimuli parameters
td_dist <- 2 # JNDs between target and decoy
tc_dist_diff <- 1 # JNDs between target and competitor on a given trial
tc_dist_easy <- 2

# jnd distance to mean for equally correct options
jnds_equal_1 <- 1
jnds_equal_2 <- 2
jnds_correct_hard <- 1
jnds_incorrect_hard <- 2
jnds_correct_easy <- 1
jnds_incorrect_easy <- 3

# First creating pairs of stimuli

# Pair where either is equally correct (i.e., equidistant to category mean)
pair_equal_1 <- tibble(
  side_correct="equal",
  stim_1_psy=in_mean_jnds-jnds_equal_1,
  stim_2_psy=in_mean_jnds+jnds_equal_1,
  difficulty_level="equal_chance_1_JND"
  )

pair_equal_2 <- tibble(
  side_correct="equal",
  stim_1_psy=in_mean_jnds-jnds_equal_2,
  stim_2_psy=in_mean_jnds+jnds_equal_2,
  difficulty_level="equal_chance_2_JND"
)

# Pair where left is correct, but it's easy
pair_left_correct_1 <- tibble(
  side_correct="left",
  difficulty_level="easy",
  stim_1_psy=in_mean_jnds-jnds_correct_easy,
  stim_2_psy=in_mean_jnds+jnds_incorrect_easy
  )

# Pair where left is correct, and it's difficult
pair_left_correct_2 <- tibble(
  side_correct="left",
  difficulty_level="difficult",
  stim_1_psy=in_mean_jnds-jnds_correct_hard,
  stim_2_psy=in_mean_jnds+jnds_incorrect_hard
)

# Pair where right is correct, but it's difficult
pair_right_correct_1 <- tibble(
  side_correct="right",
  difficulty_level="difficult",
  stim_1_psy=in_mean_jnds+jnds_correct_hard,
  stim_2_psy=in_mean_jnds-jnds_incorrect_hard
)

# Pair where right is correct, and it's easy
pair_right_correct_2 <- tibble(
  side_correct="right",
  difficulty_level="easy",
  stim_1_psy=in_mean_jnds+jnds_correct_easy,
  stim_2_psy=in_mean_jnds-jnds_incorrect_easy
)

# Bind all together
attr_stim <- bind_rows(
  pair_equal_1,
  pair_equal_2,
  pair_left_correct_1,
  pair_left_correct_2,
  pair_right_correct_1,
  pair_right_correct_2
)

# double them so each trial type can have target and competitor be both options
attr_stim <- attr_stim %>%
  bind_rows(attr_stim) %>%
  arrange(side_correct,stim_1_psy, stim_2_psy, difficulty_level)

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
  select(-c(trial_type))


# Create decoy by subtracting or adding from the target
attr_stim <- attr_stim %>%
  mutate(
    decoy=case_when(
      target < in_mean_jnds ~ target-td_dist,
      target > in_mean_jnds ~ target+td_dist
    ),
    stim_3_psy=decoy,
    trial_choice_set="trinary",
    trial_type="attraction"
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
  relocate(trial_type, trial_choice_set, side_correct, difficulty_level, stim_1_psy, stim_2_psy, stim_3_psy,
           target, competitor, decoy)

# function to get correct choice for attraction effect stimuli
# Probably could have done this above, but it works fine
corr_choice <- function(...){
  cat_mean <- 11.25 # defining manually here
  vec <- c(...)
  dists <- abs(vec-cat_mean)
  if(length(dists[dists==min(dists)])>1){ #If there's more than one right answer, correct is NA
    correct <- NA
  }
  else {
    correct<-vec[which.min(dists)]
  }
  return(correct)
}

# Find out correct option based on which is closest to cat mean
# Assign tc_correct (whether target or competitor is correct)
# correct_dist_to_mean = how far is the correct option from the mean
attr_stim <- attr_stim %>%
  mutate(
    correct=unlist(pmap(list(stim_1_psy, stim_2_psy, stim_3_psy),corr_choice)),
    tc_correct=case_when(
      side_correct=="equal"~"both",
      correct==target ~"target",
      correct==competitor ~"competitor"
    ),
    target_side=case_when(
      target<in_mean_jnds~"left",
      target>in_mean_jnds~"right",
    ),
    competitor_side=case_when(
      competitor<in_mean_jnds~"left",
      competitor>in_mean_jnds~"right"
    ),
    correct_dist_to_mean=case_when(
      side_correct=="equal"~abs(target-in_mean_jnds),
      side_correct!="equal"~abs(correct-in_mean_jnds)
    )
  )

# Making attraction effect stimuli long to make plotting easier
attr_stim_long <- attr_stim %>%
  pivot_longer(cols=c(target,competitor, decoy),names_to = "option",
               values_to = "jnds") %>%
  mutate(
    dens=dnorm(jnds, in_mean_jnds, in_sd_jnds),
    option_abbrev=case_when(
      option=="target"~"T",
      option=="competitor"~"C",
      option=="decoy"~"D"
    )
  )

# Repeating attraction effect trials 3 times in a block!
attr_stim <- attr_stim %>%
  bind_rows(attr_stim, attr_stim)

# Filler trials #
n_filler <- 24
n_trinary <- n_filler/2
n_binary <- n_filler/2
filler_sample_seed <- 74836
set.seed(filler_sample_seed)

# Create fillers
# Correct option sampled from the category, incorrect option(s) sampled from non-category
fillers <- tibble(
  trial_type=rep('filler',n_filler),
  trial_choice_set=c(rep('trinary',n_trinary),rep('binary',n_binary)),
  target=rep(NA,n_filler),
  competitor=rep(NA,n_filler),
  decoy=rep(NA,n_filler),
  correct=rnorm(n_filler,in_mean_jnds,in_sd_jnds),
  stim_1_psy=correct,
  stim_2_psy=runif(n_filler, jnd_min, jnd_max)
) %>%
  mutate(
    stim_3_psy=case_when(
      trial_choice_set=='trinary' ~ round(runif(n_filler, jnd_min, jnd_max))
    ),
    side_correct=case_when(
      correct>in_mean_jnds~"right",
      correct<in_mean_jnds~"left"
    ),
    correct_dist_to_mean=abs(correct-in_mean_jnds)
  )


# ALL transfer stimuli combined (i.e., this is everything shown in a single block#
transfer <- full_join(attr_stim, fillers) %>%
  mutate(exp_trial_type="transfer") %>%
  relocate(exp_trial_type, trial_type, trial_choice_set, difficulty_level,
           stim_1_psy, stim_2_psy, stim_3_psy, target, competitor, decoy, target_side, competitor_side,
           correct, tc_correct, correct_dist_to_mean) %>%
  mutate(stim_3_psy=replace_na(stim_3_psy, 0))

# Plots #  ============================================================

# Plot Category/Non-Category distributions
all_category_plot_psych <- ggplot(all_category, aes(jnds, dens,col=distribution))+
  geom_line()+
  geom_vline(xintercept = in_mean_jnds,linetype="dashed",alpha=.75)+
  labs(
    x="JND",
    y="Density",
    title="Category Distributions"
  )+
  theme_bw()+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=c('dark green','red'),
                     name='Category')+
  theme(
    plot.title = element_text(hjust=0.5)
  )
all_category_plot_psych


# Plot Learning Trials along w/ category densities
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
    x='JND',
    y='Probability Density',
    title='Learning Trial Stimuli'
  )+
  scale_x_continuous(limits=c(jnd_min, jnd_max))+
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
    x='JND',
    y='Frequency',
    title='Learning Trial Stimuli'
  )+
  scale_x_continuous(breaks=seq(jnd_min, jnd_max, 4))+
  theme(plot.title = (element_text(hjust=0.5)))
learn_hist

# Plot stimuli for demonstration prior to learning trials
learn_demo_plot <- learn_demo %>%
  ggplot() +
  geom_point(aes(x=jnds,y=dens,color=category),alpha=0.65)+
  geom_line(data=filter(in_category, jnds >= jnd_min & jnds <= jnd_max),
            aes(x=jnds, y=dens),color='dark green',alpha=0.3)+
  geom_line(data=filter(out_category, jnds >= jnd_min & jnds <= jnd_max),
            aes(x=jnds, y=dens),color='red', alpha=0.3)+
  scale_color_manual(labels=c('In Category','Out of Category'),
                    values=c('dark green','red'),
                    name='Category')+
  labs(
    x='JND',
    y='Density',
    title='Learning Demo Stimuli'
  )+
  theme(plot.title = element_text(hjust=0.5)); learn_demo_plot

trinary_ae_plot <- ggplot(filter(attr_stim_long, trial_choice_set=="trinary"))+
  geom_text(aes(jnds,dens,label=option_abbrev,col=option_abbrev),size=4)+
  facet_wrap(side_correct~target_side+difficulty_level,labeller = function(labs)
    {label_both(labs)})+
  geom_line(data=filter(in_category, jnds>=jnd_min & jnds <= jnd_max),
            aes(jnds, dens),alpha=.6)+
  scale_color_manual(labels=c('Competitor','Decoy','Target'),
                     values=c('dodgerblue1','firebrick2','springgreen4'),
                     name='Choice')+
  labs(x='JND',y='Density',title='Trinary Attraction Effect Trials')+
  theme(plot.title = element_text(hjust=0.5,size = 20)); trinary_ae_plot

binary_ae_plot <- ggplot(filter(attr_stim_long, trial_choice_set=="binary"))+
  geom_text(aes(jnds,dens,label=option_abbrev,col=option_abbrev),size=4)+
  facet_wrap(side_correct~target_side+difficulty_level,labeller = function(labs)
    {label_both(labs)})+
  geom_line(data=filter(in_category, jnds>=jnd_min & jnds <= jnd_max),
            aes(jnds, dens),alpha=.6)+
  scale_color_manual(labels=c('Competitor','Decoy','Target'),
                     values=c('dodgerblue1','firebrick2','springgreen4'),
                     name='Choice')+
  labs(x='JND',y='Density',title='Binary Attraction Effect Trials')+
  theme(plot.title = element_text(hjust=0.5,size = 20))


# CONVERT EVERYTHING TO LINE LENGTHS FOR EXPERIMENT ========================================================================
all_category$line_len  <- psy2phy(all_category$jnds)
attr_stim$stim_1 <- psy2phy(attr_stim$stim_1_psy)
attr_stim$stim_2 <- psy2phy(attr_stim$stim_2_psy)
attr_stim$stim_3 <- psy2phy(attr_stim$stim_3_psy)
transfer$stim_1 <- psy2phy(transfer$stim_1_psy)
transfer$stim_2 <- psy2phy(transfer$stim_2_psy)
transfer$stim_3 <- psy2phy(transfer$stim_3_psy)
transfer <- transfer %>%
  rename(correct_psy=correct,
         target_psy=target,
         competitor_psy=competitor,
         decoy_psy=decoy) %>%
  mutate(correct=psy2phy(correct_psy),
         target_phys=psy2phy(target_psy),
         competitor_phys=psy2phy(competitor_psy),
         decoy_phys=psy2phy(decoy_psy)
         )
fillers$stim_1 <- psy2phy(fillers$stim_1_psy)
fillers$stim_2 <- psy2phy(fillers$stim_2_psy)
fillers$stim_3 <- psy2phy(fillers$stim_3_psy)
learn$line_len <- psy2phy(learn$jnds)
learn_demo$line_len <- psy2phy(learn_demo$jnds)
learn_demo_in_cat <- psy2phy(learn_demo_in_cat)
learn_demo_out_cat <- psy2phy(learn_demo_out_cat)

## PLOT PSYCHOLOGICAL TO PHYSICAL RELATIONSHIP ========================================================================
psy_to_phy_learn <- ggplot(learn, aes(jnds, line_len,col=distribution))+
  geom_point()+
  scale_x_continuous(limits=c(0,23),breaks=seq(0,23,3))+
  scale_y_continuous(limits=c(150,450),breaks=seq(150,450,50))+
  facet_grid(vars(distribution))+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=c('dark green','red'),
                     name='Category')+
  labs(x="Psychological Value",
       y="Physical Value",
       caption="Physical Value = 150*(1.05^Psychological Value)",
       title="Psychological to Physical Relationship")+
  ggthemes::theme_few()+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(hjust=0.5))
psy_to_phy_learn

# Save Figures # ========================================================================
# ggsave(here('multi_stim','line_exp_1', 'R','plots', 'category_dists.png'), all_category_plot_psych,height=8, width=8, units="in")
# ggsave(here('multi_stim','line_exp_1','R','plots','learn_trials_dist.png'), learn_plot,height=8, width=8, units="in")
# ggsave(here('multi_stim','line_exp_1','R','plots','learn_trials_hist.png'), learn_hist,height=8, width=8, units="in")
# ggsave(here('multi_stim','line_exp_1','R','plots','learn_demo_stim.png'),learn_demo_plot,height=8, width=8, units="in")
# ggsave(here('multi_stim','line_exp_1','R','plots','trinary_attr_eff.png'),trinary_ae_plot,height=8, width=12, units="in")
# ggsave(here('multi_stim','line_exp_1','R','plots','binary_attr_eff.png'),binary_ae_plot,height=8, width=12, units="in")
# ggsave(here('multi_stim','line_exp_1','R','plots','psy_to_phy_learn.png'),psy_to_phy_learn,height=8, width=8, units="in")
# #

# Save stimuli / trial info # ========================================================================
# # # Save learning trials
# readr::write_csv(learn, here('multi_stim','line_exp_1','trials','learn_trials.csv'))
# jsonlite::write_json(learn, here('multi_stim','line_exp_1','trials','learn_trials.json'))
# # #
# # # # Save learning demo stim
# # # DO NOT UNCOMMENT THIS
# # # readr::write_csv(learn_demo, here('multi_stim','line_exp_1','stimuli','learn_demo_stim.csv'))
# # # jsonlite::write_json(learn_demo_in_cat,here('multi_stim','line_exp_1','stimuli','learn_demo_in_cat.json'))
# # # jsonlite::write_json(learn_demo_out_cat,here('multi_stim','line_exp_1','stimuli','learn_demo_out_cat.json'))
# # # #
# # # # Save transfer trials
# readr::write_csv(transfer, here('multi_stim','line_exp_1','trials','transfer_trials.csv'))
# readr::write_csv(attr_stim, here('multi_stim','line_exp_1','trials','attr_eff_trials.csv'))
# readr::write_csv(fillers, here('multi_stim','line_exp_1','trials','filler_trials.csv'))
# readr::write_csv(attr_stim_long, here('multi_stim','line_exp_1',"trials","attr_eff_trials_long.csv"))
# #
# # #