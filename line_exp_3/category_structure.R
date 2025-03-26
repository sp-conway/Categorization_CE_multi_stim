# clear environment
rm(list=ls())

# Libraries
library(tidyverse)
library(latex2exp)
library(here)
library(jsonlite)
library(glue)

# distribution functions
source(here("line_exp_3","dist_funcs.R"))

# Constants
min_len <- 150
max_len <- 450
conds <- c("centered","left","right")


# Defining the categories  ============================================================
source(here("line_exp_3","category_params.R"))
# double check that each category's pdf integrates to 1
area_centered <- integrate(compute_density, jnd_min, jnd_max, m=in_mean_jnds_centered, s=in_sd_jnds, nc=in_centered_nc); area_centered
area_left <- integrate(compute_density, jnd_min, jnd_max, m=in_mean_jnds_left, s=in_sd_jnds, nc=in_left_nc); area_left
area_right <- integrate(compute_density, jnd_min, jnd_max, m=in_mean_jnds_right, s=in_sd_jnds, nc=in_right_nc); area_right

# "in" the category distribution for the central condition
in_category_centered <- tibble(
  distribution="in",
  exp_condition="centered",
  jnds=jnds_1,
  dens=compute_density(jnds, in_mean_jnds_centered, in_sd_jnds, in_centered_nc)
)

# "in" the category distribution for the "left" condition
# note it's not actually skewed - it's a normal just distribution, just left of center
in_category_left <- tibble(
  distribution="in",
  exp_condition="left",
  jnds=jnds_1,
  dens=compute_density(jnds, in_mean_jnds_left, in_sd_jnds, in_left_nc)
)

# "in" the category distribution for the "right" condition
# note it's not actually skewed - it's a normal just distribution, just right of center
in_category_right <- tibble(
  distribution="in",
  exp_condition="right",
  jnds=jnds_1,
  dens=compute_density(jnds, in_mean_jnds_right, in_sd_jnds, in_right_nc)
)

out_category <- tibble(
  distribution="out",
  jnds=jnds_1,
  dens=dunif(jnds_1, jnd_min, jnd_max)
)

# Combine category/non-category distributions
all_category_centered <- bind_rows(in_category_centered, mutate(out_category,exp_condition="centered")) %>%
  mutate(in_mean_jnds=in_mean_jnds_centered)
all_category_right <- bind_rows(in_category_right, mutate(out_category,exp_condition="right")) %>%
  mutate(in_mean_jnds=in_mean_jnds_right)
all_category_left <- bind_rows(in_category_left, mutate(out_category,exp_condition="left")) %>%
  mutate(in_mean_jnds=in_mean_jnds_left)
all_categories <- bind_rows(
  all_category_centered,
  all_category_right,
  all_category_left
  
)


# Psychological mapping  ============================================================
# Psychological to physical mapping
# n = number of jnd
all_stim <- tibble(
  psych=jnds_1,
  phys=psy2phy(min_len,jnds_1)
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
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust = 0.5))
psy_to_phys_plot
# Learning Trial Stimuli ============================================================

n_per_category <- 100

# set seeds
# Do not change!
in_seed <- 1903
out_seed <- 105
write_lines(paste0("Seeds for randomly sampling learning stim\nin_seed=",
                   in_seed, "\nout_seed=",out_seed),
            here("line_exp_3","notes","seeds_for_learning_stim.txt"))
learn <- tibble()

for(cond in conds){
  in_mean <- switch(cond,
                    "centered"=in_mean_jnds_centered,
                    "left"=in_mean_jnds_left,
                    "right"=in_mean_jnds_right)
  in_nc <- switch(cond,
                  "centered"=in_centered_nc,
                  "left"=in_left_nc,
                  "right"=in_right_nc)
  
  # Learning trial stimuli - in the category
  # bins go from 3 sds below to 3 sds above the mean, in increments of 1/2 sd
  bins_in <- seq(in_mean - 3*in_sd_jnds, in_mean + 3*in_sd_jnds, in_sd_jnds/2)
  probs_in <- pnorm(bins_in[2:length(bins_in)], in_mean, in_sd_jnds)- # cumulative probability associated w/ that bin
    pnorm(bins_in[1:length(bins_in)-1],in_mean, in_sd_jnds)
  
  # Normalize
  probs_in <- probs_in/sum(probs_in)
  n_per_bin_in <- n_per_category*probs_in
  n_per_bin_in <- map_dbl(n_per_bin_in, .f=~ifelse(.x>1, round(.x), 1)) # if less than 1, make sure it's 1
  
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
  learn <- bind_rows(learn,
  tibble(
    `in`= learn_in,
    `out`=learn_out
  ) %>%
    pivot_longer(
      cols=c(`in`, `out`),names_to='distribution',
      values_to = 'jnds'
    ) %>%
    mutate(
      distribution=as_factor(distribution),
      exp_condition=cond,
      dens=case_when(
        distribution=='in'~compute_density(jnds, in_mean, in_sd_jnds, in_nc),
        distribution=='out'~dunif(jnds, jnd_min, jnd_max)
      ),
      key_correct=case_when( # for psychopy
        distribution=="in"~"u",
        distribution=="out"~"i" 
      )
    )
  )
}


# Learning Demo Stimuli  ============================================================
# Stim to show to try and aid learning
#

n_demo <- 40

learn_demo <- tibble()
in_seed_demo <- 100
out_seed_demo <- 101
write_lines(paste0("Seeds for randomly sampling learning demo stim\nin_seed=",
                  in_seed_demo, "\nout_seed=",out_seed_demo),
           here("line_exp_3","notes","seeds_for_learning_stim_demo.txt"))
for(cond in conds){
  in_mean <- switch(cond,
                    "centered"=in_mean_jnds_centered,
                    "left"=in_mean_jnds_left,
                    "right"=in_mean_jnds_right)
  in_nc <- switch(cond,
                  "centered"=in_centered_nc,
                  "left"=in_left_nc,
                  "right"=in_right_nc)
  
  # Sampling learn-in stimuli for demo
  # Basically sampling from a truncated normal
  # Learning trial stimuli - in the category
  bins_in_demo <- seq(in_mean - 3*in_sd_jnds, in_mean + 3*in_sd_jnds, in_sd_jnds/2)
  probs_in_demo <- pnorm(bins_in_demo[2:length(bins_in_demo)], in_mean, in_sd_jnds)-pnorm(bins_in_demo[1:length(bins_in_demo)-1],in_mean, in_sd_jnds)
  
  # Normalize
  probs_in_demo <- probs_in_demo/sum(probs_in_demo)
  n_per_bin_in_demo <- n_demo*probs_in_demo
  n_per_bin_in_demo <- map_dbl(n_per_bin_in_demo, .f=~ifelse(.x>1, round(.x), floor(.x))) 
  
  # Sampling learning stimuli - in category
  learn_in_demo <- c()
  set.seed(in_seed_demo)
  for(i in 2:length(bins_in_demo)){
    learn_in_demo <- c(learn_in_demo,sample(seq(bins_in_demo[i-1], bins_in_demo[i],by=.01),size=n_per_bin_in_demo[i-1],replace = T))
  }
  
  # Learning trial stimuli - out of the category
  bins_out_demo <- seq(jnd_min, jnd_max, length.out=11)
  probs_out_demo <- rep(.1, 10)
  n_per_bin_out_demo <- n_demo*probs_out_demo
  
  # Sampling learning stimuli - out of the category
  learn_out_demo <- c()
  set.seed(out_seed_demo)
  for(i in 2:length(bins_out_demo)){
    learn_out_demo <- c(learn_out_demo, sample(seq(bins_out_demo[i-1], bins_out_demo[i],by=.01),size=n_per_bin_out_demo[i-1],replace = T))
  }
  
  learn_demo <- bind_rows(learn_demo,
  tibble(
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
        category=='in_category'~compute_density(jnds, in_mean, in_sd_jnds, in_nc),
        category=='out_category'~dunif(jnds, jnd_min, jnd_max)
      ),
      exp_condition=cond
    )
  )
}

# Transfer Trial Stimuli  ============================================================

attr_stim <- tibble()
fillers <- tibble()
for(cond in conds){
  in_mean <- switch(cond,
                    "centered"=in_mean_jnds_centered,
                    "left"=in_mean_jnds_left,
                    "right"=in_mean_jnds_right)
  
  in_nc <- switch(cond,
                  "centered"=in_centered_nc,
                  "left"=in_left_nc,
                  "right"=in_right_nc)
  
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
    stim_1_psy=in_mean-jnds_equal_1,
    stim_2_psy=in_mean+jnds_equal_1,
    difficulty_level="equal_1_jnd",
  )
  
  pair_equal_2 <- tibble(
    side_correct="equal",
    stim_1_psy=in_mean-jnds_equal_2,
    stim_2_psy=in_mean+jnds_equal_2,
    difficulty_level="equal_2_jnd"
  )
  
  # Pair where shorter is correct, but it's easy
  pair_left_correct_1 <- tibble(
    side_correct="short",
    difficulty_level="easy",
    stim_1_psy=in_mean-jnds_correct_easy,
    stim_2_psy=in_mean+jnds_incorrect_easy
  )
  
  # Pair where shorter is correct, and it's difficult
  pair_left_correct_2 <- tibble(
    side_correct="short",
    difficulty_level="difficult",
    stim_1_psy=in_mean-jnds_correct_hard,
    stim_2_psy=in_mean+jnds_incorrect_hard
  )
  
  # Pair where long is correct, but it's difficult
  pair_right_correct_1 <- tibble(
    side_correct="long",
    difficulty_level="difficult",
    stim_1_psy=in_mean-jnds_incorrect_hard,
    stim_2_psy=in_mean+jnds_correct_hard
  )
  
  # Pair where long is correct, and it's easy
  pair_right_correct_2 <- tibble(
    side_correct="long",
    difficulty_level="easy",
    stim_1_psy=in_mean-jnds_incorrect_easy,
    stim_2_psy=in_mean+jnds_correct_easy
  )
  
  # Bind all together
  attr_stim_tmp <- bind_rows(
    pair_equal_1,
    pair_equal_2,
    pair_left_correct_1,
    pair_left_correct_2,
    pair_right_correct_1,
    pair_right_correct_2
  )
  
  # double them so each trial type can have target and competitor be both options
  attr_stim_tmp <- attr_stim_tmp %>%
    bind_rows(attr_stim_tmp) %>%
    arrange(side_correct,stim_1_psy, stim_2_psy, difficulty_level)
  
  # Create decoy by subtracting or adding from the target
  attr_stim_tmp <- attr_stim_tmp %>%
    mutate(
      dtmp = rep(c(T,F),nrow(attr_stim_tmp)/2),
      stim_3_psy=case_when(
        dtmp~stim_1_psy-td_dist,
        !dtmp~stim_2_psy+td_dist,
      ),
      trial_type="attraction",
      set=case_when(
        dtmp~"l-s-ds",
        !dtmp~"l-s-dl",
      ),
      set_type="trinary"
    ) %>%
    select(-dtmp)
  
  # Binary trials (no decoy)
  attr_stim_tmp_binary <- attr_stim_tmp %>%
    mutate(
      set_type="binary",
      stim_3_psy=NA_real_,
      set="l-s"
    )
  
  # Bind trinary to binary
  attr_stim_tmp <- full_join(attr_stim_tmp,attr_stim_tmp_binary) %>%
    relocate(trial_type, set_type, set, side_correct, difficulty_level, stim_1_psy, stim_2_psy, stim_3_psy)
  
  # function to get correct choice for attraction effect stimuli
  # Probably could have done this above, but it works fine
  corr_choice <- function(cat_mean,return_which,...){
    vec <- c(...)
    vec <- vec[!is.na(vec)]
    dists <- abs(vec-cat_mean)
    # if(length(dists[dists==min(dists)])>1){ #If there's more than one right answer, correct is NA
    #   stim_correct <- NA
    # }else {
    #   stim_correct<-vec[which.min(dists)]
    # }
    stim_correct <- vec[which(dists==min(dists))]
    if(length(stim_correct)==1){
      # browser()
      stim_correct <- c(stim_correct,NA_real_)
    } 
    return(stim_correct[return_which])
  }
  
  # Find out correct option based on which is closest to cat mean
  attr_stim_tmp <- attr_stim_tmp %>%
    mutate(
      stim_correct_1=unlist(pmap(list(stim_1_psy, stim_2_psy, stim_3_psy),corr_choice,cat_mean=in_mean,return_which=1)),
      stim_correct_2=unlist(pmap(list(stim_1_psy, stim_2_psy, stim_3_psy),corr_choice,cat_mean=in_mean,return_which=2)),
    )
  
  # Repeating attraction effect trials 3 times in a block!
  attr_stim_tmp <- attr_stim_tmp %>%
    bind_rows(attr_stim_tmp, attr_stim_tmp)
  
  attr_stim <- bind_rows(attr_stim,
                        mutate(attr_stim_tmp,exp_condition=cond))
  
  # Filler trials #
  n_filler <- 24
  n_trinary <- n_filler/2
  n_binary <- n_filler/2
  filler_sample_seed <- 74836
  set.seed(filler_sample_seed)
  
  # Create fillers
  # Correct option sampled from the category, incorrect option(s) sampled from non-category
  fillers_tmp <- tibble(
    trial_type=rep('filler',n_filler),
    set_type=c(rep('trinary',n_trinary),rep('binary',n_binary)),
    set=rep(NA_character_,n_filler),
    stim_1_psy=rnorm(n_filler, in_mean, in_sd_jnds),
    stim_2_psy=runif(n_filler, jnd_min, jnd_max)
  ) %>%
    mutate(
      stim_3_psy=case_when(
        set_type=='trinary' ~ round(runif(n_filler, jnd_min, jnd_max))
      ),
      stim_correct_1=stim_1_psy,
      stim_correct_2=NA_real_,
      side_correct=case_when(
        is.na(stim_correct_2) & stim_correct_1>in_mean~"long",
        is.na(stim_correct_2) & stim_correct_1<in_mean~"short"
      ),
    )
  fillers <- bind_rows(fillers,
                       mutate(fillers_tmp,exp_condition=cond))
}



# ALL transfer stimuli combined (i.e., this is everything shown in a single block#
transfer <- bind_rows(attr_stim, fillers) %>%
  mutate(exp_trial_type="transfer") %>%
  relocate(exp_trial_type, trial_type, set_type, set, difficulty_level,
           stim_1_psy, stim_2_psy, stim_3_psy, 
           stim_correct_1,stim_correct_2)

# Plots   ============================================================
col_pal <- ggsci::pal_startrek()(2)
# Plot Category/Non-Category distributions
all_category_plot_psych <- all_categories %>%
  ggplot(aes(jnds, dens,col=distribution))+
  geom_line()+
  geom_vline(aes(xintercept=in_mean_jnds),linetype="dashed",alpha=.75)+
  labs(
    x="JND",
    y="Density",
    title="Category Distributions"
  )+
  facet_grid(exp_condition~.)+
  ggthemes::theme_few()+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values= col_pal,
                     name='Category')+
  theme(
    plot.title = element_text(hjust=0.5,size=15),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
all_category_plot_psych
ggsave(filename = here("line_exp_3","plots","all_category_plot_psych.jpeg"),width = 5,height=5)


# Plot Learning Trials along w/ category densities
learn_plot <- learn %>%
  ggplot(aes(jnds, dens, color=distribution))+
  geom_point(alpha=0.6,size=3,shape='X')+
  facet_grid(exp_condition~.)+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=col_pal,
                     name='Category')+
  labs(
    x='JND',
    y='Density',
    title='Learning Trial Stimuli'
  )+
  ggthemes::theme_few()+
  scale_x_continuous(limits=c(jnd_min, jnd_max))+
  theme(plot.title = element_text(hjust=0.5,size=15),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
learn_plot
ggsave(filename = here("line_exp_3","plots","learn_plot_psych.jpeg"),width = 5,height=5)


# Histogram of learning trial stimuli
learn_hist <- learn %>%
  ggplot(aes(jnds,fill=distribution))+
  geom_histogram(alpha=0.6,linewidth=3,binwidth = 2,position="identity")+
  scale_fill_manual(labels=c('In Category','Out of Category'),
                    values=col_pal,
                    name='Category')+
  facet_grid(vars(exp_condition))+
  labs(
    x='JND',
    y='Frequency',
    title='Learning Trial Stimuli'
  )+
  ggthemes::theme_few()+
  scale_x_continuous(breaks=seq(jnd_min, jnd_max, 4))+
  theme(plot.title = (element_text(hjust=0.5,size=15)))
learn_hist
ggsave(filename = here("line_exp_3","plots","learn_hist_psych.jpeg"),width = 5,height=5)

# Plot stimuli for demonstration prior to learning trials
learn_demo_plot <- learn_demo %>%
  ggplot() +
  geom_point(aes(x=jnds,y=dens,color=category),alpha=0.65)+
  scale_color_manual(labels=c('In Category','Out of Category'),
                    values=col_pal,
                    name='Category')+
  facet_grid(vars(exp_condition))+
  labs(
    x='JND',
    y='Density',
    title='Learning Demo Stimuli'
  )+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5))
learn_demo_plot
ggsave(filename = here("line_exp_3","plots","learn_demo_plot_psych.jpeg"),width = 5,height=5)

attr_stim_long <- attr_stim %>%
  pivot_longer(contains("stim"),names_to = "stim",values_to = "jnds") %>%
  mutate(stim_abbrev=case_when(
    str_detect(stim,"stim_1")~"s",
    str_detect(stim,"stim_2")~"l",
    str_detect(stim,"stim_3") & str_detect(set,"ds")~"ds",
    str_detect(stim,"stim_3") & str_detect(set,"dl")~"dl",
  )) 

make_attr_plot <- function(d,jnds_all,in_mean_jnds,in_sd_jnds,in_nc, title=""){
  # browser()
  dist <- tibble(
    x=jnds_all,
    y=compute_density(jnds_all,in_mean_jnds,in_sd_jnds,in_nc)
  )
  d %>%
    mutate(dens=compute_density(jnds, in_mean_jnds, in_sd_jnds, in_nc)) %>%
    ggplot()+
    geom_text(aes(jnds,dens,label=stim_abbrev),size=4,col="purple1")+
    geom_line(data=dist,aes(x=x,y=y),inherit.aes=F,alpha=.2)+
    facet_wrap(set~side_correct+difficulty_level,labeller = function(labs)
    {label_both(labs)})+
    labs(x='JND',y='Density',title=title)+
    ggthemes::theme_few()+
    theme(plot.title = element_text(hjust=0.5,size = 20))
}

attr_stim_long %>%
  filter(str_detect(exp_condition,"centered") & str_detect(set_type,"tri")) %>%
  make_attr_plot(jnds_all = jnds_1, in_mean_jnds_centered, in_sd_jnds, in_centered_nc,
                     title="Center Condition\nTrinary Attraction Trials")
ggsave(filename = here("line_exp_3","plots","attr_stim_tri_center_cond.jpeg"),width = 8,height=6)
attr_stim_long %>%
  filter(str_detect(exp_condition,"left") & str_detect(set_type,"tri")) %>%
  make_attr_plot(jnds_all = jnds_1, in_mean_jnds_left, in_sd_jnds, in_left_nc,
                     title="Left Condition\nTrinary Attraction Trials")
ggsave(filename = here("line_exp_3","plots","attr_stim_tri_left_cond.jpeg"),width = 8,height=6)
attr_stim_long %>%
  filter(str_detect(exp_condition,"right") & str_detect(set_type,"tri")) %>%
  make_attr_plot(jnds_all = jnds_1, in_mean_jnds_right, in_sd_jnds, in_right_nc,
                     title="Right Condition\nTrinary Attraction Trials")
ggsave(filename = here("line_exp_3","plots","attr_stim_tri_right_cond.jpeg"),width = 8,height=6)

attr_stim_long %>%
  filter(str_detect(exp_condition,"centered") & str_detect(set_type,"bi")) %>%
  make_attr_plot(jnds_all = jnds_1, in_mean_jnds_centered, in_sd_jnds, in_centered_nc,
                     title="Center Condition\nBinary Attraction Trials")
ggsave(filename = here("line_exp_3","plots","attr_stim_bi_center_cond.jpeg"),width = 8,height=6)
attr_stim_long %>%
  filter(str_detect(exp_condition,"left") & str_detect(set_type,"bi")) %>%
  make_attr_plot(jnds_all = jnds_1, in_mean_jnds_left, in_sd_jnds, in_left_nc,
                     title="Left Condition\nBinary Attraction Trials")
ggsave(filename = here("line_exp_3","plots","attr_stim_bi_left_cond.jpeg"),width = 8,height=6)
attr_stim_long %>%
  filter(str_detect(exp_condition,"right") & str_detect(set_type,"bi")) %>%
  make_attr_plot(jnds_all = jnds_1, in_mean_jnds_right, in_sd_jnds, in_right_nc,
                     title="Right Condition\nBinary Attraction Trials")
ggsave(filename = here("line_exp_3","plots","attr_stim_bi_right_cond.jpeg"),width = 8,height=6)

fillers %>%
  select(-contains("phy")) %>%
  pivot_longer(contains("stim")) %>%
  mutate(category=case_when(
    str_detect(name,"1")~"In Category",
    str_detect(name,"2|3")~"Out Of Category",
  )) %>%
  ggplot(aes(value,fill=category))+
  geom_histogram(col="black")+
  facet_grid(exp_condition~set_type)+
  scale_fill_manual(values=col_pal)+
  ggthemes::theme_few()+
  theme(text=element_text(size=20))
ggsave(filename = here("line_exp_3","plots","fill_hist.jpeg"),width = 8,height=6)

# replace transfer correct with 0 if NA, for psychopy ======================================================
transfer <- transfer %>%
  mutate(stim_correct_2=replace_na(stim_correct_2, 0))

# CONVERT EVERYTHING TO LINE LENGTHS FOR EXPERIMENT ========================================================================
all_category_centered$line_len  <- psy2phy(min_len,all_category_centered$jnds) %>% round()
all_category_left$line_len  <- psy2phy(min_len,all_category_left$jnds) %>% round()
all_category_right$line_len  <- psy2phy(min_len,all_category_right$jnds) %>% round()
attr_stim$stim_1_phy <- psy2phy(min_len,attr_stim$stim_1_psy) %>% round()
attr_stim$stim_2_phy <- psy2phy(min_len,attr_stim$stim_2_psy) %>% round()
attr_stim$stim_3_phy <- psy2phy(min_len,attr_stim$stim_3_psy) %>% round()
transfer$stim_1_phy <- psy2phy(min_len,transfer$stim_1_psy) %>% round()
transfer$stim_2_phy <- psy2phy(min_len,transfer$stim_2_psy) %>% round()
transfer$stim_3_phy <- psy2phy(min_len,transfer$stim_3_psy) %>% round()
fillers$stim_1_phy <- psy2phy(min_len,fillers$stim_1_psy) %>% round()
fillers$stim_2_phy <- psy2phy(min_len,fillers$stim_2_psy) %>% round() 
fillers$stim_3_phy <- psy2phy(min_len,fillers$stim_3_psy) %>% round()
learn$line_len <- psy2phy(min_len,learn$jnds) %>% round()
learn_demo$line_len <- psy2phy(min_len,learn_demo$jnds) %>% round()

# split learn demo into in v out ========================================================================
learn_demo_in <- learn_demo %>% 
  filter(str_detect(category,"in"))
learn_demo_out <- learn_demo %>% 
  filter(str_detect(category,"out"))
## PLOT PSYCHOLOGICAL TO PHYSICAL RELATIONSHIP ========================================================================
psy_to_phy_learn <- ggplot(learn, aes(jnds, line_len,col=distribution))+
  geom_point()+
  scale_x_continuous(limits=c(0,23),breaks=seq(0,23,3))+
  scale_y_continuous(limits=c(150,450),breaks=seq(150,450,50))+
  facet_grid(exp_condition~distribution)+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=c('dark green','red'),
                     name='Category')+
  labs(x="JND",
       y="Line Length",
       caption=TeX("$Line\\,Length\\,= 150*1.05^{JND}$"),
       title="Psychological to Physical Relationship")+
  ggthemes::theme_few()+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(hjust=0.5))
psy_to_phy_learn
ggsave(filename = here("line_exp_3","plots","learn_psy_to_phy.jpeg"),width = 6,height=5)


# Save stimuli / trial info # ========================================================================

# Save learning trials (both to trials folder and in the experiment)
map(conds, ~write_csv(filter(learn,str_detect(exp_condition,.x)), 
                                                       file=here('line_exp_3','trials',glue('learn_trials_{.x}.csv'))))
map(conds, ~write_csv(filter(learn,str_detect(exp_condition,.x)), 
                                                       file=here('line_exp_3','experiment','conditions',glue('learn_trials_{.x}.csv'))))

# Save transfer trials (both to trials folder and in the experiment)
map(conds, ~write_csv(filter(transfer,str_detect(exp_condition,.x)), 
                                                       file=here('line_exp_3','trials',glue('transfer_trials_{.x}.csv'))))
map(conds, ~write_csv(filter(transfer,str_detect(exp_condition,.x)), 
                                                       file=here('line_exp_3','experiment','conditions',glue('transfer_trials_{.x}.csv'))))

# Save learning demo in
map(conds, ~write_csv(filter(learn_demo_in,str_detect(exp_condition,.x)), 
                                                       file=here('line_exp_3','trials',glue('learn_demo_in_{.x}.csv'))))
map(conds, ~write_lines(filter(learn_demo_in,str_detect(exp_condition,.x)) %>% pull(line_len), 
                                                       file=here('line_exp_3','experiment','conditions',glue('learn_demo_in_{.x}.txt'))))
# Save learning demo out
map(conds, ~write_csv(filter(learn_demo_out,str_detect(exp_condition,.x)), 
                      file=here('line_exp_3','trials',glue('learn_demo_out_{.x}.csv'))))
map(conds, ~write_lines(filter(learn_demo_out,str_detect(exp_condition,.x)) %>% pull(line_len), 
                        file=here('line_exp_3','experiment','conditions',glue('learn_demo_out_{.x}.txt'))))

