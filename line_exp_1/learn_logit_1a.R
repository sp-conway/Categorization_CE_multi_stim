# learn_logit_1a.R
# Bayesian Hierarchical Logistic Regression of Learning Trials
# Sean Conway
# Last modified Nov. 2021

# clear environment
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(runjags)
library(coda)
library(tidybayes)
library(glue)
library(patchwork)

# read in learning data
learn <- read_csv(here("line_exp_1","data","cleaned","dataset_learn.csv"))

# prob density functions
source(here("line_exp_1","functions","density_functions.R"))

# dbda stuff
source(here("line_exp_1","functions","DBDA2E-utilities.R"))

# Data setup ===================================================
# jnd mean
jnd_mean <- 11.25

# second half of learning only
learn_2h <- learn %>%
  filter(trial_num > 100 & rt > 100 & jnds != jnd_mean) %>% # 200 trials, so this grabs the second half of learning
  mutate(
    sub_n_new=sub_n - 99,# want subject n to start from 1 (currently starts at 99)
    cat_choice=case_when( # want 1 and 0 instead of "YES" and "NO"
      yn_choice=="YES" ~ 1,
      yn_choice=="NO" ~ 0
    ),
    prob_in=prob_in(jnds), # optimal probability a particular stim is in the category
    odds_in=prob_in/(1-prob_in),
    above_mean=case_when(
      jnds > jnd_mean ~ 1, 
      jnds < jnd_mean ~ 0, 
    )
  ) %>%
  arrange(sub_n, trial_num) %>%
  ungroup()

sub_counts_df <- learn_2h %>%
  count(sub_n_new)

# vectors to feed to jags
subs <- map2(sub_counts_df$sub_n_new, sub_counts_df$n, ~rep(x=.x, times=.y)) %>%
  unlist()
prob_in <- learn_2h$prob_in
choices <- learn_2h$cat_choice
stim_type <- as.factor(learn_2h$above_mean)
odds_in <- learn_2h$odds_in

# data list for jags
data_list_probs <- list(
  subs = subs,
  prob_in = prob_in,
  choices = choices,
  stim_type = stim_type
)

data_list_odds <- list(
  subs = subs,
  odds_in = odds_in,
  choices = choices,
  stim_type = stim_type
)

data_list_log_odds <- list(
  subs = subs,
  log_odds_in = log(odds_in),
  choices = choices,
  stim_type = stim_type
)


# Frequentist model/non-hierarchical ===================================================
freq_model_all_preds <- glm(choices~odds_in*stim_type, family=binomial(logit))
summary(freq_model_all_preds)
freq_model_no_intxn <- glm(choices~odds_in+stim_type, family=binomial(logit))
summary(freq_model_no_intxn)
freq_model_only_odds <- glm(choices~odds_in, family=binomial(logit))
summary(freq_model_only_odds)

# Bayesian Model setup/running model ===================================================

# Use odds or probability to predict?
x_var <- "odds" # "odds" or "probs" or "log_odds"

# file name for storing initial model output
file_name_model_object <- glue(here("line_exp_1", "mcmc","mcmc_logit_model_{x_var}_jags_output.RData"))

# file name for storing mcmc samples
file_name_samples <- glue(here("line_exp_1","mcmc","mcmc_logit_model_{x_var}_samples.RData"))

# file name for storing chain summary stats
file_name_chain_summary <- glue(here("line_exp_1","mcmc","mcmc_chain_summary_stats_{x_var}.RData"))

# check if files exist
if (file.exists(file_name_samples) & 
    file.exists(file_name_model_object) &
    file.exists(file_name_chain_summary)) {
  cat("Loading model output...\n")
  load(file_name_samples)
  load(file_name_model_object)
  load(file_name_chain_summary)
} else { # otherwise run model
  cat("\nModel output doesn't exist. Running MCMC.\n")
  rj_out <- run.jags(
    method=runjagsMethodDefault, 
    monitor=c("M_sl_above",
              "M_sl_below",
              "M_int_above",
              "M_int_below",
              "SD_int",
              "SD_sl",
              "sl_above",
              "sl_below"),
    model=switch(x_var,
                 "odds"=here("jags","logit_model_1a_odds.jags"),
                 "probs"=here("jags","logit_model_1a_probs.jags"),
                 "log_odds"=here("jags","logit_model_1a_log_odds.jags")),
    data=switch(x_var,
                "odds"=data_list_odds,
                "probs"=data_list_probs,
                "log_odds"=data_list_log_odds),
    n.chains=nChainsDefault,
    adapt=1000,
    burnin=1000,
    sample=10000,
    thin=5
  )
  
  #create list of samples from
  #  all the separate chains
  coda_samples <- as.mcmc.list(rj_out)
  
  #Combine chains into one big matrix 
  all_samples <- do.call(rbind, coda_samples)
  
  # chain stats
  chain_stats <- summary(coda_samples)
  
  # save model output
  save(rj_out, file=file_name_model_object)
  
  # save samples
  save(coda_samples, file=file_name_samples)
  
  # save chain summary stats
  save(chain_stats, file=file_name_chain_summary)
  
}

# parameter names (minus indiv sub params)
param_names <- c("M_sl_below",
                 "M_sl_above",
                 "M_int_below",
                 "M_int_above",
                 "SD_int",
                 "SD_sl")

# Diagnostic Figures ===================================================
make_diag_plots <- T

# Kruschke diagnostic plots (by parameter)
if(make_diag_plots){
  for(param in param_names){
    diagMCMC(coda_samples, parName = param, 
             saveName = glue(here("line_exp_1","mcmc","mcmc_diag_plot_{x_var}")),
             saveType = "png")
    dev.off()
  }
}

# Next, making gelman, autocorr, traceplots overall

# Gelman plots
make_gelman_plots <- T
if(make_gelman_plots){
  pdf(glue(here("line_exp_1","mcmc","gelman_plot_{x_var}.pdf")))
  gelman.plot(coda_samples[,c("M_sl_above",
                              "M_sl_below",
                              "M_int_above",
                              "M_int_below",
                              "SD_int",
                              "SD_sl")])
  dev.off()
}

# Chain traceplots
make_traceplots <- T
if(make_traceplots){
  pdf(glue(here("line_exp_1","mcmc","traceplots_{x_var}.pdf")))
  par(mfrow=c(3,2))
  traceplot(coda_samples[,c("M_sl_above",
                          "M_sl_below",
                          "M_int_above",
                          "M_int_below",
                          "SD_int",
                          "SD_sl")])
  dev.off()
}

# Autocorrelation plots
make_autocorr_plots <- T
if(make_autocorr_plots){
  pdf(glue(here("line_exp_1","mcmc","autocorr_plots_{x_var}.pdf")))
  par(mfrow=c(3,2))
  autocorr.plot(coda_samples[,c("M_sl_above",
                                "M_sl_below",
                                "M_int_above",
                                "M_int_below",
                                "SD_int",
                                "SD_sl")])
  dev.off()
}


# Tidying samples ===================================================

# tidying functions 

tidy_sub_params <- function(df, param_names){
  param_name_1 <- paste0(param_names,"[")
  
  df_tidy <- df %>%
    select(contains(param_name_1)) %>%
    pivot_longer(cols=everything()) %>%
    mutate(name=str_replace(name,"\\]","")) %>%
    separate(name, into=c("param","sub_n"),
             sep="\\[") %>%
    mutate(sub_n=as.numeric(sub_n)) %>%
    ungroup()
  
  return(df_tidy)
}

tidy_group_params <- function(df, 
                              param_names=c("M_sl_above",
                                            "M_sl_below",
                                            "M_int_above",
                                            "M_int_below")){
  df_tidy <- df %>%
    select(all_of(param_names)) %>%
    pivot_longer(cols = everything(),
                 names_to = "param",
                 values_to = "value") %>%
    separate(param, into=c("param","stim_space"),sep=-5) %>%
    mutate(param=case_when( 
      param=="M_sl_" ~ "slope",
      param=="M_int_" ~ "intercept"
    )) %>%
    ungroup() 
  return(df_tidy)
}

# File names for tidy samples 
file_name_tidy_samples_subs <-  glue(here("line_exp_1","mcmc","param_samples_by_sub_{x_var}.RData"))
file_name_tidy_samples_group <- glue(here("line_exp_1","mcmc","param_samples_group_{x_var}.RData"))

# Tidy samples - all params 
if(!exists("samples_tidy")) samples_tidy <- tidy_draws(coda_samples)

# Tidy indiv subject params (or load them if they exist)
if(file.exists(file_name_tidy_samples_subs)){
  load(file_name_tidy_samples_subs)
}else{
  samples_tidy_subs <- map(c("sl_above","sl_below"),
                           ~tidy_sub_params(df=samples_tidy,
                                            param_names=.x)) %>%
                       bind_rows()
  
  save(samples_tidy_subs, 
       file = file_name_tidy_samples_subs)
}

# Tidy group params (or load them if they exist)
if(file.exists(file_name_tidy_samples_group)){
  load(file_name_tidy_samples_group)
}else{
  samples_tidy_group <- tidy_group_params(samples_tidy)
  save(samples_tidy_group, 
       file = file_name_tidy_samples_group)
}

# Visualizing tidy samples ===================================================

label_function <- function(string) paste(string, "the mean")

# scale limits
sc_lims_g <- c(floor(min(samples_tidy_group$value)),
               ceiling(max(samples_tidy_group$value)))

post_plot_group_params <- ggplot(samples_tidy_group, aes(param, value, fill=param))+
  ggdist::stat_halfeye(alpha=.75, point_size=2)+
  scale_y_continuous(limits=sc_lims_g,
                     breaks=seq(sc_lims_g[1],sc_lims_g[2],1))+
  scale_fill_discrete(name="parameter")+
  facet_wrap(vars(stim_space),labeller = labeller(stim_space=label_function))+
  labs(y="value",
       x="parameter",
       title=glue("Group Parameters - Posterior Distributions\nPredictor variable={x_var}"))+
  coord_flip()+
  ggthemes::theme_few()+
  theme(plot.title = element_text(face="plain",
                                  hjust=0.5))
post_plot_group_params

samples_tidy_mean_params_sub <- samples_tidy_subs %>%
  group_by(sub_n, param) %>%
  summarise(mean_val = mean(value))

post_plot_sub_params <- samples_tidy_mean_params_sub %>%
  ggplot()+
  geom_histogram(aes(mean_val),fill="lightblue",binwidth = 0.25,col="black")+
  facet_wrap(vars(param))+
  labs(x="Mean Parameter Value per Subject",
       y="Count",
       title=glue("Mean Slope Values \nSubject Level\nPredictor variable={x_var}"))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(face="plain",hjust=0.5))
post_plot_sub_params

prop_side_better <- samples_tidy_mean_params_sub %>% 
  pivot_wider(names_from = param, values_from = mean_val, id_cols = sub_n) %>%
  ungroup() %>%
  mutate(better_side=case_when(
    sl_above - sl_below > .05 ~ "above",
    sl_below - sl_above > .05 ~ "below",
    TRUE ~ "neither"
  )) %>%
  count(better_side) %>%
  mutate(prop=n/sum(n)) 
prop_side_better

pl_caption_1 <- glue::glue("Dashed line marks equal performance on both sides of the mean.",
                    "\n{prop_side_better$n[1]} subjects had higher above than below slopes.",
                    "\n{prop_side_better$n[2]} subjects had higher below than above slopes.",
                    "\n{prop_side_better$n[3]} subjects had (approximately) equal slopes on either side (diff < .05) .")

samples_tidy_mean_params_sub_wide <- samples_tidy_mean_params_sub %>% 
  pivot_wider(names_from = param, values_from = mean_val, id_cols = sub_n)

# scale limits
sc_lims_s <- c(floor(min(c(samples_tidy_mean_params_sub_wide$sl_above,
                           samples_tidy_mean_params_sub_wide$sl_below))), 
             ceiling(max(c(samples_tidy_mean_params_sub_wide$sl_above,
                              samples_tidy_mean_params_sub_wide$sl_below))))

plot_title <- switch(x_var,
                     "odds"=glue("Mean individual subject slopes\nPredictor variable = Odds of category membership"),
                     "probs"=glue("Mean individual subject slopes\nPredictor variable = Probability of category membership"))

post_plot_mean_params_subs_scatter <-  samples_tidy_mean_params_sub_wide %>%
  ggplot(aes(sl_below,sl_above))+
  geom_point(alpha=.5, col="darkblue",size=2.5)+
  geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.75)+
  coord_fixed(xlim=sc_lims_s, ylim=sc_lims_s)+
  scale_x_continuous(breaks = seq(0,sc_lims_s[2],sc_lims_s[2]/10))+
  scale_y_continuous(breaks = seq(0,sc_lims_s[2],sc_lims_s[2]/10))+
  labs(x="Mean slope\nStimuli below the mean",
       y="Mean slope\nStimuli above the mean",
       title=plot_title,
       caption=pl_caption_1)+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0))
post_plot_mean_params_subs_scatter

save_posterior_plots <- T
if(save_posterior_plots){
  ggsave(post_plot_sub_params,
         filename = glue(here("line_exp_1","mcmc","post_plot_sub_params_{x_var}.png")),
         width = 6, height = 5)
  
  ggsave(post_plot_mean_params_subs_scatter, 
         filename = glue(here("line_exp_1","mcmc","post_plot_scatter_mean_subs_{x_var}.png")),
         width = 6, height = 5)
  
  ggsave(post_plot_group_params,
         filename = glue(here("line_exp_1","mcmc","post_plot_group_params_{x_var}.png")),
         width = 6, height = 5)
}


