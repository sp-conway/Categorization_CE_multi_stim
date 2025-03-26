# mpt_repulsion.R
# Sean Conway
# Last modified Nov. 2021

# Setup ==============================================================================
# clear environment
rm(list=ls())

# libraries
library(dplyr)
library(here)
library(purrr)
library(glue)
library(tidyr)
library(ggplot2)

# read in data
transfer_all <- data.table::fread(here("data","exp2","cleaned","dataset_transfer.csv")) %>%
  as_tibble()

# filtered data
attraction <- transfer_all %>%
  filter(rt>100 & rt<30000 & transfer_trial_type=="attraction")

# number of subjects
n_subs <- length(unique(attraction$sub_n))

# function for computing 95% CI of mean
compute_ci <- function(mean,sd, n_subs,side){
  x <- switch(as.character(side),
              "lower"=mean-qt(.025, n_subs-1,lower.tail=F)*(sd/sqrt(n_subs)),
              "upper"=mean+qt(.025, n_subs-1,lower.tail=F)*(sd/sqrt(n_subs))
  )
  return(x)
} 

# save plot?
save_plot <- T

# Data prep ==============================================================================
grouped_props <- attraction %>% 
  filter(transfer_trial_type=="attraction") %>%
  group_by(sub_n,difficulty,attr_choice) %>%
  tally() %>%
  ungroup() %>%
  group_by(sub_n, difficulty) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(difficulty,attr_choice) %>%
  summarise(mean_prop=mean(prop),
            sd_prop=sd(prop),
            ci_lower=compute_ci(mean_prop,sd_prop,n_subs,"lower"),
            ci_upper=compute_ci(mean_prop,sd_prop,n_subs,"upper"),
            n=sum(n)
  ) %>%
  group_by(difficulty) %>%
  arrange(rev(attr_choice)) %>%
  ungroup()

agg_props <- attraction %>%
  group_by(sub_n, attr_choice) %>%
  tally() %>%
  group_by(sub_n) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(attr_choice) %>%
  summarise(mean_prop=mean(prop),
            sd_prop=sd(prop),
            ci_lower=compute_ci(mean_prop,sd_prop,n_subs,"lower"),
            ci_upper=compute_ci(mean_prop,sd_prop,n_subs,"upper"),
            n=sum(n)
  ) %>%
  arrange(rev(attr_choice)) %>%
  ungroup()

agg_counts <- attraction %>%
  group_by(attr_choice) %>%
  tally() %>%
  ungroup() %>%
  mutate(prop=n/sum(n)) %>%
  arrange(rev(attr_choice)) %>%
  select(n) %>%
  as_vector() %>%
  matrix(ncol=3) 

filter_counts <- function(d, diff_lev){
  d %>%
    filter(difficulty==diff_lev) %>%
    select(n) %>% 
    as_vector() %>%
    unname()
}

counts_eq_5_jnd <- filter_counts(grouped_props, "equal_chance_5_JND")
counts_eq_6_jnd <- filter_counts(grouped_props, "equal_chance_6_JND")
counts_equal <- counts_eq_5_jnd+counts_eq_6_jnd
counts_easy <- filter_counts(grouped_props, "easy")
counts_diff <- filter_counts(grouped_props, "difficult")

grouped_counts <- matrix(c(counts_equal,
                           counts_diff,
                           counts_easy),
                         3,3,byrow=T)
grouped_counts



# MPT Repulsion functions ==============================================================================
mpt_repulsion_pred <- function(params){ 
  x <- params[1]
  b <- params[2]
  q <- params[3]
  
  # Model calculations and results -----------------
  t <- abs(x*(1/2+b) + (1-x)*q*1/2*1/2 + (1-x)*(1-q)*1/3) # Proportion target selection
  d <- abs((1-x)*q*1/2*1/2 + (1-x)*(1-q)*1/3) # Proportion decoy selection
  c <- abs(x*(1/2-b) + (1-x)*q*1/2 + (1-x)*(1-q)*1/3) # Proportion competitor selection
  preds <- c(t,d,c)+.01
  preds <- preds/sum(preds)
  return(preds)
}

repulsion_cost <- function(params, counts,q_fixed=F,n_params=3){
  if(q_fixed) params <- c(params, 1)
  pred_prop <- mpt_repulsion_pred(params)
  n_sets <- nrow(counts)
  n_d <- nrow(counts)*ncol(counts)
  ll <- vector("numeric",n_sets)
  for(r in 1:n_sets){
    ll[r] <- dmultinom(counts[r,], size=sum(counts[r,]),pred_prop,log=T)
  }
  n_ll <- -sum(ll)
  bic <- n_params*log(n_d) + 2*n_ll
  return(bic)
}

gen_start_pars <- function(q_fixed=F){
  x <- runif(1,.01,.99)
  b <- runif(1,-.5,.5)
  q <- runif(1,0,1)
  if(q_fixed){
    params <- c(x,b)
  }else{
    params <- c(x,b,q)
  }
  return(params)
}

repulsion_fit <- function(counts,n_fits=500,q_fixed=F,n_params=2,pars_low,pars_high){
  fits <- vector("list",n_fits)
  best_fit_val <- Inf
  best_fit <- NULL
  for(i in 1:n_fits){
    fits[[i]] <- optim(par=gen_start_pars(q_fixed = q_fixed),
                       fn=repulsion_cost,
                       method="L-BFGS-B",
                       lower=pars_low,
                       upper=pars_high,
                       counts=counts,
                       q_fixed=q_fixed,
                       n_params=n_params)
    if(fits[[i]]$value < best_fit_val) {
      best_fit_val <- fits[[i]]$value
      best_fit <- fits[[i]]
    }
  }
  return(list(fits=fits,best_fit=best_fit))
}

# Fitting the model ==============================================================================

# Model specifications
# will q be fixed?
q_fixed <- T

# Fitting aggregate data or data grouped by T/C difficulty?
to_fit <- "agg" # "agg" or "grouped" 

# Establishing parameter bounds
x_bounds <- c(.01,1)
b_bounds <- c(-.5,.5)
q_bounds <- c(.01,.99) # only if q is not fixed
if(q_fixed){
  pars_low <- c(x_bounds[1],b_bounds[1])
  pars_high <- c(x_bounds[2],b_bounds[2])
}else{
  pars_low <- c(x_bounds[1],b_bounds[1],q_bounds[1])
  pars_high <- c(x_bounds[2],b_bounds[2],q_bounds[2])
}

# find number of parameters
n_params <- length(pars_low)

#model object file name
model_file <- here("R","repulsion_model",
                   glue("mpt_repulsion_fit_{to_fit}{qfix}.RData",
                        qfix=ifelse(q_fixed,"_q_fixed","")
                  )
)

#getting count data
counts <- switch(to_fit,
                 "agg"=agg_counts,
                 "grouped"=grouped_counts)

# Model fit
if(file.exists(model_file)){
  load(model_file)
}else{
  model_fit <- repulsion_fit(counts,
                             pars_low = pars_low,
                             pars_high = pars_high,
                             q_fixed=q_fixed,
                             n_params=n_params)
  fits <- model_fit[[1]]
  best_fit <- model_fit[[2]]
  save(best_fit, file=model_file)
}

best_fit
# Plotting ==============================================================================

# Find model parameters
if(q_fixed){
  params <- c(best_fit$par,1)
}else{
  params <- best_fit$par
}

# get observed and predicted in a data frame
if(to_fit=="agg"){
  obs_pred <- agg_props %>%
    arrange(rev(attr_choice)) %>%
    rename(obs=mean_prop) %>%
    mutate(pred=mpt_repulsion_pred(params)) %>%
    pivot_longer(cols = c(obs,pred),
                 names_to = "data",
                 values_to = "prop")
}else if(to_fit=="grouped"){
  eq_props <- attraction %>%
    filter(difficulty=="equal_chance_5_JND"|difficulty=="equal_chance_6_JND") %>%
    group_by(sub_n,attr_choice) %>%
    tally() %>%
    mutate(prop=n/sum(n)) %>%
    group_by(attr_choice) %>%
    summarise(mean_prop=mean(prop),
              sd_prop=sd(prop),
              ci_lower=mean_prop-qt(.025, n_subs-1,lower.tail=F)*(sd_prop/sqrt(n_subs)),
              ci_lower=mean_prop-qt(.025, n_subs-1,lower.tail=F)*(sd_prop/sqrt(n_subs)),
              n=sum(n)
    ) %>%
    mutate(difficulty="equal chance") %>%
    rename(obs=mean_prop) %>%
    arrange(rev(attr_choice)) %>%
    ungroup()
  
  obs_pred <- grouped_props %>%
    filter(difficulty!="equal_chance_5_JND" & difficulty!="equal_chance_6_JND") %>%
    arrange(difficulty) %>%
    rename(obs=mean_prop) %>%
    bind_rows(., eq_props) %>%
    mutate(pred=rep(mpt_repulsion_pred(params),3)) %>%
    pivot_longer(cols = c(obs,pred),
                 names_to = "data",
                 values_to = "prop")
}

# labels for plot
if(q_fixed){
  pl_subtitle <- glue("d = {x},b = {b}, g fixed at 1.\nBIC = {bic}.\nN={n_subs}",
                      x=round(best_fit$par[1],digits=2),
                      b=round(best_fit$par[2],digits=2),
                      bic=round(best_fit$value,digits=2))
}else{
  pl_subtitle <- glue("x={x}, b={b}, q={q}.\nBIC={bic}.",
                      x=round(best_fit$par[1],digits=2),
                      b=round(best_fit$par[2],digits=2),
                      q=round(best_fit$par[3],digits=2),
                      bic=round(best_fit$value,digits=2))
}

# Plotting observed and predicted choice proportions
obs_preds_plot <- obs_pred %>%
  mutate(attr_choice=str_replace_all(attr_choice,c("target"="T","competitor"="C","decoy"="D"))) %>%
  ggplot(aes(attr_choice,prop,col=data))+
  geom_point(aes(shape=data),size=8,alpha=.8)+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.1))+
  scale_shape_manual(values=c("O","X"))+
  labs(x="choice",
       y="proportion",
       title="experiment 2 - t/c/d choices",
       subtitle = pl_subtitle)+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        axis.title=element_text(size=15),
        axis.text.x=element_text(size=12))

if(to_fit=="grouped") obs_preds_plot <- obs_preds_plot + facet_wrap(vars(difficulty))

# Saving plot 
if(save_plot){
  ggsave(here("R","repulsion_model",glue("{to_fit}_props_obs_preds_plot{qfix}.png",
                                         qfix=ifelse(q_fixed,"_q_fixed",""))),
         obs_preds_plot,
         width=6,
         height=6,
         units="in")
  
}



