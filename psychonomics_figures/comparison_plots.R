rm(list=ls())
library(tidyverse)
library(here)
library(latex2exp)

# exp 1 ======================================================================
models <- list.files(here("modeling","exp1b","model_output")) %>%
  str_subset(., ".RData")

for (model in models){
  load(here("modeling","exp1b","model_output",model))
  assign(str_replace_all(paste0(model,"_best_vals"), 
                         c("choice_model_fits_"="",".RData"="")),
         best_vals)
  assign(str_replace_all(paste0(model,"_best_params"), 
                         c("choice_model_fits_"="",".RData"="")),
         best_params)
}
rm(list=c("clust","d","d_s","exemplars","fits","params_set",
          "params_start","stimuli","f","filename","fix_b1","fix_b2","fix_b3",
          "fix_b4","fix_k","i","model","models","n_fits","n_params",
          "n_sub","n_trials","no_cores","p","param_names",
          "params_hi","params_hi_all","params_lo","params_lo_all",
          "run_fit","s","s_ind","show_plots","sub_num","best_vals"))

bic_all <- tibble(
  sub_num=sub_nums_all,
  c=c_best_vals,
  c_bu=c_b_u_best_vals,
  c_bu_bsmax=c_b_u_b_s_max_best_vals,
  c_bu_bsmaxpn=c_b_u_b_s_max_pn_best_vals,
  c_bsmax_bsmaxpn=c_b_s_max_b_s_max_pn_best_vals,
  c_bu_bsmax_bsmaxpn=c_b_u_b_s_max_b_s_max_pn_best_vals,
  c_bsmax_bsmaxpn_bu_0=c_b_s_max_b_s_max_pn_bu_0_best_vals,
  c_bsmaxpn_bu_0=c_b_s_max_pn_bu_0_best_vals,
  c_bsmax_bu_0=c_b_s_max_bu_0_best_vals
) 
bic_all$preferred <- colnames(bic_all)[-1][apply(bic_all[,-1], 1, which.min)] 
# bic_all$preferred <- as.factor(bic_all$preferred)


bic_all %>%
  count(preferred) %>%
  arrange(n)

bic_bplot_1 <- bic_all %>%
  rowwise() %>%
  mutate(preferred=case_when(
    preferred=="c"~"$c, b_{u}=1$",
    preferred=="c_bu"~"$c, b_{u}$",
    preferred=="c_bu_bsmax"~"$c, b_{u}, b_{sim}$",
    preferred=="c_bu_bsmaxpn"~"$c, b_{u}, b_{att}$",
    preferred=="c_bu_bsmax_bsmaxpn"~"$c, b_{u}, b_{sim}, b_{att}$",
    preferred=="c_bsmax_bsmaxpn"~"$c, b_{u}=1, b_{sim}, b_{att}$",
    preferred=="c_bsmax_bsmaxpn_bu_0"~"$c, b_{u}=0, b_{sim}, b_{att}$",
    preferred=="c_bsmaxpn_bu_0"~"$c, b_{u}=0, b_{att}$",
    preferred=="c_bsmax_bu_0"~"$c, b_{u}=0, b_{sim}$"
  )) %>%
  ungroup() %>%
  mutate(preferred=factor(preferred,levels=c("$c, b_{u}=1$",
                                             "$c, b_{u}$",
                                             "$c, b_{u}, b_{sim}$",
                                             "$c, b_{u}, b_{att}$",
                                             "$c, b_{u}, b_{sim}, b_{att}$",
                                             "$c, b_{u}=1, b_{sim}, b_{att}$",
                                             "$c, b_{u}=0, b_{sim}, b_{att}$",
                                             "$c, b_{u}=0, b_{att}$",
                                             "$c, b_{u}=0, b_{sim}$"),
                          ordered=T)) %>%
  count(preferred,.drop=F) %>%
  ggplot(aes(reorder(preferred,n),n))+
  scale_x_discrete(drop=F,labels=TeX)+
  scale_y_continuous(breaks=c(0,30,60),limits = c(0,60))+
  geom_col(fill="#C0C0C0")+
  coord_flip()+
  labs(x="model",
       y="n ppts best fit")+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(size=32))
bic_bplot_1

# exp 2  ============================================================
dirs <- map_chr(c("exp2_fits_fixed_c_free_attraction_all",
                  "exp2_fits_fixed_c_w_free_attraction_all",
                  "exp2_fits_fixed_c_w_bmed_free_attraction_all",
                  "exp2_fits_fixed_c_w_bmed180_bmed90_free_attraction_split",
                  "exp2_fits_fixed_c_bmed_free_attraction_all"),
                ~here("modeling","exp2",.x))

load_all_fits <- function(d){
  f <- dir_ls(d) %>%
    str_subset(pattern=".RData")
  load_fit <- function(ff){
    load(ff)
    tibble(
      sub_n=cur_sub,
      model=str_replace_all(ff,c("exp2_choice_model_fit_sub[:digit:]{1,3}_"="",
                                 "exp_2_fits_fixed"="",
                                 "_attraction_all.RData"="",
                                 "_attraction_split.RData"="",
                                 "c_free_attraction_all/"="",
                                 "c_w_free_attraction_all/"="",
                                 "c_w_bmed_free_attraction_all/"="",
                                 "c_w_bmed180_bmed90_free_attraction_split/"="",
                                 "c_bmed_free_attraction_all/"="",
                                 "/Users/seanconway/Github/categorization_choice_modeling/modeling/exp2/exp2_fits_fixed_"="")),
      bic=best_fit_val,
      par_name=names(best_fit$par),
      par_val=best_fit$par,
    )
  }
  map_dfr(f,load_fit)
}

all_fits <- map_dfr(dirs, load_all_fits)

bic_bplot_2 <- all_fits %>%
  distinct(sub_n,model,bic) %>%
  pivot_wider(names_from = model,
              values_from = bic) %>%
  rowwise() %>%
  mutate(min=min(c(c_free,c_w_free,c_w_bmed_free,c_w_bmed180_bmed90_free,c_bmed_free),na.rm = T),
         preferred=case_when(
           min==c_free~"$c, w_{a}=0.5$",
           min==c_w_free~"$c, w_{a}$",
           min==c_w_bmed_free~"$c, w_{a}, b_{att}$",
           min==c_w_bmed180_bmed90_free~"$c, w_{a}, b_{att 180},b_{att 90}$",
           min==c_bmed_free~"$c,  w_{a}=0.5, b_{att}$"
         )) %>%
  ungroup() %>%
  mutate(preferred=as.factor(preferred)) %>%
  count(preferred,.drop=F) %>%
  ggplot(aes(reorder(preferred,n),n))+
  scale_x_discrete(drop=F,labels=TeX)+
  scale_y_continuous(breaks=c(0,30,60),limits = c(0,60))+
  geom_col(fill="#C0C0C0")+
  coord_flip()+
  labs(x="model",
       y="n ppts best fit")+
  ggthemes::theme_few()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(size=32))
(bic_bplot_1|bic_bplot_2)

ggsave(filename=here("psychonomics_figures","plots","bic_bplot.png"),
       width=12,height=5)
