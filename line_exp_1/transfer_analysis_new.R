# transfer_analysis.R
# analyzing transfer trials from line categorization experiment 1
# Sean Conway
# Last modified June 2024

# SETUP ======================================================
# clear environment 
rm(list=ls())

# libraries
library(here)
library(tidyverse)
library(scales)

# attraction effect trial specs
attr_trial_specs_long <- read_csv(here("line_exp_1","trials","attr_eff_trials_long.csv")) 

# read in the transfer data
# and get some more columns ready for analysis
transfer <- here("line_exp_1","data","cleaned","dataset_transfer.csv") %>%
  read_csv() 

attraction <- transfer %>%
  filter(str_detect(transfer_trial_type,"attraction")) %>%
  mutate(choice=case_when(
    attr_choice=="competitor" & competitor_side=="left"~"s",
    attr_choice=="competitor" & competitor_side=="right"~"l",
    attr_choice=="target" & target_side=="left"~"s",
    attr_choice=="target" & target_side=="right"~"l",
    attr_choice=="decoy"~"d"
  ),
  set=case_when(
    competitor_side=="left"~"[s,l,dl]",
    competitor_side=="right"~"[s,l,ds]"
  ),
  side_correct=str_replace_all(side_correct,
                               c("left"="short",
                               "right"="long",
                               "equal"="equal"))) %>%
  mutate(across(c(choice,set,side_correct),as.factor))

# analysis trinary - trinary analysis ====================================================================================
tri_diff1 <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  group_by(sub_n, side_correct,set, choice) %>%
  summarise(n=n()) %>%
  group_by(sub_n, side_correct,set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = c(set, choice), values_from = prop,
              values_fill = 0,
              names_sep = "_") %>%
  mutate(s_diff=`[s,l,ds]_s`-`[s,l,dl]_s`,
         l_diff=`[s,l,dl]_l`-`[s,l,ds]_l`) %>%
  select(sub_n,side_correct,s_diff,l_diff)

tridiff1_avg <- tri_diff1 %>%
  group_by(side_correct) %>%
  summarise(sdiff_mean=mean(s_diff),
            sdiff_se = sd(s_diff)/sqrt(n()-1),
            ldiff_mean=mean(l_diff),
            ldiff_se = sd(l_diff)/sqrt(n()-1),
            sdiff_cilower=sdiff_mean-qt(.025,n()-1,lower.tail = F)*sdiff_se,
            sdiff_ciupper=sdiff_mean+qt(.025,n()-1,lower.tail = F)*sdiff_se,
            ldiff_cilower=ldiff_mean-qt(.025,n()-1,lower.tail = F)*ldiff_se,
            ldiff_ciupper=ldiff_mean+qt(.025,n()-1,lower.tail = F)*ldiff_se) %>%
  ungroup() %>%
  select(-contains("se")) %>%
  pivot_longer(-side_correct) %>%
  separate(name,into=c("effect","stat"),sep="_") %>%
  pivot_wider(names_from = stat, values_from=value) %>%
  mutate(effect=case_when(
    str_detect(effect,"sdiff")~"short effect",
    str_detect(effect,"ldiff")~"long effect",
  ))
tridiff1_avg %>%
  ggplot(aes(effect,mean))+
  geom_col(position="dodge",fill="lightblue",col="black")+
  geom_errorbar(aes(ymin=cilower,ymax=ciupper),width=.2)+
  facet_wrap(vars(side_correct),labeller = label_both)+
  ggthemes::theme_few()+
  theme(text=element_text(size=10))
ggsave(here("line_exp_1","plots","tri_effect_by_sidecorrect_avg.jpg"),
       width=5,height=4)

tri_diff1 %>%
  pivot_longer(-c(sub_n,side_correct), values_to = "effect") %>%
  mutate(name=str_replace_all(name,c("s_diff"="short effect","l_diff"="long effect"))) %>%
  ggplot(aes(effect))+
  geom_histogram(fill="lightblue",col="black")+
  facet_grid(side_correct~name)+
  ggthemes::theme_few()
ggsave(here("line_exp_1","plots","tri_effect_by_sidecorrect_hist.jpg"),
       width=5,height=4)

tri_diff2 <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  group_by(sub_n, set, choice) %>%
  summarise(n=n()) %>%
  group_by(sub_n,set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = c(set, choice), values_from = prop,
              values_fill = 0,
              names_sep = "_") %>%
  mutate(s_diff=`[s,l,ds]_s`-`[s,l,dl]_s`,
         l_diff=`[s,l,dl]_l`-`[s,l,ds]_l`) %>%
  select(sub_n,s_diff,l_diff) 

tridiff2_avg <- tri_diff2 %>%
  summarise(sdiff_mean=mean(s_diff),
            sdiff_se = sd(s_diff)/sqrt(n()-1),
            ldiff_mean=mean(l_diff),
            ldiff_se = sd(l_diff)/sqrt(n()-1),
            sdiff_cilower=sdiff_mean-qt(.025,n()-1,lower.tail = F)*sdiff_se,
            sdiff_ciupper=sdiff_mean+qt(.025,n()-1,lower.tail = F)*sdiff_se,
            ldiff_cilower=ldiff_mean-qt(.025,n()-1,lower.tail = F)*ldiff_se,
            ldiff_ciupper=ldiff_mean+qt(.025,n()-1,lower.tail = F)*ldiff_se) %>%
  select(-contains("se")) %>%
  pivot_longer(everything()) %>%
  separate(name,into=c("effect","stat"),sep="_") %>%
  pivot_wider(names_from = stat, values_from=value) %>%
  mutate(effect=case_when(
    str_detect(effect,"sdiff")~"short effect",
    str_detect(effect,"ldiff")~"long effect",
  ))
tri_diff2 %>%
  pivot_longer(-sub_n, values_to = "effect") %>%
  mutate(name=str_replace_all(name,c("s_diff"="short effect","l_diff"="long effect"))) %>%
  ggplot(aes(effect))+
  geom_histogram(fill="lightblue",col="black")+
  facet_wrap(vars(name))+
  ggthemes::theme_few()
ggsave(here("line_exp_1","plots","tri_effect_collapsed_hist.jpg"),
       width=5,height=4)
tridiff2_avg %>%
  ggplot(aes(effect,mean))+
  geom_col(position="dodge",fill="lightblue",col="black")+
  geom_errorbar(aes(ymin=cilower,ymax=ciupper),width=.2)+
  ggthemes::theme_few()
ggsave(here("line_exp_1","plots","tri_effect_collapsed_avg.jpg"),
       width=5,height=4)

# position analysis - per andrew ============================================================
attraction_order <- attraction %>%
  filter(trial_choice_set=="trinary") %>%
  mutate(choice_tdc=case_when(
    choice=="l" & set=="[s,l,dl]"~"t",
    choice=="s" & set=="[s,l,dl]"~"c",
    choice=="l" & set=="[s,l,ds]"~"c",
    choice=="s" & set=="[s,l,ds]"~"t",
    choice=="d"~"d"
  ),
  line1_tdc=case_when(
    target_line==line_1~"t",
    competitor_line==line_1~"c",
    decoy_line==line_1~"d"
  ),
  line2_tdc=case_when(
    target_line==line_2~"t",
    competitor_line==line_2~"c",
    decoy_line==line_2~"d"
  ),
  line3_tdc=case_when(
    target_line==line_3~"t",
    competitor_line==line_3~"c",
    decoy_line==line_3~"d"
  ),
  line_order_tdc=str_c(line1_tdc,line2_tdc,line3_tdc))

attraction_order_mprop <- attraction_order %>%
  group_by(sub_n, set, line_order_tdc, choice_tdc) %>%
  summarise(n=n()) %>%
  group_by(sub_n,set,line_order_tdc,) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(set,line_order_tdc, choice_tdc) %>%
  summarise(mprop=mean(prop),
            se=sd(prop)/sqrt(n()),
            lower=mprop-se,
            upper=mprop+se) %>%
  ungroup()
attraction_order_mprop %>%
  ggplot(aes(choice_tdc,mprop))+
  geom_col(position="dodge",fill="lightblue",col="black")+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.25)+
  facet_grid(set~line_order_tdc)+
  ggthemes::theme_few()
ggsave(here("line_exp_1","plots","tdc_choice_by_order.jpg"),
       width=5,height=4)
