rm(list=ls())
library(here)
library(tidyverse)
library(latex2exp)
library(patchwork)

# read data ========================================================
exp1b <- here("exp1b","data","cleaned","dataset_transfer.csv") %>%
  read_csv()

exp1b_attraction <- exp1b %>%
  filter(transfer_trial_type=="attraction") %>%
  rename(target_type=target_side) %>%
  mutate(target_type=case_when(
    target_type=="left"~"short",
    target_type=="right"~"long"))

nsubs_exp1b <- length(unique(exp1b$sub_n))

exp2 <- here("exp2","data","cleaned","exp2_transfer.csv") %>%
  read_csv()

exp2_attraction <- exp2 %>%
  filter(transfer_trial_type=="attraction")
nsubs_exp2 <- length(unique(exp2$sub_n))

ncond <- 2

# analysis ================================================================
exp1_drst <- exp1b_attraction %>%
  mutate(across(c(sub_n,trial_choice_set,attr_choice,target_type),as.factor)) %>%
  count(sub_n,trial_choice_set,attr_choice,.drop=F) %>%
  group_by(sub_n,trial_choice_set) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  filter(attr_choice!="decoy") %>%
  mutate(attr_choice=str_sub(attr_choice,1,1)) %>%
  pivot_wider(names_from = attr_choice,
              values_from = prop) %>%
  mutate(rst=t/(t+c)) %>%
  select(-c(t,c)) %>%
  pivot_wider(names_from = trial_choice_set,
              values_from = rst) %>%
  mutate(drst=trinary-binary)  %>%
  mutate(exp="exp 1")

exp1_mdrst <- exp1_drst %>%
  summarise(m_drst=mean(drst),
            s_drst=sd(drst),
            ci_lower=m_drst-qt(.025,nsubs_exp1b-1)*(s_drst/sqrt(nsubs_exp1b)),
            ci_upper=m_drst+qt(.025,nsubs_exp1b-1)*(s_drst/sqrt(nsubs_exp1b))) %>%
  ungroup() %>%
  mutate(exp="exp 1")

exp2_drst <- exp2_attraction %>%
  mutate(tdc_a=case_when(
    tdc_a==TRUE~"t"),
  tdc_choice=case_when(
    choice=="a" ~ tdc_a,
    choice=="b" ~ tdc_b,
    choice=="c" ~ tdc_c
  ),
  across(c(sub_n,choice_set,tc_angle,tdc_choice),as.factor)) %>%
  count(sub_n,choice_set,tc_angle,tdc_choice,.drop=F) %>%
  group_by(sub_n,choice_set,tc_angle) %>%
  mutate(prop=n/sum(n)) %>%
  select(-n) %>%
  filter(tdc_choice!="d") %>%
  pivot_wider(names_from = tdc_choice,
              values_from = prop) %>%
  mutate(rst=t/(t+c)) %>%
  select(-c(t,c)) %>%
  pivot_wider(names_from = choice_set,
              values_from = rst) %>%
  mutate(drst=trinary-binary) %>%
  rename(target_type=tc_angle) %>%
  mutate(exp="exp 2")

exp2_mdrst <- exp2_drst %>%
  group_by(sub_n) %>%
  mutate(corr_drst=drst-(1/nsubs_exp2)*sum(drst)+(1/(nsubs_exp2*ncond))*sum(exp2_drst$drst)) %>%
  group_by(target_type) %>%
  summarise(m_drst=mean(drst),
            s_drst=sqrt(var(corr_drst)*((ncond-1)/ncond)),
            ci_lower=m_drst-qt(.025,nsubs_exp2-1)*(s_drst/sqrt(nsubs_exp2)),
            ci_upper=m_drst+qt(.025,nsubs_exp2-1)*(s_drst/sqrt(nsubs_exp2))) %>%
  ungroup()  %>%
  mutate(exp="exp 2")
# 
# bind_rows(exp1_mdrst,
#           exp2_mdrst) %>%
#   ggplot(aes(target_type,m_drst))+
#   geom_col(fill="#999999")+
#   geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.35)+
#   geom_hline(yintercept=0,linetype="dashed")+
#   facet_wrap(vars(exp),scales = "free_x")+
#   labs(x="target type",
#        y=TeX("\\Delta RST"),
#        caption="Error bars represent 95% CIs.")+
#   ggthemes::theme_few()+
#   theme(text=element_text(size=23),
#         plot.caption=element_text(size=10,hjust=0))
# ggsave(here("psychonomics_figures","plots","mdrst.pdf"),
#        width=5,height=5)

exp1_drst %>%
  ggplot(aes(drst))+
  geom_histogram(fill="#999999",bins=15,col="black")+
  # geom_vline(data=bind_rows(exp1_mdrst,
  #                           exp2_mdrst),
  #            aes(xintercept=m_drst),col="purple",size=1)+
  geom_vline(data=exp1_mdrst,
             aes(xintercept=ci_lower),col="purple",
             size=1)+
  geom_vline(data=exp1_mdrst,
             aes(xintercept=ci_upper),col="purple",
             size=1)+
  #facet_wrap(target_type~exp)+
  scale_x_continuous(limits=c(-.5,.5),breaks=c(-.5,0,.5))+
  #scale_y_continuous(breaks=c(0,10,25))+
  labs(x=TeX("\\Delta RST"))+
       # subtitle=TeX("$RST = \\frac{P(T)}{P(T)+P(C)}$,  $\\Delta RST = RST_{trinary} - RST_{binary}$",
       #             bold=F),
       # caption="Purple lines mark 95% CI boundaries.")+
  ggthemes::theme_few()+
  theme(text=element_text(size=38))
ggsave(here("psychonomics_figures","plots","exp1_drst_hist.pdf"),
       width=6,height=5)

exp2_drst %>%
  ggplot(aes(drst))+
  geom_histogram(fill="#999999",bins=15,col="black")+
  # geom_vline(data=bind_rows(exp1_mdrst,
  #                           exp2_mdrst),
  #            aes(xintercept=m_drst),col="purple",size=1)+
  geom_vline(data=exp2_mdrst,
             aes(xintercept=ci_lower),col="purple",
             size=1)+
  geom_vline(data=exp2_mdrst,
             aes(xintercept=ci_upper),col="purple",
             size=1)+
  facet_grid(vars(target_type))+
  scale_x_continuous(limits=c(-.5,.5),breaks=c(-.5,0,.5))+
  #scale_y_continuous(breaks=c(0,10,25))+
  labs(x=TeX("\\Delta RST"))+
  # subtitle=TeX("$RST = \\frac{P(T)}{P(T)+P(C)}$,  $\\Delta RST = RST_{trinary} - RST_{binary}$",
  #             bold=F),
  # caption="Purple lines mark 95% CI boundaries.")+
  ggthemes::theme_few()+
  theme(text=element_text(size=38))
ggsave(here("psychonomics_figures","plots","exp2_drst_hist.pdf"),
       width=5,height=6)



exp1_mean_props <- exp1b_attraction %>%
  mutate(across(c(sub_n,trial_choice_set,attr_choice),as.factor)) %>%
  count(sub_n,trial_choice_set,attr_choice,.drop=F) %>%
  group_by(sub_n,trial_choice_set) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(trial_choice_set,attr_choice) %>%
  summarise(mprop=mean(prop),
            sprop=sd(prop),
            ci_lower=mprop-qt(.025,nsubs_exp1b-1)*(sprop/sqrt(nsubs_exp1b)),
            ci_upper=mprop+qt(.025,nsubs_exp1b-1)*(sprop/sqrt(nsubs_exp1b))) %>%
  ungroup() %>%
  mutate(attr_choice=str_sub(attr_choice,1,1)) %>%
  rename(tdc_choice=attr_choice,
         choice_set=trial_choice_set)


exp2_mean_props <- exp2_attraction %>%
  mutate(tdc_a=case_when(
    tdc_a==TRUE~"t"),
    tdc_choice=case_when(
      choice=="a" ~ tdc_a,
      choice=="b" ~ tdc_b,
      choice=="c" ~ tdc_c
    ),
    across(c(sub_n,choice_set,tc_angle,tdc_choice),as.factor)) %>%
  count(sub_n,choice_set,tc_angle,tdc_choice,.drop=F) %>%
  group_by(sub_n,choice_set,tc_angle) %>%
  mutate(prop=n/sum(n)) %>%
  group_by(choice_set,tc_angle,tdc_choice) %>%
  summarise(mprop=mean(prop),
                      sprop=sd(prop),
                      ci_lower=mprop-qt(.025,nsubs_exp2-1)*(sprop/sqrt(nsubs_exp2)),
                      ci_upper=mprop+qt(.025,nsubs_exp2-1)*(sprop/sqrt(nsubs_exp2))) %>%
  ungroup() %>%
  rename(target_type=tc_angle)

exp1_mean_props %>%
  mutate(tdc_choice=factor(tdc_choice,levels=c("t","c","d"),ordered=T)) %>%
  filter((choice_set=="trinary")|(choice_set=="binary" & tdc_choice!="d")) %>%
  ggplot(aes(tdc_choice,mprop,fill=choice_set))+
  geom_col(position = position_dodge2(preserve="single"))+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2,position = position_dodge(0.9))+
  scale_fill_tron(name="choice set")+
  scale_y_continuous(limits=c(0,.6))+
  labs(x="choice",y="mean prop")+
  ggthemes::theme_few()+
  theme(text=element_text(size=38),
        plot.caption=element_text(size=10,hjust=0),
        legend.position = "none")

ggsave(filename = here("psychonomics_figures","plots","tdc_exp1.png"),width=,height=5)

exp2_mean_props %>%
  mutate(tdc_choice=factor(tdc_choice,levels=c("t","c","d"),ordered=T)) %>%
  filter((choice_set=="trinary")|(choice_set=="binary" & tdc_choice!="d")) %>%
  ggplot(aes(tdc_choice,mprop,fill=choice_set))+
  geom_col(position = position_dodge2(preserve = "single"))+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2,position = position_dodge(0.9))+
  facet_grid(~target_type,scales="free_x")+
  scale_fill_tron(name="choice set")+
  scale_y_continuous(limits=c(0,.6))+
  labs(x="choice",y="mean prop")+
  ggthemes::theme_few()+
  theme(text=element_text(size=38),
        plot.caption=element_text(size=10,hjust=0),
        plot.title = element_text(hjust=0.5),
        legend.position = "left")

ggsave(filename = here("psychonomics_figures","plots","tdc_exp2.png"),width=7,height=5)

