rm(list=ls())

library(tidyverse)
library(here)
library(fs)
library(glue)
library(ggsci)
library(patchwork)
library(latex2exp)

# exp 1 ========================================================
exp1_files <- here("modeling","exp1b","model_output") %>%
  dir_ls() %>%
  str_subset(pattern="pred_obs")

exp1_fits <- tibble()
for(i in seq_along(exp1_files)){
  load(exp1_files[i])
  exp1_fits <- bind_rows(exp1_fits,
                         attraction_preds_obs)
  rm(attraction_preds_obs)
}

exp1_fits %>%
  pivot_wider(names_from = source,
              values_from = prop) %>%
  group_by(model) %>%
  summarise(rsq=cor(pred,obs)^2)

exp1_fits_cleaned <- exp1_fits %>% 
  filter(model %in% c("choice_model_c_b_s_max_b_s_max_pn_mle",
                      "choice_model_c_mle"))%>%
  pivot_wider(names_from = source,
              values_from = prop) %>%
  rename(model_version=model,
         data=obs,
         model=pred)
  
exp1_rsq <- exp1_fits_cleaned %>%
  group_by(model_version) %>%
  summarise(r=cor(data,model,use = "pairwise"),
            rsq=cor(data,model,use = "pairwise")^2)

exp1_fit_plot1 <- ggplot(filter(exp1_fits_cleaned,model_version=="choice_model_c_mle"),
                        aes(data,model,col=option))+
  geom_point(size=6,alpha=.4)+
  geom_text(data=filter(exp1_rsq,model_version=="choice_model_c_mle"),x=.25,y=.85,aes(label=round(rsq,digits=2)),
            inherit.aes=F,size=14)+
  geom_abline(slope=1,intercept=0,alpha=.5,linetype="dashed")+
  labs(title="exp 1", subtitle=TeX("$c, w_{a}=.5"))+
  scale_x_continuous(breaks=c(0,1))+
  scale_y_continuous(breaks=c(0,1))+
  scale_color_futurama(name="choice")+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  ggthemes::theme_few()+
  theme(strip.text.y=element_text(angle=0),
        plot.title=element_text(hjust=0.5),
        text=element_text(size=32),
        plot.caption = element_text(hjust=0),
        legend.position = "bottom",
        strip.placement = "inside")

exp1_fit_plot2 <- ggplot(filter(exp1_fits_cleaned,model_version=="choice_model_c_b_s_max_b_s_max_pn_mle"),
                         aes(data,model,col=option))+
  geom_point(size=6,alpha=.4)+
  geom_text(data=filter(exp1_rsq,model_version=="choice_model_c_b_s_max_b_s_max_pn_mle"),x=.25,y=.85,aes(label=round(rsq,digits=2)),
            inherit.aes=F,size=14)+
  geom_abline(slope=1,intercept=0,alpha=.5,linetype="dashed")+
  labs(title="exp 1", subtitle=TeX("$c, w_{a}=.5, b_{sim}, b_{att}"))+
  scale_x_continuous(breaks=c(0,1))+
  scale_y_continuous(breaks=c(0,1))+
  scale_color_futurama(name="choice")+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  ggthemes::theme_few()+
  theme(text=element_text(size=32),
        plot.title=element_text(hjust=0.5),
        plot.caption = element_text(hjust=0),
        legend.position = "bottom")
exp1_fit_plot2
# exp 2 ========================================================
load(here("modeling","exp2","attraction_only_fits_model_data_props.RData"))
exp2_fits <- model_data_props
rm(model_data_props)


exp2_fits_cleaned <- exp2_fits %>%
  filter(model_version=="c * list(, ) * w[a] * {\n    phantom() == phantom()\n} * 0.5 * list(, ) * b[med]"|
           model_version=="c * list(, ) * w[a] * {\n    phantom() == phantom()\n} * 0.5") %>%
  rename(choice=tdc_choice)

exp2_rsq <- exp2_fits_cleaned %>%
  group_by(model_version,tc_angle) %>%
  summarise(r=cor(data,model,use = "pairwise"),
            rsq=cor(data,model,use = "pairwise")^2) %>%
  ungroup() 

exp2_fit_plot1 <- ggplot(filter(exp2_fits_cleaned,
                               model_version=="c * list(, ) * w[a] * {\n    phantom() == phantom()\n} * 0.5 * list(, ) * b[med]"),
                        aes(data,model,col=choice))+
  geom_point(size=6,alpha=.4)+
  geom_text(data=filter(exp2_rsq,model_version=="c * list(, ) * w[a] * {\n    phantom() == phantom()\n} * 0.5 * list(, ) * b[med]"),
            aes(x=.25,y=.85,label=round(rsq,digits=2)),inherit.aes=F,size=14)+
  geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5)+
  labs(title="exp 2", subtitle=TeX("$c, w_{a}=0.5, b_{att}$"))+
  facet_wrap(vars(tc_angle))+
  scale_x_continuous(breaks=c(0,1))+
  scale_y_continuous(breaks=c(0,1))+
  scale_color_futurama(name="choice")+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  ggthemes::theme_few()+
  theme(
        text=element_text(size=32),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust=0),
        legend.position = "bottom")
exp2_fit_plot1

exp2_fit_plot2 <- ggplot(filter(exp2_fits_cleaned,
                                model_version=="c * list(, ) * w[a] * {\n    phantom() == phantom()\n} * 0.5"),
                         aes(data,model,col=choice))+
  geom_point(size=6,alpha=.4)+
  geom_text(data=filter(exp2_rsq,model_version=="c * list(, ) * w[a] * {\n    phantom() == phantom()\n} * 0.5"),
            aes(x=.25,y=.85,label=round(rsq,digits=2)),inherit.aes=F,size=14)+
  geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5)+
  labs(title="exp 2", subtitle=TeX("$c, w_{a}=0.5$"))+
  scale_x_continuous(breaks=c(0,1))+
  scale_y_continuous(breaks=c(0,1))+
  scale_color_futurama(name="choice")+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  facet_wrap(vars(tc_angle))+
  ggthemes::theme_few()+
  theme(text=element_text(size=32),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust=0),
        legend.position = "bottom")
exp2_fit_plot2

design <- "
13
24
"

(exp1_fit_plot1+exp1_fit_plot2+exp2_fit_plot1+exp2_fit_plot2)+
  plot_layout(design = design,
              guides="collect")+
  theme(legend.position = "bottom")
ggsave(filename = here("psychonomics_figures","plots","fit_plots.png"),
       width=25,height=15)
