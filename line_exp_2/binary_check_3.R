library(tidyverse)
library(here)

transfer_trials <- read_csv(here("line_exp_2","trials","transfer_trials_1b.csv"))

attraction_binary <- transfer_trials %>%
  filter(transfer_trial_type=="attraction" & trial_choice_set=="binary")

attraction_binary %>%
  pivot_longer(cols=c(target_phy,
                      competitor_phy),
               names_to="option",
               values_to="length") %>%
  mutate(option=str_replace(option,"_phy","")) %>%
  ggplot(aes(length))+
  geom_histogram(col="black",fill="lightblue")+
  facet_grid(option~.)+
  ggthemes::theme_few()
