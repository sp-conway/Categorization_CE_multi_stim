
library(tidyverse)
rm(list=ls())
transfer <- read_csv(here("line_exp_1","data","cleaned","dataset_transfer.csv")) %>% 
  filter(rt >= 100) %>% # for obvious reasons
  rowwise() %>% # This makes it take a long time to run bc of the number of rows
  # Should probably substitute w/ apply statement
  # Though this code is easy to read?
  mutate(median_line=median(c(line_1, line_2, line_3),na.rm=T),
         max_line=max(c(line_1, line_2, line_3), na.rm=T),
         min_line=min(c(line_1, line_2, line_3),na.rm=T)) %>%
  ungroup() %>%
  mutate(
    choice_by_rel_val=
      case_when(
        line_choice==median_line~"median",
        line_choice==min_line~"min",
        line_choice==max_line~"max"
      ),
    corr_choice_by_rel_val=
      case_when(
        corr_choice==median_line~"median",
        corr_choice==min_line~"min",
        corr_choice==max_line~"max"
      )
  )
