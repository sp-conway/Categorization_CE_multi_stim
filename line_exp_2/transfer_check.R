# transfer_check.R
# analyzing transfer data from 2nd line categorization experiment
# double checking t/d/c stuff
# Sean Conway
# Last modified Mar. 2023

# Setup ==============================================================================
# clear environment
rm(list=ls())

# libraries
library(tidyverse)
library(here)
library(BayesFactor)
library(glue)
library(patchwork)
library(tidybayes)

# Source plotting functions
source(here("line_exp_2","functions","plotting_functions.R"))
source(here("line_exp_2","functions","density_functions_2.R"))

# read in data
transfer <- data.table::fread(here("line_exp_2","data","cleaned","dataset_transfer.csv")) %>%
  as_tibble() 

attraction_bi <- transfer %>% 
  filter(transfer_trial_type=="attraction" & trial_choice_set=="binary")
attraction_tri <- transfer %>% 
  filter(transfer_trial_type=="attraction" & trial_choice_set=="trinary")

table(attraction_bi$attr_choice)
table(attraction_tri$attr_choice)

binary_lines <- matrix(c(attraction_bi$line_1_in_order, 
                         attraction_bi$line_2_in_order),
                       ncol=2)
binary_choices <- attraction_bi$choice
trinary_lines <- matrix(c(attraction_tri$line_1_in_order, 
                          attraction_tri$line_2_in_order,
                          attraction_tri$line_3_in_order),
                       ncol=3)
trinary_choices <- attraction_tri$choice
tdc_choices_bi <- character(nrow(binary_lines))
for(i in 1:nrow(binary_lines)){
  if(attraction_bi$target_side[i]=="right"){
    if(binary_choices[i]==min(binary_lines[i,])){
      tdc_choices_bi[i] <- "competitor"
    }else if(binary_choices[i]==max(binary_lines[i,])){
      tdc_choices_bi[i] <- "target"
    }
  }else if(attraction_bi$target_side[i]=="left"){
    if(binary_choices[i]==min(binary_lines[i,])){
      tdc_choices_bi[i] <- "target"
    }else if(binary_choices[i]==max(binary_lines[i,])){
      tdc_choices_bi[i] <- "competitor"
    }
  }
}
tdc_choices_tri <- character(nrow(trinary_lines))
for(i in 1:nrow(trinary_lines)){
  if(attraction_tri$target_side[i]=="right"){
    if(trinary_choices[i]==min(trinary_lines[i,])){
      tdc_choices_tri[i] <- "competitor"
    }else if(trinary_choices[i]==max(trinary_lines[i,])){
      tdc_choices_tri[i] <- "target"
    }else if(trinary_choices[i]==median(trinary_lines[i,])){
      tdc_choices_tri[i] <- "decoy"
    }
  }else if(attraction_tri$target_side[i]=="left"){
    if(trinary_choices[i]==min(trinary_lines[i,])){
      tdc_choices_tri[i] <- "target"
    }else if(trinary_choices[i]==max(trinary_lines[i,])){
      tdc_choices_tri[i] <- "competitor"
    }else if(trinary_choices[i]==median(trinary_lines[i,])){
      tdc_choices_tri[i] <- "decoy"
    }
  }
}

table(tdc_choices_bi)
table(tdc_choices_tri)


tdc_choices_tri_2 <- character(nrow(trinary_lines))
for(i in 1:nrow(trinary_lines)){
  if(abs(max(trinary_lines[i,])-median(trinary_lines[i,])) > abs(min(trinary_lines[i,])-median(trinary_lines[i,]))){
    if(trinary_choices[i]==max(trinary_lines[i,])){
      tdc_choices_tri_2[i] <- "competitor"
    }else if(trinary_choices[i]==min(trinary_lines[i,])){
      tdc_choices_tri_2[i] <- "target"
    }else if(trinary_choices[i]==median(trinary_lines[i,])){
      tdc_choices_tri_2[i] <- "decoy"
    }
  }else if(abs(min(trinary_lines[i,])-median(trinary_lines[i,])) > abs(max(trinary_lines[i,])-median(trinary_lines[i,]))){
    if(trinary_choices[i]==max(trinary_lines[i,])){
      tdc_choices_tri_2[i] <- "target"
    }else if(trinary_choices[i]==min(trinary_lines[i,])){
      tdc_choices_tri_2[i] <- "competitor"
    }else if(trinary_choices[i]==median(trinary_lines[i,])){
      tdc_choices_tri_2[i] <- "decoy"
    }
  }
}
table(tdc_choices_tri_2)
table(tdc_choices_tri)



