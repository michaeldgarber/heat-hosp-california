#This script reads in Maren's data
#February 7th, 2023

library(readr)
library(readxl)
library(here)
library(tidyverse)
setwd(here("data-processed"))
load("zcta_ca_list.RData")

# Read data------
zcta_ca_list
setwd(here("data-input"))
hosp_ratio = read_csv("HospRatio.csv") %>% 
  #Rename from ZIP to zcta for easy linking with other data
  rename(zcta = ZIP) %>% 
  mutate(ratio_diff = "ratio")

hosp_ratio

hosp_diff = read_csv("AbsoluteBenefit.csv") %>% 
  rename(zcta = ZIP) %>% 
  mutate(ratio_diff = "diff")
  
hosp_both = hosp_ratio %>% 
  bind_rows(hosp_diff)

setwd(here("data-processed"))
save(hosp_both, file = "hosp_both.RData")

# Make data longer-----
#To make it easier to write dynamic code for visualizations
hosp_both_long = hosp_both %>% 
  pivot_longer(
    cols = c(starts_with("A"), starts_with("B")),#key is using c()
    values_to = "value",
    names_to = "scenario") %>% 
  as_tibble() 

setwd(here("data-processed"))
save(hosp_both_long, file = "hosp_both_long.RData")

hosp_both_long %>% 
  filter(ratio_diff=="ratio") %>% 
  summary()

hosp_both_long %>% 
  filter(ratio_diff=="diff") %>% 
  summary()

# Create categories for ratios and differences--------
hosp_ratio_long = hosp_both_long %>% 
  filter(ratio_diff=="ratio") %>% 
  mutate(
    value_cat = 
      cut(
          value,
          include.lowest=TRUE,
          breaks=c(
            min(value,na.rm=TRUE),
            #see below distribution which informed this
            #Note that the usual quantile() approach didn't work
            #because min and 25th percentile were often the same value...not unique breaks
            0.005,
            0.01,
            0.05,
            .1,
            .15,
            .2,
            1,
            10,
            100,
            max(value,na.rm=TRUE)
          )))
  
table(hosp_ratio_long$value_cat)

hosp_diff_long = hosp_both_long %>% 
  filter(ratio_diff=="diff") %>% 
  mutate(
    value_cat = 
      cut(
        #see below distribution
        value,
        include.lowest=TRUE,
        breaks=c(
          min(value,na.rm=TRUE),
          0.0001,
          0.0002,
          0.0004,
          0.001,
          0.002,
          0.01,
          max(value,na.rm=TRUE)
        )
      )
  )

