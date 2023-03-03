#This script reads in Maren's data
#Revised Mar 3, 2023

library(readr)
library(readxl)
library(here)
library(tidyverse)
setwd(here("data-processed"))
load("zcta_ca_list.RData")

# Read data------
zcta_ca_list
setwd(here("data-input"))
#Maren revised filename Mar 3, 2023 from HospRatio.csv to StandardizedHosp.csv
hosp_ratio = read_csv("StandardizedHosp.csv") %>% 
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
#Limit to about 5 bins
.65/3
hosp_ratio_long = hosp_both_long %>% 
  filter(ratio_diff=="ratio") %>% 
  mutate(
    value_cat = 
      cut(
          value,
          include.lowest=TRUE,
          breaks=c(
            min(value,na.rm=TRUE),
            quantile(value, probs =c(0.35), na.rm=TRUE),
            quantile(value, probs =c(0.55), na.rm=TRUE),
            quantile(value, probs =c(0.75), na.rm=TRUE),
            quantile(value, probs =c(0.9), na.rm=TRUE),
            max(value,na.rm=TRUE)
          )),
    #mapview doesn't like factors, so try
    value_cat_char = as.character(value_cat)
  )

  
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
          #more cut-offs at the top of the dist'n because that's where there's more variation
          quantile(value, probs =c(0.43), na.rm=TRUE),
          quantile(value, probs =c(0.6), na.rm=TRUE),
          quantile(value, probs =c(0.8), na.rm=TRUE),
          quantile(value, probs =c(0.9), na.rm=TRUE),
          max(value,na.rm=TRUE)
        )),
    #mapview doesn't like factors, so try
    value_cat_char = as.character(value_cat)
  )

table(hosp_diff_long$value_cat)