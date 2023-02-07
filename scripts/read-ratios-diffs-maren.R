#This script reads in Maren's data
#February 7th, 2023

library(readr)
library(readxl)
library(here)
library(tidyverse)
setwd(here("data-processed"))
load("zcta_ca_list.RData")

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

save(hosp_both, file = "hosp_both.RData")


