#Miscellaneous
#July 19, 2023

#Input the predicted values given observed covariates and make histograms

library(readr)
library(readxl)
library(here)
library(tidyverse)
setwd(here("data-processed"))
load("zcta_ca_list.RData")

# Read data------
#~heat-hosp-california/data-input/measures-jul-18-2023
#This is where I put the data in my computer, so this statement is telling R
#to change my working directory to the folder containing the data.
setwd(here("data-input","measures-jul-18-2023"))

pred_values_baseline= read_csv("Y_Jul18.csv") %>% 
  #Rename from ZIP to zcta for easy linking with other data
  rename(zcta = ZIP) %>% 
  #Divide by 100, as model output is per 100 heat-wave days
  rename(
    ImpervSurf_Y_per_100_days=ImpervSurf_Y,
    TreeCanopy_Y_per_100_days=TreeCanopy_Y) %>% 
  mutate(
    ImpervSurf_Y=ImpervSurf_Y_per_100_days/100,
    TreeCanopy_Y=TreeCanopy_Y_per_100_days/100
  ) 

pred_values_baseline

#Impervious surface
pred_values_baseline %>% 
  ggplot()+
  geom_histogram(aes(ImpervSurf_Y))+
  xlab("predicted attributable hosp. rate, imperv. model")


#Tree canopy
pred_values_baseline %>% 
  ggplot()+
  geom_histogram(aes(TreeCanopy_Y))+
  xlab("predicted attributable hosp. rate, tree can. model")

summary(pred_values_baseline$ImpervSurf_Y)
summary(pred_values_baseline$TreeCanopy_Y)

#Add weights to data
names(pred_values_baseline)