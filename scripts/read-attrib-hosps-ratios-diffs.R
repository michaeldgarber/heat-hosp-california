#This script reads in Maren's data
#Revised May 2nd based on data that Maren sent me March 24th

library(readr)
library(readxl)
library(here)
library(tidyverse)
setwd(here("data-processed"))
load("zcta_ca_list.RData")

# Read data------
zcta_ca_list
setwd(here("data-input","measures-most-recent"))
#Maren revised filename Mar 3, 2023 from HospRatio.csv to StandardizedHosp.csv
#incidence rate ratio
hosp_irr = read_csv("IRR_Mar24.csv") %>% 
  #Rename from ZIP to zcta for easy linking with other data
  rename(zcta = ZIP) %>% 
  mutate(measure = "irr")

hosp_irr

#incidence rate difference
hosp_ird = read_csv("IRD_Mar24.csv") %>% 
  rename(zcta = ZIP) %>% 
  mutate(measure = "ird")
hosp_ird

#percent difference
hosp_pd = read_csv("PctDif_Mar24.csv") %>% 
  rename(zcta = ZIP) %>% 
  mutate(measure = "pd")
hosp_pd

hosp_all = hosp_irr %>% 
  bind_rows(hosp_ird) %>% 
  bind_rows(hosp_pd)

setwd(here("data-processed"))
save(hosp_all, file = "hosp_all.RData")

# Make data longer-----
#To make it easier to write dynamic code for visualizations
hosp_all_long = hosp_all %>% 
  pivot_longer(
    cols = c(starts_with("A"), starts_with("B")),#key is using c()
    values_to = "value",
    names_to = "scenario") %>% 
  as_tibble() %>% 
  ## add some additional scenario vars-------
  mutate(
    scenario_intervention=case_when(
      #needle, haystack
      grepl("A",scenario)~"Tree", #Tree canopy
      TRUE~"Imp" #Impervious surface cover
    ),
    #one of 3 categories
    scenario_type_3=case_when(
      grepl("III",scenario)~"Prop. Univ.", 
      grepl("II",scenario)~"Targeted", 
      grepl("I",scenario)~"Population-based"
  ),
  
  #
  scenario_sub_type=case_when(
    #Note these are excluding the pop.based ones
    grepl("1",scenario)~"Exposure", 
    grepl("2",scenario)~"SVI", 
    grepl("3",scenario)~"Hosp. burden"
  ),
  #a 1-7 number within the intervention type
  scenario_type_7=case_when(
    grepl("III1",scenario)~"Prop. Univ. 1",
    grepl("III2",scenario)~"Prop. Univ. 2", 
    grepl("III3",scenario)~"Prop. Univ. 3", 
    grepl("II1",scenario)~"Targeted 1",
    grepl("II2",scenario)~"Targeted 2",
    grepl("II3",scenario)~"Targeted 3",
    grepl("I",scenario)~"Pop. based"
  ),
  #abbreviated version
  scenario_type_7_abbrev=case_when(
    grepl("III1",scenario)~"PU 1",
    grepl("III2",scenario)~"PU 2", 
    grepl("III3",scenario)~"PU 3", 
    grepl("II1",scenario)~"T 1",
    grepl("II2",scenario)~"T 2",
    grepl("II3",scenario)~"T 3",
    grepl("I",scenario)~"PB"
  ),
  
  #are there are negative IRDs?
  ird_neg=case_when(
    measure=="ird" & value <0~1,
    TRUE~0),
  
  ird_miss=case_when(
    measure=="ird" & is.na(value)~1,
    TRUE~0
  )
  
  )


table(hosp_all_long$scenario)
table(hosp_all_long$scenario_intervention)
table(hosp_all_long$scenario_type_3)
table(hosp_all_long$scenario_type_7)
table(hosp_all_long$scenario_sub_type)
table(hosp_all_long$measure)
setwd(here("data-processed"))
save(hosp_all_long, file = "hosp_all_long.RData")
table(hosp_all_long$scenario)

#How many missings from the IRD?
hosp_all_long %>% 
  dplyr::filter(measure=="ird") %>% 
  mutate(
    missing=case_when(is.na(value) ~1,
         TRUE~0)
  ) %>% 
  group_by(missing) %>% 
  summarise(n=n())

#Histograms to examine distribution of each-----
table(hosp_all_long$scenario)
hosp_all_long %>% 
  filter(measure=="irr") %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_grid(
    rows=vars(scenario_type_3),
    cols=vars(scenario_intervention)
  )

hosp_all_long %>% 
  filter(measure=="irr") %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_grid(
    rows=vars(scenario_type_7),
    cols=vars(scenario_intervention)
  )


hosp_all_long %>% 
  filter(measure=="irr") %>% 
  dplyr::select(value) %>% 
  summary()



hosp_all_long %>% 
  filter(measure=="ird") %>% 
  ggplot(aes(value))+
  geom_histogram()

hosp_all_long %>% 
  filter(measure=="ird") %>% 
  filter(value<0.003) %>% #avoid outliers for easier vis
  ggplot(aes(value))+
  geom_histogram()+
  facet_grid(
    rows=vars(scenario_type_3),
    cols=vars(scenario_intervention)
  )


hosp_all_long %>% 
  filter(measure=="ird") %>% 
  filter(value<0.003) %>% #avoid outliers for easier vis
  ggplot(aes(value))+
  geom_histogram()+
  facet_grid(
    rows=vars(scenario_type_7),
    cols=vars(scenario_intervention)
  )

hosp_all_long %>% 
  filter(measure=="ird") %>% 
  dplyr::select(value) %>% 
  summary()

hosp_all_long %>% 
  filter(measure=="pd") %>% 
  ggplot(aes(value))+
  geom_histogram()


hosp_all_long %>% 
  filter(measure=="pd") %>% 
  dplyr::select(value) %>% 
  summary()

#Lookups for names-----
lookup_scenario= hosp_all_long %>% 
  dplyr::select(starts_with("scenario")) %>% 
  distinct()

lookup_scenario


# Create categories for each measure--------
#Limit to about 5 bins
## irr-------
hosp_irr_long = hosp_all_long %>% 
  filter(measure=="irr") %>% 
  mutate(
    #see histogram for values
    value_cat = 
      cut(
          value,
          include.lowest=TRUE,
          breaks=c(0, 0.66, .9, .95, 0.975, 1)
          ),
    #mapview doesn't like factors, so try
    value_cat_char = as.character(value_cat)
  )


table(hosp_irr_long$value_cat)
length(table(hosp_irr_long$value_cat))

## ird------
hosp_ird_long = hosp_all_long %>% 
  filter(measure=="ird") %>% 
  mutate(
    value_cat = 
      cut(
        #see histograms above 
        value,
        include.lowest=TRUE,
        breaks=c(
          0, 
          0.00001,
          0.0001,
#          0.0005,
          0.001,
          0.003,
          max(value,na.rm=TRUE)
        )),
    #mapview doesn't like factors, so try
    value_cat_char = as.character(value_cat)
  )

table(hosp_ird_long$value_cat)
length(table(hosp_ird_long$value_cat))

## pd--------
hosp_pd_long = hosp_all_long %>% 
  filter(measure=="pd") %>% 
  mutate(
    value_cat = 
      cut(
        #see histograms above 
        value,
        include.lowest=TRUE,
        breaks=c(
          min(value,na.rm=TRUE),
          1,5,10,20,
          max(value,na.rm=TRUE)
        )),
    value_cat_char = as.character(value_cat)
  )

table(hosp_pd_long$value_cat)
length(table(hosp_pd_long$value_cat))

# column charts - facet by scenario------

hosp_irr_long %>% 
  group_by(measure, scenario) %>% 
  summarise(irr_mean=mean(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(lookup_scenario, by ="scenario") %>% 
  ggplot(aes(y=irr_mean,x=scenario_type_7_abbrev))+
  geom_col()+
  labs(x="Scenario", y="Mean IRR over ZCTA")+
  facet_grid(
    #Move facet down to x-axis
    #https://stackoverflow.com/questions/67519146/bar-plot-with-named-groups-on-x-axis-in-ggplot2
    cols=vars(scenario_intervention),
    scales="free_x",
    space="free_x",
    switch="x"
  )+
  theme(panel.spacing = unit(0, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white") 
  )

names(hosp_ird_long)
hosp_ird_long %>% 
  group_by(measure, scenario) %>% 
  summarise(ird_mean=mean(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(lookup_scenario, by ="scenario") %>% 
  ggplot(aes(y=ird_mean,x=scenario_type_7_abbrev))+
  geom_col()+
  facet_grid(
    cols=vars(scenario_intervention)
  )

# Read in data Kristen sent August 2024----
#Double checking Maren's work.
# She said she used the 1-day 95th percentile,
#which is this variable: max95_1
setwd(here("data-input","data-kristen-sent-August-2024"))
attrib_hosps_by_zip=read_csv("joined_abs.csv") %>% 
  mutate(zcta = ZCTA5CE10) %>% 
  dplyr::select(
    zcta, max95_1 #this is the var Maren's using
  )

attrib_hosps_by_zip


