#This script reads in Maren's data

#Revising December 10, 2024 with data she updated more recently
#that just reflects metropolitan zip codes

library(readr)
library(readxl)
library(here)
library(tidyverse)
setwd(here("data-processed"))
load("zcta_ca_list.RData")

# Read data------
#Maren revised filename Mar 3, 2023 from HospRatio.csv to StandardizedHosp.csv
#incidence rate ratio

#Dec 10, 2024
#I'm renaming the files Maren sent to simply be called their measure
#name

zcta_ca_list
setwd(here("data-input","measures"))
irr = read_csv("irr.csv") %>% 
  #Rename from ZIP to zcta for easy linking with other data
  rename(zcta = ZIP) %>% 
  mutate(measure = "irr")

irr

#incidence rate difference
setwd(here("data-input","measures"))
ird = read_csv("ird.csv") %>% 
  rename(zcta = ZIP) %>% 
  mutate(measure = "ird")
ird

#number of prevented cases (not per population)
#Maren calls this absolute benefit
#call it n_cases_diff to emphasize that it's the number of prevented (or caused)
#cases not divided by population
setwd(here("data-input","measures"))
n_cases_diff=read_csv("n_cases_diff.csv") %>% 
  rename(zcta = ZIP) %>% 
  mutate(measure = "n_cases_diff")

nrow(n_cases_diff)
hosp_all = irr %>% 
  bind_rows(ird) %>% 
  bind_rows(n_cases_diff)

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
  filter(measure=="n_cases_diff") %>% 
  ggplot(aes(value))+
  geom_histogram()


hosp_all_long %>% 
  filter(measure=="n_cases_diff") %>% 
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

1/1.05
1/1.1
1/1.2
1/1.3
#install.packages("santoku")
library(santoku)#alternatives to cut_number
irr_long = hosp_all_long %>% 
  filter(measure=="irr") %>% 
  mutate(

    
    #using santoku to chop equally
    #also recall that mapview doesn't like factors
    value_cat=santoku::chop_equally(value,groups=5)
  )

#to lighten up r-shiny code, save this
setwd(here("data-processed"))
save(irr_long,file="irr_long.RData")

#some are exactly one
irr_long %>% 
  filter(value>=1) %>% 
  filter(value<=1) %>% 
  group_by(scenario_intervention,scenario_type_3,scenario_type_7) %>% 
  summarise(n=n())

class(irr_long$value_cat)
summary(irr_long$value)
table(irr_long$value_cat)




## ird------
ird_long = hosp_all_long %>% 
  filter(measure=="ird") %>% 
  mutate(
    value_cat=santoku::chop_equally(value,groups=5)
#     value_cat = 
#       cut(
#         #see histograms above 
#         value,
#         include.lowest=TRUE,
#         breaks=c(
#           0,
# #          1e-9,
#           1e-8,
#           1e-7,
#           1e-6,
#           max(value,na.rm=TRUE)
#         )),
#     #mapview doesn't like factors, so try
#     value_cat_char = as.character(value_cat)
  )


0.000036500
0.000015500
1e-7
.000000005
table(ird_long$value_cat)
table(ird_long$scenario)
table(ird_long$scenario_intervention)
table(ird_long$scenario_type_3)
table(ird_long$scenario_type_7)
summary(ird_long$value)
ird_long %>% 
  filter(value<.00000005) %>% 
  ggplot(aes(x=value))+
  geom_histogram()
ird_long %>% 
  filter(value<.000000005) %>% 
  ggplot(aes(x=value))+
  geom_histogram()


## n_cases_diff--------
n_cases_diff_long = hosp_all_long %>% 
  filter(measure=="n_cases_diff") %>% 
  mutate(
    value_cat=santoku::chop_equally(value,groups=5)
    # value_cat = 
    #   cut(
    #     #see histograms above 
    #     value,
    #     include.lowest=TRUE,
    #     breaks=c(
    #       min(value,na.rm=TRUE),
    #       1,5,10,20,
    #       max(value,na.rm=TRUE)
    #     )),
    # value_cat_char = as.character(value_cat)
  )

table(n_cases_diff_long$value_cat)
summary(n_cases_diff_long$value)
length(table(n_cases_diff_long$value_cat))

# column charts - facet by scenario------

irr_long %>% 
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

names(ird_long)
ird_long %>% 
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


