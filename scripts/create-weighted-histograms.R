#Create weighted histograms of the effect estimates
#weighted by population
#October 6, 2023

#Let's create histograms of the effect estimates, weighted by population.

# Load population data for each ZCTA---------
#We loaded population data from ACS in this script:
#heat-hosp-california/scripts/link-zcta-hosp-data.R

#Load the simplified geography with the population data,
#which was created in the above script
library(here)
library(tidyverse)
library(mapview)
library(sf)
setwd(here("data-processed"))
load("zcta_ca_geo_simplified.RData")

#What variables do we have in that data?
names(zcta_ca_geo_simplified)
#Visualize the population of each zip code
zcta_ca_geo_simplified %>% 
  mapview(zcol="pop")

#Link in Maren's model results from July 2023
#I've loaded those here
source(here("scripts", "explore-model-results.R"))

#check out the data from that script
pred_values_baseline
#Note we have a field for zcta and a field for the predicted rate differences
#corresponding to each model - impervious surface and tree canopy


# Histograms of unweighted effect estimates for each model--------
#summarize their distribution with histograms


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

# Link that data with the population data------
zcta_ca_geo_simplified_w_pred_values=zcta_ca_geo_simplified %>% 
  left_join(pred_values_baseline, by="zcta")

#check out the linked data
zcta_ca_geo_simplified_w_pred_values

#Map it, say using one of the predicted eff. estimates
zcta_ca_geo_simplified_w_pred_values %>% 
  mapview(zcol="TreeCanopy_Y")

#Add weights to each zip code corresponding to its population-------
#Okay, now we can create a weighted histogram. To do this, we can
#create weights for each zip code that is proportional to its population.
#Once we have the weights, we will then create weighted data, creating copies of
#each zip code in the data, where the number of copies is proportional to its
#population

#The first step will be to find the zip code with the largest population,
#and then for each zip code, we can determine how many times larger
#the max population is than than the pop of that zip code
#We don't need geometry at this point
#Check out the distribution of population before calculating the weights
summary(zcta_ca_geo_simplified_w_pred_values$pop)
#Okay, there are some zips with zero population. We can remove
#those before calculating the weights

#scatterplot of pop vs predicted value
zcta_ca_geo_simplified_w_pred_values %>% 
  ggplot()+
  geom_point(aes(x=pop,y=ImpervSurf_Y))


zcta_ca_pop_calc_weights=zcta_ca_geo_simplified_w_pred_values %>% 
  st_set_geometry(NULL) %>% #remove the geometry
  as_tibble() %>% #make sure it's a tibble
  filter(pop>0) %>% #remove zips with zero pop.
  mutate(
    
    #Now we can create a weight whose value is proportionate to its relative
    #population compared with other zip codes in the data.
    #A simple way to do this is to find the zip code with the
    #lowest population value and then divide each zip code's 
    #population by that value.
    
    #in this first calculation, we add a column called "pop_min_non_zero"
    #which will be the lowest non-zero pop. value
    pop_min_non_zero=min(pop,na.rm=TRUE),
    
    #Now we can divide this value by each zip code's actual population
    #to create a weight

    pop_wt=pop/pop_min_non_zero,
    
    #convert the weights to integers
    pop_wt_int=as.integer(pop_wt)
  )

zcta_ca_pop_calc_weights %>% View()

#Check out the distribution of the weights
#Okay, so the min value is actually 1, which means that each zip code's weight
#will actually simply be its population value.
summary(zcta_ca_pop_calc_weights$pop_min_non_zero)
summary(zcta_ca_pop_calc_weights$pop_wt)
summary(zcta_ca_pop_calc_weights$pop_wt_int)


zcta_ca_pop_calc_weights %>% 
  ggplot()+
  geom_histogram(aes(pop_wt))+
  xlab("Population weight")

#Now let's create weighted data, where each zip code is copied as many times
#as its population
#In R, we can do this using the uncount() function

zcta_ca_pop_weighted=zcta_ca_pop_calc_weights %>% 
  uncount(pop_wt_int, .remove=FALSE)

#This new dataset will have lot and lots of rows, as higher-population
#zip codes will repeat lots and lots of times

#how many rows?
nrow(zcta_ca_pop_weighted)
#how many rows in the original?
nrow(zcta_ca_pop_calc_weights)

# Fit a histogram in the weighted data---------
#now let's try histograms in the weighted data to see if
#the center of the distribution is more in the positive direction
zcta_ca_pop_weighted %>% 
  ggplot()+
  geom_histogram(aes(ImpervSurf_Y))+
  xlab("predicted attributable hosp. rate, imperv. model")

#Tree canopy
zcta_ca_pop_weighted %>% 
  ggplot()+
  geom_histogram(aes(TreeCanopy_Y))+
  xlab("predicted attributable hosp. rate, tree can. model")
    

#basic summaries
summary(zcta_ca_pop_weighted$TreeCanopy_Y_per_100_days)
summary(zcta_ca_pop_weighted$ImpervSurf_Y_per_100_days)

#still zero on average.