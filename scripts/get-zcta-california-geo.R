#Get spatial data for California zctas
library(tidycensus)
library(tidyverse)
library(sf)
library(here)

# Download all zip-code tabulation areas------
#and then filter them down to California manually
zcta_usa = tidycensus::get_acs(
  geography = "zcta",
  year = 2020,
  cache_table = TRUE,
  variables = "B01001_001",
  geometry = TRUE  
) %>% 
  st_transform(4326)

library(mapview)
setwd(here("data-processed"))
save(zcta_usa, file = "zcta_usa.RData")

zcta_usa_wrangle = zcta_usa %>% 
  rename(pop = estimate) %>%   #rename estimate to pop.
  mutate( 
    zcta = as.numeric(GEOID), 
    #generate random variables to play with
    A = rnorm(n=n(), mean=3, sd=3), 
    B = rnorm(n=n(), mean=10, sd=10),
    C = rnorm(n=n(), mean=20, sd=20)
  ) %>% 
  dplyr::select(starts_with("zcta"), pop, A, B, C)

zcta_usa_wrangle

#I read the list of California ZCTAs here
#Load list of California ZCTAs--------
library(readr)
library(readxl)
setwd(here("data-input"))
zcta_ca_list = read_xlsx("zcta-ca-list.xlsx") %>% 
  mutate(zcta_in_cali=1)
setwd(here("data-processed"))
save(zcta_ca_list, file = "zcta_ca_list.RData")

zcta_ca_list
nrow(zcta_ca_list)

# Filter to California ZCTAs
zcta_ca_geo = zcta_usa_wrangle %>% 
  left_join(zcta_ca_list, by = "zcta") %>% 
  filter(zcta_in_cali==1) 

#try simplifying the sf objects...
#may reduce the size of the data for eventual mapview rendering
zcta_ca_geo_simplified = zcta_ca_geo %>% 
  st_simplify(dTolerance = 500)

object.size(zcta_ca_geo) 
#dtolerance 1000: 1782032 bytes
#dtolerance 500:  2137920 bytes - let's try this
#dtolerance 100:  3932832 bytes
#dtolerance 10:   6358096 bytes

zcta_ca_geo %>% mapview(lwd=1)

#okay, save this for use elsewhere
setwd(here("data-processed"))
save(zcta_ca_geo, file = "zcta_ca_geo.RData")
save(zcta_ca_geo_simplified, file = "zcta_ca_geo_simplified.RData")

#Mapview of simplified zcta
zcta_ca_geo_simplified %>% 
  mapview(zcol="pop")

#Save a very small number of zip codes for troubleshooting
zcta_ca_first_10_geo = zcta_ca_geo %>% 
  slice(1:10)
save(zcta_ca_first_10_geo, file ="zcta_ca_first_10_geo.RData")

nrow(zcta_ca_geo)
zcta_ca_last_10_geo = zcta_ca_geo %>% 
  slice(1730:1739)
setwd(here("data-processed"))
save(zcta_ca_last_10_geo, file ="zcta_ca_last_10_geo.RData")

#mapviews
zcta_ca_first_10_geo %>% mapview()
zcta_ca_geo %>% 
  mapview(zcol = "pop",
          layer.name = "pop")

zcta_ca_geo %>% 
  mapview(zcol = "C",
          layer.name = "C")

