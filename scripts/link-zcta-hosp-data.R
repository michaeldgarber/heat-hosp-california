#Link California ZCTAs with Maren's data
library(here)
library(tidyverse)
source(here("scripts", "read-ratios-diffs-maren.R"))
library(viridis)
library(sf)
library(mapview)

setwd(here("data-processed"))
load("zcta_ca_geo.RData") #created heat-hosp-california/scripts/link-zcta-hosp-data.R
load("zcta_ca_geo_simplified.RData")
hosp_all_long_geo = hosp_all_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()


# Link to measure (long-form)-------
#Linking to the simplified versions to keep file size smaller

hosp_all_long_geo = hosp_all_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()

save(hosp_all_long_geo, file = "hosp_all_long_geo.RData")

hosp_irr_long_geo = hosp_irr_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()
hosp_ird_long_geo = hosp_ird_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()