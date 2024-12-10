#Link California ZCTAs with Maren's data
library(here)
library(tidyverse)
source(here("scripts", "read-attrib-hosps-ratios-diffs.R"))
library(viridis)
library(sf)
library(mapview)

setwd(here("data-processed"))
load("zcta_ca_geo.RData") #created heat-hosp-california/scripts/link-zcta-hosp-data.R
load("zcta_ca_geo_simplified.RData")
hosp_all_long_geo = hosp_all_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()


#don't save this, though. too big.
