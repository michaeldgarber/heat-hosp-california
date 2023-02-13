#Link California ZCTAs with Maren's data
library(here)
library(tidyverse)
source(here("scripts", "read-ratios-diffs-maren.R"))
library(viridis)
library(sf)
library(mapview)

# Link to measures (wide-form)-------
hosp_both

setwd(here("data-processed"))
load("zcta_ca_geo.RData") #created heat-hosp-california/scripts/link-zcta-hosp-data.R
load("zcta_ca_geo_simplified.RData")
hosp_both_geo = hosp_both %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()

save(hosp_both_geo, file = "hosp_both_geo.RData")

# Mapview tests-----
# #ratio
# hosp_both_geo %>% 
#   filter(ratio_diff=="ratio") %>% 
#   dplyr::select("zcta", "AI", "ratio_diff") %>% 
#   mapview(
#     col.regions= viridis_pal(option = "A", direction = 1),
#     zcol = "AI",
#     layer.name = "Tree canopy, Pop. based: Increase by 10%, all zip codes, Ratio")
# 
# #diff
# hosp_both_geo %>% 
#   filter(ratio_diff=="diff") %>% 
#   mapview(
#     col.regions= viridis_pal(option = "D", direction = 1),
#     zcol = "AI",
#           layer.name = "Tree canopy, Pop. based: Increase by 10%, all zip codes, Difference")


#what are the unique values of the scenario abbreviations?
names(hosp_both_geo)

# Link to measure (long-form)-------
#Linking to the simplified versions to keep file size smaller

hosp_both_long_geo = hosp_both_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()

save(hosp_both_long_geo, file = "hosp_both_long_geo.RData")

hosp_ratio_long_geo = hosp_ratio_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()
hosp_diff_long_geo = hosp_diff_long %>% 
  left_join(zcta_ca_geo_simplified, by = "zcta") %>% 
  st_as_sf()