#Link California ZCTAs with Maren's data
library(here)
library(tidyverse)
source(here("scripts", "read-ratios-diffs-maren.R"))
library(viridis)
library(sf)
library(mapview)

hosp_both

setwd(here("data-processed"))
load("zcta_ca_geo.RData") #createdheat-hosp-california/scripts/link-zcta-hosp-data.R

hosp_both_geo = hosp_both %>% 
  left_join(zcta_ca_geo, by = "zcta") %>% 
  st_as_sf()

save(hosp_both_geo, file = "hosp_both_geo.RData")

#ratio
hosp_both_geo %>% 
  filter(ratio_diff=="ratio") %>% 
  dplyr::select("zcta", "AI", "ratio_diff") %>% 
  mapview(
    col.regions= viridis_pal(option = "A", direction = 1),
    zcol = "AI",
    layer.name = "Tree canopy, Pop. based: Increase by 10%, all zip codes, Ratio")

#diff
hosp_both_geo %>% 
  filter(ratio_diff=="diff") %>% 
  mapview(
    col.regions= viridis_pal(option = "D", direction = 1),
    zcol = "AI",
          layer.name = "Tree canopy, Pop. based: Increase by 10%, all zip codes, Difference")

#how about a function?

mv_ratio_fun = function(var_name){
  hosp_both_geo %>% 
    filter(ratio_diff=="ratio") %>% 
    dplyr::select("zcta", all_of(var_name), "ratio_diff") %>% 
    mapview(
      col.regions= viridis_pal(option = "A", direction = 1),
      zcol = var_name,
      basemaps = c("CartoDB.Positron", "CartoDB.DarkMatter"),
      layer.name = "Ratio")
}

mv_ratio_fun(var_name = "AI") #cool. works.

#what are the unique values of the scenario abbreviations?
names(hosp_both_geo)