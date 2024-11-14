library(tidyverse)
library(spData)
library(sf)

## New Packages
library(mapview) # new package that makes easy leaflet maps
library(foreach)
library(doParallel)
registerDoParallel(4)
getDoParWorkers() # check registered cores
## 1. Download data
# go to  http://api.census.gov/data/key_signup.html and get a key, then run the line below with your key.  Don't push your key to github!
library(tidycensus)
census_api_key("f521c983ad90597d962c756e9e51bf7092fd3b37")

library(tidycensus)
race_vars <- c(
  "Total Population" = "P1_001N",
  "White alone" = "P1_003N",
  "Black or African American alone" = "P1_004N",
  "American Indian and Alaska Native alone" = "P1_005N",
  "Asian alone" = "P1_006N",
  "Native Hawaiian and Other Pacific Islander alone" = "P1_007N",
  "Some Other Race alone" = "P1_008N",
  "Two or More Races" = "P1_009N"
)


options(tigris_use_cache = TRUE)
erie <- get_decennial(geography = "block", variables = race_vars, year=2020,
                      state = "NY", county = "Erie County", geometry = TRUE,
                      sumfile = "pl", cache_table=T) 

## 2. Crop the data
erie_county <- st_crop(erie, xmin=-78.9,xmax=-78.85,ymin=42.888,ymax=42.92)

# 3. Foreach
erie_point_race <- foreach(race = unique(erie_county$variable), .combine=rbind) %dopar% {
  erie_county %>% filter(variable == race) %>% 
    st_sample(size=.$value) %>% 
    st_as_sf() %>%
    mutate(variable = race)
}

# 4. Visualize the result
library(mapview)
mapview(erie_point_race, zcol = "variable", legend = TRUE)


