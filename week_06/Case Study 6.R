# load packages
library(terra)
library(spData)
library(tidyverse)
library(sf)
install.packages("ncdf4")
library(ncdf4)
# Download temperature data
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method="curl")

tmean <-rast("crudata.nc")
print(tmean)
plot(tmean[[1]], main="Temperature - First Layer")

max_temp <- max(tmean, na.rm=TRUE)
plot(max_temp, main="Maximum Temperature")

world_sf <- world%>%
  st_as_sf()
country_temps <- terra::extract(max_temp, vect(world_sf), fun=max, na.rm=TRUE, small=TRUE)

world_clim <- world_sf%>%
  mutate(max_temp = country_temps[,2]) 

print(head(world_clim))
print(summary(world_clim$max_temp))

temp_map <- ggplot(world_clim) +
  geom_sf(aes(fill = max_temp)) +
  scale_fill_viridis_c(name="Maximum\nTemperature (C)") +
  theme_minimal() +
  theme(legend.position = 'bottom')

print(temp_map)

group_by(world_clim, continent)%>%
top_n(1, max_temp)%>%
select(continent, name_long, max_temp)%>%
arrange(continent) %>% st_set_geometry(NULL)
# hottest_continents <- hottest_continents %>% st_set_geometry(NULL)

