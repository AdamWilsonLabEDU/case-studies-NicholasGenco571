#install.packages("spData")
#install.packages("sf")
#install.packages("tidyverse")

library(sf)
library(tidyverse)
library("spData")
#load 'world' data from spData package
data(world)  
# load 'states' boundaries from spData package
data(us_states)
# plot(world[1])  #plot if desired
# plot(us_states[1]) #plot if desired

albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
newdata <- world %>% filter(name_long== "Canada") %>% select(geom)
Newyork <- us_states %>% filter (NAME== "New York") %>% select(geometry)
albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
newdata <- st_transform(newdata, albers)
Newyork <- st_transform(Newyork, albers)
canada_buffered <- st_buffer(newdata, dist = 10000)
border <- st_intersection(canada_buffered, Newyork)
ggplot() +
  geom_sf(data = Newyork) +
  geom_sf(data = border, colour = "red", fill = "red") + 
  ggtitle("New York Land within 10km")

