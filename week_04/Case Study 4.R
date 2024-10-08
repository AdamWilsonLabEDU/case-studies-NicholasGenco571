# Load the necessary packages
library(tidyverse)
library(nycflights13)
help(package = "nycflights13")
airports = airports
planes = planes
flights = flights
weather = weather
airlines = airlines

furthest_airport <- flights %>%
  arrange(desc(distance)) %>%
  slice(1) %>%
  select(dest, distance)
print(farthest_airport)
farthest_airport_with_name <- farthest_airport %>%
  left_join(airports, by = c("dest" = "faa"))
print(farthest_airport_with_name)
airportname = farthest_airport_with_name[1,3]
print(airportname)
farthest_airport = as.character(airportname)
print(farthest_airport)
