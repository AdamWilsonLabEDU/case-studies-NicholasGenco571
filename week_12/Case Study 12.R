library(tidyverse)
library(htmlwidgets)
library(widgetframe)
install.packages("widgetframe")
library(dplyr)
library(xts)
library(dygraphs)
library(openmeteo)
install.packages("openmeteo")
install.packages("xts")
library(xts)
library(dplyr)

d <- weather_history(
  c(43.00923265935055, -78.78494250958327), 
  start = "2023-01-01", 
  end = Sys.Date(), 
  daily = c("temperature_2m_max", "temperature_2m_min", "precipitation_sum")
) %>%
  mutate(daily_temperature_2m_mean = (daily_temperature_2m_max + daily_temperature_2m_min) / 2)

d_xts_temp <- as.xts(
  d %>% select(daily_temperature_2m_mean), 
  order.by = d$date
)


d_xts_prec <- as.xts(
  d %>% select(daily_precipitation_sum), 
  order.by = d$date
)

buffalo_weather <- list(
dygraph(d_xts_temp, main = "Daily Maximum Temperature in Buffalo, NY") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31")) %>%
  dySeries(),
dygraph(d_xts_temp, main = "Daily Precipitation in Buffalo, NY") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31")) %>%
  dySeries() 
)

htmltools::browsable(htmltools::tagList(buffalo_weather))
