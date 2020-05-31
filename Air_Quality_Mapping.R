library(tidyverse)
library(leaflet)
library(leaflet.extras)

# read, clean air quality data
source_dir <- "Data"
air_qual <- read.csv(file.path(source_dir, "air_quality.csv"), stringsAsFactors = F) %>%
  na.omit()
air_qual <- air_qual %>% mutate(longs = Longitude,
                              lats = Latitude,
                              radius = AQI * 0.3,
                              lab = paste("AQI: ", AQI))

# plot data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10)
mp <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = air_qual, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "green",
                   label = ~lab)
mp