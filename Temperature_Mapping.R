library(tidyverse)
library(leaflet)
library(leaflet.extras)

# source: https://data.giss.nasa.gov/gistemp/station_data_v4_globe/#form
# read, merge temp data
source_dir <- "Data"
stations <- read_csv(file.path(source_dir, "v4.temperature.inv.csv")) %>%
  mutate(Station = ID) %>%
  na.omit()
samples <- read_csv(file.path(source_dir, "v4.mean_GISS_homogenized.csv")) %>%
  na.omit()
comb_df <- merge(stations, samples, by = "Station")
comb_df <- comb_df %>% mutate(longs = Lon,
                              lats = Lat,
                              radius = mean_2019 * 0.35,
                              lab = paste("Mean temperature: ", round(mean_2019, 3), " (C)", sep = "")) 

# plot data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10)
mp <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = comb_df, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "#ff9966",
                   label = ~lab)
mp