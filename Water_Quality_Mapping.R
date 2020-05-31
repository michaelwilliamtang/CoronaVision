library(tidyverse)
library(leaflet)
library(leaflet.extras)

# source: https://www.waterqualitydata.us/portal/#startDateLo=01-01-2020&startDateHi=05-30-2020&mimeType=csv
# read, clean, merge water quality data
source_dir <- "Data"
stations <- read.csv(file.path(source_dir, "station.csv"), stringsAsFactors = F) %>%
  select(MonitoringLocationIdentifier, LongitudeMeasure, LatitudeMeasure) %>%
  na.omit()
samples <- read.csv(file.path(source_dir, "result.csv"), stringsAsFactors = F)
samples$ResultMeasureValue <- as.numeric(samples$ResultMeasureValue) # remove non-numeric vals
samples <- samples %>% 
  filter(CharacteristicName == "Total suspended solids") %>%
  select(ResultMeasureValue, MonitoringLocationIdentifier) %>%
  na.omit() %>% # don't omit based on other vars' nas
  group_by(MonitoringLocationIdentifier) %>%
  summarize(mean_result = mean(ResultMeasureValue)) %>%
  ungroup()
comb_df <- merge(stations, samples, by = "MonitoringLocationIdentifier")
comb_df <- comb_df %>% mutate(longs = LongitudeMeasure,
                              lats = LatitudeMeasure,
                              radius = mean_result * 0.01,
                              lab = paste("Suspended solids: ", round(mean_result, 3), " mg/l", sep = "")) 

# plot data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10)
mp <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = comb_df, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "blue",
                              label = ~lab)
mp