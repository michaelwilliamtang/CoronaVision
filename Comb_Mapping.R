# maps COVID cases and deaths, air quality, water quality

library(tidyverse)
library(leaflet)
library(leaflet.extras)

# source: https://covid19.who.int/
# read country data
source_dir <- "Data"
global <- read.csv(file.path(source_dir, "WHO-COVID-19-global-data.csv"), stringsAsFactors = F)
global <- global %>% group_by(Country) %>%
  summarise(cumu_cases = max(Cumulative_cases),
            cumu_deaths = max(Cumulative_deaths))
countries <- read.csv(file.path(source_dir, "countries.csv"), stringsAsFactors = F)
clats <- countries$latitude
clongs <- countries$longitude
names(clats) <- countries$name
names(clongs) <- countries$name
global <- global %>% mutate(lats = clats[Country],
                            longs = clongs[Country],
                            radius = cumu_deaths * 0.002,
                            lab = paste(global$Country, ": ", global$cumu_deaths, " deaths, ",
                                        global$cumu_cases, " cases", sep = "")) %>%
  na.omit() # sometimes non-states included for high nums, e.g. nyc
# plot country data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = global, lng = ~longs, lat = ~lats, intensity = ~cumu_deaths,
#                                               blur = 20, max = 0.1, radius = 10, group = "COVID-19")
mp <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = global, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "red",
                              label = ~lab, group = "COVID-19")

# source: https://www.cdc.gov/covid-data-tracker/index.html
# read state data
domestic <- read.csv(file.path(source_dir, "US_MAP_DATA.csv"), stringsAsFactors = F, skip = 2)
states <- read.csv(file.path(source_dir, "states.csv"), stringsAsFactors = F)
slats <- states$latitude
slongs <- states$longitude
names(slats) <- states$name
names(slongs) <- states$name
domestic <- domestic %>% mutate(lats = slats[jurisdiction],
                                longs = slongs[jurisdiction],
                                radius = Total.Death * 0.002,
                                lab = paste(domestic$jurisdiction, ": ", domestic$Total.Death, " deaths, ",
                                            domestic$Total.Cases, " cases", sep = "")) %>%
  na.omit() # sometimes non-states included for high nums, e.g. nyc
# plot state data
# mp <- mp %>% addHeatmap(data = domestic, lng = ~longs, lat = ~lats, intensity = ~Total.Death,
#                         blur = 20, max = 0.1, radius = 10, group = "COVID-19")
mp <- mp %>% addCircleMarkers(data = domestic, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "red",
                              label = ~lab, group = "COVID-19")

# source: https://www.waterqualitydata.us/portal/#startDateLo=01-01-2020&startDateHi=05-30-2020&mimeType=csv
# read, clean, merge water quality data
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

# plot water quality data
# mp <- mp %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10, group = "Water Contamination")
mp <- mp %>%
  addCircleMarkers(data = comb_df, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "blue",
                   label = ~lab, group = "Water Contamination")

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
mp <- mp %>%
  addCircleMarkers(data = air_qual, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "green",
                   label = ~lab, group = "AQI")

# layer control
mp <- mp %>% addLayersControl(
  overlayGroups = c("COVID-19", "Water Contamination", "AQI"),
  options = layersControlOptions(collapsed = FALSE)
)
mp
