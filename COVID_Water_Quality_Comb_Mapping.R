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
                            radius = 0.1 * cumu_deaths) %>%
  na.omit() # sometimes non-states included for high nums, e.g. nyc
# plot country data
mp <- leaflet() %>% addTiles() %>% addHeatmap(data = global, lng = ~longs, lat = ~lats, intensity = ~cumu_deaths,
                                              blur = 20, max = 0.1, radius = 10, group = "COVID-19, Global")
mp <- mp %>% addCircleMarkers(data = global, lng = ~longs, lat = ~lats, radius = 10, stroke = F, fillOpacity = 0,
                              label = paste(global$Country, ": ", global$cumu_deaths, " deaths, ",
                                            global$cumu_cases, " cases", sep = ""), group = "COVID-19, Global")

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
                                radius = 0.1 * Total.Death) %>%
  na.omit() # sometimes non-states included for high nums, e.g. nyc
# plot state data
mp <- mp %>% addHeatmap(data = domestic, lng = ~longs, lat = ~lats, intensity = ~Total.Death,
                        blur = 20, max = 0.1, radius = 10, group = "COVID-19, US")
mp <- mp %>% addCircleMarkers(data = domestic, lng = ~longs, lat = ~lats, radius = 10, stroke = F, fillOpacity = 0,
                              label = paste(domestic$jurisdiction, ": ", domestic$Total.Death, " deaths, ",
                                            domestic$Total.Cases, " cases", sep = ""), group = "COVID-19, US")

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
                              lats = LatitudeMeasure) 

# plot water quality data
mp <- mp %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
                                              blur = 20, max = 0.1, radius = 10, group = "Water Contamination (total suspended solids)")
# mp <- mp %>% addCircleMarkers(data = comb_df, lng = ~longs, lat = ~lats, radius = 10, stroke = F, fillOpacity = 0,
#                               label = paste(comb_df$Country, ": ", comb_df$cumu_deaths, " concentration, ", sep = ""))

# layer control
mp <- mp %>% addLayersControl(
  overlayGroups = c("COVID-19, US", "COVID-19, Global", "Water Contamination (total suspended solids)"),
  options = layersControlOptions(collapsed = FALSE)
)
mp
