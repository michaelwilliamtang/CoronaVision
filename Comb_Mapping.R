# maps COVID cases and deaths, air quality, water quality

library(tidyverse)
library(leaflet)
library(leaflet.extras)

# source: https://covid19.who.int/
# read country data
source_dir <- "Data"
global <- read.csv(file.path(source_dir, "WHO-COVID-19-global-data.csv"), stringsAsFactors = F)
global <- global %>% group_by(Country) %>%
  dplyr::summarize(cumu_cases = max(Cumulative_cases),
            cumu_deaths = max(Cumulative_deaths))
countries <- read.csv(file.path(source_dir, "countries.csv"), stringsAsFactors = F) %>%
  mutate(Country = name)
global <- merge(global, countries, by = "Country") %>% 
  na.omit() %>%
  mutate(lats = latitude,
         longs = longitude,
         radius = cumu_deaths * 0.002,
         lab = paste(Country, ": ", cumu_deaths, " deaths, ",
                     cumu_cases, " cases", sep = ""))

# plot country data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = global, lng = ~longs, lat = ~lats, intensity = ~cumu_deaths,
#                                               blur = 20, max = 0.1, radius = 10, group = "COVID-19")
mp <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = global, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "red",
                              label = ~lab, group = "COVID-19")

# source: https://www.cdc.gov/covid-data-tracker/index.html
# read state data
domestic <- read.csv(file.path(source_dir, "US_MAP_DATA.csv"), stringsAsFactors = F, skip = 2)
states <- read.csv(file.path(source_dir, "states.csv"), stringsAsFactors = F) %>%
  mutate(jurisdiction = name)
domestic <- merge(domestic, states, by = "jurisdiction") %>% 
  na.omit() %>%
  mutate(lats = latitude,
         longs = longitude,
         radius = Total.Death * 0.002,
         lab = paste(jurisdiction, ": ", Total.Death, " deaths, ",
                     Total.Cases, " cases", sep = ""))

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
  filter(ResultMeasureValue < 3000) %>% # outlier, most values < 1000
  select(ResultMeasureValue, MonitoringLocationIdentifier) %>%
  na.omit() %>% # don't omit based on other vars' nas
  group_by(MonitoringLocationIdentifier) %>%
  dplyr::summarize(mean_result = mean(ResultMeasureValue)) %>%
  ungroup()
comb_df <- merge(stations, samples, by = "MonitoringLocationIdentifier")
comb_df <- comb_df %>% mutate(longs = LongitudeMeasure,
                              lats = LatitudeMeasure,
                              radius = mean_result * 0.03,
                              lab = paste("Suspended solids: ", round(mean_result, 3), " mg/l", sep = "")) 

# plot water quality data
# mp <- mp %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10, group = "Water Contamination")
mp <- mp %>%
  addCircleMarkers(data = comb_df, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "blue",
                   label = ~lab, group = "Water Contamination")

# read, clean air quality data
air_qual <- read.csv(file.path(source_dir, "air_quality.csv"), stringsAsFactors = F) %>%
  na.omit()
air_qual <- air_qual %>% mutate(longs = Longitude,
                                lats = Latitude,
                                radius = AQI * 0.3,
                                lab = paste(City, ": AQI ", AQI), sep = "")

# plot data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10)
mp <- mp %>%
  addCircleMarkers(data = air_qual, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "green",
                   label = ~lab, group = "Air Quality Index (AQI)")

# source: https://data.giss.nasa.gov/gistemp/station_data_v4_globe/#form
# read, merge temp data
stations <- read.csv(file.path(source_dir, "v4.temperature.inv.csv"), stringsAsFactors = F) %>%
  mutate(Station = ID) %>%
  na.omit()
samples <- read.csv(file.path(source_dir, "v4.mean_GISS_homogenized.csv"), stringsAsFactors = F) %>%
  na.omit()
comb_df <- merge(stations, samples, by = "Station")
comb_df <- comb_df %>% mutate(longs = Lon,
                              lats = Lat,
                              radius = mean_2019 * 0.35,
                              lab = paste("Mean temperature: ", round(mean_2019, 3), " (C)", sep = "")) 

# plot data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = comb_df, lng = ~longs, lat = ~lats, intensity = ~mean_result,
#                                               blur = 20, max = 0.1, radius = 10)
mp <- mp %>%
  addCircleMarkers(data = comb_df, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, color = "#ff9966",
                   label = ~lab, group = "Temperature")

# source: https://epi.envirocenter.yale.edu/epi-topline
# read epi score data
epi_df <- read.csv(file.path(source_dir, "epi2018scorescurrentv01.csv"), stringsAsFactors = F) %>%
  na.omit() %>%
  mutate(Country = country)
epi_df$Country[which(epi_df$Country == "United States of America")] <- "United States"
epi_df <- merge(epi_df, countries, by = "Country") %>%
  na.omit() %>%
  mutate(lats = latitude,
         longs = longitude,
         radius_epi = EPI.current * 0.3,
         radius_bdh = BDH.current * 0.3,
         radius_h2o = H2O.current * 0.3,
         radius_ape = APE.current * 0.3,
         radius_cce = CCE.current * 0.3,
         lab_epi = paste(Country, ": EPI ", EPI.current, sep = ""),
         lab_bdh = paste(Country, ": BDH ", BDH.current, sep = ""),
         lab_h2o = paste(Country, ": H2O ", H2O.current, sep = ""),
         lab_ape = paste(Country, ": APE ", APE.current, sep = ""),
         lab_cce = paste(Country, ": CCE ", CCE.current, sep = ""))

# plot epi: epi (environmental performance index), h20 (water quality), bdh (biodiversity), ape (air pollution),
#   cce (energy emission)
mp <- mp %>%
  addCircleMarkers(data = epi_df, lng = ~longs, lat = ~lats, radius = ~radius_epi, stroke = F, color = "#99ff99",
                   label = ~lab_epi, group = "Environmental Performance Index (EPI)")
mp <- mp %>%
  addCircleMarkers(data = epi_df, lng = ~longs, lat = ~lats, radius = ~radius_bdh, stroke = F, color = "#ff00ff",
                   label = ~lab_bdh, group = "Biodiversity Score (BDH)")
mp <- mp %>%
  addCircleMarkers(data = epi_df, lng = ~longs, lat = ~lats, radius = ~radius_h2o, stroke = F, color = "#ccccff",
                   label = ~lab_h2o, group = "Water Quality Score (H2O)")
mp <- mp %>%
  addCircleMarkers(data = epi_df, lng = ~longs, lat = ~lats, radius = ~radius_ape, stroke = F, color = "#ffff00",
                 label = ~lab_ape, group = "Air Quality Score (APE)")
mp <- mp %>%
  addCircleMarkers(data = epi_df, lng = ~longs, lat = ~lats, radius = ~radius_cce, stroke = F, color = "#ff99ff",
                   label = ~lab_cce, group = "Emissions Score (CCE)")

# layer control
mp <- mp %>% addLayersControl(
  overlayGroups = c("COVID-19", "Water Contamination", "Air Quality Index (AQI)", "Temperature",
                    "Environmental Performance Index (EPI)", "Biodiversity Score (BDH)", "Water Quality Score (H2O)",
                    "Air Quality Score (APE)","Emissions Score (CCE)"),
  options = layersControlOptions(collapsed = FALSE)
)
mp
