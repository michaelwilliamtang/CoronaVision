library(tidyverse)
library(leaflet)
library(leaflet.extras)

# source: https://covid19.who.int/
# read country data
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
                            # intensity = cumu_deaths,
                            radius = cumu_deaths * 0.0015) %>%
  na.omit() # sometimes non-states included for high nums, e.g. nyc
# plot country data
# mp <- leaflet() %>% addTiles() %>% addHeatmap(data = global, lng = ~longs, lat = ~lats, intensity = ~intensity,
#                                                     blur = 20, max = 0.0001, radius = 10)
mp <- leaflet() %>% addTiles()
mp <- mp %>% addCircleMarkers(data = global, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, fillOpacity = 0.5,
                              label = paste(global$Country, ": ", global$cumu_deaths, " deaths, ",
                                            global$cumu_cases, " cases", sep = ""))

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
                            # intensity = Total.Death,
                            radius = Total.Death * 0.0015) %>%
  na.omit() # sometimes non-states included for high nums, e.g. nyc
# plot state data
# mp <- mp %>% addHeatmap(data = domestic, lng = ~longs, lat = ~lats, intensity = ~intensity,
#                                                     blur = 20, max = 0.0001, radius = 10)
mp <- mp %>% addCircleMarkers(data = domestic, lng = ~longs, lat = ~lats, radius = ~radius, stroke = F, fillOpacity = 0.5,
                              label = paste(domestic$jurisdiction, ": ", domestic$Total.Death, " deaths, ",
                                            domestic$Total.Cases, " cases", sep = ""))

mp