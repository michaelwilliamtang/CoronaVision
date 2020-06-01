# graphing based on country for COVID cases and deaths, over time

library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(Hmisc)

# source: https://covid19.who.int/
# read country data
source_dir <- "Data"
global <- read.csv(file.path(source_dir, "WHO-COVID-19-global-data.csv"), stringsAsFactors = F)
global <- global %>% group_by(Country) %>%
  summarise(cumu_cases = max(Cumulative_cases),
            cumu_deaths = max(Cumulative_deaths)) #%>%
  # arrange(desc(cumu_cases))
# global$Country2 <- as.factor(global$Country)
# levels(global$Country2) = global$Country

global[1:20,] %>% ggplot(aes(x = reorder(Country, cumu_cases), y = cumu_cases, fill = "red")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Total Cases") +
  xlab("Country") +
  theme_classic() +
  theme(legend.position = "none")