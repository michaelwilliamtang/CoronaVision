library(tidyverse)

# state deaths input
source_dir <- "Data"
deaths_df <- read.csv(file.path(source_dir, "Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv"), stringsAsFactors = F)
deaths_df <- deaths_df %>% 
  na.omit() %>%
  select(State, Start.week, COVID.19.Deaths)

# calc avg state deaths, select top states for legend
top_N <- 5
state_means <- deaths_df %>% 
  group_by(State) %>% 
  summarize(state_mean = mean(COVID.19.Deaths)) %>% 
  ungroup() %>%
  arrange(desc(state_mean))
responders <- state_means$State[1:top_N]

# plot
graph_dir <- "Graphs"
pdf(file.path(graph_dir, paste("COVID-19 Deaths.pdf", sep = "_")), width = 6, height = 4)
deaths_df %>% ggplot() + geom_line(aes(x = Start.week, y = COVID.19.Deaths, color = State, group = State)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle=45, hjust = 1)) +
  # guides(color = F) +
  scale_color_discrete(breaks = c(responders), name = "Highest") +
  xlab("Week of") +
  ylab("COVID-19 Deaths")
dev.off()