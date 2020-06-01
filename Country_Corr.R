# corr based on country for COVID cases, deaths, EPIm BDH, H2O, APE, CCE

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
            cumu_deaths = max(Cumulative_deaths))

# source: https://epi.envirocenter.yale.edu/epi-topline
# read epi score data
epi_df <- read.csv(file.path(source_dir, "epi2018scorescurrentv01.csv"), stringsAsFactors = F) %>%
  na.omit() %>%
  mutate(Country = country)

# build matrix
global2 <- global %>% select(Country, cumu_cases, cumu_deaths)
epi_df2 <- epi_df %>% select(Country, EPI.current, BDH.current, H2O.current, APE.current, CCE.current)
comb_df <- merge(global2, epi_df2, by = "Country")
cmat <- comb_df %>% select(-Country) %>% as.matrix()
rownames(cmat) <- comb_df$Country
cmat2 <- cor(cmat)

# visualize corr
library(reshape2)
cmat2[upper.tri(cmat2)] <- NA
melt_cmat2 <- melt(cmat2, na.rm = T)
melt_cmat2 %>% ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#ffffcc", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 12, 
                                   hjust = 1)) +
  xlab("Variable 1") +
  ylab("Variable 2") +
  labs(title = "COVID Infection vs. EPI Scores by Country") + 
  coord_fixed()

# alt corr
# corr_mat <- cmat2
# cor <- rcorr(format(corr_mat, digits=20), type="pearson")
# cor.data <- cor$r
# cor.data[upper.tri(cor.data, diag = T)] <- 0
# pval.data <- cor$P
# pval.data[upper.tri(pval.data, diag = T)]<- NA
# # FDR.data <- apply(pval.data, 2, p.adjust, method="BH", n = length(pval.data))
# FDR.data <- apply(pval.data, 2, p.adjust, method="BH")
# FDR.data %>% ggplot(aes(x = , y = Var2, fill = value)) +
#   geom_tile()
