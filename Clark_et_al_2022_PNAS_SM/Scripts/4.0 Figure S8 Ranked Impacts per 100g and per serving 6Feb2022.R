###
# Figure S2
# Ratio of env impact per 100g and per serving per aisle
# Across all retailers/departments/aisles

# Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(readr)

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# Data
plot.dat <- 
  read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv")) %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE))

# Aggregating
plot.dat.aisle <-
  plot.dat %>%
  filter(!grepl('Halloween',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Halloween',Shelf, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Shelf, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Department, ignore.case = TRUE)) %>%
  filter(!grepl('\\bAll\\b',Department,ignore.case = TRUE)) %>%
  mutate(Shelf = ifelse(Retailer %in% 'Cook','Cook',Shelf)) %>%
  filter(!(Shelf %in% 'All')) %>%
  filter(!(Shelf %in% 'NULL')) %>%
  filter(Shelf != 'Water') %>%
  group_by(Retailer, Department, Aisle) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled, na.rm = TRUE),
                   Tot_env_serving_scaled = mean(Tot_env_serving_scaled, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  .[complete.cases(.),] %>%
  mutate(rank_100g = rank(Tot_env_100g_scaled),
         rank_serving = rank(Tot_env_serving_scaled)) %>%
  mutate(rank_100g = rank_100g / max(.$rank_100g),
         rank_serving = rank_serving / max(.$rank_serving)) %>%
  filter(Aisle != 'Water') %>%
  mutate(Retailer = ifelse(Retailer %in% 'Tesco_Ireland','Tesco Ireland',Retailer)) %>%
  filter(!(grepl('alcohol|beer|cider|wine|spirits|tobacco|smoking',Aisle,ignore.case=TRUE))) %>%
  filter(!(Aisle %in% 'Bottled Water'))
  

# Plotting
ggplot(plot.dat.aisle, aes(x = rank_100g * 100, y = rank_serving * 100)) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7, hjust = 0)) +
  theme(legend.title = element_text(size = 7, hjust = 0)) +
  geom_point() +
  labs(x = 'Percentile of Environmental Impact Score (per 100g)', y = 'Percentile of Environmental Impact Score (per serving)')

Sys.sleep(10)

# ggsave(paste0(getwd(),'/Figures/Fig S8 Ranks per 100g vs per serving 6Feb2022.pdf'),
#               width = 18, height = 15, units = 'cm')


write.csv(plot.dat.aisle,paste0(getwd(),'/Figure Data/Data Fig S8 Impacts per serving vs per gram 6Feb2022.csv'),row.names = FALSE)

