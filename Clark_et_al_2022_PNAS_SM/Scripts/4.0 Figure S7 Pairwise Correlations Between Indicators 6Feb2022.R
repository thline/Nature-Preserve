###
# For Figure S1
# Pairwise correlations between individual environmental indicators

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
  

# Subsetting to Tesco
# And unique IDs
plot.dat <- 
  plot.dat %>%
  # filter(Retailer %in% 'Tesco') %>%
  dplyr::select(id, scaled_mean_GHG, scaled_mean_Land, scaled_mean_Eut, scaled_mean_WatScar) %>%
  unique(.)

plot.dat.x <-
  rbind(plot.dat %>% dplyr::select(id, impact_x = scaled_mean_GHG) %>% mutate(indicator_x = 'Greenhouse Gas\nEmissions'),
        plot.dat %>% dplyr::select(id, impact_x = scaled_mean_Eut) %>% mutate(indicator_x = 'Eutrophication'),
        plot.dat %>% dplyr::select(id, impact_x = scaled_mean_WatScar) %>% mutate(indicator_x = 'Water Scarcity'),
        plot.dat %>% dplyr::select(id, impact_x = scaled_mean_Land) %>% mutate(indicator_x = 'Land Use'))

plot.dat.y <-
  rbind(plot.dat %>% dplyr::select(id, impact_y = scaled_mean_GHG) %>% mutate(indicator_y = 'Greenhouse Gas\nEmissions'),
        plot.dat %>% dplyr::select(id, impact_y = scaled_mean_Eut) %>% mutate(indicator_y = 'Eutrophication'),
        plot.dat %>% dplyr::select(id, impact_y = scaled_mean_WatScar) %>% mutate(indicator_y = 'Water Scarcity'),
        plot.dat %>% dplyr::select(id, impact_y = scaled_mean_Land) %>% mutate(indicator_y = 'Land Use'))

# Merging
plot.dat.full <-
  left_join(plot.dat.x, plot.dat.y)

# Getting heat map
heat.map.dat <-
  plot.dat.full %>%
  mutate(impact_x = round(impact_x, digits = 0),
         impact_y = round(impact_y, digits = 0),
         count = 1) %>%
  group_by(indicator_x, indicator_y, impact_x, impact_y) %>%
  dplyr::summarise(count = sum(count)) %>%
  mutate(count = ifelse(count > quantile(.$count,.95), quantile(.$count, .95), count))

# # plotting
# ggplot(heat.map.dat, aes(x = impact_x, y = impact_y, fill = count)) +
#   theme_classic() +
#   geom_tile() +
#   facet_grid(indicator_x ~ indicator_y) 

# Getting percentile data
heat.map.dat <-
  plot.dat.full %>%
  group_by(indicator_x, indicator_y) %>%
  dplyr::summarise(impact_x = rank(impact_x),
                   impact_y = rank(impact_y)) %>%
  mutate(impact_x = impact_x / max(.$impact_x),
         impact_y = impact_y / max(.$impact_y)) %>%
  mutate(impact_x = round(impact_x, digits = 2),
         impact_y = round(impact_y, digits = 2),
         count = 1) %>%
  as.data.frame(.) %>%
  group_by(indicator_x, indicator_y, impact_x, impact_y) %>%
  dplyr::summarise(count = sum(count)) %>%
  mutate(count = ifelse(count > quantile(.$count,.99), quantile(.$count, .99), count)) %>%
  mutate(indicator_x = ifelse(indicator_x %in% 'Eutrophication','Eutrophication\nPotential',indicator_x)) %>%
  mutate(indicator_y = ifelse(indicator_y %in% 'Eutrophication','Eutrophication\nPotential',indicator_y))


my.palette = c('#00429d', '#2e59a8', '#4771b2', '#5d8abd', '#73a2c6', '#8abccf', '#a5d5d8', '#c5eddf', '#ffffe0')
my.palette <- colorRampPalette(my.palette)

ggplot(heat.map.dat, aes(x = impact_x, y = impact_y, fill = count)) +
  theme_classic() +
  geom_tile() +
  facet_grid(indicator_x ~ indicator_y) +
  # scale_fill_gradientn(colors = rev(my.palette(101)), trans = 'log10') +
  scale_fill_gradientn(colors = rev(my.palette(101))) +
  labs(x = 'Percentile Rank Impact', y = 'Percentile Rank Impact', fill = 'Number of\nObservations')

if(!('Figures' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figures'))
}

# ggsave(paste0(getwd(),'/Figures/Fig S7 Pairwise Correlations Between Indicators 6Feb2022.pdf'),
#        width = 8, height = 6, units = 'in')

# And spearmans correlations between indicators
indicator_y = sort(unique(plot.dat.full$indicator_y))
indicator_x = sort(unique(plot.dat.full$indicator_x))


corr.df <- 
  data.frame()
for(x in indicator_x) {
  for(y in indicator_y) {
    tmp.dat <- 
      plot.dat.full %>%
      filter(indicator_x %in% x & indicator_y %in% y)
    
    spearman.dat <-
      cor.test(tmp.dat$impact_x, tmp.dat$impact_y,
          method = 'spearman', conf.level = .95)
    
    corr.df <-
      rbind(corr.df,
            data.frame(indicator_x = x, indicator_y = y,
                       Rho = spearman.dat$estimate,
                       P_value = spearman.dat$p.value))
  }
}

if(!('Tables' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Tables'))
}

write.csv(corr.df,
          paste0(getwd(),'/Tables/Table S3 Pariwise Correlations Between Indicators 6Feb2022.csv'), row.names = FALSE)

if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}

write.csv(heat.map.dat %>% dplyr::rename(`Number of Observations` = count),
          paste0(getwd(),'/Figure Data/Data Fig S7 6Feb2022.csv'),row.names = FALSE)
