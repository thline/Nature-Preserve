###
# Fig Sx
# Distribution of impacts

# libraries
library(dplyr)
library(plyr)
library(ggplot2)
# library(factoextra)
# library(NbClust)
library(readr)

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# data
plot.dat <- 
  read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv"))


# Nutriscore data
nutriscore.dat <- 
  read_csv(paste0(getwd(),"/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv"))

# Limiting to indicators and unique values
plot.dat <-
  plot.dat %>%
  filter(product_name %in% nutriscore.dat$product_name) %>%
  filter(!grepl('Christmas|Hallow|Beer|Wine|Alcohol|Spirit|Liqu',Department)) %>%
  filter(!grepl('Christmas|Hallow|Beer|Wine|Alcohol|Spirit|Liqu',Aisle)) %>%
  filter(!grepl('Christmas|Hallow|Beer|Wine|Alcohol|Spirit|Liqu',Shelf)) %>%
  filter(id %in% nutriscore.dat$id) %>%
  filter(!is.na(NutriScorePoints)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  filter(Tot_env_100g_scaled > 0) %>%
  dplyr::select(product_name,
                Retailer, Department, Aisle,
                mean_GHG, mean_Land, mean_Eut, mean_WatScar) %>%
  unique(.) %>%
  group_by(product_name, Retailer, Department, Aisle) %>%
  dplyr::summarise(mean_GHG = mean(mean_GHG),
                   mean_Land = mean(mean_Land),
                   mean_Eut = mean(mean_Eut),
                   mean_WatScar = mean(mean_WatScar))

# Stacking
plotted.dat <-
  rbind(plot.dat %>% dplyr::select(product_name, Retailer, Department, Aisle, impact = mean_GHG) %>% mutate(indicator = 'GHGs'),
        plot.dat %>% dplyr::select(product_name, Retailer, Department, Aisle, impact = mean_Land) %>% mutate(indicator = 'Land'),
        plot.dat %>% dplyr::select(product_name, Retailer, Department, Aisle, impact = mean_Eut) %>% mutate(indicator = 'Eut'),
        plot.dat %>% dplyr::select(product_name, Retailer, Department, Aisle, impact = mean_WatScar) %>% mutate(indicator = 'WatScar')) %>%
  # dplyr::select(Aisle) %>% 
  unique(.) %>%
  transform(indicator = factor(indicator, levels = c('GHGs','Land','WatScar','Eut')))

# Getting quantiles so figs are more visible
dat.sum <-
  plotted.dat %>%
  filter(Retailer %in% 'Tesco') %>%
  group_by(indicator) %>%
  dplyr::summarise(high_impact = quantile(impact,.99))

# Merging quantiles in
plotted.dat <-
  left_join(plotted.dat, dat.sum) %>%
  filter(impact <= 5 & indicator %in% 'GHGs' |
           impact <= 10 & indicator %in% 'Land' |
           impact <= 25 & indicator %in% 'Eut' |
           impact <= 15000 & indicator %in% 'WatScar')

# Getting order of factors for indicators
factors.ghg <-
  plotted.dat %>% 
  filter(Retailer %in% 'Tesco' & indicator %in% 'GHGs') %>%
  group_by(Aisle) %>%
  dplyr::summarise(impact = mean(impact)) %>%
  arrange(impact)

factors.land <-
  plotted.dat %>% 
  filter(Retailer %in% 'Tesco' & indicator %in% 'Land') %>%
  group_by(Aisle) %>%
  dplyr::summarise(impact = mean(impact)) %>%
  arrange(impact)

factors.eut <-
  plotted.dat %>% 
  filter(Retailer %in% 'Tesco' & indicator %in% 'Eut') %>%
  group_by(Aisle) %>%
  dplyr::summarise(impact = mean(impact)) %>%
  arrange(impact)

factors.watscar <-
  plotted.dat %>% 
  filter(Retailer %in% 'Tesco' & indicator %in% 'WatScar') %>%
  group_by(Aisle) %>%
  dplyr::summarise(impact = mean(impact)) %>%
  arrange(impact)

# plotting
ghgs.plot <- ggplot(dat = plotted.dat %>% 
                      filter(Retailer %in% 'Tesco' & indicator %in% 'GHGs') %>% 
                      transform(Aisle = factor(Aisle, levels = factors.ghg$Aisle)), aes(x = impact)) +
  geom_density(colour = '#e78ac3') +
  facet_wrap(Aisle~indicator, scales = 'free_y',ncol = 7, labeller = label_wrap_gen()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Greenhouse Gas Emissions (kg CO2eq)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9)) +
  coord_cartesian(xlim = c(0,5)) +
  geom_vline(xintercept = 4.785, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 4.785, y = 1, label = 'Bovine meat\n(dairy herd)'), hjust = 1, size = 3) +
  geom_vline(xintercept = 1.28, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 1.28, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = .11, linetype = 2, colour = 'dark grey')
  # geom_text(aes(x = .11, y = 1, label = 'Wheat'), hjust = 0, size = 3)

land.plot <- ggplot(dat = plotted.dat %>% 
                      filter(Retailer %in% 'Tesco' & indicator %in% 'Land') %>% 
                      transform(Aisle = factor(Aisle, levels = factors.land$Aisle)), aes(x = impact)) +
  geom_density(colour = '#8da0cb') +
  facet_wrap(Aisle~indicator, scales = 'free_y',ncol = 7) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Land Use (square meters)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9))  +
  coord_cartesian(xlim = c(0,10)) + 
  geom_vline(xintercept = 9.73, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 9.73, y = 1, label = 'Bovine meat\n(dairy herd)'), hjust = 1, size = 3) +
  geom_vline(xintercept = 1.666, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 1.666, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = .4, linetype = 2, colour = 'dark grey')
  # geom_text(aes(x = .4, y = 1, label = 'Wheat'), hjust = 0, size = 3)

watscar.plot <- ggplot(dat = plotted.dat %>% 
                         filter(Retailer %in% 'Tesco' & indicator %in% 'WatScar') %>% 
                         transform(Aisle = factor(Aisle, levels = factors.watscar$Aisle)), aes(x = impact)) +
  geom_density(colour = '#fc8d62') +
  facet_wrap(Aisle~indicator, scales = 'free_y',ncol = 7) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Water Scarcity (liters)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9)) +
  coord_cartesian(xlim = c(0,15000)) +
  geom_vline(xintercept = 9101, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 9101, y = 1, label = 'Bovine meat\n(dairy herd)'), hjust = 1, size = 3) +
  geom_vline(xintercept = 6357, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 6357, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = 1278, linetype = 2, colour = 'dark grey')
  # geom_text(aes(x = 1278, y = 1, label = 'Wheat'), hjust = 0, size = 3)

eut.plot <- ggplot(dat = plotted.dat %>% 
                     filter(Retailer %in% 'Tesco' & indicator %in% 'Eut') %>% 
                     transform(Aisle = factor(Aisle, levels = factors.eut$Aisle)), aes(x = impact)) +
  geom_density(colour = '#66c2a5') +
  facet_wrap(Aisle~indicator, scales = 'free_y',ncol = 7) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Eutrophication (g PO4 eq per 100g)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9))  +
  coord_cartesian(xlim = c(0,25)) +
  geom_vline(xintercept = 8.16, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 8.16, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = .748, linetype = 2, colour = 'dark grey')
  # geom_text(aes(x = .748, y = 1, label = 'Wheat'), hjust = 0, size = 3)

# 
# # plotting and saving
# ghgs.plot
# Sys.sleep(60)
# ggsave(paste0(getwd(),"/Figures/Fig S2 Distribution of Indicators by Aisle GHGs.pdf"),
#        width = 30, height = 60, units = 'cm')
# ggsave(paste0(getwd(),"/Figures/Fig S2 Distribution of Indicators by Aisle GHGs.jpeg"),
#        width = 30, height = 60, units = 'cm')
# 
# eut.plot
# Sys.sleep(60)
# ggsave(paste0(getwd(),"/Figures/Fig S5 Distribution of Indicators by Aisle Eut.pdf"),
#        width = 30, height = 60, units = 'cm')
# ggsave(paste0(getwd(),"/Figures/Fig S5 Distribution of Indicators by Aisle Eut.jpeg"),
#        width = 30, height = 60, units = 'cm')
# 
# watscar.plot
# Sys.sleep(60)
# ggsave(paste0(getwd(),"/Figures/Fig S4 Distribution of Indicators by Aisle WatScar.pdf"),
#        width = 30, height = 60, units = 'cm')
# ggsave(paste0(getwd(),"/Figures/Fig S4 Distribution of Indicators by Aisle WatScar.jpeg"),
#        width = 30, height = 60, units = 'cm')
# 
# land.plot
# Sys.sleep(60)
# ggsave(paste0(getwd(),"/Figures/Fig S3 Distribution of Indicators by Aisle Land.pdf"),
#        width = 30, height = 60, units = 'cm')
# ggsave(paste0(getwd(),"/Figures/Fig S3 Distribution of Indicators by Aisle Land.jpeg"),
#        width = 30, height = 60, units = 'cm')


### And repeating for departments
# Stacking
plotted.dat <-
  rbind(plot.dat %>% dplyr::select(product_name, Retailer, Department, impact = mean_GHG) %>% mutate(indicator = 'GHGs'),
        plot.dat %>% dplyr::select(product_name, Retailer, Department, impact = mean_Land) %>% mutate(indicator = 'Land'),
        plot.dat %>% dplyr::select(product_name, Retailer, Department, impact = mean_Eut) %>% mutate(indicator = 'Eut'),
        plot.dat %>% dplyr::select(product_name, Retailer, Department, impact = mean_WatScar) %>% mutate(indicator = 'WatScar')) %>%
  # dplyr::select(Aisle) %>% 
  unique(.) %>%
  transform(indicator = factor(indicator, levels = c('GHGs','Land','WatScar','Eut')))


departments.ghgs <- ggplot(dat = plotted.dat %>% 
                      filter(Retailer %in% 'Tesco' & indicator %in% 'GHGs'), aes(x = impact)) +
  geom_density(colour = '#e78ac3') +
  facet_wrap(Department~indicator, scales = 'free_y',ncol = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Greenhouse Gas Emissions (kg CO2eq)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9)) +
  coord_cartesian(xlim = c(0,5)) +
  geom_vline(xintercept = 4.785, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 4.785, y = 1, label = 'Bovine meat\n(dairy herd)'), hjust = 1, size = 3) +
  geom_vline(xintercept = 1.28, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 1.28, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = .11, linetype = 2, colour = 'dark grey')
# geom_text(aes(x = .11, y = 1, label = 'Wheat'), hjust = 0, size = 3)

departments.eut <- ggplot(dat = plotted.dat %>% 
                             filter(Retailer %in% 'Tesco' & indicator %in% 'Eut'), aes(x = impact)) +
  geom_density(colour = '#66c2a5') +
  facet_wrap(Department~indicator, scales = 'free_y',ncol = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Eutrophication (g PO4 eq per 100g)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9))  +
  coord_cartesian(xlim = c(0,25)) +
  geom_vline(xintercept = 8.16, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 8.16, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = .748, linetype = 2, colour = 'dark grey')

departments.watscar <- ggplot(dat = plotted.dat %>% 
                             filter(Retailer %in% 'Tesco' & indicator %in% 'WatScar'), aes(x = impact)) +
  geom_density(colour = '#fc8d62') +
  facet_wrap(Department~indicator, scales = 'free_y',ncol = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Water Scarcity (liters)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9)) +
  coord_cartesian(xlim = c(0,15000)) +
  geom_vline(xintercept = 9101, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 9101, y = 1, label = 'Bovine meat\n(dairy herd)'), hjust = 1, size = 3) +
  geom_vline(xintercept = 6357, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 6357, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = 1278, linetype = 2, colour = 'dark grey')

departments.land <- ggplot(dat = plotted.dat %>% 
                             filter(Retailer %in% 'Tesco' & indicator %in% 'Land'), aes(x = impact)) +
  geom_density(colour = '#8da0cb') +
  facet_wrap(Department~indicator, scales = 'free_y',ncol = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = 'Land Use (square meters)', y = 'Density of Data') +
  theme(axis.text.x = element_text(hjust = .5, vjust = .5, size = 6)) +
  theme(axis.title = element_text(hjust = .5, vjust = .5, size = 9))  +
  coord_cartesian(xlim = c(0,10)) + 
  geom_vline(xintercept = 9.73, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 9.73, y = 1, label = 'Bovine meat\n(dairy herd)'), hjust = 1, size = 3) +
  geom_vline(xintercept = 1.666, linetype = 2, colour = 'dark grey') + 
  # geom_text(aes(x = 1.666, y = 1, label = 'Pig meat'), hjust = 0, size = 3) +
  geom_vline(xintercept = .4, linetype = 2, colour = 'dark grey')




# Merging together
library(cowplot)

merged.plot <-
  plot_grid(departments.ghgs,departments.land,departments.watscar,departments.eut,
            align = 'hv', ncol = 4)

merged.plot


# ggsave(paste0(getwd(),"/Figures/Fig S1 Indicators by Department 6Feb2022.pdf"),
#        width = 25, height = 25, units = 'cm')

write.csv(plotted.dat %>% dplyr::select(Retailer, Department, impact, indicator) %>% filter(!is.na(Retailer)),
       paste0(getwd(),"/Figure Data/Data Fig S1-S5  Distribution of Indicators by Department 6Feb2022.csv"))
