###
# Fig 3
# Impacts by Aisle for Tesco

# libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(readr)
library(cowplot)

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# data
plot.dat <- read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv"))

# Aggregating
plot.dat.aisle <-
  plot.dat %>%
  unique(.) %>%
  filter(!grepl('Halloween',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('\\bAll\\b',Aisle,ignore.case = TRUE)) %>%
  filter(!grepl('\\bAlcohol|Spirit|Beer|Cider|Cidre\\b',Aisle,ignore.case = TRUE)) %>%
  filter(!grepl('\\bAlcohol|Spirit|Beer|Cider|Cidre\\b',Department,ignore.case = TRUE)) %>%
  filter(!grepl('\\bAlcohol|Spirit|Beer|Cider|Cidre\\b',Shelf,ignore.case = TRUE)) %>%
  mutate(Shelf = ifelse(Retailer %in% 'Cook','Cook',Shelf)) %>%
  filter(!(Shelf %in% 'All')) %>%
  filter(!(Shelf %in% 'NULL')) %>%
  filter(Shelf != 'Water') %>%
  dplyr::select(product_name, id, Retailer, Department, Aisle, Tot_env_100g_scaled, Tot_env_100g_scaled_lower_ci, Tot_env_100g_scaled_upper_ci, NutriScore_Scaled,
                mean_GHG, mean_Eut, mean_Land, mean_WatScar) %>%
  .[complete.cases(.),] %>%
  unique(.) %>%
  group_by(Retailer, Department, Aisle) %>%
  dplyr::summarise(Tot_env_100g_scaled_se = sd(Tot_env_100g_scaled_lower_ci)/sqrt(n()),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled),
                   mean_GHG = mean(mean_GHG),
                   mean_WatScar = mean(mean_WatScar),
                   mean_Eut = mean(mean_Eut),
                   mean_Land = mean(mean_Land),
                   se_nutriscore = sd(NutriScore_Scaled) / sqrt(n()),
                   mean_nutriscore = mean(NutriScore_Scaled)) %>%
  as.data.frame(.) %>%
  arrange(Tot_env_100g_scaled) %>%
  filter(Aisle != 'Water') %>%
  mutate(Retailer = ifelse(Retailer %in% 'Tesco_Ireland','Tesco Ireland',Retailer)) %>%
  mutate(colour = ifelse(Retailer %in% 'Cook','#ce82ab',
                         ifelse(Retailer %in% 'Morissons','#e7a43f',
                                ifelse(Retailer %in% 'Ocado','#69b6e3',
                                       ifelse(Retailer %in% 'Sainsbury','#32a27b',
                                              ifelse(Retailer %in% 'Tesco','#eee45f',
                                                     ifelse(Retailer %in% 'Tesco Ireland','#2777b0','#d66839'))))))) %>%
  filter(!(grepl('alcohol|beer|cider|wine|spirits|tobacco|smoking',Aisle,ignore.case=TRUE))) %>%
  filter(!(Aisle %in% 'Bottled Water'))

# Ordering impacts by Aisle
dat.low <- plot.dat.aisle[1:10,]

dat.other <-
  plot.dat.aisle[11:nrow(plot.dat.aisle),] %>%
  mutate(panel_num = rank(Tot_env_100g_scaled, ties.method = 'first')) %>%
  mutate(panel_num = ifelse(panel_num < nrow(.)/6,1,
                            ifelse(panel_num <= (nrow(.)/6)*2,2,
                                   ifelse(panel_num <= (nrow(.)/6)*3,3,
                                          ifelse(panel_num <= (nrow(.)/6)*4,4,
                                                 ifelse(panel_num <= (nrow(.)/6)*5,5,6))))))


# And plotting
plot.list.tot <- list()
out.dat <- data.frame()
for (i in unique(dat.other$panel_num)) {
  tmp.dat <- 
    rbind(dat.low,dat.other  %>% filter(panel_num %in% i) %>% dplyr::select(-panel_num)) %>%
    mutate(Retailer_Department_Aisle = paste0(Retailer,Department,Aisle)) %>%
    transform(Retailer_Department_Aisle = factor(Retailer_Department_Aisle, levels = .$Retailer_Department_Aisle)) %>%
    mutate(panel_num = i) %>%
    mutate(position_x = 1:nrow(.))
  
  out.dat <- rbind(out.dat, tmp.dat)
  
    plot.list.tot[[i]] <-
      ggplot(tmp.dat, aes(x = Retailer_Department_Aisle, y = Tot_env_100g_scaled, colour = Retailer)) +
    theme_classic() +
    # geom_point(alpha = .5, colour = tmp.dat$colour) +
      geom_point(alpha = .75, colour = tmp.dat$colour) +
    # geom_errorbar(aes(ymin = (Tot_env_100g_scaled-Tot_env_100g_scaled_se), ymax = (Tot_env_100g_scaled+Tot_env_100g_scaled_se)), colour = tmp.dat$colour, alpha = .5) +
      geom_errorbar(aes(ymin = (Tot_env_100g_scaled-Tot_env_100g_scaled_se), ymax = (Tot_env_100g_scaled+Tot_env_100g_scaled_se)), alpha = .75, colour = tmp.dat$colour) +
    # geom_point(aes(y = overlap),shape = 8, colour = 'black') +
    # geom_point(aes(x = Department_Aisle_Shelf, y = scaled_tot_env), colour = 'blue') +
    # geom_errorbar(aes(ymin = scaled_tot_env_lower, ymax = scaled_tot_env_upper), colour = 'blue') +
    # scale_x_discrete(labels = substr(tmp.dat$Aisle,1,10)) +
      scale_x_discrete(labels = substr(tmp.dat$Aisle,1,20)) +
    # scale_colour_manual(values = unique(tmp.dat$colour)) +
      # scale_colour_manual(values = sort(unique(dat.other$colour[dat.other$Retailer %in% tmp.dat$Retailer]))) +
    # scale_y_continuous(trans = 'log10') +
    theme(legend.position = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 6)) +
    labs(x = NULL,y = 'Environmental Impact Score', colour = NULL) +
    geom_vline(xintercept = 10, linetype = 2) +
    theme(legend.position = 'bottom')
}

ggplot(out.dat, aes(x = position_x, y = Tot_env_100g_scaled, colour = Retailer)) +
  theme_classic() +
  # geom_point(alpha = .5, colour = tmp.dat$colour) +
  geom_point(alpha = .75, colour = tmp.dat$colour) +
  # geom_errorbar(aes(ymin = (Tot_env_100g_scaled-Tot_env_100g_scaled_se), ymax = (Tot_env_100g_scaled+Tot_env_100g_scaled_se)), colour = tmp.dat$colour, alpha = .5) +
  geom_errorbar(aes(ymin = (Tot_env_100g_scaled-Tot_env_100g_scaled_se), ymax = (Tot_env_100g_scaled+Tot_env_100g_scaled_se)), alpha = .75, colour = tmp.dat$colour) +
  # geom_point(aes(y = overlap),shape = 8, colour = 'black') +
  # geom_point(aes(x = Department_Aisle_Shelf, y = scaled_tot_env), colour = 'blue') +
  # geom_errorbar(aes(ymin = scaled_tot_env_lower, ymax = scaled_tot_env_upper), colour = 'blue') +
  # scale_x_discrete(labels = substr(tmp.dat$Aisle,1,10)) +
  scale_x_continuous(breaks = out.dat$position_x,labels = substr(out.dat$Aisle,1,20)) +
  # scale_colour_manual(values = unique(tmp.dat$colour)) +
  # scale_y_continuous(trans = 'log10') +
  theme(legend.position = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 6)) +
  labs(x = NULL,y = 'Environmental Impact Score', colour = NULL) +
  geom_vline(xintercept = 10, linetype = 2) +
  theme(legend.position = 'bottom') +
  facet_wrap(.~panel_num, nrow = 5, scales = 'free_y')


out.plot.tot <-
  plot_grid(plot.list.tot[[1]],plot.list.tot[[2]],plot.list.tot[[3]],plot.list.tot[[4]], plot.list.tot[[5]], plot.list.tot[[6]], nrow = length(plot.list.tot), align = 'hv')
dev.off()
out.plot.tot

Sys.sleep(15)

ggsave(paste0(getwd(),'/Figure S6 Impacts by Aisle.pdf'),
       width = 7.2, height = 15, units = 'in')


write.csv(plot.dat.aisle %>% dplyr::select(Retailer, Aisle, Tot_env_100g_scaled, Tot_env_100g_scaled_se, colour),
          paste0(getwd(),'/Figure Data/Figure Data S6.csv'),
          row.names = FALSE)
