### Figure 2
# Testing accuracy of impacts

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(Hmisc)
library(cowplot)
library(readr)

# Setting colour palette
# From http://bconnelly.net/posts/creating_colorblind-friendly_figures/
my.palette <- c("125 125 125",# Black
                "230 159 0",
                "86 180 233",
                "0 158 115",
                "240 228 66",
                "0 114 178",
                "213 94 0",
                "204 121 167")
# Splitting
tmp <- strsplit(my.palette, " ")
# Function to convert into hex
rgb.fun <-
  function(x) {
    rgb(x[1],x[2],x[3],maxColorValue=255)
  }
# And converting into hex
my.palette <- unlist(lapply(tmp,rgb.fun))


# Subsetting to products we use in the main analsyis
used.products <- 
  read.csv(paste0(getwd(),'/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv'), stringsAsFactors = FALSE)

# Getting estimated impacts for scaling
estimated.impacts <-
  read.csv(paste0(getwd(),'/Managed_Data/Impacts by Product 18Feb2022 Log2.csv'), stringsAsFactors = FALSE) %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE)) %>%
  mutate(scaled_GHG = mean_GHG / max(.$mean_GHG),
         scaled_Eut = mean_Eut / max(.$mean_Eut),
         scaled_WatScar = mean_WatScar / max(.$mean_WatScar),
         scaled_Land = mean_Land / max(.$mean_Land),
         scaled_lower_ci_GHG = lower_ci_GHG / max(.$mean_GHG),
         scaled_lower_ci_Eut = lower_ci_Eut / max(.$mean_Eut),
         scaled_lower_ci_Land = lower_ci_Land / max(.$mean_Land),
         scaled_lower_ci_WatScar = lower_ci_WatScar / max(.$mean_WatScar),
         scaled_upper_ci_GHG = upper_ci_GHG / max(.$mean_GHG),
         scaled_upper_ci_Eut = upper_ci_Eut / max(.$mean_Eut),
         scaled_upper_ci_Land = upper_ci_Land / max(.$mean_Land),
         scaled_upper_ci_WatScar = upper_ci_WatScar / max(.$mean_WatScar)) %>%
  mutate(scaled_tot_env = (scaled_GHG + scaled_Eut + scaled_WatScar + scaled_Land) / 4,
         scaled_tot_env_lower = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar) / 4,
         scaled_tot_env_upper = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar) / 4)

# Validated estiamtes
validate.dat <-
  read.csv(paste0(getwd(),'/Managed_Data/Validating Impacts by Product 6Feb2022.csv'), stringsAsFactors = FALSE) %>%
  filter(paste0(id, product_name) %in% paste0(used.products$id, used.products$product_name)) %>%
  mutate(scaled_GHG = mean_GHG / max(estimated.impacts$mean_GHG),
         scaled_Eut = mean_Eut / max(estimated.impacts$mean_Eut),
         scaled_WatScar = mean_WatScar / max(estimated.impacts$mean_WatScar),
         scaled_Land = mean_Land / max(estimated.impacts$mean_Land),
         scaled_lower_ci_GHG = lower_ci_GHG / max(estimated.impacts$mean_GHG),
         scaled_lower_ci_Eut = lower_ci_Eut / max(estimated.impacts$mean_Eut),
         scaled_lower_ci_Land = lower_ci_Land / max(estimated.impacts$mean_Land),
         scaled_lower_ci_WatScar = lower_ci_WatScar / max(estimated.impacts$mean_WatScar),
         scaled_upper_ci_GHG = upper_ci_GHG / max(estimated.impacts$mean_GHG),
         scaled_upper_ci_Eut = upper_ci_Eut / max(estimated.impacts$mean_Eut),
         scaled_upper_ci_Land = upper_ci_Land / max(estimated.impacts$mean_Land),
         scaled_upper_ci_WatScar = upper_ci_WatScar / max(estimated.impacts$mean_WatScar)) %>%
  mutate(scaled_tot_env = (scaled_GHG + scaled_Eut + scaled_WatScar + scaled_Land) / 4,
         scaled_tot_env_lower = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar) / 4,
         scaled_tot_env_upper = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar) / 4) %>%
  mutate(scaled_tot_env = scaled_tot_env / max(estimated.impacts$scaled_tot_env),
         scaled_tot_env_lower = scaled_tot_env_lower / max(estimated.impacts$scaled_tot_env),
         scaled_tot_env_upper = scaled_tot_env_upper / max(estimated.impacts$scaled_tot_env))

### 
# Getting average by retailer
dat.retail <-
  left_join(estimated.impacts %>% 
              dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf) %>%
              unique(.),
            validate.dat %>% 
              filter(info_validate %in% 'none') %>% 
              dplyr::select(id, product_name, tot_env_known = scaled_tot_env) %>%
              group_by(id, product_name) %>%
              dplyr::summarise(tot_env_known = mean(tot_env_known))) %>%
  left_join(.,
            validate.dat %>%
              filter(percent_known %in% 0) %>%
              dplyr::select(id, product_name, tot_env_estimated = scaled_tot_env) %>%
              group_by(id, product_name) %>%
              dplyr::summarise(tot_env_estimated = mean(tot_env_estimated))) %>%
  filter(!is.na(tot_env_known)) %>%
  filter(!is.na(tot_env_estimated)) %>%
  mutate(per_dif = 1 - tot_env_estimated/tot_env_known) %>%
  dplyr::select(Retailer, product_name, per_dif) %>%
  unique(.) %>%
  group_by(Retailer) %>%
  dplyr::summarise(mean_per_dif = mean(per_dif),
                   se_dif = sd(per_dif) / sqrt(n()))

# # Getting categorisations
cat.dat <- read_csv(paste0(getwd(),'/foodDB_dat/categories.csv'))

# And merging in NutriScore data
nutri.dat <- 
  read_csv(paste0(getwd(),'/Managed_Data/Impacts by Product 21January2022 Log2.csv')) %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE))

# Getting known impacts from using back of package info
known.estimates <-
  left_join(validate.dat %>%
              filter(info_validate %in% 'none') %>%
              group_by(id, product_name) %>%
              dplyr::summarise(mean_GHG_known = mean(mean_GHG),
                               lower_ci_GHG_known = mean(lower_ci_GHG),
                               upper_ci_GHG_known = mean(upper_ci_GHG),
                               scaled_tot_env_known = mean(scaled_tot_env),
                               scaled_tot_env_lower_known = mean(scaled_tot_env_lower),
                               scaled_tot_env_upper_known = mean(scaled_tot_env_upper)),
            validate.dat %>%
              filter(n_percent_known %in% 0 & percent_known %in% 0) %>%
              group_by(id, product_name) %>%
              dplyr::summarise(mean_GHG = mean(mean_GHG),
                               lower_ci_GHG = mean(lower_ci_GHG),
                               upper_ci_GHG = mean(upper_ci_GHG),
                               scaled_tot_env = mean(scaled_tot_env),
                               scaled_tot_env_lower = mean(scaled_tot_env_lower),
                               scaled_tot_env_upper = mean(scaled_tot_env_upper))) %>%
  unique(.) %>%
  arrange(scaled_tot_env_known)

# Merging in
known.estimates <- 
  left_join(known.estimates,cat.dat %>% dplyr::select(id = product_id, Department = main_category, Aisle = department, Shelf = aisle)) %>%
  group_by(Department, Aisle, Shelf) %>%  dplyr::summarise(mean_GHG_known = mean(mean_GHG_known),
                                                           lower_ci_GHG_known = mean(lower_ci_GHG_known),
                                                           upper_ci_GHG_known = mean(upper_ci_GHG_known),
                                                           scaled_tot_env_known = mean(scaled_tot_env_known),
                                                           scaled_tot_env_lower_known = mean(scaled_tot_env_lower_known),
                                                           scaled_tot_env_upper_known = mean(scaled_tot_env_upper_known),
                                                           mean_GHG = mean(mean_GHG),
                                                           lower_ci_GHG = mean(lower_ci_GHG),
                                                           upper_ci_GHG = mean(upper_ci_GHG),
                                                           scaled_tot_env = mean(scaled_tot_env),
                                                           scaled_tot_env_lower = mean(scaled_tot_env_lower),
                                                           scaled_tot_env_upper = mean(scaled_tot_env_upper),
                                                           count = n()) %>%
  unique(.) %>%
  filter(!(Shelf %in% 'NULL')) %>%
  arrange(scaled_tot_env_known) %>%
  mutate(Department_Aisle_Shelf = paste0(Department,Aisle,'_',Shelf)) %>%
  transform(Department_Aisle_Shelf = factor(Department_Aisle_Shelf, levels = .$Department_Aisle_Shelf)) %>%
  filter(!is.na(scaled_tot_env)) %>%
  filter(!is.na(scaled_tot_env_known)) %>%
  mutate(ratio_dif = log2(scaled_tot_env / scaled_tot_env_known)) %>%
  mutate(abs_ratio_dif = abs(ratio_dif)) %>%
  filter(Aisle != 'Bottled Water') %>%
  filter(Shelf != 'Water') %>%
  filter(!(grepl('Beer|Wine|Spirit|Cider|Alcohol|Liqu|Christ|Hallow',Department))) %>%
  filter(!(grepl('Beer|Wine|Spirit|Cider|Alcohol|Liqu|Christ|Hallow',Aisle))) %>%
  filter(!(grepl('Beer|Wine|Spirit|Cider|Alcohol|Liqu|Christ|Hallow',Shelf))) %>%
  mutate(overlap = 0) %>%
  mutate(overlap = ifelse(scaled_tot_env_lower >= scaled_tot_env_lower_known & scaled_tot_env_lower <= scaled_tot_env_upper_known, 1,
                          ifelse(scaled_tot_env_upper >= scaled_tot_env_lower_known & scaled_tot_env_upper <= scaled_tot_env_upper_known, 1, overlap)))

# Stats on % of aisles within 10% and 25% of known estimates
sum(known.estimates$abs_ratio_dif <= log2(1.1))/nrow(known.estimates)
sum(known.estimates$abs_ratio_dif <= log2(1.25))/nrow(known.estimates)

# Percent of Shelves that overlap
sum(known.estimates$overlap)/nrow(known.estimates)

# Difference by retailer
# dat.retailer <-
#   known.estimates %>%
#   group_by(Retailer) %>% 
#   dplyr::summarise(scaled_tot_env = mean(scaled_tot_env),
#                    scaled_tot_env_known = mean(scaled_tot_env_known))

dat.low <- 
  known.estimates[1:10,] %>%
  mutate(which_greater = ifelse(scaled_tot_env_upper>scaled_tot_env_upper_known,scaled_tot_env_upper * 1.25,scaled_tot_env_upper_known*1.25)) %>%
  mutate(which_greater = which_greater * 100) %>%
  mutate(overlap = NA) %>%
  mutate(overlap = ifelse(scaled_tot_env_lower >= scaled_tot_env_lower_known & scaled_tot_env_lower <= scaled_tot_env_upper_known, which_greater,
                          ifelse(scaled_tot_env_upper >= scaled_tot_env_lower_known & scaled_tot_env_upper <= scaled_tot_env_upper_known, which_greater,overlap)))
known.estimates <- known.estimates[11:nrow(known.estimates),]
known.estimates <-
  known.estimates %>%
  mutate(panel_num = rank(scaled_tot_env_known, ties.method = 'first')) %>%
  mutate(panel_num = ifelse(panel_num < nrow(known.estimates)/6,1,
                            ifelse(panel_num <= (nrow(known.estimates)/6)*2,2,
                                   ifelse(panel_num <= (nrow(known.estimates)/6)*3,3,
                                          ifelse(panel_num <= (nrow(known.estimates)/6)*4,4,
                                                 ifelse(panel_num <= (nrow(known.estimates)/6)*5,5,6)))))) %>%
  mutate(which_greater = ifelse(scaled_tot_env_upper>scaled_tot_env_upper_known,scaled_tot_env_upper * 1.25,scaled_tot_env_upper_known*1.25)) %>%
  mutate(which_greater = which_greater * 100) %>%
  mutate(overlap = NA) %>%
  mutate(overlap = ifelse(scaled_tot_env_lower >= scaled_tot_env_lower_known & scaled_tot_env_lower <= scaled_tot_env_upper_known, which_greater,
                          ifelse(scaled_tot_env_upper >= scaled_tot_env_lower_known & scaled_tot_env_upper <= scaled_tot_env_upper_known, which_greater,overlap)))


# And plotting
plot.list.tot <- list()
for (i in unique(known.estimates$panel_num)) {
  tmp.dat <- 
    rbind(dat.low,known.estimates  %>% filter(panel_num %in% i) %>% dplyr::select(-panel_num)) %>%
    transform(Department_Aisle_Shelf = factor(Department_Aisle_Shelf, levels = .$Department_Aisle_Shelf))
  
  tmp.dat <-
    rbind(tmp.dat %>% dplyr::select(Department_Aisle_Shelf,Shelf, env_mean = scaled_tot_env_known, env_lower = scaled_tot_env_lower_known, env_upper = scaled_tot_env_upper_known, overlap) %>% mutate(Known = 'Known Impacts'),
          tmp.dat %>% dplyr::select(Department_Aisle_Shelf,Shelf, env_mean = scaled_tot_env, env_lower = scaled_tot_env_lower, env_upper = scaled_tot_env_upper, overlap) %>% mutate(Known = 'Estimated Impacts')) %>%
    mutate(overlap = ifelse(is.na(overlap),NA,max(overlap,na.rm=TRUE))) %>%
    mutate(Shelf = ifelse(is.na(overlap),Shelf,paste0('* ',Shelf)))
  
  if(i %in% 2) {
    plot.list.tot[[i]] <-
      ggplot(tmp.dat, aes(x = Department_Aisle_Shelf, y = env_mean*100, colour = Known)) +
      theme_classic() +
      geom_point(alpha = 1) +
      geom_errorbar(aes(ymin = env_lower*100, ymax = env_upper*100), alpha = 1) +
      # geom_point(aes(y = overlap),shape = 8, colour = 'black') +
      # geom_point(aes(x = Department_Aisle_Shelf, y = scaled_tot_env), colour = 'blue') +
      # geom_errorbar(aes(ymin = scaled_tot_env_lower, ymax = scaled_tot_env_upper), colour = 'blue') +
      scale_x_discrete(breaks = tmp.dat$Department_Aisle_Shelf, labels = substr(tmp.dat$Shelf,1,20)) +
      # scale_y_continuous(trans = 'log10') +
      scale_colour_manual(values = c(my.palette[2],my.palette[6])) +
      theme(legend.position = NULL) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5,vjust = .5)) +
      labs(x = NULL, y = 'Environmental Impact Score', colour = NULL) +
      geom_vline(xintercept = 10, linetype = 2) +
      scale_y_continuous(limits = c(0,2), breaks = c(0,.5,1,1.5,2), expand = c(0,0)) +
      theme(legend.position = 'bottom')
  } else {
    plot.list.tot[[i]] <-
      ggplot(tmp.dat, aes(x = Department_Aisle_Shelf, y = env_mean*100, colour = Known)) +
      theme_classic() +
      geom_point(alpha = 1) +
      geom_errorbar(aes(ymin = env_lower*100, ymax = env_upper*100), alpha = 1) +
      # geom_point(aes(y = overlap),shape = 8, colour = 'black') +
      # geom_point(aes(x = Department_Aisle_Shelf, y = scaled_tot_env), colour = 'blue') +
      # geom_errorbar(aes(ymin = scaled_tot_env_lower, ymax = scaled_tot_env_upper), colour = 'blue') +
      scale_x_discrete(breaks = tmp.dat$Department_Aisle_Shelf, labels = substr(tmp.dat$Shelf,1,20)) +
      # scale_y_continuous(trans = 'log10') +
      scale_colour_manual(values = c(my.palette[2],my.palette[6])) +
      theme(legend.position = NULL) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5,vjust = .5)) +
      labs(x = NULL, y = 'Environmental Impact Score', colour = NULL) +
      geom_vline(xintercept = 10, linetype = 2) +
      scale_y_continuous(limits = c(0,max(tmp.dat$env_upper * 100 * 1.125)), expand = c(0,0), n.breaks = 4) +
      theme(legend.position = 'bottom')
  }
  
}

out.plot.tot <-
  plot_grid(plot.list.tot[[1]],plot.list.tot[[2]],plot.list.tot[[3]],plot.list.tot[[4]], plot.list.tot[[5]], plot.list.tot[[6]], nrow = length(plot.list.tot), align = 'hv')
dev.off()

out.plot.tot
out.plot.tot

Sys.sleep(15)

# ggsave(paste0(getwd(),'/Figures/Figure S12 Testing Imapcts by Category 6Feb2022.pdf'),
#        width = 7.2, height = 15, units = 'in')


# And saving data
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}

write.csv(rbind(dat.low %>% dplyr::select(Department, Aisle, Shelf, scaled_tot_env_known, scaled_tot_env_lower_known, scaled_tot_env_upper_known, scaled_tot_env_estimated = scaled_tot_env, scaled_tot_env_lower_estimated = scaled_tot_env_lower, scaled_tot_env_upper_estimated = scaled_tot_env_upper, ratio_dif),
known.estimates %>% dplyr::select(Department, Aisle, Shelf, scaled_tot_env_known, scaled_tot_env_lower_known, scaled_tot_env_upper_known, scaled_tot_env_estimated = scaled_tot_env, scaled_tot_env_lower_estimated = scaled_tot_env_lower, scaled_tot_env_upper_estimated = scaled_tot_env_upper, ratio_dif)) %>% unique(.),
          paste0(getwd(),'/Figure Data/Data Fig S12 6Feb2022.csv'),row.names = FALSE)







