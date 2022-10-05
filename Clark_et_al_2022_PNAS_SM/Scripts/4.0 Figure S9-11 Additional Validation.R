# libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)

# Setting working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# files
file.list <- list.files(path = paste0(getwd(),'/Validation/EstimatedComposition'), full.names = TRUE)
file.list <- file.list[!grepl('log',file.list,ignore.case=TRUE)]
# adding
out.df <- data.frame()
for(i in file.list) {
  out.df <- 
    rbind(out.df, 
          read.csv(i) %>%
            dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf,
                          variable, percent, Food_Category, Food_Category_sub, Food_Category_sub_sub,
                          n_ingredients, id_validate, info_validate, percent_known, n_percent_known))
}

# Limiting to products in the analysis
nutriscore.dat <- read_csv(paste0(getwd(),"/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv"))

out.df <-
  out.df %>%
  filter(product_name %in% nutriscore.dat$product_name) %>%
  filter(!grepl('beer|wine|spirits|liqu|alcohol|christmas|hallow',Department,ignore.case = TRUE)) %>%
  filter(!grepl('beer|wine|spirits|liqu|alcohol|christmas|hallow',Aisle,ignore.case = TRUE)) %>%
  filter(!grepl('beer|wine|spirits|liqu|alcohol|christmas|hallow',Shelf,ignore.case = TRUE))


# Getting fully known info
full.info <-
  out.df %>%
  filter(percent_known %in% 100)

# Merging
merged.df <-
  left_join(out.df %>% unique(),
            full.info %>% 
              dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf,
                            variable, percent_known_comp = percent) %>%
              unique())

# Comparing by ingredient order
merged.df <-
  merged.df %>%
  mutate(log_known = log2(percent / percent_known_comp)) %>%
  mutate(abs_dif = percent - percent_known_comp) 

merged.df <-
  merged.df %>%
  filter(!is.na(log_known)) %>%
  filter(!is.na(abs_dif))

merged.df <-
  merged.df %>%
  mutate(variable = gsub('V','',variable)) %>%
  mutate(variable = as.numeric(variable))

merged.df <-
  merged.df %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Barley (Beer)', 'Barley',
                                ifelse(Food_Category %in% 'Wheat & Rye (Bread)','Wheat',
                                       ifelse(Food_Category %in% 'Cereals & Oilcrops Misc.', 'Other Cereals and Oilcrops',
                                              ifelse(Food_Category %in% 'Maize (Meal)','Maize',Food_Category))))) %>%
  mutate(colour = ifelse(Food_Category %in% c('Apples','Bananas','Berries & Grapes','Barley','Wheat','Other Cereals and Oilcrops',
                                              'Brassicas','Cassava','Citrus Fruit',
                                              'Groundnuts','Maize','Nuts','Oatmeal','Olives','Onions & Leeks','Other Fruit','Other Pulses',
                                              'Other Vegetables','Peas','Potatoes','Rice','Root Vegetables','Sunflower seeds','Tomatoes'), '#009244',
                         ifelse(Food_Category %in% c('Beef','Bovine Meat (dairy herd)','Bovine Meat (beef herd)','Processed ruminant meat','Processed pig meat','Lamb & Mutton','Pig Meat'), '#c1272c',
                                ifelse(Food_Category %in% c('Crustaceans (farmed)','Fish (farmed)','Processed fish meat'),'#2e3192',
                                       ifelse(grepl('Oil',Food_Category, ignore.case = TRUE),'#f15a24',
                                              ifelse(Food_Category %in% c('Processed poultry meat','Poultry Meat'),'#ef4d83',
                                                     ifelse(Food_Category %in% c('Butter, Cream & Ghee','Cheese','Milk','Eggs'),'#666666',
                                                            ifelse(Food_Category %in% c('Dark Chocolate','Tea','Coffee'),'#4d3626',
                                                                   ifelse(grepl('Sugar',Food_Category),'#c39178',NA))))))))) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Dark Chocolate', 'Cocoa', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Pig Meat', 'Pork', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Fish (farmed)', 'Fish', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Crustaceans (farmed)', 'Crustaceans', Food_Category))

# Plotting
ggplot(merged.df %>% filter(percent_known %in% 0), aes(x = variable, y = (abs_dif), group = variable)) +
  # geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 2) +
  stat_summary() +
  labs(x = 'Order of Ingredient in the Product', y = 'Absolute difference between\nestimated and known composition\n(% of total product)') +
  theme_classic() +
  scale_x_continuous(breaks = 1:16) +
  coord_cartesian(ylim = c(-2,2))

# write.csv(merged.df %>% 
#             filter(percent_known %in% 0) %>% 
#             dplyr::select(percent_known, variable, abs_dif),
#           paste0(getwd(),'/Figure Data/Data Figure S9.csv'))

# ggsave(paste0(getwd(),'/Figures/Figure S9 Absolute difference estimated and known composition by ingredient order.pdf'),
#        width = 15, height = 8, units = 'cm')

ggplot(merged.df %>% filter(percent_known %in% 0, variable <= 10) %>%
         mutate(variable = paste0('Ingredient Order: ',variable)) %>%
         transform(variable = factor(variable, levels = paste0('Ingredient Order: ', 1:10))), 
       aes(x = abs_dif)) +
  geom_density() + 
  facet_wrap(.~variable, scales = 'free') +
  labs(x = 'Absolute difference between\nestimated and known composition\n(% of total product)', y = 'Proportion of observations') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = 2, colour = 'grey')

write.csv(merged.df %>% 
            filter(percent_known %in% 0) %>% 
            dplyr::select(variable, abs_dif),
          paste0(getwd(),'/Figure Data/Data Figures S9-S10.csv'))

# ggsave(paste0(getwd(),'/Figures/Figure S10 Density plots absolute difference in composition by ingredient order.pdf'),
#        width = 18, height = 12, units = 'cm')

# ggplot(merged.df %>% filter(percent_known %in% 0, variable <= 10), aes(x = abs_dif)) +
#   geom_density() + 
#   facet_wrap(.~Food_Category, scales = 'free') +
#   labs(x = 'Absolute difference between\nestimated and known composition\n(% of total product)', y = 'Proportion of observations') +
#   theme_classic() +
#   geom_vline(xintercept = 0, linetype = 2, colour = 'grey')

ggplot(merged.df %>% filter(percent_known %in% 0, variable <= 10) %>% filter(!is.na(Food_Category)), aes(x = variable, y = abs_dif, group = variable)) +
  stat_summary() +
  facet_wrap(.~Food_Category, scales = 'free') +
  coord_cartesian(ylim = c(-30,30), xlim = c(0,11)) +
  labs(x = 'Order of Ingredient in the Product', y = 'Absolute difference between\nestimated and known composition\n(% of total product)') +
  theme_classic() +
  scale_x_continuous(breaks = 1:16)

write.csv(merged.df %>% 
            filter(percent_known %in% 0, variable <= 10) %>% 
            filter(!is.na(Food_Category)) %>% unique() %>%
            dplyr::select(Food_Category, percent_known, variable, abs_dif) %>% unique(),
          paste0(getwd(),'/Figure Data/Data Figure S11.csv'))

# ggsave(paste0(getwd(),'/Figures/Figure S11 Difference between estimated and known composition by ingredient order and food category.pdf'),
#        width = 40, height = 30, units = 'cm')  

# write.csv(merged.df %>% dplyr::select(variable_order = variable, Food_Category, Food_Category_sub, Food_Category_sub_sub,
#                                       total_number_of_ingredients = n_ingredients, total_percent_known = percent_known,
#                                       percent_estimated = percent, percent_known = percent_known_comp,
#                                       log2_difference = log_known, abs_dif = abs_dif),
#           paste0(getwd(),'/Figure Data/Figures S9-11 Additional Validation.csv'),
#           row.names = FALSE)

# Summary stats by ingredient location
# Across all ingredients
t.test(merged.df$percent, merged.df$percent_known_comp)
t.test(merged.df$percent[merged.df$percent_known %in% 0], merged.df$percent_known_comp[merged.df$percent_known %in% 0])

# By ingredient order
out.df <- data.frame()
for(i in sort(unique(merged.df$variable))) {
  tmp <- t.test(merged.df$percent[merged.df$variable %in% i],
         merged.df$percent_known_comp[merged.df$variable %in% i])
  
  out.df <-
    rbind(out.df,
          data.frame(ingredient_order = i,
                     p_value = tmp$p.value,
                     estimated_comp = tmp$estimate[1],
                     known_comp = tmp$estimate[2],
                     ci_lower = tmp$conf.int[[1]],
                     ci_upper = tmp$conf.int[[2]],
                     df = tmp$parameter))
}

out.df$mean_dif = out.df$estimated_comp - out.df$known_comp
out.df <-
  out.df %>%
  dplyr::select(ingredient_order,
                p_value,
                estimated_comp, known_comp, dif_comp = mean_dif,
                ci_lower, ci_upper, df)

write.csv(out.df,
          paste0(getwd(),'/Tables/Table S4 Paired T-tests by ingredient order all iterations.csv'),
          row.names = FALSE)

# By ingredient order
out.df <- data.frame()
for(i in sort(unique(merged.df$variable))) {
  tmp <- t.test(merged.df$percent[merged.df$variable %in% i & merged.df$percent_known %in% 0],
                merged.df$percent_known_comp[merged.df$variable %in% i & merged.df$percent_known %in% 0])
  
  out.df <-
    rbind(out.df,
          data.frame(ingredient_order = i,
                     p_value = tmp$p.value,
                     estimated_comp = tmp$estimate[1],
                     known_comp = tmp$estimate[2],
                     ci_lower = tmp$conf.int[[1]],
                     ci_upper = tmp$conf.int[[2]],
                     df = tmp$parameter))
}

out.df$mean_dif = out.df$estimated_comp - out.df$known_comp
out.df <-
  out.df %>%
  dplyr::select(ingredient_order,
                p_value,
                estimated_comp, known_comp, dif_comp = mean_dif,
                ci_lower, ci_upper, df)

write.csv(out.df,
          paste0(getwd(),'/Tables/Table S4 Paired T-tests by ingredient order no known ingredients.csv'),
          row.names = FALSE)


# Summary stats by food category
out.df <-
  merged.df %>%
  dplyr::group_by(Food_Category) %>%
  dplyr::summarise(percent_estimated = mean(percent),
                   percent_known = mean(percent_known_comp),
                   mean_abs_dif = mean(abs_dif),
                   se_dif = sd(abs_dif) / sqrt(n()),
                   count = n())

sum(abs(out.df$mean_abs_dif<=1))
sum(abs(out.df$mean_abs_dif<=5))
sum(abs(out.df$mean_abs_dif<=10))


write.csv(out.df,
          paste0(getwd(),'/Tables/Table S5 Estimated and known composition of products by environmental food category.csv'),
          row.names = FALSE)


# Summary stats by food category
out.df <-
  merged.df %>%
  filter(percent_known %in% 0) %>%
  dplyr::group_by(Food_Category) %>%
  dplyr::summarise(percent_estimated = mean(percent),
                   percent_known = mean(percent_known_comp),
                   mean_abs_dif = mean(abs_dif),
                   se_dif = sd(abs_dif) / sqrt(n()),
                   count = n())

sum(abs(out.df$mean_abs_dif <= 1))
sum(abs(out.df$mean_abs_dif <= 5))
sum(abs(out.df$mean_abs_dif <= 10))

write.csv(out.df,
          paste0(getwd(),'/Tables/Table S5 Estimated and known composition of products by environmental food category no known ingredients.csv'),
          row.names = FALSE)
