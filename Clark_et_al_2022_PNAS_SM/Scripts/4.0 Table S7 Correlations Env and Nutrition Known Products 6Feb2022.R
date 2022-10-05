### Figure 2
# Testing accuracy of impacts

# Working directory
setwd("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Runs 19April2021")

# Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(Hmisc)
library(cowplot)
library(readr)

# Subsetting to products we use in the main analsyis
used.products <- 
  # read_csv(paste0(getwd(),'/Managed_Data/NutriScore for radar plots 6Feb2022 Log2.csv')) 
  read_csv("/Users/macuser/Desktop/Managed_Data/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv")

# Getting estimated impacts for scaling
estimated.impacts <-
  # read_csv(paste0(getwd(),'/Managed_Data/Impacts by Product 21January022 Log2.csv')) %>%
  read_csv("/Users/macuser/Desktop/Managed_Data/Managed_Data/Impacts by Product 21January2022 Log2.csv") %>%
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
  # read.csv(paste0(getwd(),'/Managed_Data/Validating Impacts by Product 6Feb2022 Log2.csv')) %>%
  read_csv("/Users/macuser/Desktop/fooddb_Managed_Data/Validating Impacts by Product 6Feb2022.csv") %>%
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

# # Getting categorisations
cat.dat <- read_csv(paste0(getwd(),'/foodDB_dat/categories.csv'))

# And merging in NutriScore data
nutri.dat <- 
  # read_csv(paste0(getwd(),'/Managed_Data/Impacts by Product 21January2022 Log2.csv'))
  read_csv("/Users/macuser/Desktop/Managed_Data/Managed_Data/Impacts by Product 21January2022 Log2.csv")

# Getting known impacts from using back of package info
known.estimates <-
  validate.dat %>%
  filter(info_validate %in% 'none') %>%
  group_by(id, product_name) %>%
  dplyr::summarise(mean_GHG_known = mean(mean_GHG),
                   lower_ci_GHG_known = mean(lower_ci_GHG),
                   upper_ci_GHG_known = mean(upper_ci_GHG),
                   scaled_tot_env_known = mean(scaled_tot_env),
                   scaled_tot_env_lower_known = mean(scaled_tot_env_lower),
                   scaled_tot_env_upper_known = mean(scaled_tot_env_upper)) %>%
  unique(.) %>%
  arrange(scaled_tot_env_known)

# Merging in
known.estimates <- 
  left_join(known.estimates,
            nutri.dat %>% dplyr::select(id, product_name, NutriScore_Scaled, NutriScoreLetter, Retailer, Department, Aisle, Shelf)) %>%
  mutate(NutriScore_Scaled = ifelse(NutriScoreLetter %in% 'A',1,
                                    ifelse(NutriScoreLetter %in% 'B',2,
                                           ifelse(NutriScoreLetter %in% 'C',3,
                                                  ifelse(NutriScoreLetter %in% 'D',4,5))))) %>%
  filter(Aisle != 'Bottled Water') %>%
  filter(Shelf != 'Water') %>%
  filter(!(grepl('Beer|Wine|Cider|Cidre|Spirit|Alcohol',Department,ignore.case=TRUE))) %>%
  filter(!(grepl('Beer|Wine|Cider|Cidre|Spirit|Alcohol',Aisle,ignore.case=TRUE))) %>%
  filter(!(grepl('Beer|Wine|Cider|Cidre|Spirit|Alcohol',Shelf,ignore.case=TRUE))) %>%
  filter(!grepl('Halloween',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas|Hallow|October',Shelf, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas|Hallow|October',Department, ignore.case = TRUE)) %>%
  filter(!(grepl('Christmas|Water',Aisle))) %>%
  filter(!(Shelf %in% 'NULL')) %>%
  group_by(Retailer, Department, Aisle) %>%  
  dplyr::summarise(se_env = sd(scaled_tot_env_known) / sqrt(n()),
                   scaled_tot_env_known = mean(scaled_tot_env_known),
                   scaled_tot_env_lower_known = mean(scaled_tot_env_lower_known),
                   scaled_tot_env_upper_known = mean(scaled_tot_env_upper_known),
                   scaled_nutrition = mean(NutriScore_Scaled, na.rm = TRUE),
                   se_nutrition = sd(NutriScore_Scaled, na.rm = TRUE)/sqrt(n()),
                   count = n()) %>%
  unique(.) %>%
  mutate(Retailer = ifelse(Retailer %in% "Tesco_Ireland",'Tesco Ireland',Retailer)) %>% 
  filter(!grepl('Water',Aisle)) %>%
  filter(Aisle != 'Speciality') %>%
  mutate(Drink = 'Food') %>%
  mutate(Drink = ifelse(grepl('Drink|Juice|Tea|Coffee|Beer',Department, ignore.case=TRUE),'Drink','Food')) %>%
  mutate(Drink = ifelse(grepl('Drink|Smoothie|juice|beer|wine|spirit|cider|cidre|milk|squash|cordial|cola|spritzer|sparkling|water|tea|coffee|hot beverages|hot chocolate', Aisle, ignore.case = TRUE),'Drink',Drink)) %>%
  mutate(se_env = ifelse(is.na(se_env),0,se_env))



# Plot
ggplot(known.estimates, aes(x = scaled_nutrition, y = scaled_tot_env_known*100, colour = Drink)) +
  theme_classic() +
  geom_point() +
  facet_wrap(.~Retailer) + 
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(limits = c(.75,5.25),breaks = 1:5, labels = c('A','B','C','D','E')) +
  labs(y = 'Environmental Impact Score',x = 'Nutrition Impact Score')

# And Spearmans Correlations
plot.dat.aisle <-
  known.estimates %>%
  dplyr::rename(NutriScore_Scaled = scaled_nutrition, se_NutriScore = se_nutrition,
                Tot_env_100g_scaled = scaled_tot_env_known, Tot_env_100g_scaled_se = se_env) %>%
  mutate(se_NutriScore = ifelse(is.na(se_NutriScore),0,se_NutriScore)) %>%
  mutate(Tot_env_100g_scaled_se = ifelse(is.na(Tot_env_100g_scaled_se),0,Tot_env_100g_scaled_se))

# Correlations between nutrition and env by retailer
# Correlations between nutrition and env by retailer
set.seed(19)

# Across all retailers
corr.df <- 
  data.frame()

tmp.dat <- 
  plot.dat.aisle %>%
  filter(Drink %in% 'Food')

spearman.dat <-
  cor.test(tmp.dat$NutriScore_Scaled, tmp.dat$Tot_env_100g_scaled,
           method = 'spearman', conf.level = .95)

corr.df <-
  rbind(corr.df,
        data.frame(Retailer = 'Across All Retailers',
                   Rho = spearman.dat$estimate,
                   P_value = spearman.dat$p.value))

corr.df.mean <- corr.df


# And randomly selecting performance data to see how assumptions on e.g. sourcing affect results
corr.df <-
  data.frame()
# for(retail in retailers) {
# counter
counter = 1
# Retailer data
tmp.dat.retail <- 
  plot.dat.aisle %>%
  # filter(Retailer %in% retail) %>%
  filter(Drink %in% 'Food')

for(i in 1:1000) {
  # Adding to counter
  counter = counter + 1
  # Randomly selecting data
  tmp.dat <- 
    tmp.dat.retail %>%
    as.data.frame(.) %>%
    group_by(Aisle) %>%
    plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
    plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = Tot_env_100g_scaled_se))
  
  # running the correlation
  spearman.dat <-
    cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = 'Across All Retailers',
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value,
                     test_iteration = counter))
  # Randomly sampling data
}
# }
corr.df.montecarlo <-
  corr.df %>% 
  mutate(count = ifelse(P_value < 0.05,1,0)) %>%
  group_by(Retailer) %>%
  dplyr::summarise(n_significant = sum(count),
                   se_Rho = sd(Rho) / sqrt(1000),
                   min_Rho = min(Rho),
                   max_Rho = max(Rho),
                   Rho = mean(Rho),
                   se_P = sd(P_value) / sqrt(1000),
                   min_P = min(P_value),
                   max_P = max(P_value),
                   P_value = mean(P_value))


# joining
corr.df.out.tot <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)

# At each retailer
retailers <- sort(unique(plot.dat.aisle$Retailer))

corr.df <- 
  data.frame()
for(retail in retailers) {
  tmp.dat <- 
    plot.dat.aisle %>%
    filter(Retailer %in% retail) %>%
    filter(Drink %in% 'Food')
  
  spearman.dat <-
    cor.test(tmp.dat$NutriScore_Scaled, tmp.dat$Tot_env_100g_scaled,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = retail,
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value))
}

corr.df.mean <- corr.df


# And randomly selecting performance data to see how assumptions on e.g. sourcing affect results
corr.df <-
  data.frame()
for(retail in retailers) {
  # counter
  counter = 1
  # Retailer data
  tmp.dat.retail <- 
    plot.dat.aisle %>%
    filter(Retailer %in% retail) %>%
    filter(Drink %in% 'Food')
  
  for(i in 1:1000) {
    # Adding to counter
    counter = counter + 1
    # Randomly selecting data
    tmp.dat <- 
      tmp.dat.retail %>%
      as.data.frame(.) %>%
      group_by(Aisle) %>%
      plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
      plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = Tot_env_100g_scaled_se))
    
    # running the correlation
    spearman.dat <-
      cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
               method = 'spearman', conf.level = .95)
    
    corr.df <-
      rbind(corr.df,
            data.frame(Retailer = retail,
                       Rho = spearman.dat$estimate,
                       P_value = spearman.dat$p.value,
                       test_iteration = counter))
    # Randomly sampling data
  }
}

# Summary stats across the tests
corr.df.montecarlo <-
  corr.df %>% 
  mutate(count = ifelse(P_value < 0.05,1,0)) %>%
  group_by(Retailer) %>%
  dplyr::summarise(n_significant = sum(count),
                   se_Rho = sd(Rho) / sqrt(1000),
                   min_Rho = min(Rho),
                   max_Rho = max(Rho),
                   Rho = mean(Rho),
                   se_P = sd(P_value) / sqrt(1000),
                   min_P = min(P_value),
                   max_P = max(P_value),
                   P_value = mean(P_value))


# joining
corr.df.out <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)

corr.df.write.foods <- 
  rbind(corr.df.out.tot, corr.df.out)

# And getting correlations for single ingredient foods ----
# Setting working directory
setwd("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Runs 19April2021")



#####
# Repeating the above for only drinks

set.seed(19)

# Across all retailers
corr.df <- 
  data.frame()

tmp.dat <- 
  plot.dat.aisle %>%
  filter(Drink %in% 'Drink')

spearman.dat <-
  cor.test(tmp.dat$NutriScore_Scaled, tmp.dat$Tot_env_100g_scaled,
           method = 'spearman', conf.level = .95)

corr.df <-
  rbind(corr.df,
        data.frame(Retailer = 'Across All Retailers',
                   Rho = spearman.dat$estimate,
                   P_value = spearman.dat$p.value))

corr.df.mean <- corr.df


# And randomly selecting performance data to see how assumptions on e.g. sourcing affect results
corr.df <-
  data.frame()
# for(retail in retailers) {
# counter
counter = 1
# Retailer data
tmp.dat.retail <- 
  plot.dat.aisle %>%
  # filter(Retailer %in% retail) %>%
  filter(Drink %in% 'Drink')

for(i in 1:1000) {
  # Adding to counter
  counter = counter + 1
  # Randomly selecting data
  tmp.dat <- 
    tmp.dat.retail %>%
    as.data.frame(.) %>%
    group_by(Aisle) %>%
    plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
    plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = Tot_env_100g_scaled_se))
  
  # running the correlation
  spearman.dat <-
    cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = 'Across All Retailers',
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value,
                     test_iteration = counter))
  # Randomly sampling data
}
# }
corr.df.montecarlo <-
  corr.df %>% 
  mutate(count = ifelse(P_value < 0.05,1,0)) %>%
  group_by(Retailer) %>%
  dplyr::summarise(n_significant = sum(count),
                   se_Rho = sd(Rho) / sqrt(1000),
                   min_Rho = min(Rho),
                   max_Rho = max(Rho),
                   Rho = mean(Rho),
                   se_P = sd(P_value) / sqrt(1000),
                   min_P = min(P_value),
                   max_P = max(P_value),
                   P_value = mean(P_value))


# joining
corr.df.out.tot <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)


corr.df <- 
  data.frame()
for(retail in retailers) {
  tmp.dat <- 
    plot.dat.aisle %>%
    filter(Retailer %in% retail) %>%
    filter(Drink %in% 'Drink')
  
  if(nrow(tmp.dat) <= 2) {
    corr.df <-
      rbind(corr.df,
            data.frame(Retailer = retail,
                       Rho = 'Not Enough Data Points',
                       P_value = 'Not Enough Data Points'))
  } else {
    
    spearman.dat <-
      cor.test(tmp.dat$NutriScore_Scaled, tmp.dat$Tot_env_100g_scaled,
               method = 'spearman', conf.level = .95)
    
    corr.df <-
      rbind(corr.df,
            data.frame(Retailer = retail,
                       Rho = spearman.dat$estimate,
                       P_value = spearman.dat$p.value))
  }
  
}

corr.df.mean <- corr.df


# And randomly selecting performance data to see how assumptions on e.g. sourcing affect results
corr.df <-
  data.frame()
for(retail in retailers) {
  # counter
  counter = 1
  # Retailer data
  tmp.dat.retail <- 
    plot.dat.aisle %>%
    filter(Retailer %in% retail) %>%
    filter(Drink %in% 'Drink')
  
  if(nrow(tmp.dat.retail) <= 2) {
    # Do nothing
  } else {
    for(i in 1:1000) {
      # Adding to counter
      counter = counter + 1
      # Randomly selecting data
      tmp.dat <- 
        tmp.dat.retail %>%
        as.data.frame(.) %>%
        group_by(Aisle) %>%
        plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
        plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = Tot_env_100g_scaled_se))
      
      # running the correlation
      spearman.dat <-
        cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
                 method = 'spearman', conf.level = .95)
      
      corr.df <-
        rbind(corr.df,
              data.frame(Retailer = retail,
                         Rho = spearman.dat$estimate,
                         P_value = spearman.dat$p.value,
                         test_iteration = counter))
      # Randomly sampling data
    }
  }
}

# Summary stats across the tests
corr.df.montecarlo <-
  corr.df %>% 
  mutate(count = ifelse(P_value < 0.05,1,0)) %>%
  group_by(Retailer) %>%
  dplyr::summarise(n_significant = sum(count),
                   se_Rho = sd(Rho) / sqrt(1000),
                   min_Rho = min(Rho),
                   max_Rho = max(Rho),
                   Rho = mean(Rho),
                   se_P = sd(P_value) / sqrt(1000),
                   min_P = min(P_value),
                   max_P = max(P_value),
                   P_value = mean(P_value)) %>%
  mutate(n_significant = n_significant / 1000)

# Adding missing retailer data
corr.df.missing <-
  data.frame(Retailer = retailers,
             n_significant = 'Not Enough Data Points',
             se_Rho = 'Not Enough Data Points',
             min_Rho = 'Not Enough Data Points',
             max_Rho = 'Not Enough Data Points',
             Rho = 'Not Enough Data Points',
             se_P = 'Not Enough Data Points',
             min_P = 'Not Enough Data Points',
             max_P = 'Not Enough Data Points',
             P_value = 'Not Enough Data Points')

corr.df.montecarlo <- 
  rbind(corr.df.montecarlo,
        corr.df.missing %>% filter(!(Retailer %in% corr.df.montecarlo$Retailer))) %>%
  arrange(Retailer)


# joining
corr.df.out <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value))
# mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)

# And r binding
corr.df.write.drinks <- 
  rbind(corr.df.out.tot, corr.df.out)

#####
# Repeating the above for food and drinks
set.seed(19)

# Across all retailers
corr.df <- 
  data.frame()

tmp.dat <- 
  plot.dat.aisle 

spearman.dat <-
  cor.test(tmp.dat$NutriScore_Scaled, tmp.dat$Tot_env_100g_scaled,
           method = 'spearman', conf.level = .95)

corr.df <-
  rbind(corr.df,
        data.frame(Retailer = 'Across All Retailers',
                   Rho = spearman.dat$estimate,
                   P_value = spearman.dat$p.value))

corr.df.mean <- corr.df


# And randomly selecting performance data to see how assumptions on e.g. sourcing affect results
corr.df <-
  data.frame()
# for(retail in retailers) {
# counter
counter = 1
# Retailer data
tmp.dat.retail <- 
  plot.dat.aisle

for(i in 1:1000) {
  # Adding to counter
  counter = counter + 1
  # Randomly selecting data
  tmp.dat <- 
    tmp.dat.retail %>%
    as.data.frame(.) %>%
    group_by(Aisle) %>%
    plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
    plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = Tot_env_100g_scaled_se))
  
  # running the correlation
  spearman.dat <-
    cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = 'Across All Retailers',
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value,
                     test_iteration = counter))
  # Randomly sampling data
}
# }
corr.df.montecarlo <-
  corr.df %>% 
  mutate(count = ifelse(P_value < 0.05,1,0)) %>%
  group_by(Retailer) %>%
  dplyr::summarise(n_significant = sum(count),
                   se_Rho = sd(Rho) / sqrt(1000),
                   min_Rho = min(Rho),
                   max_Rho = max(Rho),
                   Rho = mean(Rho),
                   se_P = sd(P_value) / sqrt(1000),
                   min_P = min(P_value),
                   max_P = max(P_value),
                   P_value = mean(P_value))


# joining
corr.df.out.tot <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)

corr.df <- 
  data.frame()
for(retail in retailers) {
  tmp.dat <- 
    plot.dat.aisle %>%
    filter(Retailer %in% retail)
  
  if(nrow(tmp.dat) <= 2) {
    corr.df <-
      rbind(corr.df,
            data.frame(Retailer = retail,
                       Rho = 'Not Enough Data Points',
                       P_value = 'Not Enough Data Points'))
  } else {
    
    spearman.dat <-
      cor.test(tmp.dat$NutriScore_Scaled, tmp.dat$Tot_env_100g_scaled,
               method = 'spearman', conf.level = .95)
    
    corr.df <-
      rbind(corr.df,
            data.frame(Retailer = retail,
                       Rho = spearman.dat$estimate,
                       P_value = spearman.dat$p.value))
  }
  
}

corr.df.mean <- corr.df


# And randomly selecting performance data to see how assumptions on e.g. sourcing affect results
corr.df <-
  data.frame()
for(retail in retailers) {
  # counter
  counter = 1
  # Retailer data
  tmp.dat.retail <- 
    plot.dat.aisle %>%
    filter(Retailer %in% retail) %>%
    filter(Retailer %in% retail)
  
  if(nrow(tmp.dat.retail) <= 2) {
    # Do nothing
  } else {
    for(i in 1:1000) {
      # Adding to counter
      counter = counter + 1
      # Randomly selecting data
      tmp.dat <- 
        tmp.dat.retail %>%
        as.data.frame(.) %>%
        group_by(Aisle) %>%
        plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
        plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = Tot_env_100g_scaled_se))
      
      # running the correlation
      spearman.dat <-
        cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
                 method = 'spearman', conf.level = .95)
      
      corr.df <-
        rbind(corr.df,
              data.frame(Retailer = retail,
                         Rho = spearman.dat$estimate,
                         P_value = spearman.dat$p.value,
                         test_iteration = counter))
      # Randomly sampling data
    }
  }
}

# Summary stats across the tests
corr.df.montecarlo <-
  corr.df %>% 
  mutate(count = ifelse(P_value < 0.05,1,0)) %>%
  group_by(Retailer) %>%
  dplyr::summarise(n_significant = sum(count),
                   se_Rho = sd(Rho) / sqrt(1000),
                   min_Rho = min(Rho),
                   max_Rho = max(Rho),
                   Rho = mean(Rho),
                   se_P = sd(P_value) / sqrt(1000),
                   min_P = min(P_value),
                   max_P = max(P_value),
                   P_value = mean(P_value)) %>%
  mutate(n_significant = n_significant / 1000)

# Adding missing retailer data
corr.df.missing <-
  data.frame(Retailer = retailers,
             n_significant = 'Not Enough Data Points',
             se_Rho = 'Not Enough Data Points',
             min_Rho = 'Not Enough Data Points',
             max_Rho = 'Not Enough Data Points',
             Rho = 'Not Enough Data Points',
             se_P = 'Not Enough Data Points',
             min_P = 'Not Enough Data Points',
             max_P = 'Not Enough Data Points',
             P_value = 'Not Enough Data Points')

corr.df.montecarlo <- 
  rbind(corr.df.montecarlo,
        corr.df.missing %>% filter(!(Retailer %in% corr.df.montecarlo$Retailer))) %>%
  arrange(Retailer)


# joining
corr.df.out <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value))
# mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)
corr.df.write.food.drinks <- 
  rbind(corr.df.out.tot, corr.df.out)

# And saving
corr.df.write <-
  rbind(corr.df.write.foods %>% mutate(Product_Type = 'Foods'),
        corr.df.write.drinks %>% mutate(Product_Type = 'Drinks'),
        corr.df.write.food.drinks %>% mutate(Product_Type = 'Foods and Drinks')) %>%
  dplyr::select(Retailer, Rho_mean, P_value_mean,
                proportion_of_correlations_that_were_significant_montecarlo,mean_Rho_montecarlo, se_Rho_montecarlo, min_Rho_montecarlo, max_Rho_montecarlo,
                mean_P_value_montecarlo,se_P_value_montecarlo,min_P_value_montecarlo,max_P_value_montecarlo,Product_Type)


write.csv(corr.df.write,
          paste0(getwd(),'/Tables/Table S7 Spearmans Correlations Between Nutrition and Env for Each Retailer NutriScoreLetter Known Products Only 6Feb2022.csv'))







