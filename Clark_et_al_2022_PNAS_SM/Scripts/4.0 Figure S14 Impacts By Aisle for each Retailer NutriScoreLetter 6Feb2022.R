###
# Figure S3
# Envionrmental impact vs nutrition impact per aisle, with separate panels by retailer

# Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
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

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# Data
plot.dat <- 
  read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv")) %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE))

# Number of total products
nutriscore.dat <-
  read_csv("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Runs 19April2021/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv")

plot.dat.count <-
  plot.dat %>%
  filter(id %in% nutriscore.dat$id) %>%
  filter(product_name %in% nutriscore.dat$product_name) %>%
  filter(!is.na(NutriScorePoints)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  filter(Tot_env_100g_scaled > 0) %>%
  group_by(product_name) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled))

length(unique(plot.dat.count$product_name))

quantile(plot.dat.count$Tot_env_100g_scaled, c(.5,.75,.95))


# Aggregating
plot.dat.aisle <-
  plot.dat %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  filter(!(grepl('Beer|Wine|Cider|Cidre|Spirit|Alcohol|Liqu|Christmas|Hallow',Department,ignore.case=TRUE))) %>%
  filter(!(grepl('Beer|Wine|Cider|Cidre|Spirit|Alcohol|Liqu|Christmas|Hallow',Aisle,ignore.case=TRUE))) %>%
  filter(!(grepl('Beer|Wine|Cider|Cidre|Spirit|Alcohol|Liqu|Christmas|Hallow',Shelf,ignore.case=TRUE))) %>%
  filter(!grepl('Halloween',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas|Hallow|October',Shelf, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas|Hallow|October',Department, ignore.case = TRUE)) %>%
  # mutate(Aisle = ifelse(grepl('\\bbeef\\b|\\blamb\\b|\\bsheep\\b|\\bgoat\\b',Shelf, ignore.case = TRUE),'Beef and Lamb',Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Fresh Meat & Poultry','Frozen Meat & Poultry','Cooked Meats, Sandwich Fillers & Deli'), 'Meat', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Frozen Meat Alternatives','Fresh Meat Alternatives'), 'Meat Alternatives', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Fresh Vegetables','Frozen Vegetables & Herbs'), 'Vegetables', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Frozen Ready Meals','Ready Meals'), 'Ready Meals', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Frozen Pizza & Garlic Bread','Fresh Pizza, Pasta & Garlic Bread'), 'Pizza & Garlic Bread', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Chilled Fish & Seafood','Frozen Fish & Seafood'), 'Fish & Seafood', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Pies, Quiches & Party Food','Frozen Pies'), 'Pies, Quiches & Party Food', Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% 'Counters','Deli Meat and Cheese',Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% 'Fresh Fruit','Fresh Fruit and Nuts',Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Frozen Free From','Free From Range'),'Gluten Free Range',Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Juices & Smoothies','Chilled Fruit Juice & Smoothies'),'Juices & Smoothies',Aisle)) %>%
  # mutate(Aisle = ifelse(Aisle %in% c('Cereals'),'Breakfast Cereals',Aisle)) %>%
  filter(!(Aisle %in% c('Beer & Cider','Spirits','Low & No Alcohol','Wine'))) %>%
  filter(!is.na(Tot_env_serving_scaled)) %>%
  filter(!grepl('\\bAll\\b',Aisle,ignore.case = TRUE)) %>%
  filter(!grepl('NULL', Shelf) | grepl('Cook|Iceland',Retailer)) %>%
  filter(Tot_env_100g_scaled > 0,
         NutriScore_Scaled > 0) %>%
  filter(!is.na(Tot_env_100g_scaled),
         !is.na(NutriScore_Scaled),
         !is.na(Tot_env_100g_scaled_upper_ci)) %>%
  mutate(scaled_GHG = mean_GHG / max(.$mean_GHG), # Rescaling for consistency
         scaled_Eut = mean_Eut / max(.$mean_Eut),
         scaled_WatScar = mean_WatScar / max(.$mean_WatScar),
         scaled_Land = mean_Land / max(.$mean_Land),
         scaled_se_GHG = se_GHG / max(.$mean_GHG),
         scaled_se_Eut = se_Eut / max(.$mean_Eut),
         scaled_se_Land = se_Land / max(.$mean_Land),
         scaled_se_WatScar = se_WatScar / max(.$mean_WatScar)) %>%
  mutate(scaled_tot_env = (scaled_GHG + scaled_Eut + scaled_WatScar + scaled_Land) / 4,
         scaled_tot_se = (scaled_se_GHG + scaled_se_Eut + scaled_se_Land + scaled_se_WatScar) / 4) %>%
  mutate(scaled_tot_se = scaled_tot_se / max(.$scaled_tot_env)) %>%
  mutate(scaled_tot_env = scaled_tot_env / max(.$scaled_tot_env)) %>%
  mutate(scaled_tot_env = scaled_tot_env * 100) %>%
  mutate(NutriScore_Scaled = ifelse(NutriScoreLetter %in% 'A',1,
                                    ifelse(NutriScoreLetter %in% 'B',2,
                                           ifelse(NutriScoreLetter %in% 'C',3,
                                                  ifelse(NutriScoreLetter %in% 'D',4,5))))) %>%
  group_by(product_name, Retailer, Department, Aisle) %>% # Averaging by product to avoid multiple obserations of same product in a given aisle
  dplyr::summarise(Tot_env_100g_scaled_se = sd(scaled_tot_env, na.rm = TRUE) / sqrt(n()),
                   Tot_env_100g_scaled = mean(scaled_tot_env, na.rm = TRUE),
                   #Tot_env_100g_scaled_se = mean(scaled_tot_se, na.rm = TRUE),
                   sd_NutriScore = sd(NutriScore_Scaled, na.rm = TRUE),
                   se_NutriScore = sd(NutriScore_Scaled, na.rm = TRUE) / sqrt(n()),
                   NutriScore_Scaled = mean(NutriScore_Scaled, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  group_by(Retailer, Department, Aisle) %>% # And now averaging across retailer aisles
  dplyr::summarise(count = n(),
                   Tot_env_100g_scaled_se = sd(Tot_env_100g_scaled, na.rm = TRUE) / sqrt(n()),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled, na.rm = TRUE),
                   #Tot_env_100g_scaled_se = mean(scaled_tot_se, na.rm = TRUE),
                   sd_NutriScore = sd(NutriScore_Scaled, na.rm = TRUE),
                   se_NutriScore = sd(NutriScore_Scaled, na.rm = TRUE) / sqrt(n()),
                   NutriScore_Scaled = mean(NutriScore_Scaled, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  mutate(Retailer = ifelse(Retailer %in% 'Tesco_Ireland', 'Tesco Ireland',Retailer)) %>%
  filter(!grepl('Water',Aisle)) %>%
  filter(Aisle != 'Speciality') %>%
  mutate(Drink = 'Food') %>%
  mutate(Drink = ifelse(grepl('Drink|Juice|Tea|Coffee|Beer',Department, ignore.case=TRUE),'Drink','Food')) %>%
  mutate(Drink = ifelse(grepl('Drink|Smoothie|juice|beer|wine|spirit|cider|cidre|milk|squash|cordial|cola|spritzer|sparkling|water|tea|coffee|hot beverages|hot chocolate', Aisle, ignore.case = TRUE),'Drink',Drink)) %>%
  mutate(Tot_env_100g_scaled_se = ifelse(is.na(Tot_env_100g_scaled_se),0,Tot_env_100g_scaled_se))



# Plotting
ggplot(plot.dat.aisle %>% filter(!is.na(Retailer)), aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled, colour = Drink)) +
  geom_errorbarh(aes(xmin = NutriScore_Scaled - se_NutriScore * qt(1 - (0.05 / 2), count - 1), xmax = NutriScore_Scaled + se_NutriScore * qt(1 - (0.05 / 2), count - 1)), colour = 'grey') +
  geom_errorbar(aes(ymin = Tot_env_100g_scaled - Tot_env_100g_scaled_se * qt(1 - (0.05 / 2), count - 1), ymax = Tot_env_100g_scaled + Tot_env_100g_scaled_se * qt(1 - (0.05 / 2), count - 1)), colour = 'grey') +
  geom_point(size = 1) +
  theme_classic() +
  facet_wrap(.~Retailer, nrow = 3) +
  scale_y_continuous(trans = 'log10', breaks = c(.1,.5,5,50)) +
  scale_x_continuous(breaks = 1:5, labels = c('A','B','C','D','E')) +
  scale_colour_manual(values = c(my.palette[2],my.palette[6])) +
  labs(x = 'Nutrition Impact Score (per 100g)',y = 'Environmental Impact Score (per 100g)') +
  coord_cartesian(ylim = c(.1,50), xlim = c(.75,5.25))



# ggsave(paste0(getwd(),'/Figures/Fig S14 Impacts per Aisle All Retailers NutriScoreLetter 6Feb2022.pdf'),
#        width = 18, height = 20, units = 'cm')

# Writing file
# And saving data
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}

# write.csv(plot.dat.aisle %>% dplyr::select(Retailer, Department, Aisle, `Food Type` = Drink, NutriScore_Scaled, se_NutriScore, Tot_env_100g_scaled, Tot_env_100g_scaled_se, number_of_products = count),
#           paste0(getwd(),'/Figure Data/Data Fig S14 6Feb2022.csv'),row.names = FALSE)


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
                     P_value = spearman.dat$p.value,
                     S_statisic = spearman.dat$statistic))

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

# And getting correlations for single ingredient foods ----
# Setting working directory

# Composition data for single ingredient foods
file.list = 
  list.files(path = paste0(getwd(),'/Outputs'),
                       pattern = 'FoodDB percent composition by ingredient ', full.names = TRUE)

# stacking these
stacked.dat = data.frame()

for(i in 1:length(file.list)) {
  tmp.dat = read.csv(file.list[i], stringsAsFactors = FALSE)
  if(i == 1) {
    stacked.dat = rbind(stacked.dat,tmp.dat) 
  } else if(length(names(tmp.dat)) != length(names(stacked.dat))) {
    tmp.dat <- tmp.dat[,names(stacked.dat)]
    stacked.dat = rbind(stacked.dat, tmp.dat)
  } else if(length(names(tmp.dat)) == length(names(stacked.dat))) {
    stacked.dat = rbind(stacked.dat, tmp.dat)
  }
  
}
# Taking unique
stacked.dat <- unique(stacked.dat)

# Names to drop - these aren't food, or at least single ingredient foods
drop.names <-
  grep('roes|caviar|tuna cubes|duck fat|\\blard\\b|gelatin|escalope|gelatine|sachet|dripping|dog treat',stacked.dat$product_name, ignore.case = TRUE)

# and dropping
stacked.dat <- stacked.dat[-drop.names,]

# drop names 2
drop.names <- grep('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', stacked.dat$product_name, ignore.case = TRUE)

stacked.dat <- stacked.dat[-drop.names,]

###
# Ingredient count per product
num.ingredients <-
  stacked.dat %>%
  group_by(id, product_name, Food_Category, Food_Category_sub, Food_Category_sub_sub) %>%
  dplyr::summarise(percent = sum(as.numeric(percent),na.rm=TRUE)) %>%
  dplyr::group_by(id,product_name) %>%
  dplyr::summarise(n_ingredients = n(),
                   percent = mean(percent)) %>%
  filter(percent >= 99)

num.ingredients <-
  num.ingredients %>%
  group_by(product_name) %>%
  dplyr::summarise(n_ingredients = min(n_ingredients)) %>%
  filter(product_name %in% plot.dat.count$product_name) %>%
  filter(n_ingredients %in% 1)


# 
# # Filtering to products where =>99% composition accounted for by a single ingredient
# This will be filtered again later for single ingredient foods
# stacked.dat <-
#   stacked.dat %>%
#   # filter(id %in% num.ingredients$id) %>%
#   # filter(product_name %in% num.ingredients$product_name)
#   mutate(percent = as.numeric(percent)) %>%
#   filter(percent >= 99)

# Identifying processed meats
stacked.dat <-
  stacked.dat %>%
  filter(product_name %in% num.ingredients$product_name) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)'), 'Processed ruminant meat', Food_Category)) %>% # Unearthed is a brand that only has processed meats in our data set
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)'), 'Processed ruminant meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Pig Meat'), 'Processed pig meat', Food_Category)) %>% # Unearthed is a brand that only has processed meats in our data set
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Pig Meat'), 'Processed pig meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Poultry Meat'), 'Processed poultry meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Poultry Meat'), 'Processed poultry meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Fish (farmed)', 'Crustaceans (farmed)'), 'Processed fish meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Fish (farmed)', 'Crustaceans (farmed)'), 'Processed fish meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)'), 'Processed ruminant meat', Food_Category)) %>% # Unearthed is a brand that only has processed meats in our data set
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)'), 'Processed ruminant meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Pigmeat'), 'Processed pig meat', Food_Category)) %>% # Unearthed is a brand that only has processed meats in our data set
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Pigmeat'), 'Processed pig meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Poultry Meat'), 'Processed poultry meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Poultry Meat'), 'Processed poultry meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', value, ignore.case = TRUE) & Food_Category %in% c('Fish (farmed)', 'Crustaceans (farmed)'), 'Processed fish meat', Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Fish (farmed)', 'Crustaceans (farmed)'), 'Processed fish meat', Food_Category)) %>%
  unique(.)

# Impact data, subsetted to Tesco
impact.dat <- 
  read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv")) %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE)) %>%
  filter(id %in% nutriscore.dat$id) %>%
  filter(product_name %in% nutriscore.dat$product_name) %>%
  filter(paste0(id, product_name) %in% paste0(stacked.dat$id, stacked.dat$product_name)) %>%
  dplyr::select(product_name, Tot_env_100g_scaled, NutriScore_Scaled, NutriScoreLetter) %>%
  unique(.) %>%
  left_join(., stacked.dat) %>%
  mutate(NutriScore_Scaled = ifelse(NutriScoreLetter %in% 'A',1,
                                    ifelse(NutriScoreLetter %in% 'B',2,
                                           ifelse(NutriScoreLetter %in% 'C',3,
                                                  ifelse(NutriScoreLetter %in% 'D',4,
                                                         ifelse(NutriScoreLetter %in% 'E',5,NA)))))) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  group_by(product_name, Food_Category, Food_Category_sub, Food_Category_sub_sub) %>%
  dplyr::summarise(NutriScore_Scaled = mean(NutriScore_Scaled),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled)) %>%
  group_by(Food_Category, Food_Category_sub, Food_Category_sub_sub) %>%
  dplyr::summarise(se_env = sd(Tot_env_100g_scaled, na.rm = TRUE) / sqrt(n()),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled, na.rm = TRUE),
                   se_nutriscore = sd(NutriScore_Scaled, na.rm = TRUE) / sqrt(n()),
                   NutriScore_Scaled = mean(NutriScore_Scaled, na.rm = TRUE)) %>%
  filter(!is.na(Food_Category)) %>%
  filter(Food_Category != 'Wine') %>%
  filter(Food_Category != 'Water')

# Adding colours to the plot
impact.dat <-
  impact.dat %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Barley (Beer)', 'Barley',
                                ifelse(Food_Category %in% 'Wheat & Rye (Bread)','Wheat',
                                       ifelse(Food_Category %in% 'Cereals & Oilcrops Misc.', 'Other Cereals and Oilcrops',
                                              ifelse(Food_Category %in% 'Maize (Meal)','Maize',Food_Category))))) %>%
  mutate(colour = ifelse(Food_Category %in% c('Apples','Bananas','Berries & Grapes','Barley','Wheat','Other Cereals and Oilcrops',
                                              'Brassicas','Cassava','Citrus Fruit',
                                              'Groundnuts','Maize','Nuts','Oatmeal','Olives','Onions & Leeks','Other Fruit','Other Pulses',
                                              'Other Vegetables','Peas','Potatoes','Rice','Root Vegetables','Sunflower seeds','Tomatoes'), '#009244',
                         ifelse(Food_Category %in% c('Beef','Bovine Meat (dairy herd)','Bovine Meat (beef herd)','Processed red meat','Lamb & Mutton','Pig Meat'), '#c1272c',
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
  

impact.dat <-
  impact.dat %>%
  mutate(Drink = ifelse(Food_Category %in% c('Milk','Coffee','Tea','Olive Oil','Palm Oil','Rapeseed Oil','Soybean Oil','Sunflower Oil'),'Drink','Food'))


# Corrlation on averages
spearman.dat <-
  cor.test(impact.dat$NutriScore_Scaled[!grepl('Tea|Coffee|Milk',impact.dat$Food_Category)], impact.dat$Tot_env_100g_scaled[!grepl('Tea|Coffee|Milk',impact.dat$Food_Category)],
           method = 'spearman', conf.level = .95)

corr.df.mean <- 
  data.frame(Retailer = 'Single Ingredient Foods',
             Rho = spearman.dat$estimate,
             P_value = spearman.dat$p.value)

# And monte carloing this
corr.df <- data.frame()
counter = 1
for(i in 1:1000) {
  # Adding to counter
  counter = counter + 1
  # Randomly selecting data
  tmp.dat <- 
    impact.dat %>%
    filter(!(grepl('Tea|Coffee|Milk',Food_Category))) %>%
    as.data.frame(.) %>%
    group_by(Food_Category) %>%
    plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
    plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = se_env)) %>%
    mutate(nutriscore_random = ifelse(is.nan(nutriscore_random),NutriScore_Scaled, nutriscore_random),
           env_random = ifelse(is.nan(env_random), Tot_env_100g_scaled, env_random))
  
  # running the correlation
  spearman.dat <-
    cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = 'Single Ingredient Foods',
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value,
                     test_iteration = counter))
  # Randomly sampling data
}

# Taking summary
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

# Merging
corr.df.single <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
          corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                               min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                               mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                               min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                               mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)


# And binding to correlations across aisles
corr.df.write.foods <- 
  rbind(corr.df.out.tot %>% dplyr::select(-S_statisic), corr.df.out, corr.df.single)

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

### Single ingredient foods
# Corrlation on averages
spearman.dat <-
  cor.test(impact.dat$NutriScore_Scaled[grepl('Tea|Coffee|Milk',impact.dat$Food_Category)], impact.dat$Tot_env_100g_scaled[grepl('Tea|Coffee|Milk',impact.dat$Food_Category)],
           method = 'spearman', conf.level = .95)

corr.df.mean <- 
  data.frame(Retailer = 'Single Ingredient Foods',
             Rho = spearman.dat$estimate,
             P_value = spearman.dat$p.value)

# And monte carloing this
corr.df <- data.frame()
counter = 1
for(i in 1:1000) {
  # Adding to counter
  counter = counter + 1
  # Randomly selecting data
  tmp.dat <- 
    impact.dat %>%
    filter((grepl('Tea|Coffee|Milk',Food_Category))) %>%
    as.data.frame(.) %>%
    group_by(Food_Category) %>%
    plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
    plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = se_env)) %>%
    mutate(nutriscore_random = ifelse(is.nan(nutriscore_random),NutriScore_Scaled, nutriscore_random),
           env_random = ifelse(is.nan(env_random), Tot_env_100g_scaled, env_random))
  
  # running the correlation
  spearman.dat <-
    cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = 'Single Ingredient Foods',
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value,
                     test_iteration = counter))
  # Randomly sampling data
}

# Taking summary
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

# Merging
corr.df.single <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)
# And r binding
corr.df.write.drinks <- 
  rbind(corr.df.out.tot, corr.df.out, corr.df.single)

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

### Single ingredient foods
# Corrlation on averages
spearman.dat <-
  cor.test(impact.dat$NutriScore_Scaled, impact.dat$Tot_env_100g_scaled,
           method = 'spearman', conf.level = .95)

corr.df.mean <- 
  data.frame(Retailer = 'Single Ingredient Foods',
             Rho = spearman.dat$estimate,
             P_value = spearman.dat$p.value)

# And monte carloing this
corr.df <- data.frame()
counter = 1
for(i in 1:1000) {
  # Adding to counter
  counter = counter + 1
  # Randomly selecting data
  tmp.dat <- 
    impact.dat %>%
    # filter((grepl('Tea|Coffee|Milk',Food_Category))) %>%
    as.data.frame(.) %>%
    group_by(Food_Category) %>%
    plyr::mutate(nutriscore_random = rnorm(nrow(.), mean = NutriScore_Scaled, sd = 0)) %>%
    plyr::mutate(env_random = rnorm(nrow(.), mean = Tot_env_100g_scaled, sd = se_env)) %>%
    mutate(nutriscore_random = ifelse(is.nan(nutriscore_random),NutriScore_Scaled, nutriscore_random),
           env_random = ifelse(is.nan(env_random), Tot_env_100g_scaled, env_random))
  
  # running the correlation
  spearman.dat <-
    cor.test(tmp.dat$nutriscore_random, tmp.dat$env_random,
             method = 'spearman', conf.level = .95)
  
  corr.df <-
    rbind(corr.df,
          data.frame(Retailer = 'Single Ingredient Foods',
                     Rho = spearman.dat$estimate,
                     P_value = spearman.dat$p.value,
                     test_iteration = counter))
  # Randomly sampling data
}

# Taking summary
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

# Merging
corr.df.single <-
  left_join(corr.df.mean %>% dplyr::rename(Rho_mean = Rho, P_value_mean = P_value),
            corr.df.montecarlo %>% dplyr::rename(proportion_of_correlations_that_were_significant_montecarlo = n_significant, se_Rho_montecarlo = se_Rho,
                                                 min_Rho_montecarlo = min_Rho, max_Rho_montecarlo = max_Rho,
                                                 mean_Rho_montecarlo = Rho, se_P_value_montecarlo = se_P,
                                                 min_P_value_montecarlo = min_P, max_P_value_montecarlo = max_P,
                                                 mean_P_value_montecarlo = P_value)) %>%
  mutate(proportion_of_correlations_that_were_significant_montecarlo = proportion_of_correlations_that_were_significant_montecarlo / 1000)

corr.df.write.food.drinks <- 
  rbind(corr.df.out.tot, corr.df.out, corr.df.single)

# And saving
corr.df.write <-
  rbind(corr.df.write.foods %>% mutate(Product_Type = 'Foods'),
        corr.df.write.drinks %>% mutate(Product_Type = 'Drinks'),
        corr.df.write.food.drinks %>% mutate(Product_Type = 'Foods and Drinks')) %>%
  dplyr::select(Retailer, Rho_mean, P_value_mean,
                proportion_of_correlations_that_were_significant_montecarlo,mean_Rho_montecarlo, se_Rho_montecarlo, min_Rho_montecarlo, max_Rho_montecarlo,
                mean_P_value_montecarlo,se_P_value_montecarlo,min_P_value_montecarlo,max_P_value_montecarlo,Product_Type)

write.csv(corr.df.write,
          paste0(getwd(),'/Tables/Table S6 Spearmans Correlations Between Nutrition and Env for Each Retailer NutriScoreLetter 6Feb2022.csv'))




