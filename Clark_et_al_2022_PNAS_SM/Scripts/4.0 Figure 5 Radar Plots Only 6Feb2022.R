###
# Fig 3
# Impacts by Aisle for Tesco

# libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(Hmisc)
library(fmsb)
library(stringr)
library(readr)
library(scales)

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# data
plot.dat <- read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv"))

# Aggregating
plot.dat <-
  plot.dat %>%
  filter(Retailer %in% 'Tesco') %>%
  mutate(Aisle = ifelse(grepl('\\bbeef\\b|\\blamb\\b|\\bsheep\\b|\\bgoat\\b',Shelf, ignore.case = TRUE),'Beef and Lamb',Aisle)) %>%
  # mutate(Aisle = ifelse(grepl('\\bbeef\\b|\\blamb\\b|\\bsheep\\b|\\bgoat\\b',Shelf, ignore.case = TRUE),'Fresh Fruit',Aisle)) %>%
  mutate(Aisle = ifelse(grepl('Nut|Seed|Almond|Pecan|Walnut|Pistachio|Cashew|Chestnut|Macadmi|Maracon|Brazil|Hazel',product_name, ignore.case = TRUE) & Aisle %in% 'Fresh Fruit','Dried Fruit, Nuts, Nutrient Powders & Seeds',Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Fresh Meat & Poultry','Frozen Meat & Poultry','Cooked Meats, Sandwich Fillers & Deli'), 'Meat', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Frozen Meat Alternatives','Fresh Meat Alternatives'), 'Meat Alternatives', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Fresh Vegetables','Frozen Vegetables & Herbs'), 'Vegetables', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Frozen Ready Meals','Ready Meals'), 'Ready Meals', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Frozen Pizza & Garlic Bread','Fresh Pizza, Pasta & Garlic Bread'), 'Pizza & Garlic Bread', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Chilled Fish & Seafood','Frozen Fish & Seafood'), 'Fish & Seafood', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Pies, Quiches & Party Food','Frozen Pies'), 'Pies, Quiches & Party Food', Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% 'Counters','Deli Meat and Cheese',Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% 'Fresh Fruit','Fresh Fruit and Nuts',Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Frozen Free From','Free From Range'),'Gluten Free Range',Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Juices & Smoothies','Chilled Fruit Juice & Smoothies'),'Juices & Smoothies',Aisle)) %>%
  mutate(Aisle = ifelse(Aisle %in% c('Cereals'),'Breakfast Cereals',Aisle)) %>%
  filter(!(Aisle %in% c('Beer & Cider','Spirits','Low & No Alcohol','Wine'))) %>%
  unique(.) %>%
  filter(!grepl('Halloween',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('Christmas',Aisle, ignore.case = TRUE)) %>%
  filter(!grepl('\\bAll\\b',Aisle,ignore.case = TRUE)) %>%
  filter(!(Shelf %in% 'All')) %>%
  filter(!(Shelf %in% 'NULL')) %>%
  dplyr::select(product_name, id, Retailer, Aisle, Tot_env_100g_scaled, Tot_env_100g_scaled_lower_ci, Tot_env_100g_scaled_upper_ci, NutriScore_Scaled,  NutriScoreLetter, Eut_100g = mean_Eut,Land_100g = mean_Land,GHGs_100g = mean_GHG,WatScar_100g = mean_WatScar) %>%
  .[complete.cases(.),] %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE)) %>%
  unique(.) %>%
  mutate(NutriScore_Scaled = ifelse(NutriScoreLetter %in% 'A',1,
                                    ifelse(NutriScoreLetter %in% 'B',2,
                                           ifelse(NutriScoreLetter %in% 'C',3,
                                                  ifelse(NutriScoreLetter %in% 'D',4,5)))))

###
# High impact cereals
cereal.dat <-
  plot.dat %>%
  filter(grepl('Breakfast.*Cer',Aisle,ignore.case=TRUE)) %>%
  dplyr::select(product_name,Tot_env_100g_scaled) %>%
  unique(.) %>%
  group_by(product_name) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled))

sum(cereal.dat$Tot_env_100g_scaled > quantile(plot.dat$Tot_env_100g_scaled,2/3))/nrow(cereal.dat)

### 
# High imapct meat pies
pie.dat <-
  plot.dat %>%
  filter(grepl('Pie.*quich',Aisle,ignore.case=TRUE)) %>%
  dplyr::select(product_name,Tot_env_100g_scaled) %>%
  unique(.) %>%
  group_by(product_name) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled))

sum(pie.dat$Tot_env_100g_scaled < quantile(plot.dat$Tot_env_100g_scaled,1/3))/nrow(pie.dat)

plot.dat.aisle <-
  plot.dat %>%
  group_by(Retailer, Aisle) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled),
                   Tot_env_100g_scaled_lower_ci = mean(Tot_env_100g_scaled_lower_ci),
                   Tot_env_100g_scaled_upper_ci = mean(Tot_env_100g_scaled_upper_ci),
                   mean_nutriscore = mean(NutriScore_Scaled),
                   sd_nutriscore = sd(NutriScore_Scaled),
                   n_nutriscore = n()) %>%
  as.data.frame(.) %>%
  mutate(se_nutriscore = sd_nutriscore / sqrt(n_nutriscore),
         lower_ci_nutriscore = mean_nutriscore - qt(1 - (0.05 / 2), n_nutriscore - 1) * se_nutriscore,
         upper_ci_nutriscore = mean_nutriscore + qt(1 - (0.05 / 2), n_nutriscore - 1) * se_nutriscore) %>%
  mutate(Env_Quantile = ifelse(Tot_env_100g_scaled <= quantile(.$Tot_env_100g_scaled,1/3,na.rm=TRUE),1,
                               ifelse(Tot_env_100g_scaled <= quantile(.$Tot_env_100g_scaled,2/3,na.rm=TRUE),2,3))) %>%
  mutate(Nut_Quantile = ifelse(mean_nutriscore <= quantile(.$mean_nutriscore,1/3,na.rm=TRUE),1,
                               ifelse(mean_nutriscore <= quantile(.$mean_nutriscore,2/3,na.rm=TRUE),2,3)))

# Adding labels
plot.dat.aisle <-
  plot.dat.aisle %>% 
  mutate(Label = Aisle) %>%
  mutate(Label = ifelse(Label %in% c('Bottled Water','Free From Range','Frozen Free From','Easy Entertaining','Frozen Free From',
                                     'World Foods','Frozen World Foods & Halal','Home Baking',
                                     'Sugar & Sweeteners','Tins, Cans & Packets','From our Bakery','Counters',
                                     'Bakery Free From','Cooking Ingredients','Sweets, Mints & Chewing Gum','Desserts','Chilled Desserts',
                                     'Cooking Suaces & Meal Kits','Crisps, Snacks & Popcorn','Crackers & Crispbreads','Cakes, Cake Bars, Slices & Pies'), ' ', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Meat & Poultry','Frozen Meat & Poultry','Cooked Meats, Sandwich Fillers & Deli'), 'Meat', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Meat Alternatives','Fresh Meat Alternatives'), 'Meat Alternatives', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Vegetables','Frozen Vegetables & Herbs'), 'Vegetables', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Pizza & Garlic Bread','Fresh Pizza, Pasta & Garlic Bread'), 'Pizza & Garlic Bread', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Dried Pasta, Rice, Noodles & Cous Cous'), 'Dried Cereal Grains', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Soup, Sandwiches & Salad Pots'), 'Soup, Sandwiches & Salad Pots', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Party Food & Sausage Rolls'), 'Sausage Rolls &\nParty Food', Label)) %>%
  # mutate(Label = ifelse(Label %in% c('Croissants, Brioche & Pastries'), 'Pastries', Label)) %>%
  # mutate(Label = ifelse(Label %in% c('Crisps, Snacks & Popcorn'), 'Popcorn, Crisps & Snacks', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Desserts, Ice Cream & Ice Lollies'), 'Frozen Desserts', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Dried Fruit, Nuts, Nutrient Powders & Seeds'), 'Nuts, Dried Fruit & Nutrient Powders', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Yorkshire Puddings & Stuffing'), 'Yorkshire Puddings', Label)) %>% 
  mutate(Label = ifelse(Label %in% c('Frozen Chips, Onion Rings, Potatoes & Rice'), 'Roasted Potatoes, Chips, Onion Rings, & Rice', Label)) %>% 
  mutate(Label = ifelse(Label %in% c('Jams, Sweet & Savoury Spreads'), 'Jams & Savoury Spreads', Label)) %>% 
  mutate(Label = ifelse(Label %in% c('Olives, Antipasti, Pickles & Chutneys'), 'Olives, Antipasti, Pickles & Chutney', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Table Sauces, Marinades & Dressings'), 'Table Sauce, Marinade & Dressing', Label)) %>%
  # mutate(Label = ifelse(Label %in% c('Cakes, Cake Bars, Slices & Pies'), 'Cakes and Pies', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Fruit and Nuts'), 'Fresh Fruit', Label)) %>%
  mutate(Label = ifelse(grepl('Dried Fruit.*Seed',Label), 'Nuts, Seeds, and Dried Fruit', Label)) %>%
  filter(Aisle != 'Bottled Water')

# Adding product types and colours
plot.dat.aisle <-
  plot.dat.aisle %>%
  mutate(Food_Type = ifelse(grepl('Drinks|Cordial|Juice|Tea|Coffee|Milkshake',Aisle),'Beverages',NA)) %>%
  mutate(Food_Type = ifelse(grepl('Dessert|Croissant|Teacakes|Cakes, Cake Bars|Sweets, Mints|Biscuits & Cereal Bars|Doughnuts',Aisle),'Desserts',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Jams,|Cooking Sauces|Table Sauces|Sugar &|Cooking Ingredients|Home Baking',Aisle), 'Cooking Accessories',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Cereal|From our Bakery|Bread &|Wraps, Pittas|Crumpets|Dried Pasta|Crackers',Aisle), 'Cereal Grains and Bread',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Meat|Counters|Cheese|Beef', Aisle), 'Dairy, Fish, and Meat', Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Fish', Aisle), 'Dairy, Fish, and Meat', Food_Type)) %>%
  # mutate(Food_Type = ifelse(grepl('Chocolate|Coffee|Tea^[cake]|Hot Drinks',Aisle),'Chocolate, Coffee, and Tea',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Yoghurts|Milk, Butter|Milkshake|Ice Cream',Aisle),'Dairy, Fish, and Meat',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Vegetables|Salad|Fruit[^ L]|Nuts',Aisle),'Fruit, Vegetables, and Nuts',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Fresh Soup|Yorkshire|Pizza|Easy|Frozen Pies|Ready Meals|Pies, Quiches|Party Food|Tins, Cans|World Foods|Frozen Breakfast',Aisle),'Prepared Foods',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Gluten Free',Aisle),'Prepared Foods',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Meat Alternative|Dairy Alternative',Aisle),'Dairy, Fish, and Meat',Food_Type)) %>%
  mutate(Food_Type = ifelse(grepl('Frozen Chips|Popcorn|Olives|Chocolate', Aisle),'Snacks',Food_Type)) %>%
  # mutate(Food_Type = ifelse(Aisle %in% 'Tea','Chocolate, Coffee, and Tea', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Fresh Fruit','Fruit, Vegetables, and Nuts', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Bakery Free From','Cereal Grains and Bread', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Free From Range','Cooking Accessories', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Frozen Free From','Prepared Foods', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Hot Chocolate & Malted Drinks','Beverages', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Frozen Desserts, Ice Cream & Ice Lollies','Desserts', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Biscuits & Cereal Bars','Desserts', Food_Type)) %>%
  mutate(Food_Type = ifelse(Aisle %in% 'Milkshake','Beverages', Food_Type)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Beverages','#0072b2', NA)) %>%
  # mutate(plot_colour = ifelse(Food_Type %in% 'Chocolate, Coffee, and Tea','#654321', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Cereal Grains and Bread','#e69f00', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Cooking Accessories','#000000', plot_colour)) %>%
  # mutate(plot_colour = ifelse(Food_Type %in% 'Plant-based alternatives','#4fa64f', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Fruit, Vegetables, and Nuts','#009e73', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Dairy, Fish, and Meat','#ed2939', plot_colour)) %>%
  # mutate(plot_colour = ifelse(Food_Type %in% 'Milk and Eggs','#a9a9a9', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Desserts','#cc79a7', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Prepared Foods','#d55e00', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Snacks','#b79268', plot_colour)) %>%
  # mutate(plot_colour = ifelse(Food_Type %in% 'Plant-based alternatives','#495e35', plot_colour)) %>%
  # mutate(plot_colour = ifelse(Food_Type %in% 'Gluten Free Range','#f6f2cd', plot_colour)) %>%
  mutate(plot_colour = ifelse(Food_Type %in% 'Seafood','#0072b2', plot_colour))

# Aggregating
# plot.dat.aisle <-
#   plot.dat.aisle %>%
#   group_by(Retailer, Aisle) %>%
#   dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled),
#                    Tot_env_100g_scaled_lower_ci = mean(Tot_env_100g_scaled_lower_ci),
#                    Tot_env_100g_scaled_upper_ci = mean(Tot_env_100g_scaled_upper_ci),
#                    mean_nutriscore = mean(NutriScore_Scaled),
#                    sd_nutriscore = sd(NutriScore_Scaled),
#                    n_nutriscore = n()) %>%
#   as.data.frame(.) %>%
#   mutate(se_nutriscore = sd_nutriscore / sqrt(n_nutriscore),
#          lower_ci_nutriscore = mean_nutriscore - qt(1 - (0.05 / 2), n_nutriscore - 1) * se_nutriscore,
#          upper_ci_nutriscore = mean_nutriscore + qt(1 - (0.05 / 2), n_nutriscore - 1) * se_nutriscore) %>%
#   mutate(Env_Quantile = ifelse(Tot_env_100g_scaled <= quantile(.$Tot_env_100g_scaled,1/3,na.rm=TRUE),1,
#                                ifelse(Tot_env_100g_scaled <= quantile(.$Tot_env_100g_scaled,2/3,na.rm=TRUE),2,3))) %>%
#   mutate(Nut_Quantile = ifelse(mean_nutriscore <= quantile(.$mean_nutriscore,1/3,na.rm=TRUE),1,
#                                ifelse(mean_nutriscore <= quantile(.$mean_nutriscore,2/3,na.rm=TRUE),2,3)))

# Adding labels
plot.dat.aisle <-
  plot.dat.aisle %>% 
  mutate(Label = Aisle) %>%
  mutate(Label = ifelse(Label %in% c('Bottled Water','Free From Range','Frozen Free From','Easy Entertaining','Frozen Free From',
                                     'World Foods','Frozen World Foods & Halal','Home Baking',
                                     'Sugar & Sweeteners','Tins, Cans & Packets','From our Bakery','Counters',
                                     'Bakery Free From','Cooking Ingredients','Sweets, Mints & Chewing Gum','Desserts','Chilled Desserts',
                                     'Cooking Suaces & Meal Kits','Crisps, Snacks & Popcorn','Crackers & Crispbreads','Cakes, Cake Bars, Slices & Pies'), ' ', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Meat & Poultry','Frozen Meat & Poultry','Cooked Meats, Sandwich Fillers & Deli'), 'Meat', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Meat Alternatives','Fresh Meat Alternatives'), 'Meat Alternatives', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Vegetables','Frozen Vegetables & Herbs'), 'Vegetables', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Pizza & Garlic Bread','Fresh Pizza, Pasta & Garlic Bread'), 'Pizza & Garlic Bread', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Dried Pasta, Rice, Noodles & Cous Cous'), 'Dried Cereal Grains', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Soup, Sandwiches & Salad Pots'), 'Soup, Sandwiches & Salad Pots', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Party Food & Sausage Rolls'), 'Sausage Rolls &\nParty Food', Label)) %>%
  # mutate(Label = ifelse(Label %in% c('Croissants, Brioche & Pastries'), 'Pastries', Label)) %>%
  # mutate(Label = ifelse(Label %in% c('Crisps, Snacks & Popcorn'), 'Popcorn, Crisps & Snacks', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Desserts, Ice Cream & Ice Lollies'), 'Frozen Desserts', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Dried Fruit, Nuts, Nutrient Powders & Seeds'), 'Nuts, Dried Fruit & Nutrient Powders', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Frozen Yorkshire Puddings & Stuffing'), 'Yorkshire Puddings', Label)) %>% 
  mutate(Label = ifelse(Label %in% c('Frozen Chips, Onion Rings, Potatoes & Rice'), 'Roasted Potatoes, Chips, Onion Rings, & Rice', Label)) %>% 
  mutate(Label = ifelse(Label %in% c('Jams, Sweet & Savoury Spreads'), 'Jams & Savoury Spreads', Label)) %>% 
  mutate(Label = ifelse(Label %in% c('Olives, Antipasti, Pickles & Chutneys'), 'Olives, Antipasti, Pickles & Chutney', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Table Sauces, Marinades & Dressings'), 'Table Sauce, Marinade & Dressing', Label)) %>%
  # mutate(Label = ifelse(Label %in% c('Cakes, Cake Bars, Slices & Pies'), 'Cakes and Pies', Label)) %>%
  mutate(Label = ifelse(Label %in% c('Fresh Fruit and Nuts'), 'Nuts and Fresh Fruit', Label)) %>%
  filter(Aisle != 'Bottled Water')

# Adding nutriscore data
nutri.dat <-
  read_csv(paste0(getwd(),"/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv")) %>%
  filter(Retailer %in% 'Tesco') %>%
  group_by(id, product_name) %>%
  dplyr::summarise(NutriCal = mean(NutriCal, na.rm = TRUE),
                   NutriSugar = mean(NutriSugar, na.rm = TRUE),
                   NutriSatFats = mean(NutriSatFats, na.rm = TRUE),
                   NutriSodium = mean(NutriSodium, na.rm = TRUE),
                   NutriFatRatioScore = mean(NutriFatRatioScore, na.rm = TRUE),
                   NutriFVNO = mean(NutriFVNO, na.rm = TRUE),
                   NutriFiber = mean(NutriFiber, na.rm = TRUE),
                   NutriProtein = mean(NutriProtein, na.rm = TRUE),
                   NutriScoreNeg = mean(NutriScoreNeg, na.rm = TRUE),
                   NutriScorePos = mean(NutriScorePos, na.rm = TRUE),
                   NutriScorePoints = mean(NutriScorePoints, na.rm = TRUE)) %>%
  unique(.)

# Merging
plot.dat <-
  left_join(plot.dat, 
            nutri.dat) %>%
  unique(.)

# Adding colour scale
my.palette <- c('#5f6ac1', # Blue
                '#6cad5e', # Green
                '#aa539e', # Purple
                '#bb8c3c', # Yellow/Orange
                '#ba494e') # Red

plot.dat.aisle <-
  plot.dat.aisle %>%
  mutate(colour_4 = ifelse(Tot_env_100g_scaled < median(Tot_env_100g_scaled,na.rm = TRUE) & mean_nutriscore < median(mean_nutriscore),my.palette[2],
                           ifelse(Tot_env_100g_scaled < median(Tot_env_100g_scaled,na.rm = TRUE) & mean_nutriscore >= median(mean_nutriscore),my.palette[1],
                                  ifelse(Tot_env_100g_scaled >= median(Tot_env_100g_scaled,na.rm = TRUE) & mean_nutriscore < median(mean_nutriscore),my.palette[4], my.palette[5])))) %>%
  mutate(grouping = ifelse(Tot_env_100g_scaled < median(Tot_env_100g_scaled, na.rm = TRUE) & mean_nutriscore < median(mean_nutriscore,na.rm = TRUE), 'Low_Low',
                           ifelse(Tot_env_100g_scaled < median(Tot_env_100g_scaled,na.rm = TRUE) & mean_nutriscore >= median(mean_nutriscore, na.rm = TRUE),'Low_High',
                                  ifelse(Tot_env_100g_scaled >= median(Tot_env_100g_scaled,na.rm = TRUE) & mean_nutriscore < median(mean_nutriscore, na.rm = TRUE),'High_Low','High_High'))))


# Template for radar plots
# First row is max value (of all data points)
# Second row is min value (of all data points)
# Third row is mean value for the target aisle
# Fourth row is lower value for target aisle
# Fifth row is upper value for target aisle

# Need 10 columns (5 for nutrition, 5 for environment)
# And 5 rows
radar.template <- matrix(0, nrow = 5, ncol = 10)

# Names
radar.template <- data.frame(radar.template)
names(radar.template) <- 
  c('NutriScore_Scaled', 'NutriSugar','NutriSatFats','NutriSodium','NutriCal',
    'Eut_100g','Land_100g','GHGs_100g','WatScar_100g','Tot_env_100g_scaled')

# Updating min and max values in this
for(j in names(radar.template)) {
  # Max value - set to the 10th percentile of all data points at tesco
  radar.template[1,j] <- quantile(plot.dat[plot.dat$Retailer %in% 'Tesco',j], .95, na.rm = TRUE)[[1]]
  # Min value - set to 90th percentile of all data points at tesco
  radar.template[2,j] <- quantile(plot.dat[plot.dat$Retailer %in% 'Tesco',j], .05, na.rm = TRUE)[[1]]
  
  # if(j %in% 'NutriScorePos') {
  #   radar.template[1,j] <- quantile(dat[dat$Retailer %in% 'Tesco',j], .1, na.rm = TRUE)[[1]]
  #   radar.template[2,j] <- -quantile(dat[dat$Retailer %in% 'Tesco',j], .9, na.rm = TRUE)[[1]]
  # }
}

# Getting list of each aisle at Tesco
# Ordered by env impacts
dat.ordered <- 
  plot.dat.aisle %>%
  mutate(env_impact = Tot_env_100g_scaled, nut_points = mean_nutriscore) %>%
  mutate(rank_env = rank(env_impact),
         rank_nut = rank(nut_points))

# Creating template
master.matrix <- matrix(nrow = 8, ncol = 8)
# master.matrix[1,] <- 'Beverages'
# master.matrix[2,1:3] <- 'Beverages'
# master.matrix[2,4:8] <- 'Fruit, Vegetables, and Nuts'
# master.matrix[3,] <- 'Cereal Grains and Bread'
# master.matrix[4,] <- 'Prepared Foods'
# master.matrix[5,1:4] <- 'Prepared Foods'
# master.matrix[5,5:8] <- 'Snacks'
# master.matrix[6,] <- 'Desserts'
# master.matrix[7,1] <- 'Desserts'
# master.matrix[7,2:7] <- 'Cooking Accessories'
# master.matrix[7,8] <- 'Dairy, Fish, and Meat'
# master.matrix[8,] <- 'Dairy, Fish, and Meat'

master.matrix[1:3,1:3] <- 'Beverages'
master.matrix[1,1] <- NA
master.matrix[4,1:3] <- 'Beverages'
master.matrix[5:8,1:3] <- 'Prepared Foods'
master.matrix[1:3,6:8] <- 'Desserts'
master.matrix[6:8,6:8] <- 'Dairy, Fish, and Meat'
master.matrix[4:5,6:8] <- 'Cooking Accessories'
master.matrix[1:4,4:5] <- 'Cereal Grains and Bread'
master.matrix[5:6,4:5] <- 'Fruit, Vegetables, and Nuts'
master.matrix[7:8,4:5] <- 'Snacks'


# master.matrix[8,1:2] <- NA
# master.matrix[1,10] <- NA
# master.matrix[1:4,1:2] <- NA
# master.matrix[1,6] <- 'High_Low'
# master.matrix[5:8,9:10] <- NA
# master.matrix[5,9] <- 'Low_High'
# master.matrix[5:8,10] <- NA
# master.matrix[5,10] <- 'Low_High'
# master.matrix[8,2] <- NA
# master.matrix[1,9] <- NA
# master.matrix[4,2] <- 'High_Low'
dat.ordered <-
  dat.ordered %>%
  mutate(mean_impact = (rank_env + rank_nut)/2) %>%
  arrange(Food_Type, mean_impact)

dat.ordered.out <-
  dat.ordered

out.list <- rep(NA, length(master.matrix))

out.matrix <- 
  master.matrix


# for(i in 1:nrow(master.matrix)) {
# tmp.check
# }


for(i in 1:nrow(master.matrix)) {
  # for(i in 1:8) {
  tmp.list <- unique(master.matrix[i,]) %>% .[!is.na(.)]
  tmp.list.nas <- master.matrix[i,]
  
  counter = 1
  for(j in tmp.list) {
    counter = counter + 1
    tmp.check <- 
      dat.ordered %>% 
      filter(!(Aisle %in% out.list)) %>%
      filter(Food_Type %in% j) %>%
      as.data.frame(.) %>%
      .$Aisle
    
    tmp.check <- tmp.check[1:sum(out.matrix[i,] %in% j)]
    
    out.list[min(which(is.na(out.list))) : (min(which(is.na(out.list))) + length(tmp.check) - 1)] <-
      tmp.check
    
    out.matrix[i,out.matrix[i,] %in% j] <- tmp.check
  }
}

# my.palette <- brewer_pal(palette = 'Accent')
my.palette <- c('#5f6ac1', # Blue
                '#6cad5e', # Green
                '#aa539e', # Purple
                '#bb8c3c', # Yellow/Orange
                '#ba494e') # Red

# Adding colours
# dat.ordered <- 
#   dat.ordered %>%
#   mutate(colour = ifelse(grouping %in% 'Low_Low',my.palette[2],
#                          ifelse(grouping %in% 'Low_High',my.palette[1],
#                                 ifelse(grouping %in% 'High_Low',my.palette[4],my.palette[5]))))

dat.ordered <-
  dat.ordered %>%
  mutate(colour = plot_colour)

# Changing graphics...
dev.off()
par(mfrow = c(nrow(out.matrix),ncol(out.matrix)))
par(mar = c(0,0,0,0))
# par(mai = c(0,0,0,0))
par(oma = c(0,0,0,0))
par(cex.main = .8)
las = 1

dat.ordered$Aisle_new <- dat.ordered$Aisle

# Adding a carriage return every 15 characters for the plot titles
i <- which(nchar(dat.ordered$Aisle) > 15)
for(ii in i) {
  new.string <- dat.ordered$Aisle[ii]
  # Locate position of all spaces
  sp_replace <-
    str_locate_all(new.string," ")[[1]][,1]
  # Locate position of last space before 15th character
  sp_replace <- max(sp_replace[sp_replace < 15])
  # Replace with a \n
  substr(new.string,sp_replace,sp_replace) <- "\n"
  
  # And now repeat
  while(nchar(gsub(".*\n",'',new.string)) > 15) {
    # Position of all spaces
    sp_replace <-
      str_locate_all(new.string," ")[[1]][,1] 
    # Position of last carriage return
    car_loc <-
      str_locate_all(new.string,"\n")[[1]][,1] %>% max(.)
    # Last potential location of space
    max.loc <- car_loc + 16
    # Finding space to replace
    sp_replace <- max(sp_replace[sp_replace < max.loc])
    # And replacing
    substr(new.string,sp_replace,sp_replace) <- "\n"
  }
  # And replacing in the data frame
  dat.ordered$Aisle_new[ii] <- new.string
}

# Save PDF at 7.2in tall x 7.2in wide

dat.ordered$colour[dat.ordered$colour %in% '#000000'] <- '#bebebe'

# Now Looping through these
for(z in 1:nrow(out.matrix)) {
  for(k in 1:ncol(out.matrix)) {
    i = out.matrix[z,k]
    
    tmp.dat <-
      plot.dat %>%
      filter(Retailer %in% 'Tesco') %>%
      filter(Aisle %in% i) %>%
      as.data.frame(.)
    
    # Looping through columns of the radar dat plot to update values for the aisle
    if(!is.na(i)) {
      for(j in names(radar.template)) {
        # Max value
        radar.template[3,j] <-
          quantile(tmp.dat[,j], .95, na.rm = TRUE)[[1]]
        
        # Lower value
        radar.template[4,j] <-
          quantile(tmp.dat[,j], .05, na.rm = TRUE)[[1]]
        
        # Mean value
        radar.template[5,j] <-
          mean(tmp.dat[,which(names(tmp.dat) %in% j)], na.rm = TRUE)
        
        
        # And squishing so it plots properly
        radar.template[,j] <-
          squish(radar.template[,j],
                 range = c(radar.template[2,j], radar.template[1,j]))
      } 
    }
    
    # Plotting
    if(!is.na(i)) {
      radarchart(radar.template,
                 axistype = 0,
                 seg = 3,
                 pty = c(32,32,16),
                 plty = c(1,1,1),
                 cglty = 1,
                 cglwd = c(.5,rep(0,10)),
                 vlabels = NA,
                 vlcex = 1,
                 palcex = 1,
                 maxmin = TRUE,
                 axislabcol = 'black',
                 pcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],dat.ordered$colour[dat.ordered$Aisle %in% i],'black'),
                 # pfcol = c(muted(dat.ordered$colour_5[which(dat.ordered$Aisle %in% i)]),'white',NA),
                 pfcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],'white',NA),
                 cglcol = 'grey',
                 # cglcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],'white',NA),
                 centerzero = FALSE)
      # title(main = NULL, sub = i, line = -1.5, las = .1)
      title(main = dat.ordered$Aisle[dat.ordered$Aisle %in% i], line = -.6, las = .1)
    } else {
      radarchart(radar.template,
                 axistype = 0,
                 seg = 3,
                 pty = c(32,32,16),
                 plty = c(1,1,1),
                 cglty = 1,
                 cglwd = c(.5,rep(0,10)),
                 vlabels = NA,
                 vlcex = 1,
                 palcex = 1,
                 maxmin = TRUE,
                 axislabcol = 'black',
                 pcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],dat.ordered$colour[dat.ordered$Aisle %in% i],'black'),
                 # pfcol = c(muted(dat.ordered$colour_5[which(dat.ordered$Aisle %in% i)]),'white',NA),
                 pfcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],'white',NA),
                 cglcol = 'grey',
                 # cglcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],'white',NA),
                 centerzero = FALSE)
      # title(main = NULL, sub = i, line = -1.5, las = .1)
      title(main = 'DELETE ME', line = -.6, las = .1)
    }
    
  }
  
}

# Save PDF at 7.2in tall x 7.2in wide



# And now making the data set that goes with the figure

# Now Looping through these
for(z in 1:nrow(out.matrix)) {
  for(k in 1:ncol(out.matrix)) {
    i = out.matrix[z,k]
    
    tmp.dat <-
      plot.dat %>% 
      filter(Retailer %in% 'Tesco') %>%
      filter(Aisle %in% i)
    
    # Looping through columns of the radar dat plot to update values for the aisle
    for(j in names(radar.template)) {
      # Max value
      radar.template[3,j] <-
        quantile(tmp.dat[,j], .95, na.rm = TRUE)[[1]]
      
      # Lower value
      radar.template[4,j] <-
        quantile(tmp.dat[,j], .05, na.rm = TRUE)[[1]]
      
      # Mean value
      radar.template[5,j] <-
        mean(as.data.frame(tmp.dat[,j])[,1], na.rm = TRUE)
      
      
      # And squishing so it plots properly
      radar.template[,j] <-
        squish(radar.template[,j],
               range = c(radar.template[2,j], radar.template[1,j]))
    }
    
    if(z %in% 1 & k %in% 1) {
      out.radar.template <- 
        radar.template %>% 
        mutate(Aisle = i, 
               Plot_Identifier = c('Max Limit and Outer Bound of Radar Chart','Min Limit and Inner Bound of Radar Chart','95th Percentile Impact for Aisle','5th Percentile Impact for Aisle','Mean Impact for Aisle')) %>%
        mutate(plot_colour = NA,
               Food_Type = NA)
    } else {
      out.radar.template <-
        rbind(out.radar.template,
              radar.template %>%
                mutate(Aisle = i, Plot_Identifier = c('Max Limit and Outer Bound of Radar Chart','Min Limit and Inner Bound of Radar Chart','95th Percentile Impact for Aisle','5th Percentile Impact for Aisle','Mean Impact for Aisle')) %>%
                filter(Plot_Identifier %in% c('95th Percentile Impact for Aisle','5th Percentile Impact for Aisle','Mean Impact for Aisle')) %>%
                mutate(plot_colour = c(dat.ordered$colour[dat.ordered$Aisle %in% i]),
                       Food_Type = dat.ordered$Food_Type[dat.ordered$Aisle %in% i]))
    }
    
    # Plotting
    # radarchart(radar.template,
    #            axistype = 0,
    #            seg = 3,
    #            pty = c(32,32,16),
    #            plty = c(1,1,1),
    #            cglty = 1,
    #            cglwd = c(.5,rep(0,10)),
    #            vlabels = NA,
    #            vlcex = 1,
    #            palcex = 1,
    #            maxmin = TRUE,
    #            axislabcol = 'black',
    #            pcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],dat.ordered$colour[dat.ordered$Aisle %in% i],'black'),
    #            # pfcol = c(muted(dat.ordered$colour_5[which(dat.ordered$Aisle %in% i)]),'white',NA),
    #            pfcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],'white',NA),
    #            cglcol = 'grey',
    #            # cglcol = c(dat.ordered$colour[dat.ordered$Aisle %in% i],'white',NA),
    #            centerzero = FALSE)
    # # title(main = NULL, sub = i, line = -1.5, las = .1)
    # title(main = dat.ordered$Aisle_new[dat.ordered$Aisle %in% i], line = -.6, las = .1)
  }
}



out.radar.template <-
  out.radar.template %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.nan(NutriScore_Scaled)) %>%
  mutate(Aisle = ifelse(is.na(Aisle), Plot_Identifier, Aisle))

out.radar.template <-
  out.radar.template[,c(11:12,14,13,1:10)]

if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}

# Writing data frame
write.csv(out.radar.template,
          paste0(getwd(),"/Figure Data/Data Fig 5 6Feb2022.csv"),
          row.names = FALSE)



# Looking for high-impact products in low-impact aisles, etc
# quantile.dat <-
#   plot.dat %>%
#   dplyr::select(id, product_name, Aisle, NutriScore_Scaled, Tot_env_100g_scaled) %>%
#   mutate(nut_quantile = ifelse(NutriScore_Scaled < quantile(.$NutriScore_Scaled, 1/3),'Low',
#                                ifelse(NutriScore_Scaled > quantile(.$NutriScore_Scaled, 2/3), 'High','Moderate')),
#          env_quantile = ifelse(Tot_env_100g_scaled < quantile(.$Tot_env_100g_scaled, 1/3),'Low',
#                                ifelse(Tot_env_100g_scaled > quantile(.$Tot_env_100g_scaled, 2/3),'High','Moderate'))) %>%
#   group_by(Aisle, nut_quantile, env_quantile) %>%
#   dplyr::summarise(count = n())
# 
