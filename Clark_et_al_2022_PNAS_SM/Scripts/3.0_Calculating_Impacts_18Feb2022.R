#!/usr/bin/env Rscript

# removing memory space
rm(list = ls())

# Libraries
# I almost certainly do not need all these
# But this works
library(ggplot2)
library(plotly)
library(cowplot)
library(tidyr)
library(plyr)
library(dplyr)
library(extrafont)
library(extrafontdb)
library(showtext)
library(readr)
library(ggrepel)
library(stringr)
library(parallel)
library(matrixStats)

# Setting working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")


# Loading functions
source(paste0(getwd(),'/Scripts/0.0_Functions_Estimating_Impacts_18Feb2022.R'))

# Number of cores
n_cores = 15


#####
###
# Importing and stacking data

#####
###
# Adding fruit, veg, and nut composition by product
# Getting data frame of products we could interpolate
file.list = list.files(path = paste0(getwd(),'/Outputs'),
                       pattern = 'FoodDB percent composition by ingredient.*Jan.*2022', full.names = TRUE)

# file.list <- list.files(path = '/Users/macuser/Desktop/foodDB_outputs',full.names=TRUE,pattern = 'composition')


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

###
# Identifying brewed coffee
# Used later for the impact calculators
brewed.coffee <- brewed.coffee.tea(stacked.dat)
brewed.coffee <- unique(stacked.dat$id[brewed.coffee])



# Identifying products that had xxxg per 100g
# E.g., those where percent composition of an ingredient is > 100 and value is NA
# For these products, dropping all other instances of that food category
# And only removing the row that corresponds with xxxg per 100g product
# List of food cats in these products
stacked.dat.100g.cats <-
  stacked.dat %>%
  filter(percent > 100 & is.na(value)) %>%
  unique(.) %>%
  mutate(percent = as.numeric(percent)) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf, variable, value, Food_Category, Food_Category_sub, Food_Category_sub_sub, value_not_embedded) %>%
  summarise(percent = mean(percent, na.rm = TRUE))

# Reorganising columns
stacked.dat.100g.cats <-
  stacked.dat.100g.cats[,names(stacked.dat)]

# Filtering out this product food category combinations from stacked dat
stacked.dat <-
  stacked.dat %>% # Filtering out this combo of food cats, products, etc to avoid replicates
  filter(!(paste0(id,product_name,Retailer,Department,Aisle,Shelf,Food_Category, Food_Category_sub, Food_Category_sub_sub) %in%
           paste0(stacked.dat.100g.cats$id, stacked.dat.100g.cats$product_name,stacked.dat.100g.cats$Retailer,stacked.dat.100g.cats$Department,stacked.dat.100g.cats$Aisle,stacked.dat.100g.cats$Shelf,stacked.dat.100g.cats$Food_Category)))

# And rbinding two data frames
stacked.dat <-
  rbind(as.data.frame(stacked.dat),
        as.data.frame(stacked.dat.100g.cats)) %>%
  unique(.)


# Data from products with no listed ingredients
# These normally correspond to i.e. potatoes, tomatoes (other produce) or things like "milk", "cheese", etc
# Also managing to have same column names and orders as stacked dat above
dat.no.ingredients = 
  read.csv(list.files(paste0(getwd(),'/Outputs'),pattern='no.*list',full.names=TRUE),
  # read.csv("/Users/macuser/Desktop/foodDB_outputs/FoodDB estimated products no ingredient list 18January2022.csv",
           stringsAsFactors = FALSE) %>%
  mutate(Food_Category_sub = NA, Food_Category_sub_sub = NA) %>%
  dplyr::select(id = product_id, product_name, Retailer, Department = department, Aisle = aisle, Shelf = shelf, Food_Category,Food_Category_sub, Food_Category_sub_sub, url) %>%
  mutate(percent = 100, value_not_embedded = NA, variable = NA, value = NA) %>%
  unique(.)
# Updating column names
dat.no.ingredients <- 
  dat.no.ingredients[,names(stacked.dat)]

# Stacking data sets
stacked.dat <-
  rbind(stacked.dat %>% dplyr::select(id, product_name, value, Retailer, Department, Aisle, Shelf, Food_Category, Food_Category_sub, Food_Category_sub_sub, percent),
        dat.no.ingredients %>% dplyr::select(id, product_name, value, Retailer, Department, Aisle, Shelf, Food_Category,  Food_Category_sub, Food_Category_sub_sub,percent)) %>%
  mutate(percent = as.numeric(percent)) %>%
  unique(.)


# Identifying salt and water ----
# Doing this to avoid skewing composition of product with NAs
# And to accurately identify prods where >= 75% of composition is known

# Identifying salt
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('\\bsalt\\b', value, ignore.case = TRUE) & !is.na(value),'Salt',Food_Category)) %>%
  mutate(Salt = ifelse(Food_Category %in% 'Salt', percent / 2.5 * 1000,0))

# Identifying Water
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('water', value, ignore.case = TRUE) & !is.na(value), 'Water',Food_Category))

# Saving dataset used for later classification of products into e.g. oils, fats, cheese, etc
stacked.dat.save <-
  stacked.dat %>%
  dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf) %>%
  unique(.)

# Identifying info for nutriscore ----
# Identifying walnut oil
walnut.oil <-
  stacked.dat %>%
  mutate(Food_Category_Old = Food_Category) %>%
  mutate(Food_Category = NA) %>%
  mutate(Food_Category = ifelse(grepl("walnut.*oil", value, ignore.case = TRUE), 'Walnut Oil',NA)) %>%
  filter(!is.na(Food_Category)) 

# Updating walnut oil in stacked dat (assuming it is olive oil, because we don't really have a better match)
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(grepl('walnut.*oil',value,ignore.case=TRUE),'Olive Oil',Food_Category))

# Getting fvno (fruit, veg, nut, and oil) and sugar composition
fvno.sugar <-
  stacked.dat %>%
  mutate(Sugar = 0) %>%
  mutate(percent = as.numeric(percent)) %>%
  mutate(FVNO = ifelse(Food_Category %in% c('Apples','Bananas','Berries & Grapes',
                                            'Brassicas','Citrus Fruit','Groundnuts',
                                            'Nuts','Olives','Onions & Leeks',
                                            'Other Pulses','Other Vegetables','Peas',
                                            'Root Vegetables','Tofu','Tomatoes',
                                            'Olive Oil','Rapeseed Oil'),percent,0)) %>%
  mutate(Sugar = ifelse(Food_Category %in% c('Cane Sugar','Beet Sugar'), percent, 0)) %>%
  unique(.) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf) %>% 
  summarise(FVNO = sum(FVNO,na.rm=TRUE), 
            Sugar = sum(Sugar,na.rm=TRUE))

# tmp <-
#   fvno.sugar %>% 
#   filter(grepl('Morrisons Take Away Stuffed Crust Pepperoni Pizza|no added sugar diet cola|lemonade|dark chocolate fruit nut|beef flavour potato sticks|italian lasagne',product_name, ignore.case=TRUE)) %>%
#   filter(grepl('Morri',product_name)) %>%
#   filter(Retailer %in% 'Morissons') %>%
#   as.data.frame() %>%
#   group_by(product_name) %>%
#   dplyr::summarise(FVNO = mean(FVNO),
#                    min_fvno = min(FVNO),
#                    max_fvno = max(FVNO))

# write.csv(tmp,'/Users/macuser/Desktop/FVNO for Richie.csv',row.names=FALSE)

# Merging with info on walnut oil
fvno.sugar <-
  left_join(fvno.sugar,
            walnut.oil) %>%
  mutate(FVNO = ifelse(!is.na(percent), FVNO + percent, FVNO)) %>%
  dplyr::select(-percent)

# Updating for soy/almond/oat/rice milks ----
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(grepl("soy.*milk|soy.*drink",product_name, ignore.case = TRUE),'Soymilk',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl("almond.*milk|almond.*drink",product_name, ignore.case = TRUE) & !grepl('with|bar|milk chocolate|tubes|[0-9]{1,}g',product_name, ignore.case = TRUE),'Almond milk',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl("cashew.*milk|\\bnut.*milk|cashew.*drink|nut.*drink",product_name, ignore.case = TRUE) & !grepl('with|bar|milk chocolate|tubes|[0-9]{1,}g',product_name, ignore.case = TRUE),'Other nut milk',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl("\\brice.*milk|rice.*drink",product_name, ignore.case = TRUE),'Rice milk',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl("\\boat.*milk|\\boat.*drink",product_name, ignore.case = TRUE),'Oat milk',Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Milk' & percent >= 50 & grepl('cheese',product_name, ignore.case = TRUE),'Cheese',Food_Category)) # And catching cheese/milk - these were flagged because of e.g cheese (milk) in the ingredients list, which were idnetified as cheese. But updating here

# Getting rid of sub categories for nut milks/rice milks/etc
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category_sub = ifelse(Food_Category %in% c('Almond milk','Rice milk','Soymilk','Oat milk','Other nut milk'), NA, Food_Category_sub)) %>%
  mutate(Food_Category_sub_sub = ifelse(Food_Category %in% c('Almond milk','Rice milk','Soymilk','Oat milk', 'Other nut milk'), NA, Food_Category_sub_sub))

# Identifying broths and stocks
stacked.dat <- broth.stock(stacked.dat)

# Identifying organic ingredients
stacked.dat <- organic.ingredients(stacked.dat)

# Aggregating data by food category ----
# This will be used throughout the script
# And used to identify environmental and nutrition impact
# Of each unique entry of each product in the database
stacked.dat <-
  stacked.dat %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf, Food_Category, Food_Category_sub, Food_Category_sub_sub, Organic_ingredient) %>% 
  summarise(percent = sum(percent,na.rm=TRUE)) %>%
  as.data.frame(.)

# List of products with less than 75% of composition identified
filter.prods <-
  stacked.dat %>%
  filter(!is.na(Food_Category)) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf) %>%
  summarise(tot_percent = sum(percent, na.rm = TRUE)) %>%
  filter(tot_percent >= 75) %>%
  unique(.) %>% as.data.frame(.) %>%
  mutate(id.drop = paste0(id,product_name,Retailer,Department,Aisle,Shelf))


# Getting list of product IDs to drop because ingredients have <0 % composition
prods.negs <-
  stacked.dat %>%
  filter(percent < 0) %>%
  dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf) %>%
  unique(.) %>%
  mutate(id.drop = paste0(id,product_name,Retailer,Department,Aisle,Shelf))


# Adding nutritional information ----
# This is in case info for one of the nutrients is not available from back-of-package information
# Or alternatively, if back-of-package information clearly isn't correct (i.e. >100g fat / 100g product)

# Importing nutritional data from GeNUS
nut.info =
  read.csv(paste0(getwd(),"/Data Inputs/Nutrient Info By LCA Category 24April2020.csv"),
           stringsAsFactors = FALSE)

# Calculating nutrient info by product
# This is average for that product across all retail outlets, departments, etc
# This takes a while. I'm not sure why.
dat <-
  left_join(stacked.dat,
            nut.info %>% dplyr::select(Food_Category = food.group,
                                       Calories, Protein, Fat, SaturatedFat = Saturated.FA,
                                       Fiber = Dietary.Fiber, Sodium, Carbohydrates)) %>%
  mutate(Calories = Calories * percent/100, # Calculating composition
         Protein = Protein * percent/100,
         Fat = Fat * percent/100,
         SaturatedFat = SaturatedFat * percent/100,
         Fiber = Fiber * percent/100,
         Carbohydrates = Carbohydrates * percent/100,
         Sodium = Sodium * percent/100)

# Adding salt
# This is much much faster than using ifelse in dplyr
dat$Salt[dat$Food_Category %in% 'Salt'] <- 
  dat$percent[dat$Food_Category %in% 'Salt'] / 2.5 * 1000

dat$Sodium[dat$Food_Category %in% 'Salt'] <- 
  dat$percent[dat$Food_Category %in% 'Salt'] / 2.5 * 1000

# and summarising by product name
dat <- 
  dat %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf) %>% # Summing by product name and id
  summarise(Calories = sum(Calories, na.rm = TRUE),
            Protein = sum(Protein, na.rm = TRUE),
            Fat = sum(Fat, na.rm = TRUE),
            SaturatedFat = sum(SaturatedFat, na.rm = TRUE),
            Fiber = sum(Fiber, na.rm = TRUE),
            Sodium = sum(Sodium, na.rm = TRUE),
            Carbohydrates = sum(Carbohydrates, na.rm = TRUE),
            Salt = sum(Salt, na.rm = TRUE)) %>% 
  unique(.) %>% as.data.frame(.) %>%
  mutate(id.drop = paste0(id,product_name,Retailer,Department,Aisle,Shelf)) %>%
  filter(!(id.drop %in% prods.negs$id.drop)) %>% # Getting rid of products with negative compositional values
  filter(id.drop %in% filter.prods$id.drop) # Keeping products with > 75% composition identified

# Joining in fnvo and sugar data
dat <-
  left_join(dat,
            fvno.sugar %>% dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf, FVNO, Sugar))

# Correcting back of package information ----
# Using listed back of package info if available
# Assuming none of the info for a product is incorrect
# If it is incorrect, then using the estimated information

# Importing back of package information
raw.dat <- 
  read_csv(paste0(getwd(),"/foodDB_dat/products.csv")) %>% #Importing data
  dplyr::select(id = product_id, product_name, # Limiting to select columns
                Sugar_pack = sugar_per_100, # Needed to calculate NutriScore
                Fat_pack = fat_per_100,
                SatFat_pack = saturates_per_100,
                Salt_pack = salt_per_100,
                Protein_pack = protein_per_100,
                Fibre_pack = fibre_per_100,
                Carbs_pack = carbohydrate_per_100,
                Energy_pack = energy_per_100,
                serving, serving_data, serving_value, serving_unit)

# Getting col indices of nutrients needed for NutriScore
nutrient.list <-
  names(raw.dat)[which(names(raw.dat) %in% 'Sugar_pack') : which(names(raw.dat) %in% 'Energy_pack')]

# Identifying and adjusting units for each nutrient
# This makes sure i.e. units are g/mg
# And the numeric value for the nutrient is correct
dat.nutrition <- 
  nutrition.adjust.function(dat = raw.dat,
                            nutrient.list = nutrient.list)

# Converting 'NaNs' to 'NA's
dat.nutrition[which(dat.nutrition[,'Sugar_pack_value'] %in% 'NaN'),'Sugar_pack_value'] <- NA
dat.nutrition[which(dat.nutrition[,'Fat_pack_value'] %in% 'NaN'),'Fat_pack_value'] <- NA
dat.nutrition[which(dat.nutrition[,'SatFat_pack_value'] %in% 'NaN'),'SatFat_pack_value'] <- NA
dat.nutrition[which(dat.nutrition[,'Salt_pack_value'] %in% 'NaN'),'Salt_pack_value'] <- NA
dat.nutrition[which(dat.nutrition[,'Protein_pack_value'] %in% 'NaN'),'Protein_pack_value'] <- NA
dat.nutrition[which(dat.nutrition[,'Fibre_pack_value'] %in% 'NaN'),'Fibre_pack_value'] <- NA
dat.nutrition[which(dat.nutrition[,'Carbs_pack_value'] %in% 'NaN'),'Carbs_pack_value'] <- NA

# Summarising nutrition by product
# And performing logic checks to make sure a product doesn't e.g. have >100g fat per 100g product
# It's impossible to tell what is correct
# But very easy to tell what is incorrect
dat.nutrition <-
  dat.nutrition %>%
  group_by(product_name) %>%
  summarise(Sugar_pack_value = mean(Sugar_pack_value, na.rm = TRUE),
            Fat_pack_value = mean(Fat_pack_value, na.rm = TRUE),
            SatFat_pack_value = mean(SatFat_pack_value, na.rm = TRUE),
            Salt_pack_value = mean(Salt_pack_value, na.rm = TRUE),
            Protein_pack_value = mean(Protein_pack_value, na.rm = TRUE),
            Fibre_pack_value = mean(Fibre_pack_value, na.rm = TRUE),
            Carbs_pack_value = mean(Carbs_pack_value, na.rm = TRUE),
            Energy_pack_value = mean(Energy_pack_value, na.rm = TRUE)) %>%
  mutate(check_pack = ifelse(Sugar_pack_value > 100 & !is.na(Sugar_pack_value), 1, # Logical checks
                             ifelse(Fat_pack_value > 100  & !is.na(Fat_pack_value), 1, 
                                    ifelse(SatFat_pack_value > 100  & !is.na(SatFat_pack_value), 1,
                                           ifelse(SatFat_pack_value > Fat_pack_value  & !is.na(SatFat_pack_value) & !is.na(Fat_pack_value), 1,
                                                  ifelse(Salt_pack_value > 100  & !is.na(Salt_pack_value), 1,
                                                         ifelse(Protein_pack_value > 100  & !is.na(Protein_pack_value), 1,
                                                                ifelse(Salt_pack_value > 100  & !is.na(Salt_pack_value), 1,
                                                                       ifelse(Carbs_pack_value > 100  & !is.na(Carbs_pack_value), 1,
                                                                              ifelse(Fibre_pack_value > 100  & !is.na(Fibre_pack_value), 1, 0)))))))))) %>%
  mutate(Calories_pack_value = Fat_pack_value * 8.84 + Carbs_pack_value * 4 + Protein_pack_value * 4) %>%
  as.data.frame(.)

# Merging in estimated nutritional value
# ANd using estimated values in cases where back of package info is clearly incorrect
dat <- 
  left_join(dat %>% unique(.), # Merging
            dat.nutrition %>% unique(.)) %>%
  mutate(Sugar = ifelse(!is.na(Sugar_pack_value) & !(check_pack %in% 1), Sugar_pack_value, Sugar), # Logic checks
         Fat = ifelse(!is.na(Fat_pack_value) & !(check_pack %in% 1), Fat_pack_value, Fat), # Basically, if back of package info is crap
         SaturatedFat = ifelse(!is.na(SatFat_pack_value) & !(check_pack %in% 1), SatFat_pack_value, SaturatedFat), # Then estimating based on our estimates
         Salt = ifelse(!is.na(Salt_pack_value) & !(check_pack %in% 1), Salt_pack_value * 1000 / 2.5, Sodium),
         Protein = ifelse(!is.na(Protein_pack_value) & !(check_pack %in% 1), Protein_pack_value, Protein),
         Fiber = ifelse(!is.na(Fibre_pack_value) & !(check_pack %in% 1), Fibre_pack_value, Fiber),
         Carbs = ifelse(!is.na(Carbs_pack_value) & !(check_pack %in% 1), Carbs_pack_value, Carbohydrates),
         Calories = ifelse(!is.na(Energy_pack_value) & !(check_pack %in% 1), Energy_pack_value, Calories)) %>%
  mutate(Sodium = Salt) %>%
  unique(.) %>%
  mutate(Calories = Fat * 8.84 + Carbs * 4 + Protein * 4)

# Classifying products for nutriscore ----
# Drinks
# Cheese
# Oils

# Identifying drink products
drinks <- 
  stacked.dat.save %>%
  dplyr::select(product_name, Retailer, Department, Aisle, Shelf) %>%
  unique(.) %>%
  mutate(drink_department = ifelse(Department %in% c('Drinks','Soft Drinks, Tea & Coffee','Tea, Coffee & Soft Drinks'), 'Drink', 
         ifelse(Department %in% c('Beer, wine & spirits','Beer, Wine & Spirits','Beer, Wines & Spritis'),'Alcohol','No'))) %>%
  mutate(drink_aisle = ifelse(Aisle %in% c('All Drinks','Ambient Juice','Arla Shop','Bottled Water','Chilled fruit juice & smoothies','Chilled Fruit Juice & Smoothies','Chilled Juice',
                      'Chilled Juice & Drinks','Chilled Juice & Smoothies','Chilled Juice, Smoothies & Drinks','Christmas drinks','Christmas Drinks','Coca Cola Shop','Coffee','Cordials',
                      'Drinks','Drinks Bigger Packs','Energy & Health Drinks','Fizzy drinks','Fizzy Drinks','Fizzy Drinks & Cola','Fruit juice & drinks','Go to Category: \n£1 Value Drinks','Go to Category: \nHalloween Drinks',
                      'Hot chocolate & malted drinks','Hot Chocolate & Malted Drinks','Hot Chocolate & Malts','Hot chocolate & milky drinks','Hot Drinks','Juices','Juices & Smoothies','Kids Drinks','Longer life juice & juice drinks',
                      'Milk & Dairy Drinks','Milk & milk drinks','Milkshake','Mixers','Mixers & adult soft drinks','Mixers & Adult Soft Drinks','Premium Drinks & Mixers','Smoothies','Smoothies, Juice & Yogrhut Drinks','Soft Drinks',
                      'Soft Drinks & Juices','Sports & Energy Drinks','Still & Sparkling','Still & Sparkling Fruit Drinks','Squash','Squash & Cordial','Squash & cordials','Squash & Cordials',
                      'Tea','Tea & Hot Drinks','Tea, coffee & hot drinks','Tea, Coffee & Hot Drinks','Tea Coffee & Juices','Tonic & Mixers','Tonic Water & Mixers','Water'),'Drink',
                      ifelse(Aisle %in% c('Alcohol Free','Alcohol gifts','Alcoholic Drinks','Ales & Stouts','Beer','Beer & Cider','Beer, Wine & Spirits','Beers and Ciders','Champagne & sparkling wine',
                                          'Champagne & Sparkling Wine','Cider','Cider, Wine & Spirits','Cocktails','Craft Beer','Lager','Low & No Alcohol','Low Alcohol & Alcohol Free Drinks','Low alcohol & gluten free',
                                          'Sparkling Wine','Spirits','Spirits & liqueurs','Spirits & Liqueurs','Spirits and Liqueurs','Wine','Wine & Champagne','Wine, Fizz & Drinks'),'Alcohol','No'))) %>%
  mutate(drink_shelf = ifelse(Shelf %in% c('Chilled Drinks','Chilled Juice & Smoothies','Chilled Juice, Smoothies & Drinks','Daily Yoghurt Drinks','Dairy Alternative Drinks','Drink Coolers','Drinks','Energy Drinks','Energy drinks','Essences, Juices & Flavourings',
                                     'Extracts, Essences & Juices','Fizzy Drinks','Fresh Fruit juice','Fresh Juice and Herbal Tea','Frozen smoothie mixes','Half & half','Goats Milk','Italian Coffee','Juices','Kings & Tonic','Long Life Drinks','Long Life Juice','Longlife Milk','Long Life Milk',
                                     'Long Life UHT Milk','Milk','Milk & cream','Milkshake, Iced Coffee & Protein Drinks','Mixers','Original Lemonade','Schweppes 1783','Soft Drinks','Sporks drinks','Sports nutritional drinks','Sports Drinks','Sports cap water','Stock','Stocks','Stocks & Gravies',
                                     'Tea','Tea, Sake & Other Beverages','Tea, Coffee & Hot Drinks','Tea, Coffee & Soft Drinks','The Ultimate Light Mixer','The Ultimate Mixer','Yoghurt Drinks','Yogurt drinks','Yogurt Drinks'), 'Drinks',
                        ifelse(Shelf %in% c('Beer','Beer & Cider','Beer & Spirits','Beers, Wine & Spirits','Dessert Wine','Italian Wine','Spirits, Beer & Cider','Wine','Wine & Champagne','Wine & Fizz','Wines'), 'Alcohol','No'))) %>%
  mutate(drink = ifelse(drink_department %in% 'Drink' | drink_aisle %in% 'Drink' | drink_shelf %in% 'Drink', 'Drinks',
                        ifelse(drink_department %in% 'Alcohol' | drink_aisle %in% 'Alcohol' | drink_shelf %in% 'Alcohol','Alcohol','No'))) %>%
  dplyr::select(product_name, drink) %>%
  filter(drink %in% c('Drinks','Alcohol')) %>%
  unique(.)

# or if water > 90% of the product
drinks <- 
  rbind(drinks,
        stacked.dat %>% 
          filter(Food_Category %in% c('Water','Milk','Soymilk','Rice milk','Almond milk','Oat milk')) %>%
          group_by(id, product_name, Department, Aisle, Shelf) %>%
          dplyr::summarise(percent = sum(percent, na.rm = TRUE)) %>%
          mutate(drink = ifelse(percent >= 90,'Drink',NA)) %>%
          filter(drink %in% 'Drink') %>%
          as.data.frame(.) %>%
          dplyr::select(product_name, drink) %>%
          unique(.)) %>%
  unique(.)

# Identifying Cheese
cheese <- 
  stacked.dat.save %>%
  dplyr::select(product_name, Retailer, Department, Aisle, Shelf) %>%
  unique(.) %>%
  mutate(cheese_department = ifelse(grepl('cheese',Department, ignore.case = TRUE), 'Cheese','No')) %>%
  mutate(cheese_aisle = ifelse(grepl('cheese',Aisle, ignore.case = TRUE), 'Cheese','No')) %>%
  mutate(cheese_shelf = ifelse(Shelf %in% c('All Cheese','Blue Cheese','Build Your Cheeseboard','Brie & Camembert','Cheddar Cheese','Cheese','Cheese & Accompaniments','Cheese & Crackers','Cheese Counter','Cheese for Entertaining','Cheese Selections',
                                            'Cheese Slices, Spreads & Triangles','Cheese Snacking','Cheese Spreads & Snacks','Cheeseboards','Cheese Snacks & Spreads','Cheeseboard & Deli','Cheeseboards & Selections','Cheesemongers','Continental Cheese',
                                            'Continental & Specialty Cheese','Cottage & Soft Cheese','Counter Cheese','Cottage Cheese & Soft Cheese','Counter - Cheese','Cream, Soft & Cottage Cheese','Dairy Alternative Cheese','Dairy Free Cheese & Alternatives',
                                            'Deli, Cheese & Accompaniments','Deli Style Cheese','Feta & Goats Cheese','Feta & Halloumi','Feta, Halloumi & Paneer','Grated & Sliced Cheese','Grated & Sliced','Goats Cheese','Hard Cheese','Italian Cheese','Lighter & Low Fat Cheese',
                                            'Lunch Box Cheese','Mozzarella, Mascarpone & Ricotta','No.1 Cheese','Parmesan & Pecorino','Quark','Quark, Soft Cream & Cottage Cheese','Reduced Fat Cheese','Regional Cheese','Sliced & Grated Cheese','Snacking Cheese & Lunchboxes',
                                            'Stilton & Blue Cheese','Vegan & Dairy Free Cheese'),'Cheese','No')) %>%
  mutate(cheese = ifelse(cheese_department %in% c('Cheese'), 'Cheese',
                        ifelse(cheese_aisle %in% 'Cheese','Cheese',
                               ifelse(cheese_shelf %in% 'Cheese','Cheese','No')))) %>%
  dplyr::select(product_name, cheese) %>%
  filter(cheese %in% 'Cheese') %>%
  unique(.)

# or if cheese > 90% of the product
cheese <- 
  rbind(cheese,
        stacked.dat %>% 
          filter(grepl('Cheese',Food_Category)) %>%
          group_by(id, product_name, Department, Aisle, Shelf) %>%
          dplyr::summarise(percent = sum(percent, na.rm = TRUE)) %>%
          mutate(cheese = ifelse(percent >= 90,'Cheese',NA)) %>%
          as.data.frame(.) %>%
          unique(.) %>%
          dplyr::select(product_name, cheese) %>%
          filter(cheese %in% 'Cheese')) %>%
  unique(.)

# Identifying Oils and Fats
fats.oils <- 
  stacked.dat.save %>%
  dplyr::select(product_name, Retailer, Department, Aisle, Shelf) %>%
  unique(.) %>%
  mutate(fats.oils = ifelse(Shelf %in% c('Butter, Spreads & Margarine','Butter, Fats & Spreads','Butter, spreads & pastry','Butter, Spreads & Pastry','Butters, Fats & Spreads','Oils','Oil','Oils & Fats',
                                         'Oils & Vinegar','Oils & Vinegars'),'Fats.Oils','No')) %>%
  mutate(fats.oils = ifelse(grepl('Vinegar|pastry|jus ros|jus-ros|jr feuille|dough|balsamic', product_name, ignore.case = TRUE), 'No',fats.oils)) %>%
  dplyr::select(product_name, fats.oils) %>%
  filter(fats.oils %in% 'Fats.Oils') %>%
  unique(.)

# or if fats/oils > 90% of the product
fats.oils <- 
  rbind(fats.oils,
        stacked.dat %>% 
          filter(Food_Category %in% c('Butter, Cream & Ghee',"Oils Misc.",'Olive Oil','Palm Oil','Rapeseed Oil',"Soybean Oil","Sunflower Oil")) %>%
          group_by(id, product_name, Department, Aisle, Shelf) %>%
          dplyr::summarise(percent = sum(percent, na.rm = TRUE)) %>%
          mutate(fats.oils = ifelse(percent >= 90,'Fats.Oils',NA)) %>%
          filter(fats.oils %in% 'Fats.Oils') %>%
          as.data.frame(.) %>%
          dplyr::select(product_name, fats.oils) %>%
          unique(.)) %>%
  unique(.)

# Updating classifications for these products in the big data set
dat <-
  dat %>%
  mutate(cheese = ifelse(product_name %in% cheese$product_name,'Cheese','No'),
         fat.oil = ifelse(product_name %in% fats.oils$product_name, 'Fat.Oil','No'),
         alcohol = ifelse(product_name %in% drinks$product_name[drinks$drink %in% 'Alcohol'],'Alcohol','No'),
         drinks = ifelse(product_name %in% drinks$product_name[drinks$drink %in% 'Drinks'],'Drinks','No'))

# Calculating nutriscore
# And making sure we only have products we're keeping
nutriscore = 
  nutriscore.function(dat = dat) %>%
  filter(!(id.drop %in% prods.negs$id.drop)) %>%
  filter(id.drop %in% filter.prods$id.drop)

# Creating directory
dir.create(paste0(getwd(),"/Managed_Data"))

# Saving for radar plots
write.csv(nutriscore %>% dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf, 
                                       NutriCal, NutriSugar, NutriSatFats, NutriSodium, NutriFatRatioScore, NutriFVNO, NutriFiber, NutriProtein,
                                       NutriScoreNeg, NutriScorePos, NutriScorePoints),
          paste0(getwd(),"/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv"),
          row.names = FALSE)

# Calculating environmental impacts per 100g ----
# Importing LCA data set
# Managing lca dat
lca.dat <- 
  read.csv(paste0(getwd(),"/Data Inputs/jp_lca_dat.csv"),
           stringsAsFactors = FALSE) %>%
  mutate(Weight = as.numeric(gsub("%","",Weight)))

# Adding translation for subcategories
lca.subcats <- read.csv(paste0(getwd(),'/Data Inputs/Search words, second round, 22Jan2022.csv'))

# And updating lca categories
lca.dat <-
  left_join(lca.dat, # Merging
            lca.subcats %>% dplyr::select(Data.S2.Name = LCA_Category, Product_details, LCA_Category_sub = LCA_sub_category, LCA_Category_sub_sub = LCA_sub_sub_category, 
                                          Average_of_original_category, Average_of_sub_category) %>% 
              filter(LCA_Category_sub != '') %>% unique(.)) %>%
  unique(.) #%>%
  # mutate(LCA_Category_sub_sub = ifelse(Average_of_sub_category %in% 'Yes',NA,LCA_Category_sub_sub)) %>%
  # mutate(LCA_Category_sub = ifelse(Average_of_original_category %in% 'Yes',NA,LCA_Category_sub))

# Adding conversion estimates
# lca.dat <-
#   rbind(lca.dat,
#         conversion.function(indicators = c('^Land.Use','GHG','Eutrophication','Scarcity','Acidification','^Water','Biodiversity')) %>% dplyr::select(-food.group)) %>%
#   .[,c('Data.S2.Name','LCA_Category_sub','LCA_Category_sub_sub','Weight','Land.Use..m2.year.','GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks.',
#        'Eutrophication..g.PO43.eq.','Scarcity.Weighted.Water.Use..L.eq.','Acidification..g.SO2eq.','Water.Use..L.','Biodiversity..sp.yr.10.14.',
#        'Average_of_original_category','Average_of_sub_category')]


lca.dat <-
  rbind(conversion.function(indicators = c('^Land.Use','GHG','Eutrophication','Scarcity','Acidification','^Water','Biodiversity')) %>% dplyr::select(-food.group),
        lca.dat %>% filter(!grepl('Cheese',Data.S2.Name))) %>% # Conversion function goes from cheese to other types of cheese
  .[,c('Data.S2.Name','LCA_Category_sub','LCA_Category_sub_sub','Weight','Land.Use..m2.year.','GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks.',
       'Eutrophication..g.PO43.eq.','Scarcity.Weighted.Water.Use..L.eq.','Acidification..g.SO2eq.','Water.Use..L.','Biodiversity..sp.yr.10.14.',
       'Average_of_original_category','Average_of_sub_category','Sys')] 
# And adding in other cheese category
lca.dat <-
  rbind(lca.dat,
        lca.dat %>% filter(grepl('Medium Cheese',LCA_Category_sub)) %>% mutate(LCA_Category_sub = 'Other Cheese')) # And adding in the other cheese category

# Updating categories for almond milk vs other milk
lca.dat <-
  lca.dat %>%
  mutate(Data.S2.Name = ifelse(Data.S2.Name %in% 'Almond milk' & !(LCA_Category_sub %in% 'Almonds'),'Other nut milk', Data.S2.Name)) %>%
  mutate(LCA_Category_sub = ifelse(Data.S2.Name %in% c('Almond milk','Other nut milk','Oat milk','Soymilk','Rice milk'),NA, LCA_Category_sub)) %>%
  mutate(LCA_Category_sub_sub = ifelse(Data.S2.Name %in% c('Almond milk','Other nut milk','Oat milk','Soymilk','Rice milk'),NA, LCA_Category_sub_sub))

  

# Adding butter, misc oils, and pig meat
# These weightings recommended by Joseph Poore, folliwng methods in Poore and Nemecek 2018 Science
lca.dat <-
  rbind(lca.dat,
        lca.dat %>% filter(Data.S2.Name %in% 'Milk') %>% mutate(Data.S2.Name = 'Butter, Cream & Ghee'),
        lca.dat %>% filter(Data.S2.Name %in% 'Rapeseed Oil') %>% mutate(Data.S2.Name = 'Oils Misc.'),
        lca.dat %>% filter(Data.S2.Name %in% 'Pig Meat') %>% mutate(Data.S2.Name = 'Animal Fats')) %>%
  rbind(., # Adding info for tea, coffee, chocolate
        read.csv(paste0(getwd(),"/Data Inputs/lcadat 17october2019.csv"),
                 stringsAsFactors = FALSE) %>%
          filter(Data.S2.Name %in% 'Tea') %>% mutate(Weight = 100) %>% 
          mutate(LCA_Category_sub_sub = '',LCA_Category_sub = '', Sys = 'C',
                 Average_of_original_category = NA, Average_of_sub_category = NA) %>% # Adding column names
          .[,names(lca.dat)]) # And ordering columns to rbind

# Adding data on tea

# and updating names to merge with rest of script
names(lca.dat)[names(lca.dat) %in% 'Data.S2.Name'] <- 'Food_Category'
names(lca.dat)[names(lca.dat) %in% 'Land.Use..m2.year.'] <- 'Land'
names(lca.dat)[names(lca.dat) %in% 'GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks.'] <- 'GHG'
names(lca.dat)[names(lca.dat) %in% 'Eutrophication..g.PO43.eq.'] <- 'Eut'
names(lca.dat)[names(lca.dat) %in% 'Scarcity.Weighted.Water.Use..L.eq.'] <- 'WatScar'
names(lca.dat)[names(lca.dat) %in% 'Biodiversity..sp.yr.10.14.'] <- 'Biodiversity'
names(lca.dat)[names(lca.dat) %in% 'Acidification..g.SO2eq.'] <- 'Acidification'
names(lca.dat)[names(lca.dat) %in% 'Water.Use..L.'] <- 'WaterUse'


# and limiting lca dat to only necessary columns
lca.dat <- 
  lca.dat[,c('Food_Category','LCA_Category_sub','LCA_Category_sub_sub','Weight','Land','GHG','Eut','WatScar','Biodiversity','Acidification','WaterUse','Average_of_original_category','Average_of_sub_category','Sys')] %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Fish (farmed)','Fish (wild caught)','Crustaceans (farmed)','Crustaceans (wild caught)'),
                                gsub(" \\(farmed\\)| \\(wild caught\\)","",Food_Category),
                                Food_Category)) %>%
  rbind(., data.frame(Food_Category = 'Salt',LCA_Category_sub = NA, LCA_Category_sub_sub = NA, Weight = 1, Land = 0, GHG = 0, Eut = 0, WatScar = 0, Biodiversity = 0, Acidification = 0, WaterUse = 0,Average_of_original_category=NA,Average_of_sub_category=NA, Sys = 'C')) %>% # Adding data for salt
  rbind(., data.frame(Food_Category = 'Water',LCA_Category_sub = NA, LCA_Category_sub_sub = NA, Weight = 1, Land = 0, GHG = 0, Eut = 0, WatScar = 0, Biodiversity = 0, Acidification = 0, WaterUse = 0,Average_of_original_category=NA,Average_of_sub_category=NA, Sys = 'C')) # Adding data for water

# Renaming column - doing this for merging with food data later
lca.dat <-
  lca.dat %>%
  dplyr::rename(Food_Category_sub = LCA_Category_sub,
                Food_Category_sub_sub = LCA_Category_sub_sub)

# Adding in fisheries data
lca.dat <-
  rbind(lca.dat,
        fish.env.function('yay'))
  

# Updating food category
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Fish (farmed)','Fish (wild caught)','Crustaceans (farmed)','Crustaceans (wild caught)'),
                                gsub(" \\(farmed\\)| \\(wild caught\\)","",Food_Category),
                                Food_Category))

# Formatting  data for monte carlo for the env estimates
food.df <-
  stacked.dat %>%
  mutate(check.id = paste0(product_name, id, Retailer, Department, Aisle, Shelf)) %>%
  filter(!(check.id %in% .$check.id[.$percent <= 0])) %>%
  group_by(product_name, id, Retailer, Department, Aisle, Shelf, Food_Category, Food_Category_sub, Food_Category_sub_sub, Organic_ingredient) %>%
  dplyr::summarise(amount = sum(percent, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  mutate(product_name = paste0(product_name, id, Retailer, Department, Aisle, Shelf)) %>% 
  filter(!is.na(Food_Category))

# Merging in the coffee data
food.df <-
  food.df %>%
  mutate(brewed_coffee = 'NO') %>%
  mutate(brewed_coffee = ifelse(id %in% brewed.coffee & Food_Category %in% 'Coffee' & amount >= 10, 'Coffee','No'))

# Getting list of product ids for parallelization
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
# splitting
split.product.list <- chunk2(unique(food.df$product_name), n_cores)
split.product.list <- chunk2(unique(food.df$product_name)[1:6], 2)

# and running
# this will take a while...
t1 = Sys.time()
out.df <- do.call(rbind, mclapply(split.product.list, monte.carlo.lca, mc.cores = 2))
Sys.time() - t1

# Saving...
write.csv(out.df,
          paste0(getwd(),"/Managed_Data/Monte Carlo  by Product 18Feb2022 Log2.csv"),
          row.names = FALSE)

# Getting min and max again in the case of the same product being in different aisles/shelves
min.max <-
  out.df %>%
  dplyr::select(id,min_Land,min_GHG,min_Eut,min_WatScar,min_Biodiversity,min_Acidification,min_WaterUse,
                max_Land,max_GHG,max_Eut,max_WatScar,max_Biodiversity,max_Acidification,max_WaterUse)

out.df <-
  out.df %>%
  group_by(id) %>%
  dplyr::summarise(mean_Land = mean(mean_Land, na.rm = TRUE),
                   mean_GHG = mean(mean_GHG, na.rm = TRUE),
                   mean_Eut = mean(mean_Eut, na.rm = TRUE),
                   mean_WatScar = mean(mean_WatScar, na.rm = TRUE),
                   mean_Biodiversity = mean(mean_Biodiversity, na.rm = TRUE),
                   mean_Acidification = mean(mean_Acidification, na.rm = TRUE),
                   mean_WaterUse = mean(mean_WaterUse, na.rm = TRUE),
                   se_Land = mean(se_Land, na.rm = TRUE),
                   se_GHG = mean(se_GHG, na.rm = TRUE),
                   se_Eut = mean(se_Eut, na.rm = TRUE),
                   se_WatScar = mean(se_WatScar, na.rm = TRUE),
                   se_Biodiversity = mean(se_Biodiversity, na.rm = TRUE),
                   se_Acidification = mean(se_Acidification, na.rm = TRUE),
                   se_WaterUse = mean(se_WaterUse, na.rm = TRUE),
                   lower_ci_Land = mean(lower_ci_Land, na.rm = TRUE),
                   lower_ci_GHG = mean(lower_ci_GHG, na.rm = TRUE),
                   lower_ci_Eut = mean(lower_ci_Eut, na.rm = TRUE),
                   lower_ci_WatScar = mean(lower_ci_WatScar, na.rm = TRUE),
                   lower_ci_Biodiversity = mean(lower_ci_Biodiversity, na.rm = TRUE),
                   lower_ci_Acidification = mean(lower_ci_Acidification, na.rm = TRUE),
                   lower_ci_WaterUse = mean(lower_ci_WaterUse, na.rm = TRUE),
                   upper_ci_Land = mean(upper_ci_Land, na.rm = TRUE),
                   upper_ci_GHG = mean(upper_ci_GHG, na.rm = TRUE),
                   upper_ci_Eut = mean(upper_ci_Eut, na.rm = TRUE),
                   upper_ci_Biodiversity = mean(upper_ci_Biodiversity, na.rm = TRUE),
                   upper_ci_Acidification = mean(upper_ci_Acidification, na.rm = TRUE),
                   upper_ci_WaterUse = mean(upper_ci_WaterUse, na.rm = TRUE),
                   upper_ci_WatScar = mean(upper_ci_WatScar, na.rm = TRUE),
                   lower_fifth_Land = mean(lower_fifth_Land, na.rm = TRUE),
                   lower_fifth_GHG = mean(lower_fifth_GHG, na.rm = TRUE),
                   lower_fifth_Eut = mean(lower_fifth_Eut, na.rm = TRUE),
                   lower_fifth_WatScar = mean(lower_fifth_WatScar, na.rm = TRUE),
                   lower_fifth_Biodiversity = mean(lower_fifth_Biodiversity, na.rm = TRUE),
                   lower_fifth_Acidification = mean(lower_fifth_Acidification, na.rm = TRUE),
                   lower_fifth_WaterUse = mean(lower_fifth_WaterUse, na.rm = TRUE),
		   lower_tenth_Land = mean(lower_tenth_Land, na.rm = TRUE),
                   lower_tenth_GHG = mean(lower_tenth_GHG, na.rm = TRUE),
                   lower_tenth_Eut = mean(lower_tenth_Eut, na.rm = TRUE),
                   lower_tenth_WatScar = mean(lower_tenth_WatScar, na.rm = TRUE),
                   lower_tenth_Biodiversity = mean(lower_tenth_Biodiversity, na.rm = TRUE),
                   lower_tenth_Acidification = mean(lower_tenth_Acidification, na.rm = TRUE),
                   lower_tenth_WaterUse = mean(lower_tenth_WaterUse, na.rm = TRUE),
		   lower_twentyfifth_Land = mean(lower_twentyfifth_Land, na.rm = TRUE),
		   lower_twentyfifth_GHG = mean(lower_twentyfifth_GHG, na.rm = TRUE),
		   lower_twentyfifth_Eut = mean(lower_twentyfifth_Eut, na.rm = TRUE),
		   lower_twentyfifth_WatScar = mean(lower_twentyfifth_WatScar, na.rm = TRUE),
		   lower_twentyfifth_Biodiversity = mean(lower_twentyfifth_Biodiversity, na.rm = TRUE),
		   lower_twentyfifth_Acidification = mean(lower_twentyfifth_Acidification, na.rm = TRUE),
		   lower_twentyfifth_WaterUse = mean(lower_twentyfifth_WaterUse, na.rm = TRUE),
		   fifty_Land = mean(fifty_Land, na.rm = TRUE),
                   fifty_GHG = mean(fifty_GHG, na.rm = TRUE),
                   fifty_Eut = mean(fifty_Eut, na.rm = TRUE),
                   fifty_WatScar = mean(fifty_WatScar, na.rm = TRUE),
                   fifty_Biodiversity = mean(fifty_Biodiversity, na.rm = TRUE),
                   fifty_Acidification = mean(fifty_Acidification, na.rm = TRUE),
                   fifty_WaterUse = mean(fifty_WaterUse, na.rm = TRUE),
		   upper_seventyfifth_Land = mean(upper_seventyfifth_Land, na.rm = TRUE),
		   upper_seventyfifth_GHG = mean(upper_seventyfifth_GHG, na.rm = TRUE),
		   upper_seventyfifth_Eut = mean(upper_seventyfifth_Eut, na.rm = TRUE),
		   upper_seventyfifth_WatScar = mean(upper_seventyfifth_WatScar, na.rm = TRUE),
		   upper_seventyfifth_Biodiversity = mean(upper_seventyfifth_Biodiversity, na.rm = TRUE),
		   upper_seventyfifth_Acidification = mean(upper_seventyfifth_Acidification, na.rm = TRUE),
		   upper_seventyfifth_WaterUse = mean(upper_seventyfifth_WaterUse, na.rm = TRUE),
		   upper_ninety_Land = mean(upper_ninety_Land, na.rm = TRUE),
		   upper_ninety_GHG = mean(upper_ninety_GHG, na.rm = TRUE),
		   upper_ninety_Eut = mean(upper_ninety_Eut, na.rm = TRUE),
		   upper_ninety_WatScar = mean(upper_ninety_WatScar, na.rm = TRUE),
		   upper_ninety_Biodiversity = mean(upper_ninety_Biodiversity, na.rm = TRUE),
		   upper_ninety_Acidification = mean(upper_ninety_Acidification, na.rm = TRUE),
		   upper_ninety_WaterUse = mean(upper_ninety_WaterUse, na.rm = TRUE),
		   upper_ninetyfifth_Land = mean(upper_ninetyfifth_Land, na.rm = TRUE),
                   upper_ninetyfifth_GHG = mean(upper_ninetyfifth_GHG, na.rm = TRUE),
                   upper_ninetyfifth_Eut = mean(upper_ninetyfifth_Eut, na.rm = TRUE),
                   upper_ninetyfifth_WatScar = mean(upper_ninetyfifth_WatScar, na.rm = TRUE),
                   upper_ninetyfifth_Biodiversity = mean(upper_ninetyfifth_Biodiversity, na.rm = TRUE),
                   upper_ninetyfifth_Acidification = mean(upper_ninetyfifth_Acidification, na.rm = TRUE),
                   upper_ninetyfifth_WaterUse = mean(upper_ninetyfifth_WaterUse, na.rm = TRUE),) %>%
  unique(.)

# Min max
names.loop <- names(min.max)[which(names(min.max) %in% 'min_Land') : ncol(min.max)]
# Scaling to get low/high impacts for products with reps
for(i in names.loop) {
  min.max[,paste0('scaled_',i)] <- min.max[,i]/max(out.df[,gsub("min|max","mean",i)], na.rm = TRUE)
}
# Summing the scaling
min.max <-
  min.max %>%
  mutate(min_scaled = scaled_min_Land + scaled_min_GHG + scaled_min_Eut + scaled_min_WatScar,
         max_scaled = scaled_max_Land + scaled_max_GHG + scaled_max_Eut + scaled_max_WatScar)
# Minimum observation
min.df <-
  min.max %>%
  group_by(id) %>%
  filter(min_scaled %in% min(min_scaled)) %>%
  dplyr::select(id, min_Land, min_GHG, min_Eut, min_WatScar, min_Biodiversity, min_Acidification, min_WaterUse) %>%
  unique(.)
# max observation
max.df <-
  min.max %>%
  group_by(id) %>%
  filter(max_scaled %in% max(max_scaled)) %>%
  dplyr::select(id, max_Land, max_GHG, max_Eut, max_WatScar, max_Biodiversity, max_Acidification, max_WaterUse) %>%
  unique(.)
# Merging back together
min.max <- left_join(min.df, max.df) %>% unique(.)
# And merging this back into out.df
out.df <- left_join(out.df, min.max)
  
# Creating id
dat.env <-
  left_join(out.df,
            stacked.dat %>% dplyr::select(product_name, id, product_name, Retailer, Department, Aisle, Shelf) %>% unique(.)) %>%
  mutate(id.drop = paste0(id, product_name, Retailer, Department, Aisle, Shelf)) %>%
  mutate(tot_env = mean_GHG + mean_WatScar + mean_Land + mean_Eut) %>%
  filter(product_name %in% nutriscore$product_name & id %in% nutriscore$id)

# Merging env and nutrition data ----
# And calculating averages per product_name
dat.env.nut <-
  full_join(nutriscore,
            dat.env) %>%
  filter(!is.na(tot_nutrition)) %>%
  filter(!is.na(tot_env)) %>%
  mutate(Water = ifelse(product_name %in% stacked.dat$product_name[stacked.dat$percent %in% 100 & stacked.dat$Food_Category %in% 'Water'],'Water','No')) %>%
  filter(tot_nutrition > 0 | Water %in% 'Water') %>%
  filter(tot_env > 0 | Water %in% 'Water') %>%
  mutate(NutriScoreLetter = ifelse(Water %in% 'Water','A',NutriScoreLetter)) %>%
  dplyr::select(product_name, id, NutriScore_Scaled, 
                NutriScorePoints, NutriScoreLetter, 
                mean_GHG, mean_Land, mean_Eut, mean_WatScar, mean_Biodiversity, mean_Acidification, mean_WaterUse,
                se_GHG, se_Land, se_Eut, se_WatScar, se_Biodiversity, se_Acidification, se_WaterUse,
                lower_ci_GHG, lower_ci_Land, lower_ci_Eut, lower_ci_WatScar, lower_ci_Biodiversity, lower_ci_Acidification, lower_ci_WaterUse,
                upper_ci_GHG, upper_ci_Land, upper_ci_Eut, upper_ci_WatScar, upper_ci_Biodiversity, upper_ci_Acidification, upper_ci_WaterUse,
                min_GHG, min_Land, min_Eut, min_WatScar, min_Biodiversity, min_Acidification, min_WaterUse,
                max_GHG, max_Land, max_Eut, max_WatScar, max_Biodiversity, max_Acidification, max_WaterUse,
                lower_fifth_Land, lower_fifth_GHG, lower_fifth_Eut, lower_fifth_WatScar, lower_fifth_Biodiversity, lower_fifth_Acidification, lower_fifth_WaterUse,
                lower_tenth_Land, lower_tenth_GHG, lower_tenth_Eut, lower_tenth_WatScar, lower_tenth_Biodiversity, lower_tenth_Acidification, lower_tenth_WaterUse,
		lower_twentyfifth_Land, lower_twentyfifth_GHG, lower_twentyfifth_Eut, lower_twentyfifth_WatScar, lower_twentyfifth_Biodiversity, lower_twentyfifth_Acidification, lower_twentyfifth_WaterUse,
		fifty_Land, fifty_GHG, fifty_Eut, fifty_WatScar, fifty_Biodiversity, fifty_Acidification, fifty_WaterUse,
		upper_seventyfifth_Land, upper_seventyfifth_GHG, upper_seventyfifth_Eut, upper_seventyfifth_WatScar, upper_seventyfifth_Biodiversity, upper_seventyfifth_Acidification, upper_seventyfifth_WaterUse,
		upper_ninety_Land, upper_ninety_GHG, upper_ninety_Eut, upper_ninety_WatScar, upper_ninety_Biodiversity, upper_ninety_Acidification, upper_ninety_WaterUse,
		upper_ninetyfifth_Land, upper_ninetyfifth_GHG, upper_ninetyfifth_Eut, upper_ninetyfifth_WatScar, upper_ninetyfifth_Biodiversity, upper_ninetyfifth_Acidification, upper_ninetyfifth_WaterUse,
                Calories, Protein, alcohol, drinks) %>%
  filter(!(alcohol %in% 'Alcohol')) %>%
  unique(.)

# Calculating impacts per serving ----
# Extracting serving size
dat.serving <-
  extract.serving.dat(dat = raw.dat,
                      serving.col = 'serving_data') 

# Getting average serving size across all entries for the product
dat.serving.estimated <-
  dat.serving %>%
  group_by(id, product_name) %>%
  summarise(serving_value = mean(serving_value, na.rm = TRUE),
            pack_size_new = mean(pack_size_new)) %>%
  as.data.frame(.) %>%
  group_by(product_name) %>%
  summarise(serving_value = mean(serving_value, na.rm = TRUE),
            pack_size_new = mean(pack_size_new))

# Getting known serving values
dat.serving.known <-
  dat.serving %>%
  filter(!is.na(serving_value)) %>%
  group_by(product_name) %>%
  summarise(serving_value_known = mean(serving_value))

# Merging in and updating
dat.serving <-
  left_join(dat.serving.estimated,
            dat.serving.known) %>%
  mutate(serving_value = ifelse(!is.na(serving_value_known), serving_value_known, serving_value)) %>%
  dplyr::select(-serving_value_known)
  
# Merging in serving sizes
dat.env.nut <-
  left_join(dat.env.nut,
            dat.serving %>% dplyr::select(product_name, serving_value) %>% unique(.)) %>%
  unique(.) %>% 
  mutate(serving_value = as.numeric(serving_value))

# Adding retailer, department, aisle, and shelf info back in
dat.env.nut <- 
  left_join(dat.env.nut,
            stacked.dat.save %>% dplyr::select(product_name, Retailer, Department, Aisle, Shelf) %>% unique(.))

# Calculating average serving size by Department, Aisle, Shelf, etc
# Doing this to approximate serving size for products without this listed
# This is not perfect
# But probably the best we can do
serving.sizes.new <-
  update.serving.size(dat.env.nut) %>%
  unique(.)

# Merging in
dat.env.nut <-
  left_join(dat.env.nut,
            serving.sizes.new) %>%
  unique(.) %>%
  mutate(serving_value_updated = ifelse(!is.na(serving_value), serving_value, serving_value_updated))

# merging with product id, and writing csv for anonymisation purposes
# serving.size.anon <-
#   left_join(dat.env.nut %>% dplyr::select(product_name, serving_value_updated),
#             stacked.dat %>% dplyr::select(product_name, id) %>% unique) %>%
#   dplyr::select(id, serving_value_updated) %>% unique(.)
# 
# write.csv(serving.size.anon,
#           "/Users/macuser/Desktop/Estimating Impacts of Foods/foodDB_dat/servings.csv",
#           row.names = FALSE)

# Calculating env impacts per serving ----
dat.env.nut[,paste0("serving_",names(dat.env.nut)[which(names(dat.env.nut) %in% 'mean_GHG'):which(names(dat.env.nut) %in% 'upper_ninetyfifth_WaterUse')])] <-
  dat.env.nut[,which(names(dat.env.nut) %in% 'mean_GHG'):which(names(dat.env.nut) %in% 'upper_ninetyfifth_WaterUse')] * (dat.env.nut$serving_value_updated)/100
  

###
# Scaling
dat.scaled <-
  scaling.function(dat = dat.env.nut,
                   env.indicators = c('GHG|Land|WatScar|Eut'))

###
# And saving data
write.csv(dat.scaled,
          paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv"),
          row.names = FALSE)



