#!/usr/bin/env Rscript

# removing memory space
rm(list = ls())

n_cores = 15

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
library(stringi)
library(matrixStats)

# Setting working directory
 setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# Loading functions
source(paste0(getwd(),'/Scripts/0.0_Functions_Estimating_Impacts_18Feb2022.R'))

#####
###
# Adding fruit, veg, and nut composition by product
# Getting data frame of products we could interpolate
file.list = list.files(path = paste0(getwd(),'/Validation/EstimatedComposition'),
                       full.names = TRUE) %>% .[!grepl('log',.,ignore.case=TRUE)]

# stacking these
stacked.dat = data.frame()

for(i in 1:length(file.list)) {
  tmp.dat = read.csv(file.list[i], stringsAsFactors = FALSE)
  if(i == 1) {
    stacked.dat = rbind(stacked.dat,tmp.dat) 
  } else if(length(names(tmp.dat)) != length(names(stacked.dat))) {
    names.both <-
      names(tmp.dat)[names(tmp.dat) %in% names(stacked.dat)]
    stacked.dat <- stacked.dat[,names.both]
    tmp.dat <- tmp.dat[,names.both]
    stacked.dat = rbind(stacked.dat, tmp.dat)
  } else if(length(names(tmp.dat)) == length(names(stacked.dat))) {
    stacked.dat = rbind(stacked.dat, tmp.dat)
  }
}

# Unique values only
stacked.dat <- unique(stacked.dat)


# Identifying broths and stocks
stacked.dat <- broth.stock(stacked.dat)
# Identifying organic ingredients
stacked.dat <- organic.ingredients(stacked.dat)
# Identifying brewed coffee
brewed.coffee <- brewed.coffee.tea(stacked.dat)
brewed.coffee <- unique(stacked.dat$id[brewed.coffee])

# Getting products with no embedded ingredients
stacked.dat <-
  stacked.dat %>%
  mutate(has_embedded = ifelse(embedded_ingredients %in% '' | is.na(embedded_ingredients), 0, 1))

has.embedded <- 
  stacked.dat %>%
  group_by(id,product_name) %>%
  dplyr::summarise(count_embedded = sum(has_embedded)) %>%
  filter(count_embedded != 1)

# Creating user id
stacked.dat <-
  stacked.dat %>%
  filter(product_name %in% has.embedded$product_name) %>%
  mutate(u_id = paste0(id, product_name, Retailer, Department, Aisle, Shelf, info_validate))

# List of products with less than 75% of composition identified
filter.prods <-
  stacked.dat %>%
  filter(!is.na(Food_Category)) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf, info_validate) %>%
  dplyr::summarise(tot_percent = sum(percent, na.rm = TRUE)) %>%
  filter(tot_percent >= 75) %>%
  unique(.) %>% as.data.frame(.) %>%
  mutate(id.drop = paste0(id,product_name,Retailer,Department,Aisle,Shelf,info_validate))

# Limiting to products used in the analysis
# That is - those products with >75% of composition sorted into a food category
stacked.dat <-
  stacked.dat %>%
  filter(paste0(id,product_name,Retailer,Department,Aisle,Shelf,info_validate) %in% filter.prods$id.drop)

# Saving dataset used for later classification of products into e.g. oils, fats, cheese, etc
stacked.dat.save <-
  stacked.dat %>%
  dplyr::select(u_id, id, product_name, Retailer, Department, Aisle, Shelf, 
                percent_known, n_percent_known, variable_removed_validate, info_validate, n_ingredients) %>%
  unique(.)

# Stacking data sets
stacked.dat <-
  stacked.dat %>% 
  dplyr::select(u_id, id, product_name, value, Retailer, Department, Aisle, Shelf, 
                Food_Category, Food_Category_sub, Food_Category_sub_sub, 
                percent, percent_known, n_percent_known, variable_removed_validate, info_validate, n_ingredients,
                Organic_ingredient) %>%
  mutate(percent = as.numeric(percent)) %>%
  unique(.)


# Identifying salt
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('\\bsalt\\b', value, ignore.case = TRUE) & !is.na(value),'Salt',Food_Category)) %>%
  mutate(Salt = ifelse(Food_Category %in% 'Salt', percent / 2.5 * 1000,0))

# Identifying Water
stacked.dat <-
  stacked.dat %>%
  mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('water', value, ignore.case = TRUE) & !is.na(value), 'Water',Food_Category))

###
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

###
# Getting list of product IDs where percent composition is < 0
prods.negs <-
  stacked.dat %>%
  filter(percent < 0) %>%
  mutate(id.drop = paste0(id, product_name, Retailer, Department, Aisle, Shelf, info_validate)) %>%
  dplyr::select(product_name) %>%
  unique(.)

# and filtering to get rid of the negative observations
stacked.dat <-
  stacked.dat %>%
  filter(!(paste0(id, product_name, Retailer, Department, Aisle, Shelf, info_validate) %in% prods.negs$id.drop))

# And keeping only products used in the full analysis.
prods.keep <-
  read_csv(paste0(getwd(),'/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv'))

stacked.dat <-
  stacked.dat %>%
  filter(product_name %in% prods.keep$product_name)

###
# Calculating env impacts
# Importing data first
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
# Now running the env estimates
food.df <-
  stacked.dat %>%
  mutate(check.id = paste0(product_name, id, Retailer, Department, Aisle, Shelf, info_validate)) %>%
  filter(!(check.id %in% .$check.id[.$percent <= 0])) %>%# removing negative percents...
  group_by(product_name, id, Retailer, Department, Aisle, Shelf, Food_Category, Food_Category_sub, Food_Category_sub_sub, info_validate, Organic_ingredient) %>%
  dplyr::summarise(amount = sum(percent, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  mutate(product_name = paste0(product_name, id, Retailer, Department, Aisle, Shelf, info_validate)) %>% 
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
# split.product.list <- food.df$product_name

# and running
# this will take a while...
t1 = Sys.time()
out.df <- do.call(rbind, mclapply(split.product.list, monte.carlo.lca, mc.cores = n_cores))
Sys.time()-t1

# Merging in retailer/department/aisle/shelf to take summary across these
out.df <-
  left_join(out.df, 
            stacked.dat %>% dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf, info_validate) %>% mutate(product_name = paste0(product_name, id, Retailer, Department, Aisle, Shelf, info_validate))) %>%
  mutate(drop.gsub = paste0(Retailer, Department, Aisle, Shelf)) %>%
  unique(.)

# Dropping the retailer/department/aisle/shelf from the product name used for validation
out.df$product_name <-
  stri_trim(stri_replace(out.df$product_name, fixed=out.df$drop.gsub, ""))

# Getting min, max, 5th%, and 95th% estimates
min.max <-
  out.df %>%
  dplyr::select(product_name, info_validate, 
                min_Land,min_GHG,min_Eut,min_WatScar, min_Biodiversity, min_Acidification, min_WaterUse,
                max_Land,max_GHG,max_Eut,max_WatScar, max_Biodiversity, max_Acidification, max_WaterUse)

# And summarising across product name and info validate
out.df <-
  out.df %>%
  group_by(product_name, info_validate) %>%
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
                   upper_ninetyfifth_Land = mean(upper_ninetyfifth_Land, na.rm = TRUE),
                   upper_ninetyfifth_GHG = mean(upper_ninetyfifth_GHG, na.rm = TRUE),
                   upper_ninetyfifth_Eut = mean(upper_ninetyfifth_Eut, na.rm = TRUE),
                   upper_ninetyfifth_WatScar = mean(upper_ninetyfifth_WatScar, na.rm = TRUE),
                   upper_ninetyfifth_Biodiversity = mean(upper_ninetyfifth_Biodiversity, na.rm = TRUE),
                   upper_ninetyfifth_Acidification = mean(upper_ninetyfifth_Acidification, na.rm = TRUE),
                   upper_ninetyfifth_WaterUse = mean(upper_ninetyfifth_WaterUse, na.rm = TRUE),) %>%
  unique(.)

# Scaling Min max
names.loop <- names(min.max)[which(names(min.max) %in% 'min_Land') : which(names(min.max) %in% 'max_WatScar')]
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
  group_by(product_name, info_validate) %>%
  filter(min_scaled %in% min(min_scaled)) %>%
  dplyr::select(product_name, info_validate,
                min_Land, min_GHG, min_Eut, min_WatScar, min_Biodiversity, min_Acidification, min_WaterUse) %>%
  unique(.)
# max observation
max.df <-
  min.max %>%
  group_by(product_name, info_validate) %>%
  filter(max_scaled %in% max(max_scaled)) %>%
  dplyr::select(product_name, info_validate, 
                max_Land, max_GHG, max_Eut, max_WatScar, max_Biodiversity, max_Acidification, max_WaterUse) %>%
  unique(.)

# Merging back together
min.max <- full_join(min.df, max.df) %>% unique(.) %>%
  as.data.frame(.) %>%
  group_by(product_name, info_validate) %>%
  dplyr::summarise(min_Land = mean(min_Land, na.rm = TRUE),
                   min_GHG = mean(min_GHG, na.rm = TRUE),
                   min_Eut = mean(min_Eut, na.rm = TRUE),
                   min_WatScar = mean(min_WatScar, na.rm = TRUE),
                   min_Biodiversity = mean(min_Biodiversity, na.rm = TRUE),
                   min_Acidification = mean(min_Acidification, na.rm = TRUE),
                   min_WaterUse = mean(min_WaterUse, na.rm = TRUE),
                   max_Land = mean(max_Land, na.rm = TRUE),
                   max_GHG = mean(max_GHG, na.rm = TRUE),
                   max_Eut = mean(max_Eut, na.rm = TRUE),
                   max_WatScar = mean(max_WatScar, na.rm = TRUE),
                   max_Biodiversity = mean(max_Biodiversity, na.rm = TRUE),
                   max_Acidification = mean(max_Acidification, na.rm = TRUE),
                   max_WaterUse = mean(max_WaterUse, na.rm = TRUE),)
# And merging this back into out.df
out.df <- left_join(out.df, min.max)


# Creating id
dat.env <-
  left_join(out.df %>% dplyr::rename(merge.id = product_name),
            stacked.dat %>% mutate(merge.id = paste0(product_name,id, info_validate)) %>% dplyr::select(merge.id, id, product_name, Retailer, Department, Aisle, Shelf, percent_known, n_percent_known, variable_removed_validate, info_validate, n_ingredients) %>% unique(.)) %>%
  # mutate(id.drop = paste0(product_name, Retailer, Department, Aisle, Shelf)) %>%
  mutate(tot_env = mean_GHG + mean_WatScar + mean_Land + mean_Eut) %>%
  filter(tot_env > 0) %>%
  unique(.)

# scaling will be done in the validation script to ensure consistent scoring
# dat.scaled <-
#   scaling.function(dat = dat.env,
#                    env.indicators = c('GHG|Land|WatScar|Eut'))

# Filtering products where exactly 100% composition is known ----
# Need to do this because composition information for some products sums to > 100%
# Adding fruit, veg, and nut composition by product
# Getting data frame of products we could interpolate
file.list = list.files(path = paste0(getwd(),'/Outputs'),
                       pattern = 'FoodDB percent composition by ingredient ', full.names = TRUE)

# stacking these
stacked.dat = data.frame()

for(i in 1:length(file.list)) {
  tmp.dat = read.csv(file.list[i], stringsAsFactors = FALSE)
  if(i == 1) {
    stacked.dat = rbind(stacked.dat,tmp.dat) 
  } else if(length(names(tmp.dat)) != length(names(stacked.dat))) {
    names.both <-
      names(tmp.dat)[names(tmp.dat) %in% names(stacked.dat)]
    stacked.dat <- stacked.dat[,names.both]
    tmp.dat <- tmp.dat[,names.both]
    stacked.dat = rbind(stacked.dat, tmp.dat)
  } else if(length(names(tmp.dat)) == length(names(stacked.dat))) {
    stacked.dat = rbind(stacked.dat, tmp.dat)
  }
}

stacked.dat <- unique(stacked.dat)

# Summing to get total % by product
stacked.dat.sum <-
  stacked.dat %>%
  # mutate(u_id = paste0(id, product_name, Retailer, Department, Aisle, Shelf)) %>%
  mutate(percent = as.numeric(percent)) %>%
  group_by(product_name, id, Retailer, Department, Aisle, Shelf) %>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm = TRUE)) %>%
  mutate(percent = round(percent, digits = 0)) %>%
  filter(percent %in% 100) %>%
  as.data.frame(.) %>%
  mutate(filter.id = paste0(id, product_name, Department, Retailer, Aisle, Shelf))

# # Filtering dat.env for products w/ 100%
# dat.filter <-
#   dat.env

dat.env <- 
  dat.env %>%
  mutate(filter.id = paste0(id, product_name, Department, Retailer, Aisle, Shelf)) %>%
  filter(filter.id %in% stacked.dat.sum$filter.id) %>%
  filter(tot_env >0) %>% # and dropping department/etc to get unique products
  dplyr::select(-Department) %>% dplyr::select(-Retailer) %>% dplyr::select(-Aisle) %>% dplyr::select(-Shelf) %>% dplyr::select(-filter.id) %>%
  unique(.)


###
# Saving data
write.csv(dat.env,
          paste0(getwd(),"/Managed_Data/Validating Impacts by Product 6Feb2022.csv"),
          row.names = FALSE)


