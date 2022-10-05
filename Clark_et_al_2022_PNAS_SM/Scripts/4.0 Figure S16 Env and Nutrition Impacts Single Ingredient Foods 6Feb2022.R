###
# Figure S4
# Impacts of single ingredient foods

# Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(readr)

# Setting working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# Composition data for single ingredient foods
file.list = list.files(path = paste0(getwd(),'/Outputs'),
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
  group_by(id, product_name) %>%
  dplyr::summarise(n_ingredients = n())
# 
# # Filtering to products where =>99% composition accounted for by a single ingredient
# This will be filtered again later for single ingredient foods
stacked.dat <-
  stacked.dat %>%
  # filter(id %in% num.ingredients$id) %>%
  # filter(product_name %in% num.ingredients$product_name)
  mutate(percent = as.numeric(percent)) %>%
  filter(percent >= 99)

# Identifying processed meats
stacked.dat <-
  stacked.dat %>%
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
  mutate(Food_Category = ifelse(grepl('tapas selection|tapas platter|pork scratchings|crackling|British Wiltshire|Brdy Fmly Thinly Sliced|Proscuitto|Italian Meat Selection|Italian Selection|Serrano|Cured|Spanish Selection|\\bSpeck\\b|Antipasto|Smoked Pork|Continental Meat|Continental Selection|sausisson|Quarterpounder|burger|meat ball|meatball|Picanha|salt beef|Bresaola|Zywiecka|bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|Saucisson|Chorizo|Pancetta|Prosciutto|Cabanossi|Kielbasa|Krakowska|Crackles|Bellota|Peperami|French Charcuterie|Kabanos|Mortadella|unearthed', product_name, ignore.case = TRUE) & Food_Category %in% c('Fish (farmed)', 'Crustaceans (farmed)'), 'Processed fish meat', Food_Category))

# Only keeping single ingredient foods, unless it is processed meat
stacked.dat <-
  stacked.dat %>%
  filter(stacked.dat$id %in% num.ingredients$id[num.ingredients$n_ingredients %in% 1] | grepl('Processed',Food_Category,ignore.case = TRUE) & percent >= 99.9)

# Nutriscore dat
nutriscore.dat <-
  read_csv(paste0(getwd(),'/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv'))

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
  group_by(product_name, Food_Category) %>%
  dplyr::summarise(NutriScore_Scaled = mean(NutriScore_Scaled),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled)) %>%
  group_by(Food_Category) %>%
  dplyr::summarise(se_env = sd(Tot_env_100g_scaled, na.rm = TRUE) / sqrt(n()),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled, na.rm = TRUE),
                   se_nutriscore = sd(NutriScore_Scaled, na.rm = TRUE) / sqrt(n()),
                   NutriScore_Scaled = mean(NutriScore_Scaled, na.rm = TRUE)) %>%
  filter(!is.na(Food_Category)) %>%
  filter(!grepl('Wine|Water|Beer|Salt',Food_Category))

# Adding colours to the plot
impact.dat1 <-
  impact.dat %>%
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


impact.dat1 <-
  impact.dat1 %>%
  filter(!(Food_Category %in% c('Salt','Water','Wine','Animal Fats')))

# Plotting
ggplot(impact.dat1 %>% filter(!is.na(colour)), aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled, group = Food_Category)) +
  theme_classic() +
  # geom_hline(yintercept = quantile(keep.dat.plot$Tot_env_100g_scaled,1/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_hline(yintercept = quantile(keep.dat.plot$Tot_env_100g_scaled,2/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_vline(xintercept = quantile(keep.dat.plot$NutriScore_Scaled,1/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_vline(xintercept = quantile(keep.dat.plot$NutriScore_Scaled,2/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_vline(xintercept = .34) +
  # geom_vline(xintercept = .52) +
  # geom_smooth(method = 'lm') +
  # geom_text(aes(label = substr(Aisle,1,5))) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7, hjust = 0)) +
  theme(legend.title = element_text(size = 7, hjust = 0)) +
  # geom_errorbar(aes(ymin = Tot_env_100g_scaled - se_env * 1.96, ymax = Tot_env_100g_scaled + se_env * 1.96)) +
  # geom_errorbarh(aes(xmin = NutriScore_Scaled - se_nutriscore * 1.96, xmax = NutriScore_Scaled + se_nutriscore * 1.96)) +
  geom_text_repel(aes(label = Food_Category), size = 2.55, box.padding = 0.04, point.padding = 0.04, colour = impact.dat1$colour[!is.na(impact.dat1$colour)]) +
  # scale_x_continuous(limits = c(0,80), expand = c(0,0)) +
  scale_x_continuous(limits = c(.75,5.25),breaks = 1:5, labels = c('A','B','C','D','E')) +
  scale_y_continuous(trans = 'log10', limits = c(.10, 70), breaks = c(.1,.5,5,50), expand = c(0,0)) +
  # scale_y_continuous(trans = 'log10') +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score')

Sys.sleep(10)

# ggsave(paste0(getwd(),"/Figures/Fig S16 Environmental and Nutrition Impacts Single Ingredient Foods 6Feb2022.pdf"),
#        width = 18, height = 9, units = 'cm')

# Writing file
# And saving data
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}
write.csv(impact.dat %>% dplyr::select(Food_Category, Tot_env_100g_scaled, NutriScore_Scaled),
          paste0(getwd(),'/Figure Data/Data Fig S16 Env and Nutrition Impacts Single Ingredient Foods 6Feb2022.csv'),row.names = FALSE)


# Now by subcategories by food type

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
  filter(!grepl('Wine|Water|Beer|Salt',Food_Category))

# Adding colours to the plot
impact.dat1 <-
  impact.dat %>%
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

# Getting rid of water and wine...
impact.dat1 <-
  impact.dat1 %>%
  filter(!(Food_Category %in% c('Salt','Water','Wine','Animal Fats')))

# Adding labels
impact.dat1 <-
  impact.dat1 %>%
  mutate(plot_label = Food_Category_sub_sub) %>%
  mutate(plot_label = ifelse(is.na(plot_label), Food_Category_sub, plot_label)) %>%
  mutate(plot_label = ifelse(is.na(plot_label), Food_Category, plot_label))

# Adding food types
impact.dat1 <-
  impact.dat1 %>%
  filter(!is.na(colour)) %>%
  unique() %>%
  mutate(food_type = 'Plant-based foods') %>%
  mutate(food_type = ifelse(colour %in% '#666666','Dairy & Eggs',food_type)) %>%
  mutate(food_type = ifelse(colour %in% '#2e3192','Fish & Seafood',food_type)) %>%
  mutate(food_type = ifelse(colour %in% '#4d3626','Tea, Coffee & Chocolate',food_type)) %>%
  mutate(food_type = ifelse(colour %in% '#c1272c','Red Meat',food_type)) %>%
  mutate(food_type = ifelse(colour %in% '#ef4d83','Poultry Meat',food_type)) %>%
  mutate(food_type = ifelse(colour %in% '#f15a24','Oils',food_type)) %>%
  mutate(food_type = ifelse(colour %in% '#c39178','Sugar',food_type)) %>%
  mutate(plot_label = ifelse(food_type %in% 'Plant-based foods',NA,plot_label))


# Plotting
ggplot(impact.dat1, aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled)) +
  theme_classic() +
  # geom_hline(yintercept = quantile(keep.dat.plot$Tot_env_100g_scaled,1/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_hline(yintercept = quantile(keep.dat.plot$Tot_env_100g_scaled,2/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_vline(xintercept = quantile(keep.dat.plot$NutriScore_Scaled,1/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_vline(xintercept = quantile(keep.dat.plot$NutriScore_Scaled,2/3, na.rm = TRUE), colour = 'grey', alpha = .5, linetype = 2) +
  # geom_vline(xintercept = .34) +
  # geom_vline(xintercept = .52) +
  # geom_smooth(method = 'lm') +
  # geom_text(aes(label = substr(Aisle,1,5))) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7, hjust = 0)) +
  theme(legend.title = element_text(size = 7, hjust = 0)) +
  # geom_errorbar(aes(ymin = Tot_env_100g_scaled - se_env * 1.96, ymax = Tot_env_100g_scaled + se_env * 1.96)) +
  # geom_errorbarh(aes(xmin = NutriScore_Scaled - se_nutriscore * 1.96, xmax = NutriScore_Scaled + se_nutriscore * 1.96)) +
  geom_text_repel(aes(label = plot_label), size = 2.55, box.padding = 0.04, point.padding = 0.04, colour = impact.dat1$colour, max.overlaps = 50) +
  geom_point(aes(x = impact.dat1$NutriScore_Scaled[is.na(impact.dat1$plot_label)], y = impact.dat1$Tot_env_100g_scaled[is.na(impact.dat1$plot_label)], colour = impact.dat1$colour[is.na(impact.dat1$plot_label)])) +
  # scale_x_continuous(limits = c(0,80), expand = c(0,0)) +
  scale_x_continuous(limits = c(.75,5.25),breaks = 1:5, labels = c('A','B','C','D','E')) +
  scale_y_continuous(trans = 'log10', limits = c(.10, 70), breaks = c(.1,.5,5,50), expand = c(0,0)) +
  # scale_y_continuous(trans = 'log10') +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score') +
  facet_wrap(.~food_type, scales = 'free')

Sys.sleep(10)

# ggsave(paste0(getwd(),"/Figures/Fig Sx Environmental and Nutrition Impacts Single Ingredient Foods Sub Categories 6Feb2022.pdf"),
#        width = 30, height = 30, units = 'cm')

# Writing file
# And saving data
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}
write.csv(impact.dat %>% dplyr::select(Food_Category, Tot_env_100g_scaled, NutriScore_Scaled),
          paste0(getwd(),'/Figure Data/Data Fig S12 Env and Nutrition Impacts Single Ingredient Foods Sub Categories 6Feb2022.csv'),row.names = FALSE)
