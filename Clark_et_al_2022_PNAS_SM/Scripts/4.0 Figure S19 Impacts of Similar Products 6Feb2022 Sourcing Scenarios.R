###
# Figure 5
# Impacts of similar products
# As well as associated stats tests

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

# Data
dat <- 
  read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv")) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  filter(!is.na(NutriScore_Scaled))

dat <-
  dat %>%
  mutate(Tot_env_100g_scaled_lower_ci = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar)/4,
         Tot_env_100g_scaled_upper_ci = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar)/4,
         Tot_env_100g_scaled_min = (scaled_min_GHG + scaled_min_Eut + scaled_min_Land + scaled_min_WatScar)/4,
         Tot_env_100g_scaled_max = (scaled_max_GHG + scaled_max_Eut + scaled_max_Land + scaled_max_WatScar)/4,
         Tot_env_100g_scaled_lower_fifth = (scaled_lower_fifth_GHG + scaled_lower_fifth_Eut + scaled_lower_fifth_Land + scaled_lower_fifth_WatScar)/4,
         Tot_env_100g_scaled_lower_tenth = (scaled_lower_tenth_GHG + scaled_lower_tenth_Eut + scaled_lower_tenth_Land + scaled_lower_tenth_WatScar)/4,
         Tot_env_100g_scaled_lower_twentyfifth = (scaled_lower_twentyfifth_GHG + scaled_lower_twentyfifth_Eut + scaled_lower_twentyfifth_Land + scaled_lower_twentyfifth_WatScar)/4,
         Tot_env_100g_scaled_upper_seventyfifth = (scaled_upper_seventyfifth_GHG + scaled_upper_seventyfifth_Eut + scaled_upper_seventyfifth_Land + scaled_upper_seventyfifth_WatScar)/4,
         Tot_env_100g_scaled_upper_ninety = (scaled_upper_ninety_GHG + scaled_upper_ninety_Eut + scaled_upper_ninety_Land + scaled_upper_ninety_WatScar)/4,
         Tot_env_100g_scaled_upper_ninetyfifth = (scaled_upper_ninetyfifth_GHG + scaled_upper_ninetyfifth_Eut + scaled_upper_ninetyfifth_Land + scaled_upper_ninetyfifth_WatScar)/4,
         Tot_env_100g_scaled = (scaled_mean_GHG + scaled_mean_Eut + scaled_mean_Land + scaled_mean_WatScar)/4) %>%
  mutate(Tot_env_100g_scaled_lower_ci = Tot_env_100g_scaled_lower_ci / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_upper_ci = Tot_env_100g_scaled_upper_ci / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_min = Tot_env_100g_scaled_min / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_max = Tot_env_100g_scaled_max / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_lower_fifth = Tot_env_100g_scaled_lower_fifth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_lower_tenth = Tot_env_100g_scaled_lower_tenth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_lower_twentyfifth = Tot_env_100g_scaled_lower_twentyfifth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_upper_seventyfifth = Tot_env_100g_scaled_upper_seventyfifth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_upper_ninety = Tot_env_100g_scaled_upper_ninety / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled_upper_ninetyfifth = Tot_env_100g_scaled_upper_ninetyfifth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
         Tot_env_100g_scaled = Tot_env_100g_scaled / max(Tot_env_100g_scaled, na.rm = TRUE) * 100)


dat <-
  dat %>%
  group_by(product_name) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled),
                   NutriScore_Scaled = mean(NutriScore_Scaled),
                   Tot_env_100g_scaled_min = mean(Tot_env_100g_scaled_min),
                   Tot_env_100g_scaled_max = mean(Tot_env_100g_scaled_max),
                   Tot_env_100g_scaled_lower_ci = mean(Tot_env_100g_scaled_lower_ci),
                   Tot_env_100g_scaled_upper_ci = mean(Tot_env_100g_scaled_upper_ci),
                   Tot_env_100g_scaled_lower_fifth = mean(Tot_env_100g_scaled_lower_fifth),
                   Tot_env_100g_scaled_lower_tenth = mean(Tot_env_100g_scaled_lower_tenth),
                   Tot_env_100g_scaled_lower_twentyfifth = mean(Tot_env_100g_scaled_lower_twentyfifth),
                   Tot_env_100g_scaled_upper_seventyfifth = mean(Tot_env_100g_scaled_upper_seventyfifth),
                   Tot_env_100g_scaled_upper_ninety = mean(Tot_env_100g_scaled_upper_ninety),
                   Tot_env_100g_scaled_upper_ninetyfifth = mean(Tot_env_100g_scaled_upper_ninetyfifth)) %>%
  mutate(Tot_env_100g_scaled_lower_sd = Tot_env_100g_scaled - ((Tot_env_100g_scaled - Tot_env_100g_scaled_lower_ci) * sqrt(1000)) / 1.96,
         Tot_env_100g_scaled_upper_sd = Tot_env_100g_scaled + ((Tot_env_100g_scaled + Tot_env_100g_scaled_upper_ci) * sqrt(1000)) / 1.96)


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
    tmp.dat <- tmp.dat[,names(stacked.dat)]
    stacked.dat = rbind(stacked.dat, tmp.dat)
  } else if(length(names(tmp.dat)) == length(names(stacked.dat))) {
    stacked.dat = rbind(stacked.dat, tmp.dat)
  }
  
}

# Unique values only
stacked.dat <- unique(stacked.dat)

# Getting products with more than one ingredient
stacked.dat.sum <-
  stacked.dat %>%
  group_by(id, product_name, Department, Aisle, Shelf) %>%
  dplyr::summarise(count = n()) %>%
  as.data.frame(.) %>%
  group_by(product_name) %>%
  dplyr::summarise(count = min(count))

# And filtering
stacked.dat <-
  stacked.dat %>%
  filter(product_name %in% stacked.dat.sum$product_name[stacked.dat.sum$count > 1])

# Sausages----
saus.dat <-
  stacked.dat %>%
  # filter(percent < 100) %>%
  filter(grepl('sausage',product_name, ignore.case = TRUE)) %>%
  dplyr::select(product_name) %>% unique(.) %>%
  filter(!grepl("baked Bean|spaghetti|pasta|roll|mash|triple|casserole|turkey breast stuffed with|muffin|egg|pizza|
                kettle & apple|tortelloni|tortellini|heinz|soup|stew", product_name, ignore.case = TRUE))

# And labelling which of these are vegan/veg/chicken/fish/pork/beef
type.dat <-
  stacked.dat %>%
  filter(product_name %in% saus.dat$product_name) %>%
  filter(grepl('Bovine Meat|Cheese|Fish|Crustaceans|Pig Meat|Poultry Meat|Lamb|Egg',Food_Category)) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf, Food_Category) %>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm = TRUE)) %>%
  as.data.frame(.) %>% unique(.) %>%
  mutate(Food_Category = ifelse(grepl('Bovine|Lamb', Food_Category), 'Ruminant',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('Fish|Crustacean',Food_Category),'Seafood',Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Pig Meat'),'Pork',Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Poultry Meat', 'Poultry', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Eggs','Cheese'),'Vegetarian',Food_Category)) %>%
  group_by(product_name, Food_Category) %>%
  dplyr::summarise(percent = mean(percent, na.rm = TRUE))

# Adding percent composition of major types to the info
saus.dat <-
  left_join(saus.dat, type.dat %>% filter(Food_Category %in% 'Ruminant') %>% dplyr::select(product_name, percent_rum = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Pork') %>% dplyr::select(product_name, percent_pork = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Poultry') %>% dplyr::select(product_name, percent_poultry = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Seafood') %>% dplyr::select(product_name, percent_fish = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Vegetarian') %>% dplyr::select(product_name, percent_vege = percent)) %>%
  mutate(percent_rum = ifelse(is.na(percent_rum), 0, percent_rum),
         percent_pork = ifelse(is.na(percent_pork), 0, percent_pork),
         percent_poultry = ifelse(is.na(percent_poultry), 0, percent_poultry),
         percent_fish = ifelse(is.na(percent_fish), 0, percent_fish),
         percent_vege = ifelse(is.na(percent_vege), 0, percent_vege)) %>%
  mutate(sausage_type = ifelse(percent_rum + percent_pork + percent_poultry + percent_fish + percent_vege == 0, 'Vegan', NA)) %>%
  mutate(sausage_type = ifelse(percent_rum + percent_pork + percent_poultry + percent_fish == 0 & is.na(sausage_type), 'Vegetarian', sausage_type)) %>%
  mutate(sausage_type = ifelse(percent_rum > percent_pork & percent_rum > percent_poultry & percent_rum > percent_fish & is.na(sausage_type), 'Ruminant', sausage_type)) %>%
  mutate(sausage_type = ifelse(percent_pork > percent_rum & percent_pork > percent_poultry & percent_pork > percent_fish  & is.na(sausage_type), 'Pork', sausage_type)) %>%
  mutate(sausage_type = ifelse(percent_poultry > percent_rum & percent_poultry > percent_pork & percent_poultry > percent_fish  & is.na(sausage_type), 'Poultry', sausage_type)) %>%
  mutate(sausage_type = ifelse(percent_fish > percent_rum & percent_fish > percent_poultry & percent_fish > percent_pork  & is.na(sausage_type), 'Fish', sausage_type)) %>%
  mutate(check = percent_rum + percent_pork + percent_poultry + percent_fish) %>%
  filter(check >= 25 | (sausage_type %in% c('Vegetarian','Vegan'))) %>%
  filter(sausage_type != 'Fish')


# And adding back in env and nutrition info
saus.dat <-
  left_join(saus.dat,
            dat %>% dplyr::select(product_name, NutriScore_Scaled, 
                                  Tot_env_100g_scaled,Tot_env_100g_scaled_min,Tot_env_100g_scaled_max,
                                  Tot_env_100g_scaled_lower_ci,Tot_env_100g_scaled_upper_ci,
                                  Tot_env_100g_scaled_lower_fifth, Tot_env_100g_scaled_lower_tenth, Tot_env_100g_scaled_lower_twentyfifth,
                                  Tot_env_100g_scaled_upper_seventyfifth,Tot_env_100g_scaled_upper_ninety,Tot_env_100g_scaled_upper_ninetyfifth,
                                  Tot_env_100g_scaled_lower_sd,Tot_env_100g_scaled_upper_sd)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) %>%
  mutate(Tot_env_100g_scaled = ifelse(sausage_type %in% c('Vegan','Vegetarian'), Tot_env_100g_scaled_upper_ninetyfifth, Tot_env_100g_scaled_lower_fifth))

# Plotting

model = lm(saus.dat$Tot_env_100g_scaled ~ saus.dat$sausage_type)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'saus.dat$sausage_type', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)


# Repeating for pesto ----
pesto.dat <-
  stacked.dat %>%
  filter(percent < 100) %>%
  filter(grepl('pesto',product_name, ignore.case = TRUE)) %>%
  dplyr::select(product_name) %>% unique(.) %>%
  filter(!grepl("dough|base|sauce|yeast|allinson's|mix|baguette|flour|bread sticks|hot & spicy chicken|fried chicken g|arancini bites|mozzarella sticks|potato wedges|john crabbie's|drink|lemonade|appletiser|coleslaw|
                dip|elderflower|\\bcoke\\b|coca-cola|zero sugar|\\bcola\\b|steak pie|mushroom pie|cheese burger|garlic slices|pizza bread|chicken bites|cooked chips|orangeade|chicken goujons|white rolls|garlic puree|
                tortelloni|lasagne|cappelletti|penne 300g|gnocchi|penne twinpack|fusilli|tagliatelle|ravioli|ciabatta|linguine|spaghetti|linguine|breadsticks|seeded garlic flatbread|garlic tear & share|
                mezzelune|garlic bread|hot dog|garlic flatbread|garlic rustic wheel|flatbread|soup|vegetables & grains|bolognese|beef lasagne|macaroni cheese|fettucini|moussaka|risotta|pasta bake|tagliatelle|canneloni|
                penne|sicilian veg one pot|mushroom carbonara|tart frozen|tart flambee|baking tray|daal|Broccoletti Mezzelune|Mezzelune|piri piri|grated hard cheese|canelloni|canneloni|cannelloni|Finest Lamb, Rosemary & Garlic|
                whirls|lattice|grains|panini|fresh ideas chicken|escalopes|pizza|bruschetta|bites|houmous|antipasti|chicken tray bake|risotto|salad|pesto butter|quiche|swirls|parmesan pasta|chicken pesto pasta|pesto escalope|
                tortelloni|quiche|tray bake|salad|sandwich|melts|roasting tray|chicken fillet|pasta with spinach|dressing|british lamb|vegetarian mozzarella|chicken pesto breast|semi dried tomato|salmon fillet|whirl|palmier|white wine mustard|
                focaccia|pasta with|chicken with|Tortelloni|breasts with|quinoa|Focaccia|spinach pasta|grissini", product_name, ignore.case = TRUE))

# And labelling which of these are vegan/veg/chicken/fish/pork/beef
type.dat <-
  stacked.dat %>%
  filter(product_name %in% pesto.dat$product_name) %>%
  filter(grepl('Bovine Meat|Cheese|Fish|Crustaceans|Pig Meat|Poultry Meat|Lamb|Milk|Nuts||Butter, Cream & Ghee',Food_Category)) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf, Food_Category) %>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm = TRUE)) %>%
  as.data.frame(.) %>% unique(.) %>%
  mutate(Food_Category = ifelse(grepl('Bovine|Lamb', Food_Category), 'Ruminant',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('Fish|Crustacean',Food_Category),'Seafood',Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Milk','Cheese',"Butter, Cream & Ghee"),'Dairy',Food_Category)) %>%
  group_by(product_name, Food_Category) %>%
  dplyr::summarise(percent = mean(percent, na.rm = TRUE))

# Adding percent chocolate to the info
pesto.dat <-
  left_join(pesto.dat, type.dat %>% filter(Food_Category %in% 'Dairy') %>% dplyr::select(product_name, percent_cheese = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Pig Meat') %>% dplyr::select(product_name, percent_pork = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Seafood') %>% dplyr::select(product_name, percent_fish = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Nuts') %>% dplyr::select(product_name, percent_nuts = percent)) %>%
  mutate(pesto_type = ifelse(!is.na(percent_fish) | !is.na(percent_pork), 'Not Vegan',
                             ifelse(!is.na(percent_cheese),'Not Vegan', 'Vegan'))) %>%
  mutate(percent_fish = ifelse(is.na(percent_fish),0,percent_fish)) %>%
  mutate(percent_cheese = ifelse(is.na(percent_cheese),0,percent_cheese)) %>%
  mutate(percent_pork = ifelse(is.na(percent_pork),0,percent_pork)) %>%
  mutate(percent_nuts = ifelse(is.na(percent_nuts),0,percent_nuts)) %>%
  mutate(contains_nuts = ifelse(percent_nuts > 0,'Contains Nuts','Does Not Contain Nuts'))

# And adding back in env and nutrition info
pesto.dat <-
  left_join(pesto.dat,
            dat %>% dplyr::select(product_name, NutriScore_Scaled, 
                                  Tot_env_100g_scaled,Tot_env_100g_scaled_min,Tot_env_100g_scaled_max,
                                  Tot_env_100g_scaled_lower_ci,Tot_env_100g_scaled_upper_ci,
                                  Tot_env_100g_scaled_lower_fifth, Tot_env_100g_scaled_lower_tenth, Tot_env_100g_scaled_lower_twentyfifth,
                                  Tot_env_100g_scaled_upper_seventyfifth,Tot_env_100g_scaled_upper_ninety,Tot_env_100g_scaled_upper_ninetyfifth,
                                  Tot_env_100g_scaled_lower_sd,Tot_env_100g_scaled_upper_sd)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) 
  

pesto.dat$plot_group <- paste(pesto.dat$pesto_type, pesto.dat$contains_nuts)

pesto.dat <-
  pesto.dat %>%
  mutate(Tot_env_100g_scaled = ifelse(contains_nuts %in% 'Contains Nuts',
                                      Tot_env_100g_scaled_lower_twentyfifth, Tot_env_100g_scaled_upper_seventyfifth))

model = lm(pesto.dat$Tot_env_100g_scaled ~ pesto.dat$plot_group)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'pesto.dat$plot_group', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)


# T-test on nutrition. Inconsistent effects from the Tukey test
t.test(pesto.dat$NutriScore_Scaled[pesto.dat$pesto_type %in% 'Not Vegan'], pesto.dat$NutriScore_Scaled[pesto.dat$pesto_type %in% 'Vegan'])

pesto.sum <-
  pesto.dat %>%
  group_by(pesto_type, contains_nuts) %>% 
  dplyr::summarise(env = mean(Tot_env_100g_scaled),
                   nut = mean(NutriScore_Scaled))


# Repeating for lasagne ----
lasagne.dat <-
  stacked.dat %>%
  filter(percent < 100) %>%
  filter(grepl('lasagne|lasagna',product_name, ignore.case = TRUE)) %>%
  dplyr::select(product_name) %>% unique(.) %>%
  filter(!grepl("barilla|noodle|sauce|sheet|mix for lasagne|recipe mix|lasagne mix|lasagna mix|meal kit", product_name, ignore.case = TRUE))

# And labelling which of these are vegan/veg/chicken/fish/pork/beef
type.dat <-
  stacked.dat %>%
  filter(product_name %in% lasagne.dat$product_name) %>%
  filter(grepl('Bovine Meat|Cheese|Fish|Crustaceans|Pig Meat|Poultry Meat|Lamb|Milk|Nuts|Butter, Cream & Ghee|Egg',Food_Category)) %>%
  group_by(id, product_name, Retailer, Department, Aisle, Shelf, Food_Category) %>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm = TRUE)) %>%
  as.data.frame(.) %>% unique(.) %>%
  mutate(Food_Category = ifelse(grepl('Bovine|Lamb', Food_Category), 'Ruminant',Food_Category)) %>%
  mutate(Food_Category = ifelse(grepl('Fish|Crustacean',Food_Category),'Seafood',Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Milk','Cheese','Butter, Cream & Ghee','Eggs'),'Dairy',Food_Category)) %>%
  group_by(product_name, Food_Category) %>%
  dplyr::summarise(percent = mean(percent, na.rm = TRUE))

# Adding percent chocolate to the info
lasagne.dat <-
  left_join(lasagne.dat, type.dat %>% filter(Food_Category %in% 'Dairy') %>% dplyr::select(product_name, percent_cheese = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Pig Meat') %>% dplyr::select(product_name, percent_pork = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Seafood') %>% dplyr::select(product_name, percent_fish = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Poultry Meat') %>% dplyr::select(product_name, percent_poultry = percent)) %>%
  left_join(., type.dat %>% filter(Food_Category %in% 'Ruminant') %>% dplyr::select(product_name, percent_ruminant = percent)) %>%
  mutate(percent_pork = ifelse(is.na(percent_pork),0,percent_pork)) %>%
  mutate(percent_poultry = ifelse(is.na(percent_poultry),0,percent_poultry)) %>%
  mutate(percent_ruminant = ifelse(is.na(percent_ruminant),0,percent_ruminant)) %>%
  mutate(percent_cheese = ifelse(is.na(percent_cheese),0,percent_cheese)) %>%
  mutate(percent_fish = ifelse(is.na(percent_fish),0,percent_fish)) %>%
  mutate(lasagne_type = NA) %>%
  mutate(lasagne_type = ifelse(percent_ruminant + percent_poultry + percent_pork + percent_fish+percent_cheese == 0, 'Vegan',NA)) %>%
  mutate(lasagne_type = ifelse(percent_ruminant + percent_poultry + percent_pork + percent_fish == 0 & is.na(lasagne_type),'Vegetarian', lasagne_type)) %>%
  mutate(lasagne_type = ifelse(percent_ruminant > percent_poultry & percent_ruminant > percent_pork & is.na(lasagne_type),'Ruminant',lasagne_type)) %>%
  mutate(lasagne_type = ifelse(percent_poultry > percent_pork & is.na(lasagne_type), 'Poultry',lasagne_type)) %>%
  mutate(lasagne_type = ifelse(is.na(lasagne_type),'Pork',lasagne_type)) %>%
  mutate(percent_fish = ifelse(is.na(percent_fish),0,percent_fish)) %>%
  mutate(percent_cheese = ifelse(is.na(percent_cheese),0,percent_cheese)) %>%
  mutate(percent_pork = ifelse(is.na(percent_pork),0,percent_pork)) %>%
  mutate(percent_poultry = ifelse(is.na(percent_poultry),0,percent_poultry)) %>%
  mutate(percent_ruminant = ifelse(is.na(percent_ruminant),0,percent_ruminant))

# And adding back in env and nutrition info
lasagne.dat <-
  left_join(lasagne.dat,
            dat %>% dplyr::select(product_name, NutriScore_Scaled, 
                                  Tot_env_100g_scaled,Tot_env_100g_scaled_min,Tot_env_100g_scaled_max,
                                  Tot_env_100g_scaled_lower_ci,Tot_env_100g_scaled_upper_ci,
                                  Tot_env_100g_scaled_lower_fifth, Tot_env_100g_scaled_lower_tenth, Tot_env_100g_scaled_lower_twentyfifth,
                                  Tot_env_100g_scaled_upper_seventyfifth,Tot_env_100g_scaled_upper_ninety,Tot_env_100g_scaled_upper_ninetyfifth,
                                  Tot_env_100g_scaled_lower_sd,Tot_env_100g_scaled_upper_sd)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.)

lasagne.dat <-
  lasagne.dat %>%
  mutate(Tot_env_100g_scaled = ifelse(lasagne_type %in% c('Vegan','Vegetarian'),
                                      Tot_env_100g_scaled_upper_ninetyfifth, Tot_env_100g_scaled_lower_fifth))

# Tukey test for env impacts
model = lm(lasagne.dat$Tot_env_100g_scaled ~ lasagne.dat$lasagne_type)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'lasagne.dat$lasagne_type', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

lasagne.sum <-
  lasagne.dat %>%
  group_by(lasagne_type) %>%
  dplyr::summarise(env = mean(Tot_env_100g_scaled),
                   nut = mean(NutriScore_Scaled))

###
# Cookies
# Cookie plots ----
cookie.dat <-
  stacked.dat %>%
  filter(percent < 100) %>%
  filter(Aisle %in% c('Bakery Counter','Biscuits & Cereal Bars','Bakery','Bakery Free From','Baking, Desserts & Spreads','Biscuits','Biscuits & Chocolate',
                      'Cookies & Biscuits','Doughnuts, Muffins & Cookies','Desserts & pastry','Free From','Free From Range','Free From Bakery','Freefrom',
                      'From our Bakery')) %>%
  filter(grepl('cookie|biscuit',product_name, ignore.case = TRUE)) %>%
  filter(!grepl('breakfast',product_name, ignore.case = TRUE)) %>%
  filter(!grepl('cheese|cheddar|christmas|hallo|halo',product_name,ignore.case=TRUE)) %>%
  dplyr::select(product_name) %>% unique(.)

# And labelling which of these are vegan/veg/chicken/fish/pork/beef
type.dat <-
  stacked.dat %>%
  filter(product_name %in% cookie.dat$product_name) %>%
  filter(grepl('Choc',Food_Category)) %>%
  group_by(id, product_name, Food_Category) %>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm = TRUE)) %>%
  as.data.frame(.) %>% unique(.) %>%
  group_by(product_name, Food_Category) %>%
  dplyr::summarise(percent = mean(percent, na.rm = TRUE))

# Adding percent chocolate to the info
cookie.dat <-
  left_join(cookie.dat, type.dat) %>%
  mutate(Chocolate = ifelse(is.na(percent),'No Chocolate','Chocolate'))

# And adding back in env and nutrition info
cookie.dat <-
  left_join(cookie.dat,
            dat %>% dplyr::select(product_name, NutriScore_Scaled, 
                        Tot_env_100g_scaled,Tot_env_100g_scaled_min,Tot_env_100g_scaled_max,
                        Tot_env_100g_scaled_lower_ci,Tot_env_100g_scaled_upper_ci,
                        Tot_env_100g_scaled_lower_fifth, Tot_env_100g_scaled_lower_tenth, Tot_env_100g_scaled_lower_twentyfifth,
                        Tot_env_100g_scaled_upper_seventyfifth,Tot_env_100g_scaled_upper_ninety,Tot_env_100g_scaled_upper_ninetyfifth,
                        Tot_env_100g_scaled_lower_sd,Tot_env_100g_scaled_upper_sd)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) %>%
  transform(Chocolate = factor(Chocolate, levels = (c('Chocolate','No Chocolate'))))

model = lm(cookie.dat$Tot_env_100g_scaled ~ cookie.dat$Chocolate)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'cookie.dat$Chocolate', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)


cookie.dat <-
  cookie.dat %>%
  mutate(Tot_env_100g_scaled = ifelse(Chocolate %in% c('No Chocolate'),
                                      Tot_env_100g_scaled, Tot_env_100g_scaled_lower_tenth))

t.test(cookie.dat$Tot_env_100g_scaled[cookie.dat$Chocolate %in% 'Chocolate'],
       cookie.dat$Tot_env_100g_scaled[cookie.dat$Chocolate %in% 'No Chocolate'])


# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','sausage_type')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- saus.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(saus.dat))]
  names(tmp.df)[3] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  transform(sausage_type = factor(sausage_type, levels = c('Vegan','Vegetarian','Poultry','Pork','Ruminant'))) 
  
  
# Plotting
saus.plot <-
  ggplot(out.df1, aes(x = sausage_type, y = impact, fill = sausage_type, alpha = impact_type)) +
  geom_boxplot() +
  theme_classic() +
  # annotate(geom = 'text',label = 'Sausages', x = .5, y = 1) +
  # scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  # scale_y_continuous(breaks = c(0,25,50,75)) +
  scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  # scale_colour_manual(values = ) +
  scale_fill_manual(values = my.palette[c(4,3,8,2,7)]) +
  # geom_hline(yintercept = 0, linetype = 2) +
  # geom_hline(yintercept = 25, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  # geom_hline(yintercept = 75, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type', title = 'Sausages') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

saus.plot


# For pesto
# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','pesto_type','contains_nuts')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- pesto.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(pesto.dat))]
  names(tmp.df)[4] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  mutate(pesto_type = paste(pesto_type,'\\n',contains_nuts))


# Plotting
pesto.plot <-
  ggplot(out.df1, aes(x = pesto_type, y = impact, fill = pesto_type, alpha = impact_type)) +
  geom_boxplot() +
  theme_classic() +
  # scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  scale_x_discrete(labels = c('Not Vegan, Contains Nuts','Not Vegan, No Nuts','Vegan, Contains Nuts','Vegan, No Nuts')) +
  scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  scale_fill_manual(values = rev(my.palette[c(4,3,8,7)]),
                    labels = c('Minimum Observed','5th Percentile','10th Percentile','25th Percentile','Mean Impact',
                               '75th Percentile','90th Percentile', '95th Percentile','Maximum Observed')) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_hline(yintercept = 10, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type',title =  'Pesto Sauces') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

pesto.plot


# For Lasagne
# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','lasagne_type')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- lasagne.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(lasagne.dat))]
  names(tmp.df)[3] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  transform(lasagne_type = factor(lasagne_type, levels = c('Vegan','Vegetarian','Poultry','Pork','Ruminant')))



# Plotting
lasagne.plot <-
  ggplot(out.df1, aes(x = lasagne_type, y = impact, fill = lasagne_type, alpha = impact_type)) +
  geom_boxplot() +
  theme_classic() +
  # scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  # scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  scale_fill_manual(values = my.palette[c(4,3,8,2,7)]) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_hline(yintercept = 10, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type',title = 'Lasagne') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

lasagne.plot



# For cookies
# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','Chocolate')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- cookie.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(cookie.dat))]
  names(tmp.df)[3] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  transform(Chocolate = factor(Chocolate, levels = c('No Chocolate','Chocolate')))



# Plotting
cookie.plot <-
  ggplot(out.df1, aes(x = Chocolate, y = impact, fill = Chocolate, alpha = impact_type)) +
  geom_boxplot() +
  theme_classic() +
  # scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  # scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  scale_fill_manual(values = my.palette[c(6,2)]) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_hline(yintercept = 10, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type',title = 'Cookies and Biscuits') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

cookie.plot



# Sticking together and saving
out.plot <-
  plot_grid(saus.plot, pesto.plot,
            lasagne.plot, cookie.plot,
            nrow = 2, ncol = 2, align = 'hv')
out.plot
Sys.sleep(5)
# ggsave(paste0(getwd(),'/Figures/Figure Sx Variation in Impacts of Sausages, Pesto, Lasagne, Cookies Linear.pdf'),
#               width = 35, height = 15, units = 'cm')

# And now logged versions





# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','sausage_type')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- saus.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(saus.dat))]
  names(tmp.df)[3] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  transform(sausage_type = factor(sausage_type, levels = c('Vegan','Vegetarian','Poultry','Pork','Ruminant'))) 


# Plotting
saus.plot <-
  ggplot(out.df1, aes(x = sausage_type, y = impact, fill = sausage_type, alpha = impact_type)) +
  stat_summary(position = dodge) +
  theme_classic() +
  # annotate(geom = 'text',label = 'Sausages', x = .5, y = 1) +
  scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  # scale_y_continuous(breaks = c(0,25,50,75)) +
  scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  # scale_colour_manual(values = ) +
  scale_colour_manual(values = my.palette[c(4,3,8,2,7)]) +
  # geom_hline(yintercept = 0, linetype = 2) +
  # geom_hline(yintercept = 25, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  # geom_hline(yintercept = 75, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type', title = 'Sausages') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

saus.plot

write.csv(out.df1 %>%
            dplyr::select(sausage_type, impact, impact_type),
          paste0(getwd(),'/Figure Data/Fig S19 Sausages.csv'))


# For pesto
# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','pesto_type','contains_nuts')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- pesto.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(pesto.dat))]
  names(tmp.df)[4] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  mutate(pesto_type = paste(pesto_type,'\\n',contains_nuts))


# Plotting
pesto.plot <-
  ggplot(out.df1, aes(x = pesto_type, y = impact, fill = pesto_type, alpha = impact_type)) +
  # geom_boxplot() +
  stat_summary(position = dodge) +
  theme_classic() +
  scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  scale_x_discrete(labels = c('Not Vegan, Contains Nuts','Not Vegan, No Nuts','Vegan, Contains Nuts','Vegan, No Nuts')) +
  scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  scale_fill_manual(values = rev(my.palette[c(4,3,8,7)]),
                    labels = c('Minimum Observed','5th Percentile','10th Percentile','25th Percentile','Mean Impact',
                               '75th Percentile','90th Percentile', '95th Percentile','Maximum Observed')) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_hline(yintercept = 10, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type', title =  'Pesto Sauces') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

pesto.plot


# For Lasagne
# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','lasagne_type')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- lasagne.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(lasagne.dat))]
  names(tmp.df)[3] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  transform(lasagne_type = factor(lasagne_type, levels = c('Vegan','Vegetarian','Poultry','Pork','Ruminant')))



# Plotting
lasagne.plot <-
  ggplot(out.df1, aes(x = lasagne_type, y = impact, fill = lasagne_type, alpha = impact_type)) +
  # geom_boxplot() +
  stat_summary(position = dodge) +
  theme_classic() +
  scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  # scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  scale_fill_manual(values = my.palette[c(4,3,8,2,7)]) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_hline(yintercept = 10, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type',title = 'Lasagne') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

lasagne.plot



# For cookies
# Making a big data set to plot all of these
col.order <- c('min','_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth','max')
cols.keep <- c('product_name','Chocolate')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- cookie.dat[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(cookie.dat))]
  names(tmp.df)[3] <- 'impact'
  out.df <-
    rbind(out.df,
          tmp.df %>% mutate(impact_type = gsub("_","",i)))
}
out.df1 <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  transform(Chocolate = factor(Chocolate, levels = c('No Chocolate','Chocolate')))



# Plotting
cookie.plot <-
  ggplot(out.df1, aes(x = Chocolate, y = impact, fill = Chocolate, alpha = impact_type)) +
  # geom_boxplot() +
  stat_summary(position = dodge) +
  theme_classic() +
  scale_y_continuous(trans = 'log10', breaks = c(.1,1,10,50)) +
  # scale_colour_manual(values = grey.colors(length(unique(out.df1$impact_type)))) +
  scale_fill_manual(values = my.palette[c(6,2)]) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_hline(yintercept = 10, linetype = 2) +
  # geom_hline(yintercept = 50, linetype = 2) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL, fill = 'Impact Type',title = 'Cookies and Biscuits') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(plot.title = element_text(hjust = .5, size = 7))

cookie.plot



# Sticking together and saving
out.plot <-
  plot_grid(saus.plot, pesto.plot,
            lasagne.plot, cookie.plot,
            nrow = 2, ncol = 2, align = 'hv')
out.plot
Sys.sleep(5)
# ggsave(paste0(getwd(),'/Figures/Figure S18 Variation in Impacts of Sausages, Pesto, Lasagne, Cookies Log.pdf'),
#        width = 35, height = 15, units = 'cm')

