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
  filter(!is.na(NutriScore_Scaled)) %>%
  group_by(product_name) %>%
  dplyr::summarise(Tot_env_100g_scaled = mean(Tot_env_100g_scaled),
                   NutriScore_Scaled = mean(NutriScore_Scaled))


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
            dat %>% dplyr::select(product_name, NutriScore_Scaled, Tot_env_100g_scaled)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) 

# Plotting
saus.plot <-
  ggplot(saus.dat, aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled, colour = factor(sausage_type), group = product_name)) +
  theme_classic() +
  geom_point(size = 1.5, alpha = .75) +
  # scale_y_continuous(trans = 'log10', limits = c(0.05,65)) +
  scale_y_continuous(trans = 'log10', limits = c(.15,55)) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_shape_manual(values = c(19,3)) +
  # scale_colour_manual(values = my.palette[c(7,1,8,3,4)]) +
  scale_colour_manual(values = my.palette[c(2,8,7,4,3)]) +
  # scale_colour_manual(values = c('#332288','#117733','#999933')) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  # theme(legend.position = c(.8,.3)) 
  theme(legend.position = c(.75,.3),legend.direction = 'vertical') 

# (1) Ratio of 10th to 90th percentile for env and nut harm
(quantile(saus.dat$NutriScore_Scaled, .75) / quantile(saus.dat$NutriScore_Scaled, .25)) * 100
(quantile(saus.dat$Tot_env_100g_scaled, .75) / quantile(saus.dat$Tot_env_100g_scaled, .25)) * 100


model = lm(saus.dat$Tot_env_100g_scaled ~ saus.dat$sausage_type)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'saus.dat$sausage_type', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

tukey.env = TUKEY

model = lm(saus.dat$NutriScore_Scaled ~ saus.dat$sausage_type)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'saus.dat$sausage_type', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

tukey.nut = TUKEY

saus.sum <-
  saus.dat %>%
  group_by(sausage_type) %>%
  dplyr::summarise(nut = mean(NutriScore_Scaled),
                   env = mean(Tot_env_100g_scaled))

t.test(saus.dat$Tot_env_100g_scaled[saus.dat$sausage_type %in% 'Ruminant'],
       saus.dat$Tot_env_100g_scaled[saus.dat$sausage_type %in% 'Pork'])
t.test(saus.dat$Tot_env_100g_scaled[saus.dat$sausage_type %in% 'Pork'],
       saus.dat$Tot_env_100g_scaled[saus.dat$sausage_type %in% 'Poultry'])
t.test(saus.dat$Tot_env_100g_scaled[saus.dat$sausage_type %in% 'Poultry'],
       saus.dat$Tot_env_100g_scaled[saus.dat$sausage_type %in% c('Vegan','Vegetarian')])

t.test(saus.dat$NutriScore_Scaled[saus.dat$sausage_type %in% c('Ruminant','Pork')],
       saus.dat$NutriScore_Scaled[saus.dat$sausage_type %in% 'Poultry'])
t.test(saus.dat$NutriScore_Scaled[saus.dat$sausage_type %in% 'Poultry'],
       saus.dat$NutriScore_Scaled[saus.dat$sausage_type %in% c('Vegan','Vegetarian')])


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
            dat %>% dplyr::select(product_name, NutriScore_Scaled, Tot_env_100g_scaled)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) 

# Plotting
pesto.plot <-
  ggplot(pesto.dat, aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled, colour = factor(pesto_type), group = product_name)) +
  theme_classic() +
  geom_point(alpha = 1, size = 1, aes(shape = factor(contains_nuts))) +
  # scale_y_continuous(trans = 'log10', limits = c(0.05,65)) +
  scale_y_continuous(trans = 'log10', limits = c(.15,55)) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_shape_manual(values = c(19,17)) +
  scale_colour_manual(values = my.palette[c(2,6)]) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL, shape = NULL) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  # theme(legend.position = c(.8,.3)) 
  theme(legend.position = 'bottom') 

pesto.plot


# (1) Ratio of 10th to 90th percentile for env and nut harm
(quantile(pesto.dat$NutriScore_Scaled, .75) / quantile(pesto.dat$NutriScore_Scaled, .25)) * 100
(quantile(pesto.dat$Tot_env_100g_scaled, .75) / quantile(pesto.dat$Tot_env_100g_scaled, .25)) * 100

pesto.dat$plot_group <-
  paste(pesto.dat$pesto_type, pesto.dat$contains_nuts)
model = lm(pesto.dat$Tot_env_100g_scaled ~ pesto.dat$plot_group)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'pesto.dat$plot_group', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

pesto.dat$plot_group <-
  paste(pesto.dat$pesto_type, pesto.dat$contains_nuts)
model = lm(pesto.dat$NutriScore_Scaled ~ pesto.dat$plot_group)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'pesto.dat$plot_group', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

# T-test on nutrition. Inconsistent effects from the Tukey test
t.test(pesto.dat$NutriScore_Scaled[pesto.dat$pesto_type %in% 'Not Vegan'], pesto.dat$NutriScore_Scaled[pesto.dat$pesto_type %in% 'Vegan'])
t.test(pesto.dat$NutriScore_Scaled[pesto.dat$contains_nuts %in% 'Contains Nuts'], pesto.dat$NutriScore_Scaled[!(pesto.dat$contains_nuts %in% 'Contains Nuts')])

t.test(pesto.dat$Tot_env_100g_scaled[pesto.dat$pesto_type %in% 'Not Vegan'], pesto.dat$Tot_env_100g_scaled[pesto.dat$pesto_type %in% 'Vegan'])
t.test(pesto.dat$Tot_env_100g_scaled[pesto.dat$contains_nuts %in% 'Contains Nuts'], pesto.dat$Tot_env_100g_scaled[!(pesto.dat$contains_nuts %in% 'Contains Nuts')])

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
            dat %>% dplyr::select(product_name, NutriScore_Scaled, Tot_env_100g_scaled)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) %>%
  transform(lasagne_type = factor(lasagne_type, levels = (c('Ruminant','Pork','Poultry','Vegetarian','Vegan'))))

# Plotting ----
lasagne.plot <-
  ggplot(lasagne.dat, aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled, colour = factor(lasagne_type))) +
  theme_classic() +
  geom_point(alpha = 1, size = 1.5, shape = 19) +
  # scale_y_continuous(trans = 'log10', limits = c(0.05,65)) +
  scale_y_continuous(trans = 'log10', limits = c(.15,55)) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_colour_manual(values = my.palette[c(2,8,7,4,3)]) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(legend.position = 'bottom', legend.justification = c(0,1))
# theme(legend.position = c(.8,.15))

lasagne.plot

# Summary stats ----

# (1) Ratio of 10th to 90th percentile for env and nut harm
(quantile(lasagne.dat$NutriScore_Scaled, .75) / quantile(lasagne.dat$NutriScore_Scaled, .25)) * 100
(quantile(lasagne.dat$Tot_env_100g_scaled, .75) / quantile(lasagne.dat$Tot_env_100g_scaled, .25)) * 100

model = lm(lasagne.dat$Tot_env_100g_scaled ~ lasagne.dat$lasagne_type)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'lasagne.dat$lasagne_type', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

model = lm(lasagne.dat$NutriScore_Scaled ~ lasagne.dat$lasagne_type)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'lasagne.dat$lasagne_type', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

lasagne.sum <-
  lasagne.dat %>%
  group_by(lasagne_type) %>%
  dplyr::summarise(env = mean(Tot_env_100g_scaled),
                   nut = mean(NutriScore_Scaled))


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
            dat %>% dplyr::select(product_name, NutriScore_Scaled, Tot_env_100g_scaled)) %>%
  filter(!is.na(NutriScore_Scaled)) %>%
  filter(!is.na(Tot_env_100g_scaled)) %>%
  unique(.) %>%
  transform(Chocolate = factor(Chocolate, levels = (c('Chocolate','No Chocolate'))))

# Plotting ----
cookie.plot <-
  ggplot(cookie.dat, aes(x = NutriScore_Scaled, y = Tot_env_100g_scaled, colour = factor(Chocolate))) +
  theme_classic() +
  geom_point(alpha = 1, size = 1.5, shape = 19) +
  # scale_y_continuous(trans = 'log10', limits = c(0.05,65)) +
  scale_y_continuous(trans = 'log10', limits = c(.15,55)) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_colour_manual(values = my.palette[c(2,6)]) +
  labs(x = 'Nutrition Impact Score', y = 'Environmental Impact Score', colour = NULL) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(plot.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(legend.position = 'bottom', legend.justification = c(0,1))
# theme(legend.position = c(.8,.15))

cookie.plot

# Summary stats ----

# (1) Ratio of 10th to 90th percentile for env and nut harm
(quantile(cookie.dat$NutriScore_Scaled, .75) / quantile(cookie.dat$NutriScore_Scaled, .25)) * 100
(quantile(cookie.dat$Tot_env_100g_scaled, .75) / quantile(cookie.dat$Tot_env_100g_scaled, .25)) * 100

model = lm(cookie.dat$Tot_env_100g_scaled ~ cookie.dat$Chocolate)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'cookie.dat$Chocolate', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

model = lm(cookie.dat$NutriScore_Scaled ~ cookie.dat$Chocolate)
ANOVA = aov(model)
TUKEY = TukeyHSD(x = ANOVA, 'cookie.dat$Chocolate', conf.level = .95)
TUKEY
plot(TUKEY, cex = .25, cex.axis = .5, las = 1)

cookie.sum <-
  cookie.dat %>%
  group_by(Chocolate) %>%
  dplyr::summarise(env = mean(Tot_env_100g_scaled),
                   nut = mean(NutriScore_Scaled))

t.test(cookie.dat$Tot_env_100g_scaled[cookie.dat$Chocolate %in% 'Chocolate'],
       cookie.dat$Tot_env_100g_scaled[cookie.dat$Chocolate %in% 'No Chocolate'])

t.test(cookie.dat$NutriScore_Scaled[cookie.dat$Chocolate %in% 'Chocolate'],
       cookie.dat$NutriScore_Scaled[cookie.dat$Chocolate %in% 'No Chocolate'])


# And saving ----
saus.plot
Sys.sleep(10)
# ggsave(paste0(getwd(),"/Figures/Fig 6 Variation in sausages 6Feb2022.pdf"),
#        width = 10, height = 8, units = 'cm')


# Supplemental with pesto, lasagne, and biscuits
supp.fig <-
  plot_grid(pesto.plot, lasagne.plot, cookie.plot,
            nrow = 1, align = 'hv')

supp.fig
Sys.sleep(10)
# ggsave(paste0(getwd(),"/Figures/Fig 6 Variation in pesto, lasagna, cookies 6Feb2022.pdf"),
#        width = 18, height = 8, units = 'cm')

# And writing data
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}


write.csv(pesto.dat %>% dplyr::select(fig_id_num = product_name, contains_nuts, pesto_type, NutriScore_Scaled, Tot_env_100g_scaled) %>% mutate(fig_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig S18 Pesto Sauces 6Feb2022.csv'),row.names=FALSE)
write.csv(lasagne.dat %>% dplyr::select(fig_id_num = product_name, lasagne_type, NutriScore_Scaled, Tot_env_100g_scaled) %>% mutate(fig_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig S18 Lasagne 6Feb2022.csv'),row.names=FALSE)
write.csv(saus.dat %>% dplyr::select(fig_id_num = product_name, sausage_type, NutriScore_Scaled, Tot_env_100g_scaled) %>% mutate(fig_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig 6 Sausages 6Feb2022.csv'),row.names=FALSE)
write.csv(cookie.dat %>% dplyr::select(fig_id_num = product_name, Chocolate, NutriScore_Scaled, Tot_env_100g_scaled) %>% mutate(fig_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig S18 Cookies 6Feb2022.csv'),row.names=FALSE)
