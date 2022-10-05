###
# Fig 3
# Impacts by Aisle for Tesco

# libraries
library(plyr)
library(dplyr)
library(ggplot2)
# library(factoextra)
# library(NbClust)
library(readr)
library(cowplot)

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

# data
plot.dat <- read_csv(paste0(getwd(),"/Managed_Data/Impacts by Product 18Feb2022 Log2.csv"))

plot.dat <-
  plot.dat %>%
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

# Aggregating
plot.dat.aisle <-
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
  # dplyr::select(product_name, id, Retailer, Aisle, Tot_env_100g_scaled, Tot_env_100g_scaled_lower_ci, Tot_env_100g_scaled_upper_ci, NutriScore_Scaled,NutriScoreLetter) %>%
  .[complete.cases(.),] %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE)) %>%
  unique(.) %>%
  mutate(NutriScore_Scaled = ifelse(NutriScoreLetter %in% 'A',1,
                                    ifelse(NutriScoreLetter %in% 'B',2,
                                           ifelse(NutriScoreLetter %in% 'C',3,
                                                  ifelse(NutriScoreLetter %in% 'D',4,5)))))


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


# Clustering analysis if you want to do this
# cluster.df <- plot.dat.aisle[,grepl('mean_[^nut]',names(plot.dat.aisle))]
# cluster.df <- scale(cluster.df)
# # Getting optimal number of clusters
# fviz_nbclust(cluster.df, FUNcluster = kmeans)
# tmp <- kmeans(cluster.df, 8)
# plot.dat.aisle$cluster <- tmp$cluster

# Adding product type
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


# Stacking data
# For cookies
# Making a big data set to plot all of these
col.order <- c('_fifth','_tenth','_twentyfifth','scaled','_seventyfifth','_ninety','_ninetyfifth')
cols.keep <- c('Retailer','Department','Aisle','Label','Food_Type','plot_colour')
out.df <- data.frame()
for(i in col.order) {
  tmp.df <- plot.dat.aisle[,grepl(paste0(c(cols.keep,paste0(i,'\\b')), collapse = '|'),names(plot.dat.aisle))]
  tmp.df <- tmp.df[!grepl('serving',names(tmp.df))]
  names(tmp.df)[!(names(tmp.df) %in% cols.keep)] <- 'impact'
  tmp.df$impact_type <- gsub("_","",i)
  if(i %in% col.order[1]) {
    out.df <- tmp.df
  } else {
    out.df <-
      rbind(out.df,
            tmp.df[,names(out.df)])
  }
}
out.df <-
  out.df %>%
  transform(impact_type = factor(impact_type, levels = gsub("_","",col.order))) %>%
  mutate(Label = ifelse(Label %in% ' ',Aisle,Label))

out.plot <-
  ggplot(out.df, aes(x = Label, y = impact, fill = Food_Type, alpha = impact_type)) +
  theme_classic() +
  # stat_summary() +
  geom_boxplot() +
  scale_fill_manual(values = c('#0072b2',# beverages
                               '#e69f00', # cereals
                               '#000000', # cooking accessories
                               '#ed2939', # animal source foods
                               '#cc79a7', # desserts
                               '#009e73', # fruit veg and nuts
                               '#d55e00', # prepared foods
                               '#b79268')) + # snacks
  facet_wrap(.~Food_Type, scales = 'free',ncol=2) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) #+
  # scale_y_continuous(trans = 'log10')

# Sys.sleep(15)
out.plot

# ggsave(paste0(getwd(),'/Figures/Figure S13 Variation in Impacts by Tesco Aisle 6Feb2022.pdf'),
#        width = 25, height = 50, units = 'cm')

# And saving data
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}

write.csv(out.df %>% unique(.),
          paste0(getwd(),'/Figure Data/Data Fig S13 Variation in Impacts by Retail Aisle 6Feb2022.csv'),row.names = FALSE)


