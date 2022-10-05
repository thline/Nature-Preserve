###
# S Figure - Impacts by single food ingredient by indicator

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


# Getting number of ingredients
dat.count <-
  stacked.dat %>%
  mutate(count_col = paste0(id, Retailer, Department, Aisle, Shelf, product_name)) %>%
  dplyr::group_by(count_col) %>%
  dplyr::summarise(count_ingredients = n()) %>%
  filter(count_ingredients %in% 1)

# Filtering full data set
dat.plot <-
  dat %>%
  filter(paste0(id, Retailer, Department, Aisle, Shelf, product_name) %in% dat.count$count_col) %>%
  dplyr::group_by(product_name) %>%
  dplyr::summarise(ghgs = mean(mean_GHG),
                   land = mean(mean_Land),
                   watscar = mean(mean_WatScar),
                   eut = mean(mean_Eut),
                   Tot_env_100g_scaled = mean(Tot_env_100g_scaled))

# Merging in food category
dat.plot <-
  left_join(dat.plot,
            stacked.dat %>% 
              dplyr::select(product_name, Food_Category, Food_Category_sub, Food_Category_sub_sub) %>%
              unique())

# Making sure they're all the same food category...
dat.plot.count <-
  dat.plot %>%
  dplyr::group_by(product_name) %>%
  dplyr::summarise(count = n()) %>%
  filter(count %in% 1)

# And removing the ones that aren't all in the same food category
dat.plot <-
  dat.plot %>%
  filter(product_name %in% dat.plot.count$product_name)

# Summarising by food group
dat.plot <-
  dat.plot %>%
  dplyr::group_by(Food_Category, Food_Category_sub, Food_Category_sub_sub) %>%
  dplyr::summarise(mean_tot = mean(Tot_env_100g_scaled),
                   mean_ghgs = mean(ghgs),
                   mean_land = mean(land),
                   mean_watscar = mean(watscar),
                   mean_eut = mean(eut),
                   se_tot = sd(Tot_env_100g_scaled) / sqrt(n()),
                   se_ghgs = sd(ghgs) / sqrt(n()),
                   se_land = sd(land) / sqrt(n()),
                   se_watscar = sd(watscar) / sqrt(n()),
                   se_eut = sd(eut) / sqrt(n()))


# Stacking by indicator
dat.stacked <- data.frame()

for(i in c('tot','ghgs','land','watscar','eut')) {
  dat.tmp <-
    dat.plot[,c(grep('Food_Category',names(dat.plot)),
                grep(i,names(dat.plot)))]
  names(dat.tmp)[4:5] <- c('mean_impact','se_impact')
  dat.tmp$indicator <- i
  dat.stacked <- rbind(dat.stacked,dat.tmp)
}

# Sorting
dat.stacked <-
  dat.stacked %>%
  arrange(Food_Category, Food_Category_sub, Food_Category_sub_sub) %>%
  mutate(plot_cat = Food_Category_sub_sub) %>%
  mutate(plot_cat = ifelse(is.na(plot_cat), Food_Category_sub,plot_cat)) %>%
  mutate(plot_cat = ifelse(is.na(plot_cat), Food_Category,plot_cat)) %>%
  mutate(indicator = ifelse(indicator %in% 'tot','Total Environmental Impact Score',indicator)) %>% 
  mutate(indicator = ifelse(indicator %in% 'ghgs','GHG Emissions (kg CO2e)',indicator)) %>% 
  mutate(indicator = ifelse(indicator %in% 'eut','Eutrophication Potential (g PO4e)',indicator)) %>%
  mutate(indicator = ifelse(indicator %in% 'land','Land Use (sq. meters)',indicator)) %>%
  mutate(indicator = ifelse(indicator %in% 'watscar','Scarcity Weighted Water Use (L)',indicator))
  
# Setting plot levels for the environmental indicators
dat.stacked <-
  dat.stacked %>%
  transform(indicator = factor(indicator, 
                               levels = c('Total Environmental Impact Score',
                                  'GHG Emissions (kg CO2e)',
                                  'Land Use (sq. meters)',
                                  'Eutrophication Potential (g PO4e)',
                                  'Scarcity Weighted Water Use (L)'))) %>%
  mutate(plot_cat = ifelse(plot_cat %in% 'Other farmed fish', 'Other fish', plot_cat)) %>%
  mutate(plot_cat = ifelse(plot_cat %in% 'Other Crustaceans (farmed)', 'Other crustaceans', plot_cat))

# Setting order of plotting for the food groups
dat.tot <-
  dat.stacked %>%
  filter(grepl('Total',indicator)) %>%
  arrange(mean_impact) %>%
  mutate(plot_cat = ifelse(plot_cat %in% 'Other farmed fish', 'Other fish', plot_cat)) %>%
  mutate(plot_cat = ifelse(plot_cat %in% 'Other Crustaceans (farmed)', 'Other crustaceans', plot_cat))
  

dat.stacked <-
  dat.stacked %>%
  transform(plot_cat = factor(plot_cat,
                              levels = dat.tot$plot_cat))

# Adding plot colour
dat.stacked <-
  dat.stacked %>%
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
  mutate(colour = ifelse(grepl('Barley|Wheat|Maize', Food_Category),'#009244',colour)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Dark Chocolate', 'Cocoa', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Pig Meat', 'Pork', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Fish (farmed)', 'Fish', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Crustaceans (farmed)', 'Crustaceans', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Other farmed fish', 'Other fish', Food_Category)) %>%
  mutate(Food_Category = ifelse(Food_Category %in% 'Other Crustaceans (farmed)', 'Other crustaceans', Food_Category))

dat.stacked <-
  dat.stacked %>%
  arrange(indicator, plot_cat) %>%
  filter(!grepl('Water|Wine|Beer', plot_cat, ignore.case = TRUE))

# And plotting
ggplot(dat.stacked %>% filter(!grepl('Water|Wine|Beer',plot_cat)), aes(x = plot_cat, y = mean_impact)) +
  theme_classic() +
  geom_point(colour = dat.stacked$colour[!grepl('Water|Wine|Beer',dat.stacked$plot_cat)]) +
  geom_errorbar(aes(ymin = mean_impact - se_impact * 1.96,
                    ymax = mean_impact + se_impact * 1.96),
                colour = dat.stacked$colour) +
  facet_wrap(.~indicator, ncol = 1, scales = 'free') +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) +
  labs(y = 'Environmental Impact (per 100g)')
  # scale_y_continuous(trans = 'log10')

ggsave(paste0(getwd(),'/Figures/Figure S17 Impacts by Single Ingredient Food 6Feb2022.pdf'),
       width = 15, height = 25)

# Saving data
write.csv(dat.stacked,
          paste0(getwd(),'/Figure Data/Data Figure S17 Impacts by Single Ingredient Food 6Feb2022.csv'))
