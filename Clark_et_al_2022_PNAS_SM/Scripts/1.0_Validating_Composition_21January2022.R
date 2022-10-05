#######
# General purpose of this code is to 
# (a) identify individual ingredients within the food products captured by food db
# (b) identify percent composition of the ingredients that have these listed on back of pack labeling
# (c) categorize these ingredients into ~40 categories that pair with Joseph Poore's LCA database
# (d) Repeat part (c) for product names in case products do no have any listed ingredients, or do not have any ingredients with listed % composition
# (e) Merge with LCA database
# (f) Interpolate % composition for ingredients that do not have a % composition listed 
# (g) Estimate total impacts per 100g 
#######

### Setting working directory
#setwd("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Runs 19April2021")
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

###
# Loading functions
#source(paste0(getwd(),"/Scripts/0.0 Functions File Estimating Percent Composition 22January2022.R"))
source(paste0(getwd(),'/Scripts/0.0_Functions_Estimating_Composition_22January2022.R'))

###
# Importing libraries
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(reshape2)
library(dismo)
library(gtools)

#####
###
# Importing and managing the data set
product.dat <- 
  read_csv(paste0(getwd(),"/foodDB_dat/products.csv"))

# Salt info, used to set max composition of salt in the product
salt.dat <-
  product.dat %>%
  dplyr::select(product_id, salt_per_100) %>%
  mutate(unit = gsub("\\.","",salt_per_100)) %>%
  mutate(unit = gsub(".*[0-9]{1,}","",unit)) %>%
  mutate(salt_amount_g = str_extract(salt_per_100,"[0-9]{1,}\\.[0-9]{1,}")) %>%
  mutate(salt_amount_g = ifelse(is.na(salt_amount_g),str_extract(salt_per_100,"[0-9]{1,}"), salt_amount_g)) %>% 
  mutate(salt_amount_g = as.numeric(salt_amount_g)) %>%
  mutate(salt_amount_g = ifelse(grepl('mg',unit,ignore.case = TRUE),salt_amount_g/1000,salt_amount_g)) %>%
  mutate(salt_amount_g = ifelse(!grepl('mg|g',unit,ignore.case=TRUE),NA,salt_amount_g)) %>% # be careful with this - this works in our data set, but you might need to be more specific in yours
  filter(!is.na(salt_amount_g)) %>%
  group_by(product_id) %>%
  dplyr::summarise(salt_amount_g = mean(salt_amount_g)) %>%
  mutate(unit = 'g')

# View(product.dat[grepl("[0-9]{1,3}(\\s)?g.*per(\\s)?[0-9]{1,3}(\\s)?g",product.dat$ingredients_text),])

# Categories
cat.dat <- 
  read_csv(paste0(getwd(),"/foodDB_dat/categories.csv")) %>%
  dplyr::select(-shelf) %>%
  dplyr::rename(shelf = aisle) %>%
  dplyr::rename(aisle = department) %>%
  dplyr::rename(department = main_category)

# Merging
# Need to do this in two steps
dat <-
  left_join(product.dat, 
            cat.dat %>% dplyr::select(product_id, department, aisle, shelf)) %>%
  unique(.)

# Removing rows where ingredient text == \\N 
# This indicates products that don't contain an ingredients list
# i.e. often "6 Braeburn Apples" or "1kg potatoes"
# Will estimate the impact of these products later
dat <-
  dat %>%
  filter(!ingredients_text %in% c('\\N','NULL')) %>%
  filter(!is.na(ingredients_text))
  
# Creating supermarket department
# You will need to update this based on your data sample
# Commented out because retailer info already provided
dat <-
  dat %>%
  mutate(Retailer = ifelse(grepl("www.tesco.ie",url,ignore.case = TRUE), 'Tesco_Ireland',
                           ifelse(grepl('www.tesco',url, ignore.case = TRUE), 'Tesco',
                                  ifelse(grepl('www.Sainsbury',url,ignore.case=TRUE),'Sainsbury',
                                         ifelse(grepl('www.Ocado',url,ignore.case=TRUE),'Ocado',
                                                ifelse(grepl('groceries.Morrison',url,ignore.case=TRUE),'Morissons',
                                                       ifelse(grepl('www.Waitrose',url,ignore.case=TRUE),'Waitrose',
                                                              ifelse(grepl('www.Iceland',url,ignore.case=TRUE),'Iceland',
                                                                     ifelse(grepl('groceries.asda',url,ignore.case=TRUE),'Asda',
                                                                            ifelse(grepl('www.Cookfood',url,ignore.case=TRUE),'Cook',NA))))))))))


# Limiting data frame to select columns
# And select products (ones which we can categorize)
# This does not necessarily need to be done, but working on data frames with excess columns annoys me more than it should
dat <- 
  dplyr::select(dat, # Limiting to select columns
                id = product_id,
                product_name,
                ingredients_text,
                Retailer, Department = department, Aisle = aisle, Shelf = shelf) %>%
  filter(!is.na(Retailer)) %>% # Limiting to products which we can identify retailers
  unique(.)

# Changing semi colons to commas in the ingredient text
# Most products separate individual ingredients by commas, but some use semi colons
# Doing this to be able to run the same code on everything
dat$ingredients_text <-
  gsub(";",
       ",",
       dat$ingredients_text)

# Updating "percent" to %
# Doing this to be able to identify percent composition of ingredients later in the text
dat$ingredients_text <-
  gsub("per(\\s)?cent",
       "%",
       dat$ingredients_text, ignore.case = TRUE)

# Allergen advice - this does not indicate any ingredient
dat$ingredients_text <- 
  gsub("Allergy Advice: For allergens see highlighted ingredients",
       "",
       dat$ingredients_text)

###
# Converting brackets to parentheses to mesh with rest of script
dat$ingredients_text <-
  gsub("\\[|\\{", "(", dat$ingredients_text, perl = TRUE)
dat$ingredients_text <-
  gsub("\\]|\\}", ")", dat$ingredients_text, perl = TRUE)

# Managing search words
# This is used to categorize products into lca and nutritional categories
search.words = 
  read.csv(paste0(getwd(),"/Data Inputs/Search words 6April2020 try2.csv"), stringsAsFactors = FALSE) %>%
  mutate(count = NA) %>% # Creating count column
  cbind(., # Parsing search words
        str_split_fixed(.$Search_Words,"\\|",n = 500))

# Converting search words to character strings
for(i in which(names(search.words) %in% '1') : which(names(search.words) %in% '500')) {
  search.words[,i] <- as.character(search.words[,i])
}

# And search terms to go from the main category to the sub category
search.words.sub <- 
  read.csv(paste0(getwd(),'/Data Inputs/Search words, second round, 22Jan2022.csv'), stringsAsFactors = FALSE) %>%
  filter(!(LCA_sub_category %in% '') | !(LCA_sub_sub_category %in% '')) %>% # Getting rid of sub categories without enough observations
  arrange(Search_order, Search_order_sub)
search.words.sub <-
  left_join(search.words.sub,
            search.words.sub %>% dplyr::group_by(LCA_Category) %>% 
              dplyr::summarise(max_cat_sub = max(Search_order, na.rm=TRUE))) %>% # Identifying max search category by main category - assigning all other foods to this category
  left_join(.,
            search.words.sub %>% filter(!is.na(Search_order_sub)) %>% dplyr::group_by(LCA_Category,LCA_sub_category) %>% 
              dplyr::summarise(max_cat_sub_sub = max(Search_order_sub, na.rm=TRUE)))


# Getting rid of everything after "ALLERGEN ADVICE:" and "MAY CONTAIN"
# dat.whole$ingredients_text <- 
#   gsub("ALLERGY ADVICE.*","",dat.whole$ingredients_text, ignore.case = TRUE)
# dat.whole$ingredients_text <- 
#   gsub("ALLERGEN ADVICE.*","",dat.whole$ingredients_text, ignore.case = TRUE)
# dat.whole$ingredients_text <- 
#   gsub("MAY CONTAIN.*","",dat.whole$ingredients_text, ignore.case = TRUE)

#####
###
# Doing this in a series of loops for each supermarket and then department
# Where unknown data is supplemented by known data within the same department
# Number of loops needed to process the data frame
# This is identified based on the number of departments
# Saving dat before starting this
dat.whole = dat 

# List of retailers to loop through
retailers = sort(unique(dat.whole$Retailer))

# Setting seed for replicability
set.seed(19)

# and looping across retailers
for(retail in retailers) {
  # Getting dat limited to the retailer
  dat.retailer = dat.whole %>% filter(Retailer %in% retail)
  
  # Getting loops in department
  loops = length(unique(dat.retailer$Department))
  
  # Departments
  departments = sort(unique(dat.retailer$Department))
  
  
  # Setting max number of embedded strings to extract from ingredients list for each product
  # Increase or decrease to extract more or less
  extract_embedded = 10
  
  # Empty list used to store products that can't be estimated
  filter.out.ids = c()
  
  # Looping through deparments
  # for(z in 1:1) {
  for(z in 1:loops) {
    
    # Managing search words
    # This is used to categorize products into lca and nutritional categories
    search.words = 
      read.csv(paste0(getwd(),"/Data Inputs/Search words 6April2020 try2.csv"), stringsAsFactors = FALSE) %>%
      mutate(count = NA) %>% # Creating count column
      cbind(., # Parsing search words
            str_split_fixed(.$Search_Words,"\\|",n = 500))
    
    # Converting search words to character strings
    for(i in which(names(search.words) %in% '1') : which(names(search.words) %in% '500')) {
      search.words[,i] <- as.character(search.words[,i])
    }
    
    dat = dat.retailer[dat.retailer$Department %in% departments[z],]
    
    rows.loop = which(dat$ingredients_text != '')
    ###
    # Running function to get embedded ingredients list
    dat = 
      embedded.function(dat = dat %>% as.data.frame(), # Data set
                        num_embedded = extract_embedded, # Max number of embedded ingredients list to extract for each product
                        ingredient_list = 'ingredients_text', # Name of column containing the ingredients list
                        rows.loop = rows.loop,  # Which rows from which to extract embedded ingredients lists
                        embedded = 'keep') # Keep embedded string
    
    
    
    ###
    # Splitting into multiple columns
    # Doing first for initial ingredients list
    # But will need to do later for each of the substrings
    # Keeping first 50 ingredients
    dat <-
      cbind(dat,
            str_split_fixed(dat$ingredients_text_parsed, # Splitting the parsed text
                            ",", # Based on commas
                            n = 50) %>% as.data.frame())
    
    # The above code creates leveled factors of the individual ingredients
    # We don't want this, because it can do weird things in R
    # Now converting these columns to character, because these ended up getting converted to factors earlier
    # Note that this preserves the digits/text within each column, but removes teh association to underlying numeric values (levels) that unique strings might have
    cols.loop =
      which(names(dat) %in% 'V1') :
      which(names(dat) %in% 'V50')
    for(i in cols.loop) {
      dat[,i] <- as.character(dat[,i])
    }
    
    ###
    # This doesn't split perfectly (because of added or misplaced characters in the ingredient list)
    # So going back and updating these
    dat =
      corrected.split.function(dat = dat,
                               cols.loop = cols.loop)
    
    ###
    # And in some cases, this may still not split perfectly
    # This is largely because i.e. there are misplaced "(" or ")"
    # These are used to identify the embedded lists
    # But if these are misplaced (i.e. there are extra ones or not enough of them)
    # Then the embedded lists are not going to be identified perfectly
    # So...making a note of these products for data quality purposes later
    dat <-
      dat %>% 
      mutate(equal_parentheses = ifelse(str_count(dat$ingredients_text, "\\(") == str_count(dat$ingredients_text,"\\)"),
                                        'Equal',
                                        'Unequal'))
    
    
    ###
    # Converting from long to wide
    # Instead of having a data frame with 50ish columns, now have a data frame with a few columns but many more rows
    dat.long <-
      melt(dat[,c(which(names(dat) %in% 'id') : which(names(dat) %in% 'Shelf'),
                  which(names(dat) %in% 'V1') : which(names(dat) %in% 'V50'))],
           id = c('id','product_name','Retailer','Department','Aisle','Shelf')) %>%
      left_join(.,
                dat %>% dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf, equal_parentheses))
    
    # Getting rid of rows with no identified ingredient
    # In other words, if a product has 3 ingredients, there would be 47 rows for that product that contain empty text
    # Removing these rows
    dat.long <- 
      dat.long %>% 
      filter(value != "") %>% 
      filter(variable != 'ingredients_text')
    
    
    # Getting list of ingredients with xxxg per 100g product
    # Saving these for later
    dat.dat.tmp <-
      rbind(dat.long %>% filter(grepl("[0-9]{1,3}(\\s)?g(\\s)per(\\s)?[0-9]{1,3}", value, ignore.case = TRUE)),
            dat.long %>% filter(grepl("[0-9]{1,3}(\\s)?g(\\s)[^0-9]{1,}per(\\s)?[0-9]{1,3}", value, ignore.case = TRUE)))
    
    ###
    # Merging in embedded ingredients lists
    # Looping to merge these in
    for(i in which(names(dat) %in% 'ing_string1') : which(names(dat) %in% paste0('ing_string',extract_embedded))) {
      tmp.df <-
        dat[,c(which(names(dat) %in% 'id'),
               which(names(dat) %in% 'product_name'),
               which(names(dat) %in% 'Department'),
               which(names(dat) %in% 'Aisle'),
               which(names(dat) %in% 'Shelf'),
               i,i - extract_embedded)] %>%
        mutate(pasted_string = paste0('V',.[,grep("ing_string[0-9]{1,2}", names(.))])) %>%
        filter(pasted_string != 'VNA') %>%
        mutate(variable = pasted_string)
      
      
      if(length(-grep("\\b\\([A-z]*\\),\\b",tmp.df[,4])) > 0) {
        tmp.df <-
          tmp.df[-grep("\\b\\([A-z]*\\),\\b",tmp.df[,4]),]
      } else if(length(-grep("\\b\\([A-z]*\\),\\b",tmp.df[,4])) == 0) {
        tmp.df <- tmp.df
      }
      
      
      dat.long <-
        dat.long %>%
        left_join(.,
                  tmp.df[,c(which(names(tmp.df) %in% 'id'),
                            which(names(tmp.df) %in% 'product_name'),
                            which(names(tmp.df) %in% 'Department'),
                            which(names(tmp.df) %in% 'Aisle'),
                            which(names(tmp.df) %in% 'Shelf'),
                            which(names(tmp.df) %in% 'variable'),
                            grep('\\bstring[0-9]{1,2}',names(tmp.df)))])
      
    }
    
    ###
    # Collapsing these strings into a single column
    # First need to convert NAs to empty strings
    dat.long$embedded_ingredients = ''
    for(i in which(names(dat.long) %in% 'string1'): which(names(dat.long) %in% paste0('string',extract_embedded))) {
      dat.long[is.na(dat.long[,i]),i] <- ''
      dat.long$embedded_ingredients <- paste0(dat.long$embedded_ingredients, dat.long[,i])
    }
    
    ###
    # Sorting df
    dat.long$sort <- as.numeric(gsub("V","",dat.long$variable))
    dat.long <-
      dat.long[order(dat.long$id, dat.long$sort),]
    dat.long <- dat.long %>% dplyr::select(-sort)
    
    #####
    ###
    # Identifying % composition as identified in the ingredients list 
    # Removing commas in ingredient texts
    # Doing this helps match percentages later
    dat.long <- dat.long %>% mutate(id.new = paste0(id, Department, Aisle, Shelf))
    
    # removing values that are 0% fat
    # to avoid having the algorithm key in on these
    which.fat <-
      grep('%.*fat|%.*less|%.*reduced|%.*lower',dat.long$value,ignore.case=TRUE)
    # And removing
    dat.long[which.fat,'value'] <-
      gsub('%.*fat|%.*less|%.*reduced|%.*lower',"",dat.long[which.fat,'value'],ignore.case=TRUE)
    
    # getting % composition
    dat.long <-
      percent.composition.function(dat = dat.long,
                                   ingredient.col = 'value',
                                   variable.col = 'variable',
                                   id.var = 'id.new')
    
    # Getting list of products where every (or all but 1) percent composition is listed
    dat.all.listed <-
      dat.long %>%
      mutate(percent_listed = ifelse(!is.na(percent0) & percent0 > 0,1,0)) %>% # Dummy variable if percent listed
      mutate(embedded_ings = ifelse(embedded_ingredients %in% '',0,1)) %>% # Dummy variable for embedded ingredients
      group_by(product_name, Retailer, Department, Aisle, Shelf) %>%
      dplyr::summarise(n_ingredients = dplyr::n(), # Number of ingredients in ingredient
                       n_percent = sum(percent_listed),
                       n_embedded = sum(embedded_ings),
                       percent = sum(percent0, na.rm = TRUE)) %>% # Number of products with percent listed
      mutate(perfect_data = ifelse(n_ingredients - n_percent <= 1,'CHECK','DROP')) %>% # Creating dummy varaible to identify products to validate 
      mutate(perfect_data = ifelse(n_embedded >= 1,'DROP',perfect_data)) %>% # Making sure no embedded ingredients
      filter(perfect_data %in% 'CHECK') %>%
      filter(percent <= 105)
    
    # Saving file
    # Creating validation directory if it does not exist
    folders <- list.files(getwd())
    if(!("Validation" %in% folders)) {
      dir.create(paste0(getwd(),"/Validation"))
      dir.create(paste0(getwd(),"/Validation/EstimatedComposition"))
      dir.create(paste0(getwd(),"/Validation/ValidateProducts"))
    }
    
    # write.csv(dat.all.listed,
    #           paste0("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Validation/ValidateProducts/",
    #                  retail, "_",departments[z],'17August2020.csv'))
    
    if(nrow(dat.all.listed) >= 1) {
      
      # Updating percent composition in the "CHECK" products
      # if(nrow(dat.all.listed) >= 1) {
      #   dat.long <-
      #     dat.long %>%
      #     mutate(percent0 = ifelse(product_name %in% dat.all.listed$product_name, NA, percent0),
      #            percent_numeric = ifelse(product_name %in% dat.all.listed$product_name, NA, percent_numeric))
      # }
      
      
      # Filtering products without any percents identified
      filter.out =
        dat.long %>%
        filter(variable %in% c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15')) %>%
        mutate(count = ifelse(!is.na(percent0),1,0)) %>%
        group_by(id, product_name) %>%
        dplyr::summarise(count = sum(count)) %>%
        filter(count %in% 0)
      
      filter.out.ids = 
        c(filter.out.ids,
          filter.out$id)
      
      # And updating
      dat.long <-
        dat.long %>%
        # filter(!(id %in% filter.out$id)) %>%
        mutate(percent0 = gsub("%","", percent0)) %>% # Getting rid of % sign
        mutate(percent_0 = ifelse(str_count(embedded_ingredients,",") %in% c(0,0) # identifying percent of ingredients with
                                  & str_count(embedded_ingredients,"%") %in% c(1) # (a) a single comma in embedded ingredients, (b) a single percent in embedded ingredients
                                  & is.na(percent0), # and (c) no percent recognized
                                  str_extract(embedded_ingredients, "[0-9]{1,3}(\\s)?%|[0-9]{1,3}(\\.)[0-9]{1,2}(\\s)?%"),
                                  percent0)) %>% 
        mutate(percent_0 = gsub("%","",percent_0)) %>%
        mutate(percent0 = ifelse(is.na(percent0), percent_0,percent0)) %>%
        dplyr::select(-percent_0) %>%
        mutate(percent0 = as.numeric(percent0)) %>% # Changing to percent
        mutate(percent = percent0) %>% # Adding other columns to mesh with rest of code
        mutate(percent_updated = percent)# See above line
      
      #####
      ### 
      # Next chunk of code identifies words in the ingredient list (column = 'value') and assigns these to LCA food groups
      # Doing this based on a list of key words for each food group
      # These lists are not all-inclusive, and many ingredients
      # Later on, I manually go through the unmatched ingredients to pair them with food groups
      
      ###
      # And now removing a few characters to avoid complications in the code below
      # Removing parentheses to avoid issues matching ingredients
      dat.long <-
        dat.long %>%
        mutate(value = gsub("\\(","",value, perl = TRUE)) %>% # Getting rid of parentheses
        mutate(value = gsub("\\)","",value, perl = TRUE)) %>% # Getting rid of parentheses
        mutate(value = gsub("%","",value, perl = TRUE)) %>% # Getting rid of percent signs
        mutate(value = gsub("Vitamin(\\s)?(B)?[0-9]*","VitaminNUMBER",value,perl=TRUE,ignore.case=TRUE)) %>% # Converting vitamins to NUMBER (i.e. Vitamin B3 = Vitamin BNUMBER)
        mutate(value = gsub("[0-9]","",value,perl = TRUE)) %>% # getting rid of any remaining numbers
        mutate(value = gsub("[*]","",value, perl = TRUE)) %>% # getting rid of remaining asterisks
        mutate(value = trimws(value, which = 'left')) %>% # removing leading white space
        mutate(Food_Category = NA)
      
      # Removing leading white space
      # This also messes with grepl later
      dat.long$value <- trimws(dat.long$value, which = 'left')
      
      # And updating values for ingredients that are formatted such as cheese (milk)
      # The match ingredient function below would identify this as milk, but it should be identified as cheese
      # exceptions are for: (1) cheese; (2) butter, cream, & ghee; (3) plant based milk; and (4) tofu
      # Only doing this if there are no commas (cheese), one or fewer commas (others)
      
      # Columns containing embedded ingredients
      columns.loop <- paste0('string',1:10)
      
      # Looping across columns to update these values
      for(col.loop in columns.loop) {
        if(sum(!is.na(dat.long[,col.loop])) >=  1) {
          # Rows where this meets criteria for cheese
          which.index <- 
            which(grepl('cheese',dat.long$value,ignore.case=TRUE) & grepl('milk',dat.long[,col.loop], ignore.case = TRUE) & str_count(dat.long[,col.loop], ",") %in% 0)
          # Updating these values for cheese
          dat.long[which.index,col.loop] <- '(Cheese)'
          
          # All others where there's just (word)
          # These are normally allergens
          # So want to identify the main ingredient, not what is in the parentheses
          which.index <- 
            which(grepl('\\b([A-z]*)\\b',dat.long$value,ignore.case=TRUE) & !grepl('(Cheese)',dat.long[,col.loop], ignore.case = TRUE) & str_count(dat.long[,col.loop], ",") %in% 0 & dat.long[,col.loop] != '')
          # Updating these values for cheese
          dat.long[which.index,col.loop] <- dat.long[which.index,'value']
          
          
          # Rows for butter
          which.index <- 
            which(grepl('butter',dat.long$value,ignore.case=TRUE) & grepl('milk',dat.long[,col.loop], ignore.case = TRUE) & str_count(dat.long[,col.loop], ",") <= 3)
          # Updating these values for cheese
          dat.long[which.index,col.loop] <- '(Butter)'
          
          # Rows for tofu
          which.index <- 
            which(grepl('tofu',dat.long$value,ignore.case=TRUE) & grepl('soy',dat.long[,col.loop], ignore.case = TRUE) & str_count(dat.long[,col.loop], ",") <= 3)
          # Updating these values for cheese
          dat.long[which.index,col.loop] <- '(Tofu)'
          
          # Rows for soymilk
          which.index <- 
            which(grepl('soy.*milk|soy.*drink',dat.long$value,ignore.case=TRUE) & grepl('soy',dat.long[,col.loop], ignore.case = TRUE) & str_count(dat.long[,col.loop], ",") <= 3)
          # Updating these values for cheese
          dat.long[which.index,col.loop] <- '(Soymilk)'
        }
      }
      
      #####
      ### 
      # Identifying ingredients in the list
      # Doing this twice
      # First to get a count of each key word
      # Then second to sort into categories
      # Doing this because we want to identify the rarer ingredients first
      # Because this script identifies the first occurence a key word occurs, and does not overwrite with subsequent matches
      
      ###
      # For each ingredient, the logic is
      # if item has not already been sorted into a category, then search that ingredient for a key word, and update the lca food category with the approriate value
      # There are 40ish LCA categories
      # LCA categories are rather broad, so the list of search words for these categories can be quite long
      # Search goes from least common to most common
      dat.long <-
        match.ingredient.function(dat = dat.long,
                                  search.words = search.words)
      
      # Saving search words
      # This will be used to identify impacts 
      search.words.g.per.100g <- dat.long[[2]]
      
      dat.long <- dat.long[[1]]
      
      # Matching ingredients from the xxxg per 100g product
      if(nrow(dat.dat.tmp) > 0) {
        dat.dat.tmp <-
          match.ingredient.function.xxxg.per.100g(dat = dat.dat.tmp,
                                                  search.words = search.words.g.per.100g)
      }
      
      # Identifying salt
      # used in interpolating
      dat.long <-
        dat.long %>%
        mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('\\bsalt\\b', value, ignore.case = TRUE) & !is.na(value),'Salt',Food_Category)) %>%
        mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('water', value, ignore.case = TRUE) & !is.na(value), 'Water',Food_Category))
      
      ###
      # Updating percent for embedded ingredients if only a single percent
      # a single ingredient
      # and percent of embedded ingredient <= 100/n, where n = nth ingredient in ingredient list
      # then repeating if there is only a single embedded ingredient and food category is not identified
      dat.long <-
        dat.long %>%
        mutate(count_percent_embedded = str_count(embedded_ingredients, "%")) %>% # Count of % signs in embedded lists
        mutate(count_comma_embedded = str_count(embedded_ingredients, ",")) %>% # Count of commas in embedded lists
        mutate(percent_embedded = ifelse(count_percent_embedded %in% 1 & count_comma_embedded %in% 0 & is.na(percent0), # Only identify percent embedded if 1 percent and 0 commas
                                         str_extract(embedded_ingredients,"[0-9]{1,3}(\\s)?%|[0-9]{1,3}(\\.)[0-9]{1,2}(\\s)?%"),
                                         NA)) %>%
        mutate(percent_embedded = ifelse(percent_embedded > 100 / as.numeric(gsub("V","",variable)), # Embedded percent cannot be > 100/n
                                         NA,
                                         percent_embedded)) %>%
        mutate(percent0 = ifelse(is.na(percent0) & !is.na(percent_embedded), # Updating percent0
                                 percent_embedded, 
                                 percent0)) 
      
      ###
      # Repeating search for embedded ingredients if:
      # (a) primary ingredient not identified, and
      # (b) no commas in embedded ingredients  
      
      # A bit of data formatting before this
      dat.long <-
        dat.long %>%
        mutate(value = ifelse(is.na(Food_Category) &
                                !is.na(embedded_ingredients) &
                                embedded_ingredients != '' &
                                count_comma_embedded %in% 0,
                              embedded_ingredients,
                              value))
      
      # And repeating for these updated values
      dat.long <-
        match.ingredient.function(dat = dat.long,
                                  search.words = search.words)
      
      dat.long <- dat.long[[1]]
      
      # Identifying salt and water
      # used in interpolating
      dat.long <-
        dat.long %>%
        mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('\\bsalt\\b', value, ignore.case = TRUE) & !is.na(value),'Salt',Food_Category)) %>%
        mutate(Food_Category = ifelse(is.na(Food_Category) & grepl('water', value, ignore.case = TRUE) & !is.na(value), 'Water',Food_Category))
      
      ###
      # Subcategorising the food categories
      # E.g. going from 'brassicas' to 'broccoli','cauliflower', and a few other categories
      dat.long <- match.ingredient.function.subcategories(dat.long, search.words.sub, embedded = 'no')
      
      # saving the data frame for interpolating composition of other ingredients
      dat.percent.save <- dat.long
      
      ###
      # Getting list of ingredients that do not match based on the above search words
      dat.check <- 
        dat.long %>% 
        mutate(count = 1) %>% 
        filter(is.na(Food_Category)) %>% 
        group_by(value) %>% 
        dplyr::summarise(count = sum(count))
      
      
      #####
      ###
      # Next chunk of script gets list of products that didn't have any matched ingredients
      # In other words, this is very similar to the above few hundred lines of code
      # But runs on products, rather than ingredients
      # Searches through the product names of these products
      # And tries to sort these products into the LCA food categories
      
      ###
      # Getting products that don't have any listed food categories
      # Getting products without any matched ingredients
      # Or where matched ingredients have a summed percent composition of 0
      # First setting percent composition of ingredients with a single item to 100
      dat.long <-
        left_join(dat.long,
                  dat.long %>% mutate(count = 1) %>% group_by(id, product_name, Department, Aisle, Shelf) %>% dplyr::summarise(n_ingredients = sum(count))) %>%
        mutate(percent0 = ifelse(n_ingredients %in% 1, 100, percent0)) %>%
        mutate(percent0 = gsub("%","",percent0)) %>%
        mutate(percent0 = as.numeric(percent0))
      
      dat.long <-
        left_join(dat.long,
                  dat.long %>% group_by(id, product_name, Department, Aisle, Shelf) %>% dplyr::summarise(tot_percent = sum(percent0, na.rm = TRUE)))
      
      
      
      test1 <- 
        dat.long %>%
        mutate(count = 1) %>%
        mutate(count_variable = 1) %>%
        mutate(count = ifelse(is.na(Food_Category),0,count)) %>%
        mutate(percent_updated = gsub("%","",percent0)) %>%
        mutate(percent_updated = as.numeric(percent_updated)) %>%
        group_by(id) %>% 
        dplyr::summarise(count = sum(count,na.rm=TRUE), count_variable = sum(count_variable, na.rm = TRUE), percent = sum(percent_updated,na.rm=TRUE))
      
      #####
      ###
      # Now interpolating estimates of a food's environmental footprint
      # This function works in a series of steps, assuming:
      # (1) Total composition sums to 100, and
      # (2) Products listed in order of composition
      
      # Steps are: 
      # (1) Identify known composition (done above)
      # (2) Use this known composition to estimate unknown composition of products within the same shelf, aisle (if shelf not possible), and department (if aisle not possible)
      # (3) While keeping known compositions constant, update the unknown compositions such that assumptions (1) and (2) above remain true
      
      # Few notes on this:
      # (1) Some products do not list water. This is problematic for things like soup.
      # (2) Some products have more than 100g ingredients per 100g finished product.
      # Ketchup (tomatoes in it) is a good example of this. Composition of these ingredients are identified and updated below
      
      # Estimating composition of unknown ingredients
      dat.long <- 
        dat.long %>%
        filter(!grepl("SEE UNDERLINED INGREDIENT IN BOLD",value, ignore.case = TRUE)) %>%
        filter(!grepl("Allergen advice",value, ignore.case = TRUE)) %>%
        filter(!grepl("Allergy advice",value, ignore.case = TRUE)) %>%
        filter(!grepl("May contain",value, ignore.case = TRUE)) %>%
        filter(!grepl("Manufactured",value, ignore.case = TRUE)) 
      
      # Getting data for products where all info is known
      dat.validate <-
        dat.long %>%
        filter(product_name %in% dat.all.listed$product_name) %>%
        unique(.) %>%
        filter(n_ingredients > 1)
      
      # Dropping products with embedded ingredient lists
      has.embedded <-
        dat.validate %>%
        mutate(has.embedded = ifelse(embedded_ingredients == '', 0,1)) %>%
        group_by(id, product_name, Retailer, Department, Aisle, Shelf) %>%
        dplyr::summarise(count_embedded = dplyr::n()) %>%
        filter(count_embedded > 0)
      
      # Don't need to filter, because I've done this above
      # dat.validate <- 
      #   dat.validate %>%
      #   filter(!(id %in% has.embedded$id))
      
      # Dropping products with xxx% per 100g
      dat.validate <-
        dat.validate %>%
        filter(!c(product_name %in% dat.dat.tmp$product_name))
      
      # Interpolating to validate
      if(nrow(dat.validate) >= 1) {
        
        # Solving to set percent composition for all ingredients
        t1 = Sys.time()
        # Splitting product ids into equal length list to parallelise
        # unique.foods <- unique(dat.validate$id.new)
        # chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
        # unique.foods.list <- chunk2(unique.foods, 3)
        # chunk.df <- function(food.list) {
        #   df.new <- dat.validate %>% filter(id.new %in% food.list)
        #   return(df.new)
        # }
        # dat.long.list = lapply(unique.foods.list, chunk.df)
        # 
        # And running in parallel
        # And rbinding back together
        dat.validate <- interpolate.food.ingredients.trial(dat.validate)
        Sys.time() - t1
        
        
        t1 = Sys.time()
        # Getting potential combinations of each product
        # Splitting for parallelization
        # unique.foods.list <- chunk2(unique.foods, 3)
        # dat.long.list = lapply(unique.foods.list, chunk.df)
        # Doing this in parallel to save time
        dat.validate <- 
          validate.combos.function(dat.validate) %>%
          mutate(id.new.old = id.new,
                 id.new = paste0(id, product_name, Department, Aisle, Shelf, variable_removed_validate, info_validate, id_validate)) %>%
          mutate(percent0 = percent,
                 percent_numeric = percent,
                 percent_updated = percent)
        # And estimating composition
        dat.ingredients.validate <- 
          interpolate.food.ingredients.trial(dat.validate)# %>%
        # filter(!(variable_removed_validate %in% 'none'))
        
        Sys.time() - t1
        
        ###
        # Saving data frame
        write.csv(dat.ingredients.validate %>% dplyr::select(id, product_name, Retailer, Department, Aisle, Shelf, variable, value, Food_Category, Food_Category_sub, Food_Category_sub_sub, n_ingredients, percent, 
                                                             id_validate, info_validate, variable_removed_validate, percent_known, n_percent_known,
                                                             embedded_ingredients),
                  paste0(getwd(),"/Validation/EstimatedComposition/",
                         retail,"_", departments[z],
                         "_18January2022.csv"),
                  row.names = FALSE)
        cat(retail, departments[z])
      }
    }
  }
}
