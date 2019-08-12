################################################################
# This script contains a function that cleans a toy data set (with
# product purchases from an electronics store)

# # To work properly this function require:
#   refine_original.csv
### Arguments:
#   purchases_originaldf : data frame with purchases information
#                          (company, Product code / number, address, city, country, name)
#
### Return: 
#   purchases_cleandf : purchases_originadf after being cleaned and modified
#
#
# Author: Paula Andrea Ortiz Otalvaro
# Created:  August 2019
# Last modified:   12-08-2019
#
################################################################

# --------------------------------------------------------------
#                        LOAD PACKAGES
# --------------------------------------------------------------
library(dplyr)
library(tidyr)

# --------------------------------------------------------------
#                    DEFINE GLOBAL VARIABLES
# --------------------------------------------------------------
misspell_phil <- c('phillips', 'fillips', 'phlips','phillps', 'phllips')
misspell_unil <- c('unilver')
misspell_akzo <- c('ak zo', 'akz0')


# --------------------------------------------------------------
#                        FUNCTIONS
# --------------------------------------------------------------

# ********* Get product category from product code *************
get_product_category <- function(prcode){
  if(prcode == "p") {prcode="Smartphone"}
  else if(prcode == "v") {prcode="TV"}
  else if(prcode == "x") {prcode = "Laptop"}
  else if(prcode == "q") {prcode = "Tablet"}
  prcode
}


# *********************** Wrangling function ***********************

clean_purchases_df <- function(purchases_originaldf){
    
    purchases_cleandf <- purchases_originaldf %>% 
                       # ********** 1. Clean up brand names *******************
                       mutate(company = tolower(company)) %>%
                       mutate(company = lapply(company, function(x) if(x %in% misspell_phil) x='philips' else x)) %>% 
                       mutate(company = lapply(company, function(x) if(x %in% misspell_unil) x='unilever' else x)) %>%
                       mutate(company = lapply(company, function(x) if(x %in% misspell_akzo) x='akzo' else x)) %>%
    
                       # ********** 2. Separate code and number *****************  
                       separate(col = "product_code_number", into = c("product_code", "product_number"), sep = "-") %>%
    
                       # ********** 3. Add product categories **********
                       mutate(product_category = lapply(product_code, get_product_category) ) %>%
    
                       # ********** 4. Add full address **********
                       mutate(full_address = paste(address, city, country, sep = ", ") ) %>%
    
                       # ********** 5. Create dummy variables **********
                       fastDummies::dummy_cols(select_columns = "company") %>% 
                       fastDummies::dummy_cols(select_columns = "product_category")
    
    
    # *********************** Rename columns to requested names ***********************
    colnames(purchases_clean)[colnames(purchases_clean)=='company_van houten'] <- "company_van_houten"
    
    # *********************** Write cleaned dataframe to file ***********************
    data.table::fwrite(purchases_cleandf, file = "refine_clean.csv" )
    
    
    return(purchases_cleandf)
}


# --------------------------------------------------------------
#             CALL FUNCTION ON TOY DATA SET
# --------------------------------------------------------------

# *********************** Load data ***********************
purchases <- read.csv("refine_original.csv", sep=";", stringsAsFactors = FALSE)
names(purchases)[1] <- "company"
names(purchases)[2] <- "product_code_number"


# *********************** Wrangle data ***********************
purchases_cleaned <- clean_purchases_df(purchases)
