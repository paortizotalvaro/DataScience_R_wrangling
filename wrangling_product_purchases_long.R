################################################################
# This function ...

# # To work properly this function require:
# 1. refine_original.csv
### Arguments:
#       : 
#       :    
#
### Return: 
#
#       : 
#
#
# Author: Paula Andrea Ortiz Otalvaro
# Created:  Feb 2019
# Last modified:   14-03-2019
#
################################################################

# ********************* Load packages *********************
library(dplyr)
library(tidyr)

# *********************** Load data ***********************
purchases <- read.csv("refine_original.csv", sep=";", stringsAsFactors = FALSE)
names(purchases)[2] <- "product_code_number"
names(purchases)[1] <- "company"

# **************** 1. Clean up brand names *******************
purchases_clean <- mutate(purchases, company = tolower(company))

misspell_phil <- c('philips', 'fillips', 'phlips','phillps', 'phllips')
misspell_unil <- c('unilver')
misspell_akzo <- c('ak zo', 'akz0')

purchases_clean <- mutate(purchases_clean, 
                          company = lapply(company, function(x) if(x %in% misspell_phil) x='phillips' else x)  )

purchases_clean <- mutate(purchases_clean, 
                          company = lapply(company, function(x) if(x %in% misspell_unil) x='unilever' else x)  )

purchases_clean <- mutate(purchases_clean, 
                          company = lapply(company, function(x) if(x %in% misspell_akzo) x='akzo' else x)  )


# ********** 2. Separate product code and number **********
purchases_clean <- separate(purchases_clean, product_code_number, c("product_code", "product_number"), sep = "-")

# ********** 3. Add product categories **********

get_product_category <- function(prcode){
  if(prcode == "p") {prcode="Smartphone"}
  else if(prcode == "v") {prcode="TV"}
  else if(prcode == "x") {prcode = "Laptop"}
  else if(prcode == "q") {prcode = "Tablet"}
  prcode
}

#purchases_clean <- group_by(purchases_clean, product_code)

purchases_clean <- mutate( purchases_clean, product_category = lapply(product_code, get_product_category) )

# ********** 4. Add full address **********
purchases_clean <- mutate( purchases_clean, full_address = paste(address, city, country, sep = ", ") )



# ********** Create dummy **********
# USE MUTATE

purchases_clean <- fastDummies::dummy_cols(purchases_clean, select_columns = "company")

colnames(purchases_clean)[colnames(purchases_clean)=='company_van houten'] <- "company_van_houten"

purchases_clean <- fastDummies::dummy_cols(purchases_clean, select_columns = "product_category")
