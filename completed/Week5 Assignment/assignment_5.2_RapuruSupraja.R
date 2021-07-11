# Assignment: ASSIGNMENT 5.2
# Name: Supraja, Rapuru
# Date: 2021-07-10
#Analysis of housing data

## Load the readxl package
library(readxl)

## Load the dplyr package
library(dplyr)

## Load the purrr package
library(purrr)


## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

## Load the `data/acs-14-1yr-s0201.csv` to
housing_df <- read_excel("data/week-6-housing.xlsx")
str(housing_df)
head(housing_df)

#Rename the'Sale Date` and`Sale Price`
colnames(housing_df)[1] <- "Sale_Date"
colnames(housing_df)[2] <- "Sale_Price"

str(housing_df)

# a. Using the dplyr package, use the 6 different operations to analyze/transform 
# the data - GroupBy, Summarize, Mutate, Filter, Select, and Arrange - Remember 
# this isn't just modifying data, you are learning about your data also - so play 
# around and start to understand your dataset in more detail

#Getting mean sale price using group_by() and summarize() functions 
housing_df %>% group_by(zip5) %>% summarize("Avg_Sale_Price" = mean(Sale_Price))

#Getting mean sale price using group_by() and summarize() functions 
housing_df %>% group_by(zip5,ctyname) %>% summarize("Avg_Sale_Price" = mean(Sale_Price))

#Getting mean sale price using group_by() and summarize() functions 
housing_df %>% group_by(bedrooms) %>% summarize("Avg_Sale_Price" = mean(Sale_Price))

#Getting mean sale price using group_by() and summarize() functions 
housing_df %>% group_by(year_built) %>% summarize("Avg_Sale_Price" = mean(Sale_Price))

#Calculate sales_price_per_sqft using mutate() function
housing_df<-housing_df %>% mutate("sales_price_per_sqft"=square_feet_total_living/Sale_Price)

str(housing_df)

#Calculate sales_year using mutate() function
housing_df<-housing_df %>% mutate("sale_year"=substr(Sale_Date,1,4))
str(housing_df)

#Filter all 4-bedroom houses using filter() function
housing_df %>% filter(bedrooms==4)

#Filter all houses whose sale price < 500000 using filter() function
housing_df %>% filter(Sale_Price<500000)

#Filter all houses which are sold in 2006 and sale price is less than 500000 using filter() function
housing_df %>% filter(Sale_Price<500000& sale_year=='2006')


#Select Sale_Date, sale_price and zip from the dataset using select() function
housing_df %>% select(Sale_Date,Sale_Price,zip5)

#Select Sale_Date, sale_price and zip from the dataset for 11-bedroom house using filter() and select() function
housing_df %>% filter(bedrooms==11)%>% select(Sale_Date,Sale_Price,zip5) 

#Arrange the dataset based on sales price from high to low
housing_df %>% arrange(desc(Sale_Price)) 


# b.Using the purrr package - perform 2 functions on your dataset.  
# You could use zip_n, keep, discard, compact, etc.

#Using keep function list all the sales prices which are greater than 2000000
sales_price_gt_2m <-purrr::keep(housing_df$Sale_Price, ~ .x>2000000)

class(sales_price_gt_2m)
str(sales_price_gt_2m)

#Perform map function on the list to generate a list with sales price increased by 5%
sales_price_gt_2m %>% map(function(x) x*.05)

#Using discard function list all the sale year which are greater than 2000 
sale_year_gt_2000<-purrr::discard(housing_df$sale_year, ~ .x<2000) 
class(sale_year_gt_2000)
str(sale_year_gt_2000)
unique(sale_year_gt_2000)


# c.Use the cbind and rbind function on your dataset

#using cbind function add city_indicator
housing_df <-cbind(housing_df,city_indicator=!is.na(housing_df$ctyname))
str(housing_df)
housing_df %>% select(ctyname,city_indicator)


#Using rbind function to combine 2 dataframes
hs_sale_yr_bfr_2010<-housing_df %>%filter(sale_year<2010)
head(hs_sale_yr_bfr_2010)
hs_sale_yr_aftr_2010<-housing_df %>%filter(sale_year>=2010)
head(hs_sale_yr_aftr_2010)
new_housing_df<-rbind(hs_sale_yr_bfr_2010,hs_sale_yr_aftr_2010)
head(new_housing_df)
identical(new_housing_df,housing_df)


# d.Split a string, then concatenate the results back together

library(stringr)

#split the Sale_Date columns
sales_date_list<-str_split(string=housing_df$Sale_Date,pattern = '-') 
head(sales_date_list) 

#Create dataframe from the list
sales_date_matrix=data.frame(Reduce(rbind,sales_date_list))
head(sales_date_matrix)

#assign names to the new columns
names(sales_date_matrix)<- c('sale_year','sale_month','sale_date')
head(sales_date_matrix)

#combine the housing dataframe with new dataframe
housing_df<-cbind(housing_df,sales_date_matrix)
head(housing_df)
