# Assignment: ASSIGNMENT 4.2.2
# Name: Rapuru, Supraja
# Date: 2021-07-02

## Load the readxl package
library(readxl)

## Load the plyr package
library(plyr)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

## Load the `data/week-6-housing.xlsx` to
housing_df <- read_excel("data/week-6-housing.xlsx")
head(housing_df)
str(housing_df)

#1.Use the apply function on a variable in your dataset

#get sum of the sale price using apply function
apply(housing_df[,2],MARGIN=2,FUN=sum, na.rm=TRUE)

# 2.Use the aggregate function on a variable in your dataset

#get mean sales price by cityname using aggregate function
colnames(housing_df)[1] <- "Sale_Date"
colnames(housing_df)[2] <- "Sale_Price"
aggregate(Sale_Price ~ ctyname, housing_df, mean)


# 3.Use the plyr function on a variable in your
# dataset – more specifically, I want to see you split some data,
# perform a modification to the data, and then bring it back together

ddply(housing_df, .(bedrooms), function(x) sum(x$Sale_Price))

# 4.Check distributions of the data
summary(housing_df)

#Sale_Price vary between 698 to 4400000. The mean Sale_Price is 660738.
#Bedrooms vary between 0 to 11. There are so many variants available.
#year_built vary from 1900 to 2016. Some houses are very old available for sale.
#sq_ft_lot vary between 785 to 1631322.
#sale_Date vary between 2006-01-03 to 2016-12-16.

unique(housing_df$prop_type)
#All the houses are of type "R" i.e residential.

unique(housing_df$ctyname)
#All the houses are located in "REDMOND" and"SAMMAMISH"

library(ggplot2)
bar <- ggplot(housing_df, aes(housing_df$zip5,housing_df$Sale_Price, fill = housing_df$ctyname))
bar + stat_summary(fun = mean, geom = "bar", position="dodge",width = 8)+ facet_wrap( ~ housing_df$ctyname)

#Sale_price are more in SAMMAMISH then REDMOND.
#There are some zips codes for which city name is NA.

ggplot(housing_df, aes(x=housing_df$bedrooms, y=housing_df$Sale_Price)) + geom_point() +  xlim(0, 11)

#It looks like 4 bedroom houses are more popular for sale.

# 5.Identify if there are any outliers

ggplot(housing_df) +
  aes(x = housing_df$bedrooms) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#All houses with bedroom >6 and <2 are outliers

ggplot(housing_df) +
  aes(x = housing_df$year_built) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#All houses with built year < 1950 are outliers

ggplot(housing_df) +
  aes(x = housing_df$Sale_Price) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#All housese with sales price > 2000000 are outliers

ggplot(housing_df) +
  aes(x = housing_df$Sale_Date) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#There are no outliers for sale date


# 6.Create at least 2 new variables
# deriving year of sale of the house
housing_df["sale_year"] <- substr(housing_df$Sale_Date,1,4)
# derive renovated flag
housing_df["renovated_flag"] <- ifelse(housing_df$year_renovated != 0, 'Yes', 'No')
str(housing_df)