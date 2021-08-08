# Assignment: Project step 2
# Name: Rapuru, Supraja
# Date: 2021-08-07
#Analysis of how AirBnB rentals prices affects the nearby housing rental 
#prices in Chicago

## Load the readxl package
library(readxl)
## Load the plyr package
library(dplyr)
## Load the plyr package
library(plyr)
## Load the tidyverse package
library(tidyverse)
library(ggplot2)
## Set the working directory to the root of your DSC 520 directory
setwd('C:/Users/Supraja/Desktop/3. DSC520/Project')

#Above data set contains information across US cities
#Filtering the data based on city==Chicago as we are focusing on Chicago
library(readr)
airbnb_chicago_df <- readr::read_csv('airbnb-listings.csv')
str(airbnb_chicago_df)
summary(airbnb_chicago_df)

## Load the Affordable rental housing dataset
housing_df=read.csv("Affordable_Rental_Housing_Developments.csv")
glimpse(housing_df)

## Load the Average rent Chicago neighborhood dataset 
avg_rent_df <- read_excel("Average_rent_Chicago_neighbourhood.xlsx")
glimpse(avg_rent_df)

#Merge the airbnb df with rental housing df based on neighbourhood
final_1_df <- left_join(airbnb_chicago_df,housing_df,by="neighbourhood_cleansed" )
glimpse(final_1_df)
head(final_1_df)

#Merge the above df with Average rent df based on neighbourhood
final_2_df <- inner_join(x=final_1_df,y=avg_rent_df,by=c("neighbourhood_cleansed") )
glimpse(final_2_df)

#By looking at the data we can say that
#Airbnb data
# 1. Variable id is just an identifier and we can ignore it.
# 2. We can factor the field room.type - Private room,Entire home/apt,Hotel 
# room, Shared room
# 3. We can drop the host.id and host.name,neighbourhood.group,name fields 
# from the dataset
# 4. We can drop fields like last.review,number.of.reviews,
# reviews.per.month,calculated.host.listings.count

#Average rent Chicago neighborhood data
# 5. We can drop Property Name,Phone Number,Management Company,Units,Zip Codes
# from the # dataset

#Average rent Chicago neighborhood data
# 6. rename the Average Rent to Average_Rent

# Apply above transformation to the dataframe
final_df <- subset(final_2_df, select = c("neighbourhood_cleansed",
                                          "latitude",
                                          "longitude",
                                          "room_type",
                                          "price","minimum_nights",
                                          "availability_365",
                                          "property_type",
                                          "Zip.Code","X.Coordinate",
                                          "Y.Coordinate",
                                          "Latitude","Longitude",
                                          "Average Rent") )
glimpse(final_df)

#Rename Average Rent to Average_Rent
colnames(final_df)[14] <- "Average_Rent"

# Checking the summary of data set to gauge the value range of each numerical 
# variable
summary(final_df)

# 7. Range of values prices are varies from 0 to 10000. 
# It looks like there are outliers in the field.
# 8. Range of values minimum_nights varies from 1 to 365. 
# It looks like there are outliers in the field.
# 9. Range of values for availability_365 varies from 0 to 365. 
# 10. Range of values for Average_Rent varies from 675 to 2350.

#Calculate the 30 days price for airbnb property.
final_df$airbnb_30_days_price=final_df$price * 30
summary(final_df)

#Check missing values
apply(final_df, 2, function(x) any(is.na(x)))

#It looks like there are some missing values for 
#X.Coordinate ,Y.Coordinate, Latitude, Longitude, Zip.Code

## 2.What does the final data set look like?
glimpse(final_df)

## 3. Questions for future steps.
# a) Need to learn how to visualize more than two variables. 
# b) Need to learn application of variable scaling and techniques. 
# c) Need to learn how lm() function takes care of variable scaling.
# d) Need to learn correlation between different variables.

## 4.What information is not self-evident?
# To uncover new information in the data that is not self-evident -
# 1. visualize data to uncover patterns and trends
# 2. correlation among variables
# 3. Check data distribution of variables
# 4. detect outliers and influencial cases
# 5.What are different ways you could look at this data?

# Checking relation between airbnb_30_days_price and Average_Rent using 
ggplot()
library(ggplot2)
ggplot(data = final_df, aes(x = airbnb_30_days_price, y = Average_Rent)) +
  geom_point() + geom_smooth(fill=NA)

# Checking relation between neighbourhood_cleansed and Average_Rent using 
ggplot()
library(ggplot2)
ggplot(data = final_df, aes(y = neighbourhood_cleansed, x = Average_Rent)) +
  geom_point() + geom_smooth(fill=NA)

# Checking relation between neighbourhood_cleansed and airbnb_30_days_price using 
ggplot()
library(ggplot2)
ggplot(data = final_df, aes(y = neighbourhood_cleansed, x = airbnb_30_days_price)) +
  geom_point() + geom_smooth(fill=NA)

#We can see that there is relationship between neighbourhood and prices
# Checking if data distribution of numeric variables is normal
# combining pipe operator between dplyr transformation and ggplot
final_df %>% select(airbnb_30_days_price, Zip.Code, Average_Rent) %>%
  gather() %>%
  ggplot(., aes(sample = value)) +
  stat_qq() +
  facet_wrap(vars(key), scales ='free_y')

#None of the variables looks normally distributed
ggplot(data = final_df, aes(x = neighbourhood_cleansed , y = airbnb_30_days_price)) +
  geom_boxplot() + ylab("airbnb_30_days_price")

# We can see that there are so many outliers for many neighbourhoods
# thus data is not normally distributed
ggplot(data = final_df, aes(x = room_type , y = airbnb_30_days_price)) +
  geom_boxplot() + ylab("30 days price")

# We can see that there are so many outliers for room_type
# thus data is not normally distributed
ggplot(data = final_df, aes(x = property_type , y = Average_Rent)) +
  geom_boxplot() + ylab("Property Type")

# We can see that there are so many outliers for Property_Type
# thus data is not normally distributed

# 6.How do you plan to slice and dice the data?
unique(final_df[c("Zip.Code")])

unique(final_df[c("neighbourhood_cleansed")])

#I think need to slice the datasets by zip codes or neighbourhood to analyze 
# the data in more granular level

# 7.How could you summarize your data to answer key questions?
library("ggpubr")

ggscatter(final_df, x = "airbnb_30_days_price", y = "Average_Rent", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

#a) What are the Airbnb rental prices for different areas in Chicago?
ggplot(data=final_df,aes(y=neighbourhood_cleansed)) + geom_histogram(stat = "count")

ggplot(aes(y=neighbourhood_cleansed,x=airbnb_30_days_price),data=final_df)+
  geom_point()

# From graph it looks like "West town" have major number of airbnb properties
# Also the prices of "West town" properties are high for airbnb rental.

# b) What is the correlation between the Airbnb rental prices and Chicago 
# neighborhood rent prices?
  
cor(final_df$airbnb_30_days_price,final_df$Average_Rent)

# It is evident from the plots that there is positive correlation between 
# airbnb prices and average rent

# c)What are the average rent prices by the neighborhood?
ggplot(aes(y=neighbourhood_cleansed,x=mean(final_df$Average_Rent)),data=final_df)+
  geom_point()

#The average rent price is ~1600 per month
# d)What are the average rent prices for Airbnb by the neighborhood?

ggplot(aes(y=neighbourhood_cleansed,x=mean(airbnb_30_days_price)),data=final_df)+
  geom_point()

#The average airbnb price is ~ 5400 per month

# e) What type of houses are most rented on Airbnb?
ggplot(data=final_df,aes(y=room_type)) + geom_histogram(stat ="count")

#It looks like Entire home/apt are most rented on Airbnb

# f)What is the monthly rent from the Airbnb properties?
df1 <-final_df%>%select(neighbourhood_cleansed, airbnb_30_days_price, Average_Rent)
df1 %>% group_by(neighbourhood_cleansed) %>% summarize(mean_airbnb_30_days_price =
                                                mean(airbnb_30_days_price))

#Airbnb monrthly average rent is 5462.622

# 9)Do you plan on incorporating any machine learning techniques to answer 
# your research questions? Explain.
# performing multiple linear regression
# splitting the data into training and test set
library(caTools)
mymodel_1 <-lm(airbnb_30_days_price ~ neighbourhood_cleansed,data = final_df)
summary(mymodel_1)

mymodel_2 <-lm(airbnb_30_days_price ~ Zip.Code,data = final_df)
summary(mymodel_2)

# Questions for future steps?
# 1. I would like to plot the airbnb properties on map
# 2. I think I need to look for more data to determine the correlation and to 
# predict prices accurately