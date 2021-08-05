# Assignment: ASSIGNMENT 9.2.1
# Name: Rapuru, Supraja
# Date: 2021-08-04
# Analysis of Thoracic Surgery Binary Dataset

## Load the foreign package
library(foreign)
library(caTools)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

thoraric_surgery_df <- read.arff("data/ThoraricSurgery.arff")
str(thoraric_surgery_df)
head(thoraric_surgery_df)

# i.Fit a binary logistic regression model to the data set that predicts whet
# her or not the patient survived for one year (the Risk1Y variable) after the 
# surgery. Use the glm() function to perform the logistic regression. 
# See Generalized Linear Models for an example. Include a summary 
# using the summary() function in your results.

#Fit the binary logistic regression model to the data set 
mymodel <-glm(Risk1Yr ~ .,data = thoraric_surgery_df, family = 'binomial')

summary(mymodel)

# ii. According to the summary, which variables had the greatest 
# effect on the survival rate?
# As all the below variables have less p-value, it looks like 
# below are the g  ood predictors for the whether or not the 
# patient Risk1Y variable) after the surgery. 
# PRE5,PRE9T,PRE14OC13,PRE14OC14,PRE17T,PRE30T

# iii. To compute the accuracy of your model, use the dataset 
# to predict the outcome variable. The percent of correct 
# predictions is the accuracy of your model. 
# What is the accuracy of your model?

#Split the data into test and train datasets
split <- sample.split(thoraric_surgery_df,SplitRatio = 0.8)
split

train<- subset(thoraric_surgery_df,split=="TRUE")
test<- subset(thoraric_surgery_df,split=="FALSE")

#run the test data through model
res<- predict(mymodel,test,type="response")
res

#run the train data through model
res<- predict(mymodel,train,type="response")
res

#Validate the model - confusion Matrix
confmatrix <- table(Actual_Value=train$Risk1Yr,Predicted_Value = res >0.5)
confmatrix

#Accuracy of the model
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

#The accuracy of the model is 82.3%