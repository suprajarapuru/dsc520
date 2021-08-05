# Assignment: ASSIGNMENT 9.2.2
# Name: Rapuru, Supraja
# Date: 2021-08-04
#Fit a Logistic Regression Model

library(caTools)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

binary_df <- read.csv("data/binary-classifier-data.csv")

str(binary_df)
head(binary_df)

# a.Fit a logistic regression model to the binary-classifier-data.csv dataset
mymodel <-glm(label ~ .,data = binary_df, family = 'binomial')
# View the summary of the model
summary(mymodel)

# As y variable has low p-value, it is good predictor for label

# b.The dataset (found in binary-classifier-data.csv) contains three 
# variables; label, x, and y. The label variable is either 0 or 1 
# and is the output we want to predict using the x and y variables.

# i.What is the accuracy of the logistic regression classifier?
#Split the data into test and train datasets
split <- sample.split(binary_df,SplitRatio = 0.8)
split

train<- subset(binary_df,split=="TRUE")
test<- subset(binary_df,split=="FALSE")

#run the test data through model
res<- predict(mymodel,test,type="response")
res

#run the train data through model
res<- predict(mymodel,train,type="response")
res

#Validate the model - confusion Matrix
confmatrix <- table(Actual_Value=train$label,Predicted_Value = res >0.5)
confmatrix

#Accuracy of the model
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

#The accuracy of the model is 58%