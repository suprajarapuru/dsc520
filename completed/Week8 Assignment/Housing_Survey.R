# Assignment: Housing Survey
# Name: Rapuru,Supraja
# Date: 2021-08-01

library(readxl)
library(dplyr)
library(purrr)
library(QuantPsyc)
library(car)
library(tidyverse)
library(ggplot2)
library(lmtest)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

housing <- read.csv("Housing.csv")

str(housing)
glimse(housing)

sum(is.na(housing$ctyname))
apply(housing, 2, function(x) any (is.na(x)))

# By looking at the data, i can see that there is missing data for sale_warning and ctyname


# I. Explain any transformations or modifications you made to the dataset ----
colnames(housing)[1] <- "Sale_Date"
colnames(housing)[2] <- "Sale_Price"

library(magrittr)

housing %<>%
  mutate ("year_of_sale" = substr(housing$Sale_Date,1,4))
str(housing)

# I have Changed the name of Sale Date and Sale Price
# I have also created new field year_of_sale that will be useful to predict the sale price

# II. Create two variables;----
#    one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression)
#    and one that will contain Sale Price and several additional predictors of your choice.
#    Explain the basis for your additional predictor selections.
housing_lm_1 <- lm(formula = Sale.Price ~ sq_ft_lot, data = housing)
housing_lm_2 <- lm(formula = Sale.Price ~ zip5 + bedrooms + year_built, data = housing)

# III. Execute a summary() function on two variables defined in the previous step to compare the model results. ----
#    What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model.
#    Did the inclusion of the additional predictors help explain any large variations found in Sale Price?
summary(housing_lm_1)
summary(housing_lm_2)

# IV. Considering the parameters of the multiple regression model you have created, ----
#     What are the standardized betas for each parameter and what do the values indicate?
librar(lm.beta)

coef_lmbeta <- lm.beta(housing_lm_2)
coef_lmbeta

# zip5 (standardized β = 0.04458759) - This value indicates that as zip code increase by
# 1 standard deviation, sales price increase by 0.04458759 standard deviation.
# bedrooms (standardized β = 0.22417183) -This value indicates that as bedrooms
# increase by 1 standard deviation, sales price increase by 0.22417183 standard deviation.
# year_built(standardized β = 0.23537926) - This value indicates that as year_# built
# increase by 1 standard deviation, sales price increase by 0.23537926 standard deviation.

# V. Calculate the confidence intervals for the parameters in your model and
#     explain what the results indicate.
confint(housing_lm_2)

# In this model, the two best predictor (year_built) have very tight confidence intervals,
# indicating that the estimates for the current model are likely
# to be representative of the true population
# values. The interval for (zip5 and bedrooms) is wider (but still does not cross zero),
# indicating that the parameter for this variable is less representative, but nevertheless significant.

# VI. Assess the improvement of the new model compared to your original model (simple regression model) ----
#     by testing whether this change is significant by performing an analysis of variance.
anova(housing_lm_1,housing_lm_2)

# The value in column labelled Pr(>F) is 2.2e−16 (i.e., 2.2 with the decimal
# place moved 16 places to the left, or a very small value indeed);
# we can say that housing_lm_2 significantly improved
# the fit of the model to the data compared to housing_lm_1, F(2, 12861) = 69
# 3.82, p < .001.

# VII. Perform casewise diagnostics to identify outliers and/or influential cases, ----
#     storing each function's output in a dataframe assigned to a unique variable name.
housing$residuals<-resid(housing_lm_2)
housing$standardized.residuals<- rstandard(housing_lm_2)
housing$studentized.residuals<-rstudent(housing_lm_2)
housing$cooks.distance<-cooks.distance(housing_lm_2)
housing$dfbeta<-dfbeta(housing_lm_2)
housing$dffit<-dffits(housing_lm_2)
housing$leverage<-hatvalues(housing_lm_2)
housing$covariance.ratios<-covratio(housing_lm_2)

housing

# VIII. Calculate the standardized residuals using the appropriate command, ----
#     specifying those that are +-2, storing the results of large residuals in a variable you create.
housing$large.residual <- housing$standardized.residuals > 2 | housing$standardized.residuals < -2

# IX. Use the appropriate function to show the sum of large residuals. ----
sum(housing$large.residual)

# X. Which specific variables have large residuals (only cases that evaluate as TRUE)? ----
housing[housing$large.residual,c("Sale_Price", "zip5", "bedrooms", "yea
r_built","standardized.residuals")]

# XI. Investigate further by calculating the ----
#    leverage,
#    cooks distance,
#    and covariance rations.
# Comment on all cases that are problematics.
housing[housing$large.residual , c("cooks.distance", "leverage", "covar
iance.ratios")]

# Executing this command prints the variables (or columns) labelled cooks.
# distance, leverage, and covariance.ratios but only for cases for which large.
# residual is TRUE.
# Output shows these values; none of them has a Cook’s distance greater than 1 ,
# so none of the cases is having an undue influence
# on the model. The average leverage can be calculated as 0.011 (k + 1/n = 4/346)
# and so we are looking for values either twice as large as this (0.022) or
# three times as large (0.033) depending on which statistician you trust most.
# All cases are within the boundary of three times the average and only case 1
# is close to two times the average.

# XII. Perform the necessary calculations to assess the assumption of independence ----
#    and state if the condition is met or not.
durbinWatsonTest(housing_lm_2)

# From the output we can see that the test statistic is 0.7442029 and the
# corresponding p-value is 0. Since this p-value is less than 0.05, we can reject
# the null hypothesis and conclude that the residuals in this regression model
# are autocorrelated. As a conservative rule, D-W Statistic values less than 1
# or greater than 3 should definitely raise alarm bells.
# The closer to 2 that the value is, the better, and for these data the value
# is 0.744, which is less than 1 suggests that the assumption might not certainly
# been met.

# XIII. Perform the necessary calculations to assess the assumption of no multicollinearity ----
#    and state if the condition is met or not.
vif(housing_lm_2)
#tolerance statistics
1/vif(housing_lm_2)
mean(vif(housing_lm_2))

# For our current model the VIF values are all well below 10 and the tolerance
# statistics all well above 0.2. Also, the average VIF is very close to 1.
# Based on these measures we can safely conclude that there is no collinearity
# within our data.

# XIV. Visually check the assumptions related to the residuals using the plot() and hist() functions. ----
#     Summarize what each graph is informing you of and if any anomalies are present.
housing$fitted <- housing_lm_2$fitted.values
library(ggplot2)

histogram<-ggplot(housing, aes(studentized.residuals)) + geom_histogram(ae
s(y = ..density..), colour = "black", fill = "white") + labs(x = "Studentized
Residual", y = "Density")
histogram + stat_function(fun = dnorm, args = list(mean = mean(housing$stu
dentized.residuals, na.rm = TRUE), sd = sd(housing$studentized.residuals,
na.rm = TRUE)), colour= "red", size = 1)

qplot(sample = housing$studentized.residuals, stat="qq") + labs(x ="Theore
tical Values", y = "Observed Values")
# The histogram should look like a normal distribution (a bell-shaped curve).
# For the housing data data, the distribution is roughly normal.
# We could summarize by saying that the model appears, in most senses, to be
# both accurate for the sample and generalizable to the population.

# XV. Overall, is this regression model unbiased? ----
#    If an unbiased regression model, what does this tell us about the sample vs. the entire population model?

# vif values to check model bias
# When we check multi collinearity we check for vif score
vif(housing_lm_2)

# None of the vif scores are near 5 or greater and thus predictors does not
# have any significant multi collinearity. Multi collinearity problems consist of
# including, in the model, different variables that have a similar predictive
# relationship with the outcome.
mean(vif(housing_lm_2))
# Average vif is >1 but nowhere close to 5 or greater. Model does not appear
to have significant proof that model is biased.