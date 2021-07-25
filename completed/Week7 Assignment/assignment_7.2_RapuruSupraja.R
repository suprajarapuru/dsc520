# Assignment: ASSIGNMENT 7.2.2
# Name: Supraja, Rapuru
# Date: 2021-07-24
#Analysis of Student Survey

# import required packages
library(ggplot2)
library(ggm)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

## Load the `data/acs-14-1yr-s0201.csv` to
ss_df <- read.csv("data/student-survey.csv")
str(ss_df)
head(ss_df)

## I ----
# Calculate covariance of survey variables
# Why use this calculation?
# What do the results indicate?
cov(ss_df[, c(1:4)])

# Conclusions from the  covariance
# 1.Time of reading is negatively related to Time of watching TV,
# 2.Time of reading is negatively related to Happiness.
# 3.Time of watching TV is positively related to Happiness.
# 4.As gender is represented as integer, we can ignore the covariance associated with gender.

## II ----
# What measurement is being used for the variables?
# Explain what effect changing the measurement being used for the variables
#   would have on the covariance calculation.
# Would this be a problem? Explain and provide a better alternative if needed.

#TimeReading: time, in hours (rounded to whole hr)
#TimeTV: time, in minutes (rounded to nearest 5 min)
#Happiness: looks like percent. It looks like Happiness index varies from 45.67% to 89.52%
#Gender - By looking at the values, I assumed that the Gender is measure in boolean.
#It is not specified that 0 or 1 mean male/female. Need so more info on the variable.

#It would be better to keep time measurements consistent.
#Calculations for cov and cor assume similar units, so it would
#be better to convert TimeReading to minutes or TimeTV to hours.
#Here we have the edited information to TimeReading is in minutes.

TimeReadingMin <- ss_df$TimeReading*60
ss_edit_df <- cbind(ss_df[-c(1)], TimeReadingMin)
ss_edited_df <- ss_edit_df[,c(4,1:3)]
str(ss_edited_df)

# iii.Choose the type of correlation test to perform, explain why you chose this test,
# and make a prediction if the test yields a positive or negative correlation?

#checking normality of data

#Probability Plot of the TimeReading variable.
ggplot(ss_edited_df, aes(sample=TimeReadingMin)) + stat_qq() + stat_qq_line()

#Probability Plot of the TimeTV variable.
ggplot(ss_edited_df, aes(sample=TimeTV)) + stat_qq() + stat_qq_line()

#Probability Plot of the Happiness variable.
ggplot(ss_edited_df, aes(sample=Happiness)) + stat_qq() + stat_qq_line()


#By looking at plots, I can confirm that data is normally distributed.
#We can used Perason’s correlation coefficient to check the correlation between variables.

cor.test(ss_edited_df$TimeReadingMin,ss_edited_df$TimeTV)

cor.test(ss_edited_df$TimeReadingMin,ss_edited_df$Happiness)

cor.test(ss_edited_df$Happiness,ss_edited_df$TimeTV)


# iv. Perform a correlation analysis of:
# 1.All variables
cor(ss_edited_df)

# 2.A single correlation between two a pair of the variables
cor(ss_edited_df$TimeReadingMin, ss_edited_df$TimeTV)


# 3.Repeat your correlation test in step 2 but set the confidence interval at 99%
cor.test(ss_edited_df$TimeReadingMin, ss_edited_df$TimeTV, conf.level = 0.99)


# Describe what the calculations in the correlation matrix suggest about the relationship between the variables. Be specific with your explanation.
cor(ss_edited_df)

#Correlation coefficient between a variable and itself is 1 i.e. completely positively correlated.
#Correlation coefficient is < 0 that would signify negative correlation.
# TimeReading and TimeTV have negative correlation.
# TimeReading and Happiness have negative correlation.
# TimeTV and Happiness have positive correlation.

# V. Calculate the correlation coefficient and the coefficient of determination,
# describe what you conclude about the results.

# calculating correlation coefficient (r) between variables
cor(ss_edited_df)

# calculating coefficient of determination (rˆ2) between two variables
cor(ss_edited_df)^2

# The coefficient of determination is a measurement used to explain how
# much variability of one factor can be caused by its relationship to another
# related factor. This correlation, known as the "goodness of fit," is
# represented as a value between 0.0 and 1.0.

# Looking at coeffiecient of determination between TimeTV and Happiness shared variability is
# about 40% which would imply that TV time variability effects Happiness upto 40% only, while
# remaining 60% variability in Happiness must be caused by some other variable.



# vi. Based on your analysis can you say that watching more TV caused students to read less? Explain.

cor(ss_edited_df$TimeReadingMin, ss_edited_df$TimeTV)^2

# Looking at coefficient of determination (rˆ2) we can say that variability in TimeReading can
# cause upto 77% variability in TimeTV
# There could be other variables that may cause 23% variability in TimeTV.

# vii. Pick three variables and perform a partial correlation, documenting which variable you are
# “controlling”. Explain how this changes your interpretation and explanation of the results.

ss_edited_df2 <- ss_edited_df[,1:3]

# Run partial correlation between TimeTV and Happiness while controlling TimeReading

pcor(c("TimeTV","Happiness","TimeReadingMin"), var(ss_edited_df2))

pcor(c("TimeTV","Happiness","TimeReadingMin"), var(ss_edited_df2))^2


#If we keep TimeReading controlling , the correlation coefficient between TV time and happiness decrease to 0.59
# and coefficient of determination has decreased to 35%. This decrease
# suggests that variation in Happiness was also effected positively by TimeReading by about 5%.              )