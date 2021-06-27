# Assignment: ASSIGNMENT 3.2, American Community Survey Exercise
# Name: Rapuru, Supraja
# Date: 2021-06-26

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Load the pastecs package
library(pastecs)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Supraja/dsc520")

## Load the `data/acs-14-1yr-s0201.csv` to
acsy_df <- read.csv("data/acs-14-1yr-s0201.csv")

## 1.What are the elements in your data (including the categories and data types)?
summary(acsy_df)

## 2.Please provide the output from the following functions: str(); nrow(); ncol()
str(acsy_df)
nrow(acsy_df)
ncol(acsy_df)

## 3.Create a Histogram of the HSDegree variable using the ggplot2 package.
ggplot(acsy_df, aes(x=HSDegree)) + geom_histogram()

## 3.1.Set a bin size for the Histogram.
ggplot(acsy_df, aes(HSDegree)) + geom_histogram(bins = 10)

## 3.2.Include a Title and appropriate X/Y axis labels on your Histogram Plot.
ggplot(acsy_df, aes(HSDegree)) + geom_histogram(bins = 10) + ggtitle("HSDegree vs Frequnecy") + xlab("HSDegree(%)") +ylab("Frequnecy")

## 4.Answer the following questions based on the Histogram produced:

## 4.1.Based on what you see in this histogram, is the data distribution unimodal?
## As the probability distribution in given histogram has a single lump or peak, the data distribution unimodal.

## 4.2.Is it approximately symmetrical?
##  The distribution is not symmetrical

## 4.3.Is it approximately bell-shaped?
## The distribution is not bell-shaped

## 4.4.Is it approximately normal?
## The distribution is not normal.

## 4.5.If not normal, is the distribution skewed? If so, in which direction?
## The distribution skewed in left (so it is negatively Skewed)

## 4.6.Include a normal curve to the Histogram that you plotted.
ggplot(acsy_df, aes(x=HSDegree)) + geom_histogram(aes(y = ..density..),bins = 10)+ stat_function(fun = dnorm, colour = "red",args = list(mean = mean(acsy_df$HSDegree, na.rm = TRUE), sd = sd(acsy_df$HSDegree, na.rm = TRUE))) + ggtitle("HSDegree vs Frequnecy") + xlab("HSDegree(%)") +ylab("Frequnecy")

## 4.7.Explain whether a normal distribution can accurately be used as a model for this data.
## A normal distribution can not accurately be used as a model for this data because the histogram does not have shape of normal curve and
##The histogram shows that the distribution is negatively skewed.

## 5.Create a Probability Plot of the HSDegree variable.
ggplot(acsy_df, aes(sample=HSDegree)) + stat_qq() + stat_qq_line()

## 6.Answer the following questions based on the Probability Plot:

## 6.1.Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
## The straight line in this plot represents a normal distribution, and the points represent the observed residuals. Therefore, in
## a perfectly normally distributed data set, all points will lie on the line. As the points are not on line, it is not normal distribution.

## 6.2.If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
##In this plot, the lower end of QQ plot deviates from straight line then we can clearly say that the distribution has a
## longer tail to its left or simply it is left-skewed (or negatively skewed)

## 7. Now that you have looked at this data visually for normality,
##you will now quantify normality with numbers using the stat.desc() function.
##Include a screen capture of the results produced.
stat.desc(acsy_df, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
stat.desc(acsy_df["HSDegree"], basic=FALSE, desc=TRUE, norm=TRUE)

## 8 In several sentences provide an explanation of the result produced for
##skew, kurtosis, and z-scores. In addition, explain how a change in the sample size may change your explanation?
## The negative value for skewness indicates that the data are skewed left.
## The positive value of of kurtosis indicates a "heavy-tailed" distribution.
## Z score for skew (skew.2SE) and Z score for kurtosis(kurt.2SE) are greater than 1 and hence both skew and kurtosis are significant.
##Large samples will give rise to small standard errors and so when sample sizes are big, significant values arise from even small deviations from normality. Also for larger sample size we need to use visualizations for data analysis.