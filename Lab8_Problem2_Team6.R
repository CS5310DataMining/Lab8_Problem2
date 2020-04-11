#Lab: 8
#Team: 6
#Team Members:
  #Melvin Zaldivar - Members contribution: 33.33%
  #Rahim Abdulmalik - Members contribution: 33.33%
  #Raul Beiza - Members contribution: 33.33%

## Clustering with k-means
##=============================================
## Setp 1 - Exploring and preparing the data
##=============================================

## Load the data into a data frame
teens <- read.csv("snsdata.csv")

## Review the data
str(teens)

## Cleaning age data from dataset
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)

## Create dummy variables for female and unknown gender
teens$female <- ifelse (teens$gender == "F" &
                          !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

## Imputing the missing values in age
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)                

## Checking if missing values were replaced
summary(teens$age)

##=============================================
## Setp 2 - Training a model on the data
##=============================================

## Loading the stats package if not already in R
library(stats)

## Creating data frame only the 36 features
interest <- teens[5:40]

## Applying z-score standardization to the interest data frame
interest_z <- as.data.frame(lapply(interest, scale))

## Setting a set.seed() function to ensure that the results match the output
RNGversion("3.5.2")
set.seed(2345)
teens_cluster <- kmeans(interest_z,5)

##=============================================
## Setp 3 - Evaluating model performance
##=============================================

## Evaluating the clusters size
teens_cluster$size

## Examining centers
teens_cluster$centers

##=============================================
## Setp 4 - Improving model performance
##=============================================

## Apply the clusters back onto the full dataset
teens$cluster <- teens_cluster$cluster

## Examining the first five teenages in the SNS data
teens[1:5, c("cluster", "gender", "age", "friends")]

## Using the aggregate() function to look at the demographic
aggregate(data=teens, age~cluster, mean)

## Demographic for females in cluster
aggregate(data=teens, female~cluster, mean)







