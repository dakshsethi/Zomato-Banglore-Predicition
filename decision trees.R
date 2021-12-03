library(ggplot2)
library(dplyr)
library(MASS)
library(datarium)
library(rpart)
library(rpart.plot)
library(readr)
library(party)
#setwd("F:/3rd Year/3505_Foundation of Data Analytics/Project/decision tree")
data1 <- read.csv("zomato1.csv")
##View(data1)

rm(list=ls())

#### PREPARING THE DATASET #####
str(data1)
dim(data1)
summary(data1)

#### PARTITIONING DATASET INTO VALIDATION DATASETS ####

set.seed(1234)
pd <- sample(2,nrow(data1),replace=TRUE, prob=c(0.8,0.2))
train <- data1[pd==1,]
validate <- data1[pd==2,]


#### DECISION TREE WITH PARTY ####
View(data1)
#setting the confidence level
# setting minimum size as 200 => branch will split into 2 only when sample size is 200

## when there are restaurants within a good price range and having good votes
tree <- ctree(Restaurant.ID~Price.range+Votes,data=train, controls = ctree_control(mincriterion = 0.9, minsplit=200))
tree
plot(tree)

## when there are restaurants with average cost for 2 people within set price range and with votes
tree1 <- ctree(Restaurant.ID~Average.Cost.for.two+Price.range+Votes,data=train)
tree1
plot(tree1)

###PREDICTION ###
predict(tree,validate,type="prob")
predict(tree1,validate,type="prob")