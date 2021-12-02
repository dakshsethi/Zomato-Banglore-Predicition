zom <- read.csv("zomato1.csv")
View(zom)
library(tidyverse)
library(e1071)
library(caTools)
library(class)
data1 <- read.csv("zomato1.csv")
View(data1)
nrow(data1)

split <- sample.split(data1, SplitRatio = 0.7)
train_cl <- subset(data1, split == "TRUE")
test_cl <- subset(data1, split == "FALSE")

train_scale <- scale(train_cl[, c("Country.Code","Price.range","Aggregate.rating","Votes")])
test_scale <- scale(test_cl[, c("Country.Code","Price.range","Aggregate.rating","Votes")])

classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Rating.text,
                      k = 1)
classifier_knn

cm <- table(test_cl$Rating.text, classifier_knn)
cm

misClassError <- mean(classifier_knn != test_cl$Rating.text)
print(paste('Accuracy =', 1-misClassError))
