install.packages("party")
library(randomForest)
library(party)
getwd()
zomato <- read.csv("zomato.csv")
str(zomato)
print(head(zomato))
zomato$Rating.text=factor(zomato$Rating.text)
zomato$Country.Code=factor(zomato$Country.Code)
zomato$Currency=factor(zomato$Currency)
typeof(zomato$Average.Cost.for.two)

output.forest <- randomForest(Rating.text ~ Price.range + Currency + Has.Online.delivery + Votes + Has.Table.booking, 
                              data = zomato)
print(output.forest)
out.importance <- round(importance(output.forest), 2)
print(out.importance )

zomato$Rating.color
zomato$Rating.color[zomato$Rating.color=="Red"] <- "1"
zomato$Rating.color[zomato$Rating.color=="Yellow"] <- "2"
zomato$Rating.color[zomato$Rating.color=="White"] <- "3"
zomato$Rating.color[zomato$Rating.color=="Orange"] <- "4"
zomato$Rating.color[zomato$Rating.color=="Green"] <- "5"
zomato$Rating.color[zomato$Rating.color=="Dark Green"] <- "6"
zomato$Rating.color <- as.numeric(zomato$Rating.color)
factor(zomato$Rating.color)
zomato$Rating.color


library(tidyverse)
zomato_india <- subset(zomato,zomato$Country.Code==1)
zomato_india


model <- lm(zomato_india$Aggregate.rating ~ zomato_india$Rating.color + zomato_india$Votes + zomato_india$Price.range, data = zomato)
summary(model)
confint(model)
write.csv(zomato,"zomato1.csv")

zomato.rf <- randomForest(Aggregate.rating ~ ., data = zomato_india, mtry = 3,
                         importance = TRUE, na.action = na.omit)
print(zomato.rf)