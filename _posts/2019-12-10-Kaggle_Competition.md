---
title: "Kaggle Competition - Predict Airbnb Listing Price"
date: 2019-12-10
tags: [Predictive Analysis, Data Wrangling]
header:
  image: "/images/airbnb.jpg"
excerpt: "Data Wrangling, Data Science, Messy Data"
mathjax: "true"
---
## Project Introduction
The objective of the competition was to predict the listing price with the analysis of a set of Airbnb housing listing data. The accuracy of the prediction is measured by RMSE. I applied several different models, including linear regression, forward/backward selection, random forest and random forest with cross validation. I ended up using the Random Forest model with cross validation since it yielded the best RMSE. 

## Project Code
```r
library(caret)
library(corrplot)
library(MASS)
library(ModelMetrics)
library(leaps)
library(rpart)
library(rpart.plot)

setwd('/Users/559/Downloads')
# read data 
data  <- read.csv("analysisData.csv")
data.score <- read.csv("scoringData.csv")  # scoring data

str(data)

ggplot(data = data, aes(x=price)) +
  geom_histogram()

numericVars <- which(sapply(data, is.numeric)) #index numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on

all_numVar <- data[, numericVars]
all_numVar$id <- NULL   #remove id
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with price
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

cat("The number of duplicated rows are", nrow(data) - nrow(unique(data)))
colSums(sapply(data, is.na))


# select columns, replace NA with 0
indexes <- c()
for(i in 1:ncol(data)){
  if(is.numeric(data[,i])==T){
    indexes <- c(indexes, i)
    data[,i][is.na(data[,i])] <- 0
  }else if(is.factor(data[,i])==T){
    n <- length(levels(data[,i]))
    if(n>1 & n<=5){
      indexes <- c(indexes, i)
    }
  }
}

# replace NA with 0
for(i in 1:ncol(data.score)){
  if(is.numeric(data.score[,i])==T){
    data.score[,i][is.na(data.score[,i])] <- 0
  }
}


set.seed(123)
split = createDataPartition(y=data$price,p = 0.7,list = F,groups = 100)
train = data[split,indexes]
test = data[-split,indexes]

model = lm(price~.,train)
summary(model)

# step forward modal
start_mod = lm(price~1,data=train[,-1])
empty_mod = lm(price~1,data=train[,-1])
full_mod = lm(price ~ .,data=train[,-1])

forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')

summary(forwardStepwise)

rmse(predict(forwardStepwise, test), test$price)


fit = lm(price~minimum_nights+review_scores_rating,train)
rmse(predict(fit, test), test$price)

price <- predict(fit, data.score)
df <- data.frame(id=data.score$id, price=price)
write.csv(df, "forward.csv", row.names = F)

# backword modal
start_mod = lm(price~.,data=train[,-1])
empty_mod = lm(price~1,data=train[,-1])
full_mod = lm(price~.,data=train[,-1])
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
rmse(predict(backwardStepwise, test), test$price)

fit <- lm(price~host_is_superhost + host_listings_count + host_has_profile_pic + 
            host_identity_verified + neighbourhood_group_cleansed + room_type + 
            accommodates + bathrooms + bedrooms + beds + weekly_price + 
            cleaning_fee + guests_included + extra_people + minimum_nights + 
            number_of_reviews_ltm + review_scores_rating + review_scores_accuracy + 
            review_scores_cleanliness + review_scores_checkin + review_scores_communication + 
            review_scores_location + review_scores_value + license + reviews_per_month, data=train)
rmse(predict(fit, test), test$price)

#  72.66377




# Tree Model

tree = rpart(price~.,data=train[,-1])
predTree = predict(tree,newdata=test)
rmseTree = sqrt(mean((predTree-test$price)^2))
rmseTree

maximalTree = rpart(price~.,data=train[,-1],control=rpart.control(minbucket=1))
predMaximalTree = predict(maximalTree,newdata=test)
rmseMaximalTree = sqrt(mean((predMaximalTree-test$price)^2))
rmseMaximalTree

trControl = trainControl(method="cv",number = 10)
tuneGrid = expand.grid(.cp = seq(0.001,0.1,0.001))
set.seed(617)
cvModel = train(price~.,data=train,method="rpart",
                trControl = trControl,tuneGrid = tuneGrid)
cvModel$bestTune

treeCV = rpart(price~.,data=train,
               control=rpart.control(cp = cvModel$bestTune))
predTreeCV = predict(treeCV,newdata=test)
rmseCV = sqrt(mean((predTreeCV-test$price)^2))
rmseCV


# Bag Model
library(randomForest)
forest = randomForest(price~host_is_superhost + host_listings_count +
                        neighbourhood_group_cleansed + room_type + 
                        accommodates + bathrooms + bedrooms + beds + weekly_price + 
                        cleaning_fee + guests_included + extra_people + minimum_nights + 
                        number_of_reviews_ltm + review_scores_rating + review_scores_accuracy + 
                        review_scores_cleanliness + review_scores_checkin + review_scores_communication + 
                        review_scores_location + review_scores_value + license + reviews_per_month,data=train,ntree = 1000)

predForest = predict(forest,newdata=test)
rmseForest = sqrt(mean((predForest-test$price)^2))
rmseForest


# 64.23544
col_names <- colnames(data.score)
col_names <- col_names[col_names%in%colnames(train)]
xtest <- data.score[,col_names]
xtest <- rbind(train[1,-17] , xtest)
xtest <- xtest[-1,]
price <- predict(forest, newdata=xtest)

df <- data.frame(id=xtest$id, price=price)
write.csv(df, file="forest.csv", row.names = F)

# cross validation

trControl=trainControl(method="cv",number=10)
tuneGrid = expand.grid(mtry=1:5)
set.seed(617)
cvForest = train(price~host_is_superhost + host_listings_count +
                   neighbourhood_group_cleansed + room_type + 
                   accommodates + bathrooms + bedrooms + beds + weekly_price + 
                   cleaning_fee + guests_included + extra_people + minimum_nights + 
                   number_of_reviews_ltm + review_scores_rating + review_scores_accuracy + 
                   review_scores_cleanliness + review_scores_checkin + review_scores_communication + 
                   review_scores_location + review_scores_value + license + reviews_per_month,data=train,
                 method="rf",ntree=100,trControl=trControl,tuneGrid=tuneGrid )
cvForest  # best mtry was 2

rmse(predict(cvForest, test),test$price)

price <- predict(cvForest, newdata=xtest)

df <- data.frame(id=xtest$id, price=price)
write.csv(df, file="forest3.csv", row.names = F)
```
