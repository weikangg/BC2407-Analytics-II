library(earth)
library(dplyr)
library(caret)
library(vip)
library(pdp)
library(caTools)


setwd("C:/Users/ASUS/OneDrive - Nanyang Technological University/Documents/BC2407-Analytics-II-Project/Dataset")
data1 = read.csv("trafficAccident2020_cleaned.csv")

summary(data1)

set.seed(888)
train <- sample.split(Y = data1$Fatality_Rate, SplitRatio = 0.7)
trainset <- subset(data1, train == T)
testset <- subset(data1, train == F)

#1 Degree Mars Model
m.mars1 <- earth(Fatality_Rate ~ . , degree=1, data=data1)
summary(m.mars1)
m.mars1.yhat <- predict(m.mars1)
RMSE.mars1 <- round(sqrt(mean((data1$Fatality_Rate - m.mars1.yhat)^2)))
RMSE.mars1
sqrt(m.mars1$rss/nrow(data1))

#1 Degree Mars Model (train-test)
m.mars1 <- earth(Fatality_Rate ~ . , degree=1, data=trainset)
summary(m.mars1)
m.mars1.yhat <- predict(m.mars1,newdata = testset)
RMSE.mars1 <- round(sqrt(mean((data1$Fatality_Rate - m.mars1.yhat)^2)))
RMSE.mars1
sqrt(m.mars1$rss/nrow(data1))

#2 Degree Mars Model
m.mars2 <- earth(Fatality_Rate ~ . , degree=2, data=data1)
summary(m.mars2)
m.mars2.yhat <- predict(m.mars2)
RMSE.mars2 <- round(sqrt(mean((data1$Fatality_Rate - m.mars2.yhat)^2)))
RMSE.mars2
sqrt(m.mars2$rss/nrow(data1))

#3 Degree Mars Model
m.mars3 <- earth(Fatality_Rate ~ . , degree=3, data=data1)
summary(m.mars3)
m.mars3.yhat <- predict(m.mars3)
RMSE.mars3 <- round(sqrt(mean((data1$Fatality_Rate - m.mars3.yhat)^2)))
RMSE.mars3
sqrt(m.mars3$rss/nrow(data1))

#4 Degree Mars Model
m.mars4 <- earth(Fatality_Rate ~ . , degree=4, data=data1)
summary(m.mars4)
m.mars4.yhat <- predict(m.mars4)
RMSE.mars4 <- round(sqrt(mean((data1$Fatality_Rate - m.mars4.yhat)^2)))
RMSE.mars4
sqrt(m.mars4$rss/nrow(data1)) #RMSE = 14.56192

#5 Degree Mars Model 
m.mars5 <- earth(Fatality_Rate ~ . , degree=5, data=data1)
summary(m.mars5)
m.mars5.yhat <- predict(m.mars5)
RMSE.mars5 <- round(sqrt(mean((data1$Fatality_Rate - m.mars5.yhat)^2)))
RMSE.mars5
sqrt(m.mars5$rss/nrow(data1)) #(Degree > 5 is all same RMSE as Degree 4)

#With CV
m.mars6 <- earth(Fatality_Rate ~ . , degree = 4, pmethod = "cv", nfold = 10, ncross = 1, data=data1)
summary(m.mars6)
RMSE.mars6 <- sqrt((m.mars6$rss)/nrow(data1))
RMSE.mars6 #RMSE = 14.56694
  #varimpt <- evimp(m.mars5)
  #print(varimpt)


#-----------------
#Parameter Tuning Testing
#-----------------
hyper_grid <- expand.grid(
  degree = 1:10,
  nprune = seq(2, 50, length.out = 10) %>% floor()
)

head(hyper_grid)

set.seed(123) 

cv_mars <- train(
  x = subset(data1, select = -Fatality_Rate),
  y = data1$Fatality_Rate,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# View results
cv_mars$bestTune

cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)

help("earth")
cv_mars$results
