# ========================================================================================================
# Purpose:        BC2407 Group Project Data Cleaning
# Seminar Group:  03              
# Team:           05                                         
# DOC:            14-03-2023
# Topics:         Data Cleaning
# Data:           trafficAccident2020_cleaned.csv
#=========================================================================================================

#--------------------------------IMPORTING REQUIRED LIBRARIES-------------------------------------#

list.of.packages <- c("data.table", "RCurl", "tidyverse", "randomForest", "Boruta")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(RCurl)
library(tidyverse)
library(randomForest)
library(Boruta)
library(caTools)

#-----------------------------------GETTING CSV FROM GITHUB----------------------------------------#

x <- getURL("https://raw.githubusercontent.com/weikangg/BC2407-Analytics-II-Project/main/Dataset/trafficAccident2020_cleaned.csv")

#---------------------IMPORTING DATASET & GETTING AN OVERVIEW OF THE DATASET-----------------------#
trafficAccident2020.dt <- fread(text = x, stringsAsFactors = T)
trafficAccident2020.dt %>% glimpse()
trafficAccident2020.dt %>% head()
str(trafficAccident2020.dt)

#extracting columns to be converted to categorical
categoricalCols = c("Day", "Month", "Day_Of_Week", "Hour")

#convert data to categorical accordingly
trafficAccident2020.dt[, (categoricalCols) := lapply(.SD, as.factor), .SDcols = categoricalCols]

# Checking
str(trafficAccident2020.dt)

#set seed to ensure reproducibility of results
set.seed(2407)  

#70-30 train test split to have a constant evaluation point across models
traintest_split <- sample.split(Y = trafficAccident2020.dt$Fatality_Rate, SplitRatio = 0.7)
trainset <- subset(x = trafficAccident2020.dt, subset = traintest_split == TRUE)
testset <- subset(x = trafficAccident2020.dt, subset = traintest_split == FALSE)

# Feature selection
boruta.train <- Boruta(Fatality_Rate ~., data = trainset, doTrace = 2)
print(boruta.train)
boruta.train$finalDecision


# Random Forest Algorithm

rf_trafficAccident1 <- randomForest(Fatality_Rate ~ . , data = trafficAccident2020.dt,importance = T, do.trace = TRUE)

var.impt <- importance(rf_trafficAccident1)
varImpPlot(rf_trafficAccident1, type = 1)

plot(rf_trafficAccident1)

# finding optimal mtry(optimal random subset feature)
mtry <- tuneRF(trafficAccident2020.dt[,-39, with=FALSE],trafficAccident2020.dt$Fatality_Rate, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
mtry
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

# Plot to see when the tree settle down
plot(rf_trafficAccident1)

# Making predictions
train_predictions <- predict(model, newdata = train)
train_predictions
test_predictions <- predict(model, newdata = test)
test_predictions

# Calculate the RMSE of the predictions
train_RMSE <- sqrt(mean((train$mpg - train_predictions)^2))
test_RMSE <- sqrt(mean((test$mpg - test_predictions)^2))

# Print the RMSE value
print(train_RMSE)
print(test_RMSE)

# putting it in a table
table <- data.frame(Data = c("Train_RMSE", "Test_RMSE"), RMSE = c(train_RMSE, test_RMSE))
print(table)

# Check for overfitting by comparing trainset error and test set error
# https://ai.stackexchange.com/questions/19881/is-it-normal-to-have-the-root-mean-squared-error-greater-on-the-test-dataset-tha


# # Load libraries
# library(randomForest)
# library(caTools)
# 
# # Load data (example using mtcars dataset)
# data(mtcars)
# str(mtcars)
# 
# 
# mtcars$vs <- factor(mtcars$vs)
# mtcars$am <- factor(mtcars$am)
# 
# # Set seed for reproducibility
# set.seed(123)
# 
# # Split data into training and test sets
# split <- sample.split(mtcars$mpg, SplitRatio = 0.8)
# train <- mtcars[split, ]
# test <- mtcars[!split, ]
# 
# # Fit a Random Forest model to the training data
# model <- randomForest(mpg ~ ., data = train)
# 
# # Make predictions on the test data using the predict function
# train_predictions <- predict(model, newdata = train)
# train_predictions
# test_predictions <- predict(model, newdata = test)
# test_predictions
# 
# # Calculate the RMSE of the predictions
# train_RMSE <- sqrt(mean((train$mpg - train_predictions)^2))
# test_RMSE <- sqrt(mean((test$mpg - test_predictions)^2))
# 
# # Print the RMSE value
# print(train_RMSE)
# print(test_RMSE)
# 
# # table
# table <- data.frame(Data = c("Train_RMSE", "Test_RMSE"), RMSE = c(train_RMSE, test_RMSE))
# print(table)

