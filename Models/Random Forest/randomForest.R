# ========================================================================================================
# Purpose:        BC2407 Group Project Data Cleaning
# Seminar Group:  03              
# Team:           05                                         
# DOC:            14-03-2023
# Topics:         Data Cleaning
# Data:           trafficAccident2020_cleaned.csv
#=========================================================================================================

#--------------------------------IMPORTING REQUIRED LIBRARIES-------------------------------------#

list.of.packages <- c("data.table", "RCurl", "tidyverse", "randomForest", "Boruta", "varImp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(RCurl)
library(tidyverse)
library(randomForest)
library(Boruta)
library(caTools)
library(caret)
library(ROCR)
library(varImp)

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

# Dropping redundant columns
trafficAccident2020.dt[, c("ST_CASE", "VEH_NO", "PER_NO" ) := NULL]

# Checking
str(trafficAccident2020.dt)

#set seed to ensure reproducibility of results
set.seed(2407)  

# Feature selection
boruta.results <- Boruta(Death ~., data = trafficAccident2020.dt, doTrace = 2)
print(boruta.results)
boruta.results$finalDecision

# Results of Feature Selection
# Boruta performed 99 iterations in 10.48033 mins.
# 32 attributes confirmed important: Age, Airbag, Critical_Activity, Drinking, Driver_Distracted and 27 more;
# 4 attributes confirmed unimportant: Day, Day_Of_Week, Hazardous_Mat_Involvement, Month;
# 2 tentative attributes left: Pavement_Type, Previous_Speed_Convictions;

#===============================================================================#

#                           Modelling with Random Forest

#===============================================================================#

#generate dataframes to store random forest results

#generate empty data frame to store test set results for balanced data
testset_accuracy_rf <- data.frame('Model' = "Random Forest (Testset)", 'FPR' = 0, 'FNR' = 0, 
                                  'Acc' = 0, 'AUC' = 0, 'F1 Score' = 0, 'F2 Score' = 0)
testset_accuracy_rf

#generate empty data frame to store train set results
trainset_accuracy_rf <- data.frame('Model' = "Random Forest (Trainset)", 'FPR' = 0, 'FNR' = 0,
                                   'Acc' = 0, 'AUC' = 0, 'F1 Score' = 0,'F2 Score' = 0)
trainset_accuracy_rf

#-----------------------------------TRAIN TEST SPLIT------------------------------------------#

# 70-30 train test split to have a constant evaluation point across models
traintest_split <- sample.split(Y = trafficAccident2020.dt$Death, SplitRatio = 0.7)
trainset <- subset(x = trafficAccident2020.dt, subset = traintest_split == TRUE)
testset <- subset(x = trafficAccident2020.dt, subset = traintest_split == FALSE)

# 14143 rows within the trainset
nrow(trainset)

# 6060 rows within the testset
nrow(testset)

# Setting threshold
threshold <- 0.5

#--------------RANDOM FOREST MODEL WITH DEFAULT MTRY (random SUBSET FEATURES)----------------------#

rf_trafficAccident1 <- randomForest(Death ~ . , data = trainset,importance = T, do.trace = TRUE)
rf_trafficAccident1

# Plot to see when the error rates settle down, it stabilises at about 250 trees
plot(rf_trafficAccident1)

# Variable Importance
var.impt <- importance(rf_trafficAccident1)
par(mfrow = c(1,2))
varImpPlot(rf_trafficAccident1, type = 1) 
varImpPlot(rf_trafficAccident1, type = 2)
par(mfrow = c(1,1))

varImp(rf_trafficAccident2)

#Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_trafficAccident1, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Yes > threshold, "Yes", "No")
rf_train_cf <- confusionMatrix(data = table(trainset$Death, trainset_pred, deparse.level = 2))
rf_train_cf[["table"]]

#getting the area under curve
pred = prediction(trainset_prob[,2], trainset$Death)
auc = performance(pred, measure = "auc")

# Storing the metrics into a table
trainset_accuracy_rf[1,2] <- (rf_train_cf[["table"]][3] / (rf_train_cf[["table"]][3] + rf_train_cf[["table"]][1])) * 100 #fpr
trainset_accuracy_rf[1,3] <- (rf_train_cf[["table"]][2] / (rf_train_cf[["table"]][2] + rf_train_cf[["table"]][4])) * 100 #fnr

# False negatives are more significant! we are more afraid of the model predicting the person won't die when he will actually die in that condition!
trainset_accuracy_rf[1,4] <- ((rf_train_cf[["overall"]][1])) * 100#error
trainset_accuracy_rf[1,5] <- auc@y.values
trainset_accuracy_rf[1,6] <- (rf_train_cf[["table"]][4] / (rf_train_cf[["table"]][4] + (0.5 * (rf_train_cf[["table"]][3] + rf_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_rf[1,7] <- (rf_train_cf[["table"]][4] / (rf_train_cf[["table"]][4] + 0.2 * rf_train_cf[["table"]][3] + 0.8 * rf_train_cf[["table"]][2])) #f2 score

# Checking
trainset_accuracy_rf

#Testing our model on an unseen test set and plotting the confusion matrix
testset_prob <- predict(rf_trafficAccident1, type='prob', newdata = testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Yes > threshold, "Yes", "No")
rf_test_cf <- confusionMatrix(data = table(testset$Death, testset_pred,deparse.level = 2))

rf_test_cf

#getting the area under curve
pred = prediction(testset_prob[,2], testset$Death)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_rf[1,2] <- (rf_test_cf[["table"]][3] / (rf_test_cf[["table"]][3] + rf_test_cf[["table"]][1])) *100 #fpr
testset_accuracy_rf[1,3] <- (rf_test_cf[["table"]][2] / (rf_test_cf[["table"]][2] + rf_test_cf[["table"]][4])) * 100#fnr
testset_accuracy_rf[1,4] <- ((rf_test_cf[["overall"]][1])) * 100 #error
testset_accuracy_rf[1,5] <- auc@y.values
testset_accuracy_rf[1,6] <- (rf_test_cf[["table"]][4] / (rf_test_cf[["table"]][4] + (0.5 * (rf_test_cf[["table"]][3] + rf_test_cf[["table"]][2])))) #f1 score
testset_accuracy_rf[1,7] <- (rf_test_cf[["table"]][4] / (rf_test_cf[["table"]][4] + 0.2 * rf_test_cf[["table"]][3] + 0.8 * rf_test_cf[["table"]][2])) #f2 score

# Checking
testset_accuracy_rf

#--------------OPTIMISING RANDOM FOREST MODEL WITH OPTIMISED MTRY (random SUBSET FEATURES)----------------------#

# finding optimal mtry(optimal random subset feature) on the trainset using third party library
# result was 9, this will lag quite awhile if done on whole dataset
mtry <- tuneRF(trainset[,-39, with=FALSE],trainset$Death, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
mtry
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
best.m

# OPTIMISED RF MODEL

rf_trafficAccident2 <- randomForest(Death ~ . , mtry = best.m, data = trainset,importance = T, do.trace = TRUE)
rf_trafficAccident2

# Plot to see when the error rates settle down, it stabilises at about 250 trees
plot(rf_trafficAccident2)

# Variable Importance
var.impt <- importance(rf_trafficAccident2)
par(mfrow = c(1,2))
varImpPlot(rf_trafficAccident2, type = 1) 
varImpPlot(rf_trafficAccident2, type = 2)
par(mfrow = c(1,1))

#Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_trafficAccident2, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Yes > threshold, "Yes", "No")
rf_train_cf <- confusionMatrix(data = table(trainset$Death, trainset_pred, deparse.level = 2))
rf_train_cf[["table"]]

#getting the area under curve
pred = prediction(trainset_prob[,2], trainset$Death)
auc = performance(pred, measure = "auc")

# Storing the metrics into a table
trainset_accuracy_rf[1,2] <- (rf_train_cf[["table"]][3] / (rf_train_cf[["table"]][3] + rf_train_cf[["table"]][1])) * 100 #fpr
trainset_accuracy_rf[1,3] <- (rf_train_cf[["table"]][2] / (rf_train_cf[["table"]][2] + rf_train_cf[["table"]][4])) * 100 #fnr

# False negatives are more significant! we are more afraid of the model predicting the person won't die when he will actually die in that condition!
trainset_accuracy_rf[1,4] <- ((rf_train_cf[["overall"]][1])) * 100#error
trainset_accuracy_rf[1,5] <- auc@y.values
trainset_accuracy_rf[1,6] <- (rf_train_cf[["table"]][4] / (rf_train_cf[["table"]][4] + (0.5 * (rf_train_cf[["table"]][3] + rf_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_rf[1,7] <- (rf_train_cf[["table"]][4] / (rf_train_cf[["table"]][4] + 0.2 * rf_train_cf[["table"]][3] + 0.8 * rf_train_cf[["table"]][2])) #f2 score

# Checking
trainset_accuracy_rf

#Testing our model on an unseen test set and plotting the confusion matrix
testset_prob <- predict(rf_trafficAccident2, type='prob', newdata = testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Yes > threshold, "Yes", "No")
rf_test_cf <- confusionMatrix(data = table(testset$Death, testset_pred,deparse.level = 2))

rf_test_cf

#getting the area under curve
pred = prediction(testset_prob[,2], testset$Death)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_rf[1,2] <- (rf_test_cf[["table"]][3] / (rf_test_cf[["table"]][3] + rf_test_cf[["table"]][1])) *100 #fpr
testset_accuracy_rf[1,3] <- (rf_test_cf[["table"]][2] / (rf_test_cf[["table"]][2] + rf_test_cf[["table"]][4])) * 100#fnr
testset_accuracy_rf[1,4] <- ((rf_test_cf[["overall"]][1])) * 100 #error
testset_accuracy_rf[1,5] <- auc@y.values
testset_accuracy_rf[1,6] <- (rf_test_cf[["table"]][4] / (rf_test_cf[["table"]][4] + (0.5 * (rf_test_cf[["table"]][3] + rf_test_cf[["table"]][2])))) #f1 score
testset_accuracy_rf[1,7] <- (rf_test_cf[["table"]][4] / (rf_test_cf[["table"]][4] + 0.2 * rf_test_cf[["table"]][3] + 0.8 * rf_test_cf[["table"]][2])) #f2 score

# Checking
testset_accuracy_rf

# Optimised model improves by 0.4% accuracy for both test and train set.
# Final Accuracy: 86.8%

# Most important variables that decrease model accuracy by 10% when not included.
# Total of 18 variables
var.impt <- as.data.frame(var.impt)
sorted.var.impt <- var.impt[order(var.impt$MeanDecreaseAccuracy, decreasing = TRUE), ]
subset.var.impt <- subset(sorted.var.impt, MeanDecreaseAccuracy > 10)
nrow(subset.var.impt)

# Save as RDS File so that it doesn't need to be retrained. (Set working directory first)
# setwd()
saveRDS(rf_trafficAccident2, "randomForest.rds")
