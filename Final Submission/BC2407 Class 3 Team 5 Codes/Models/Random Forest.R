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
library(cvms)

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

# Looking at whether class of Y Variable is balanced
ggplot(data=trafficAccident2020.dt,aes(Death,fill=Death))+
  geom_bar()+
  labs(title = "Death Bar Plot", x = "Death", y = "Count") 
# Percentages
trafficAccident2020.dt %>% 
  group_by(Death) %>%
  summarise(no_rows = length(Death), percentage = length(Death)/nrow(trafficAccident2020.dt) * 100)


# Feature selection
boruta.results <- Boruta(Death ~., data = trafficAccident2020.dt, doTrace = 2)
print(boruta.results)
boruta.results$finalDecision

# Results of Feature Selection
# Boruta performed 99 iterations in 10.48033 mins.
# 32 attributes confirmed important: Age, Airbag, Critical_Activity, Drinking, Driver_Distracted and 27 more;
# 4 attributes confirmed unimportant: Day, Day_Of_Week, Hazardous_Mat_Involvement, Month;
# 2 tentative attributes left: Pavement_Type, Previous_Speed_Convictions;

# Re-leveling of Death factor
levels(trafficAccident2020.dt$Death)
trafficAccident2020.dt$Death <- relevel(trafficAccident2020.dt$Death, ref = "No")
levels(trafficAccident2020.dt$Death)

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

#Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_trafficAccident1, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_prob
trainset_pred <- ifelse(test = trainset_prob$Yes > threshold, "Yes", "No")
trainset$Death %>% summary()
trainset_pred %>% glimpse()
rf_train_cf <- confusionMatrix(data = table(trainset$Death, trainset_pred, deparse.level = 2), positive = "Yes")
rf_train_cf
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

# Computing train set predictions and plotting of confusion matrix
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
rf_test_cf <- confusionMatrix(data = table(testset$Death, testset_pred,deparse.level = 2), positive = 'Yes')

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

table1 <- table(Testset.Actual =  testset$Death, testset_pred, deparse.level = 2)
table1
cfm1 <- as_tibble(table1)
cfm1
# Confusion matrix
plot_confusion_matrix(cfm1, 
                      target_col = "Testset.Actual", 
                      prediction_col = "testset_pred",
                      counts_col = "n",
                      place_x_axis_above = FALSE,
                      add_col_percentages = FALSE,
                      add_row_percentages = FALSE)


# Optimised model improves by 0.4% accuracy for both test and train set.
# Final Accuracy: 86.8%

# Most important variables that decrease model accuracy by 10% when not included.
# Total of 18 variables
var.impt <- as.data.frame(var.impt)
sorted.var.impt <- var.impt[order(var.impt$MeanDecreaseAccuracy, decreasing = TRUE), ]
subset.var.impt <- subset(sorted.var.impt, MeanDecreaseAccuracy > 10)
subset.var.impt
nrow(subset.var.impt)

#extracting variables names for top 20 RF variables
impvar_under <- rownames(var.impt)[order(var.impt[,3], decreasing=TRUE)][1:20]
impvar_under

#plotting the top 20 variables partial dependence plots
par(mfrow=c(2, 2))

for (i in 1:2) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}
# no seatbelt or restraint equipment, die

par(mfrow=c(2, 2))
for (i in 3:4) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

par(mfrow=c(2, 2))
for (i in 5:6) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

par(mfrow=c(2, 2))
for (i in 7:8) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

par(mfrow=c(2, 2))
for (i in 9:10) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

par(mfrow=c(2, 2))
for (i in 11:12) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}


par(mfrow=c(2, 2))
for (i in 13:14) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}


par(mfrow=c(2, 2))
for (i in 15:16) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

par(mfrow=c(2, 2))
for (i in 17:18) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

par(mfrow=c(2, 2))
for (i in 19:20) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

# plot id 1,2,3,5,8,13
plotid = c(1,2,3,5,8,13)
par(mfrow=c(2, 3))
for (i in plotid) {
  partialPlot(rf_trafficAccident2, testset, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Yes", las = 3)
}

#reset plot settings
par(mfrow=c(1,1))

#----------------RANDOM FOREST MODEL WITH ONLY KEY PREDICTORS----------------------#

key_predictor_variables_train <- trainset[,c(1,5,8,9,28,33,39)]
key_predictor_variables_train %>% glimpse()

key_predictor_variables_test <- testset[,c(1,5,8,9,28,33,39)]
key_predictor_variables_test %>% glimpse()

mtry <- tuneRF(key_predictor_variables_train[,-7, with=FALSE],key_predictor_variables_train$Death, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
mtry
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
best.m

# OPTIMISED RF MODEL
set.seed(2000)
rf_trafficAccident3 <- randomForest(Death ~ . , mtry = best.m, data = key_predictor_variables_train,importance = T, do.trace = TRUE)
rf_trafficAccident3

# Plot to see when the error rates settle down, it stabilises at about 250 trees
plot(rf_trafficAccident3)

# Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_trafficAccident3, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Yes > threshold, "Yes", "No")
class(trainset_pred)
class(key_predictor_variables_train$Death)
rf_train_cf <- confusionMatrix(data = table(key_predictor_variables_train$Death, trainset_pred, deparse.level = 2))
rf_train_cf

#getting the area under curve
pred = prediction(trainset_prob[,2], key_predictor_variables_train$Death)
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
testset_prob <- predict(rf_trafficAccident3, type='prob', newdata = key_predictor_variables_test)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Yes > threshold, "Yes", "No")
rf_test_cf <- confusionMatrix(data = table(key_predictor_variables_test$Death, testset_pred,deparse.level = 2), positive = 'Yes')

rf_test_cf

#getting the area under curve
pred = prediction(testset_prob[,2], key_predictor_variables_test$Death)
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

table1 <- table(Testset.Actual =  key_predictor_variables_test$Death, testset_pred, deparse.level = 2)
table1
cfm1 <- as_tibble(table1)
cfm1
# Confusion matrix
plot_confusion_matrix(cfm1, 
                      target_col = "Testset.Actual", 
                      prediction_col = "testset_pred",
                      counts_col = "n",
                      place_x_axis_above = FALSE,
                      add_col_percentages = FALSE,
                      add_row_percentages = FALSE)


#-----------------------------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------END OF ANALYSIS----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------------#
