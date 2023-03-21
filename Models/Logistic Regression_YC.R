library(data.table)
library(caTools)
library(car) #for vif function
library (nnet) #for multi-categorical Y (if necessary)4
install.packages("DescTools")
library (DescTools)
library(dplyr)
install.packages("caret")
library(caret)

#set working directory
setwd("C:/Users/tohyi/Documents/NBS/Year 3/Sem 2/BC2407 Analytics II/BC2407 Course Materials/Group Project/BC2407-Analytics-II-Project/Dataset")

#Read & Summarize Data
data <- fread("trafficAccident2020_cleaned.csv", stringsAsFactors = T)
summary(data)
View(data)
dim(data) #42 columns 20203 rows
data <- data %>% select(-c(ST_CASE, VEH_NO, PER_NO))
View(data)
dim(data) #39 columns 20203 rows

str(data) #to make sure each variable has the correct data type

# To check if there are any NA values in our data set
sum(is.na(data))
## 0 NAs in the data set

# To check baseline reference level for our Y variable, "Death"
levels(data$Death) #Baseline is Death = "No"


### Logistic Regression Modelling ==============================================

### lm1; Logistic Regression Model with all variables
lm1 <- glm(Death ~ ., family = binomial, data = data)
summary (lm1)
vif(lm1) 
## Vehicle_Configuration, Pre_Crash_Activity, Critical_Activity, Vehicle_Classification, Relation_To_Junction and Type_Of_Intersection 
## have GVIF > 10 [6 variables]


#===============================================================================

### lm1.BE; Backward Elimination on lm1
lm1.BE <- step(lm1)
summary (lm1.BE)
vif (lm1.BE)
## Vehicle_Configuration, Pre_Crash_Activity, Critical_Activity and Vehicle_Classification
## have GVIF > 10 [4 variables]


#===============================================================================

### lm2; Remove insignificant variables from lm1.BE
## Based on summary(lm1.BE), the insignificant variables are Seat_Position, Restraint_Equipment_Usage, Driver_Height, Driver_Weight, Month, Light_Condition
## are all insignificant variables (i.e. less than 2 *s)

lm2 = update (lm1.BE, ~. -Seat_Position-Restraint_Equipment_Usage-Driver_Height-Driver_Weight-Month-Light_Condition) 
summary(lm2)
## Roadway_Profile is an insignificant variable (i.e. less than 2 *s)
vif(lm2)
## Vehicle_Configuration, Pre_Crash_Activity, Critical_Activity and Vehicle_Classification
## have GVIF > 10 [4 variables]


#===============================================================================

###lm3; Remove insignificant variables from lm2

lm3 = update(lm2, ~. -Roadway_Profile)
summary(lm3) # Relation_To_Junction is an insignificant variable (i.e. less than 2 *s)
vif(lm3)
## Vehicle_Configuration, Critical_Activity and Vehicle_Classification
## have GVIF > 10 [3 variables]


#===============================================================================

###lm4; Remove insignificant variables from lm3

lm4 = update(lm3, ~. -Relation_To_Junction)
summary(lm4) # no more insignificant variables (i.e. all variables have at least 2 *s)
vif(lm4)
## Vehicle_Configuration, Critical_Activity and Vehicle_Classification
## have GVIF > 10 [3 variables] 


#===============================================================================

### Perform correlation to see which of the 3 variables to remove first.

### Perform correlation for Vehicle_Configuration & Critical_Activity
round(ContCoef(data$Vehicle_Configuration, data$Critical_Activity, correct=FALSE),3) #0.127
round(ContCoef(data$Vehicle_Configuration, data$Critical_Activity, correct=TRUE),3) #0.146

### Perform correlation for Vehicle_Configuration & Vehicle_Classification
round(ContCoef(data$Vehicle_Configuration, data$Vehicle_Classification, correct=FALSE),3) #0.756
round(ContCoef(data$Vehicle_Configuration, data$Vehicle_Classification, correct=TRUE),3) #0.873

### Perform correlation for Critical_Activity & Vehicle_Classification
round(ContCoef(data$Critical_Activity, data$Vehicle_Classification, correct=FALSE),3) #0.24
round(ContCoef(data$Critical_Activity, data$Vehicle_Classification, correct=TRUE),3) #0.252

## Conclusion: To remove Vehicle_Classification because:
## 1) It has a high correlation with Vehicle_Configuration; and
## 2) It has a higher correlation with Critical_Activity than Vehicle_Configuration with Critical_Activity.


#===============================================================================

###lm5; Remove Vehicle_Classification from lm4 because of the multicollinearity issue.

lm5 = update(lm4, ~. -Vehicle_Classification)
summary(lm5)# no more insignificant variables (i.e. all variables have at least 2 *s)
vif(lm5) 
## Only Critical_Activity has a GVIF > 10.


#===============================================================================

###lm6; Remove Critical_Activity from lm5 because of the multicollinearity issue.

lm6 = update(lm5, ~. -Critical_Activity)
summary(lm6) #no more insignificant variables (i.e. all variables have at least 2 *s)
vif(lm6) #all variables have GVIF < 10, i.e. no more multicollinearity issue.

### MODEL THAT WILL BE USED FOR TRAIN-TEST ###

#===============================================================================

### Train-test Split
set.seed(2407) # Generate a random number sequence that can be reproduced to verify results.

## 70% trainset. Stratify on Y = Death
train <- sample.split(Y = data$Death, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)


## Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$Death)
summary(testset$Death)
dim(trainset) # 14143 observations, 39 columns
dim(testset) # 6060 observations, 39 columns


#===============================================================================#

#generate dataframes to store random forest results

#generate empty data frame to store test set results
testset_accuracy_log <- data.frame('Model' = "Logistic Regression (Testset)", 'FPR' = 0, 'FNR' = 0, 
                                  'Acc' = 0, 'F1 Score' = 0, 'F2 Score' = 0)
testset_accuracy_log

#generate empty data frame to store train set results
trainset_accuracy_log <- data.frame('Model' = "Logistic Regression (Trainset)", 'FPR' = 0, 'FNR' = 0,
                                   'Acc' = 0, 'F1 Score' = 0,'F2 Score' = 0)
trainset_accuracy_log

#===============================================================================

### train_lm6; Develop Model on trainset

train_lm6 <- glm(Death ~ Age + Person_Type + Airbag + Drugs + Num_Of_Occupants + Hit_And_Run + Vehicle_Configuration + Num_Of_Lanes + Pre_Crash_Activity + Drinking + Driver_Distracted + Driver_Factors + Hour + Harmful_Event + Location_Of_Crash, family = binomial, data = trainset)
summary(train_lm6)

OR1 <- exp(coef(train_lm6))
OR1

OR1.CI <- exp(confint(train_lm6))
OR1.CI

#===============================================================================

### Confusion Matrix & Accuracy of Trainset

## Set threshold = 0.5
threshold <- 0.5

## Confusion Matrix on Trainset
prob.train <- predict(train_lm6, type ='response')
prob.train <- as.data.frame(prob.train)
train_lm6.predict <- ifelse(prob.train > threshold,"Yes","No")
log_train <- confusionMatrix(data = table(trainset$Death, train_lm6.predict, deparse.level = 2))
log_train[["table"]]

## PAST WORKING [PLS IGNORE]
#table1 <- table(trainset.Actual=trainset$Death, train_lm6.predict, deparse.level = 2)
#table1 ## table 1 is same as log_train!!!
#round(prop.table(table1),3)

##Overall Accuracy 
#mean(train_lm6.predict == trainset$Death)
#0.846 (3sf)


#===============================================================================

## False Positive & False Negative terminology:
# In this case, a positive case is death, and a negative case is no death.
# Therefore, a false positive is the test predict there is death, but actual = no death.
# A false negative is the test predict no death when there is actually a death.
# TP: 5725
# TN: 6238
# FP: 1419
# FN: 761


## Storing the metrics into a table
trainset_accuracy_log[1,2] <- (log_train[["table"]][3] / (log_train[["table"]][3] + log_train[["table"]][1])) * 100 #fpr -> FP/Total Number of Negatives
trainset_accuracy_log[1,3] <- (log_train[["table"]][2] / (log_train[["table"]][2] + log_train[["table"]][4])) * 100 #fnr -> FN /Total Number of Positives

# We should be more concerned about false negatives because these are cases that result in deaths too.
trainset_accuracy_log[1,4] <- ((log_train[["overall"]][1])) * 100#error
#trainset_accuracy_log[1,5] <- auc@y.values # no AUC
trainset_accuracy_log[1,5] <- (log_train[["table"]][4] / (log_train[["table"]][4] + (0.5 * (log_train[["table"]][3] + log_train[["table"]][2])))) #f1 score
trainset_accuracy_log[1,6] <- (log_train[["table"]][4] / (log_train[["table"]][4] + 0.2 * log_train[["table"]][3] + 0.8 * log_train[["table"]][2])) #f2 score

## Checking all the model evaluation metrics for the trainset
trainset_accuracy_log


#===============================================================================

### Confusion Matrix & Accuracy of Testset

#predict on the test date, which is testset
prob.test <- predict(train_lm6, newdata = testset, type ='response')
prob.test
train_lm6.predict.test <- ifelse (prob.test > threshold,"Yes","No")

log_test <- confusionMatrix(data = table(testset$Death, train_lm6.predict.test,deparse.level = 2))
log_test

## PAST WORKING [PLS IGNORE]
#table2 <- table (testset.Actual = testset$Death, train_lm6.predict.test, deparse.level = 2)
#table2
#round(prop.table(table2),3)

#Overall Accuracy
#mean(train_lm6.predict.test == testset$Death)
# 0.847 (3sf)

## Storing the metrics into a table
testset_accuracy_log[1,2] <- (log_test[["table"]][3] / (log_test[["table"]][3] + log_test[["table"]][1])) * 100 #fpr -> FP/Total Number of Negatives
testset_accuracy_log[1,3] <- (log_test[["table"]][2] / (log_test[["table"]][2] + log_test[["table"]][4])) * 100 #fnr -> FN /Total Number of Positives
testset_accuracy_log[1,4] <- ((log_test[["overall"]][1])) * 100#error
testset_accuracy_log[1,5] <- (log_test[["table"]][4] / (log_test[["table"]][4] + (0.5 * (log_test[["table"]][3] + log_test[["table"]][2])))) #f1 score
testset_accuracy_log[1,6] <- (log_test[["table"]][4] / (log_test[["table"]][4] + 0.2 * log_test[["table"]][3] + 0.8 * log_test[["table"]][2])) #f2 score

## Checking all the model evaluation metrics for the testset
testset_accuracy_log

#===============================================================================
#===============================================================================

### The subsequent lines of code are deployed to find answers to our next business problem regarding the identification of a high risk driver.







