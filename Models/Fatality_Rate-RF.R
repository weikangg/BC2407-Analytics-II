# ========================================================================================================
# Purpose:      RF on Resale Flat Price
# Author:       Neumann Chew
# DOC:          12-03-2020
# Topics:       Random Forest
# Data:         resale-flat-prices-2019.csv from Housing & Dev Board.
#=========================================================================================================

library(caTools)            # Train-Test Split
library(earth)              # MARS
library(randomForest)       # Random Forest

setwd("C:/Users/Loy/Documents/GitHub/BC2407-Analytics-II-Project/Models")

data1 <- read.csv("C:/Users/Loy/Documents/GitHub/BC2407-Analytics-II-Project/Dataset/trafficAccident2020_cleaned.csv", stringsAsFactors = T)

sum(is.na(data1))
## verifies no missing values


# Filtering out Categorical Features. ------------------------
cat_vars <- sapply(data1, is.factor)



# Change the Baseline Reference level for All Categorical Features.
for (var in names(data1)[cat_vars]) {
  data1[[var]] <- factor(data1[[var]], levels = unique(data1[[var]]))
}
 

#Baselines 

cat_levels <- lapply(data1[, cat_vars], levels)
cat_levels

# Train-test split ---------------------------------------------------------
set.seed(1)
train <- sample.split(Y=data1$Fatality_Rate, SplitRatio = 0.7)
trainset <- subset(data1, train==T)
testset <- subset(data1, train==F)


# MARS degree 2 -------------------------------------------------------------
m.mars2 <- earth(Fatality_Rate ~ ., degree=2, data=trainset)

m.mars2.yhat <- predict(m.mars2, newdata = testset)

RMSE.test.mars2 <- round(sqrt(mean((testset$resale_price - m.mars2.yhat)^2)))

# Estimated Variable Importance in degree 2 MARS
var.impt.mars <- evimp(m.mars2)
print(var.impt.mars)
## Location_Of_CrashOff_Roadway is relatively most impt, followed by Num_Of_Occupants(No surprise , it is very correlated to survival rate).


# RF at default settings of B = 500 , RSF = 12, data = all_data to compare OOB error w other para for RF ------------------------------------- 
m.RF.1 <- randomForest(Fatality_Rate ~ . , data = data1, 
                       na.action = na.omit, 
                       importance = T)
m.RF.1

###Variable Importance on Trainset
var.impt <- importance(m.RF.1)

varImpPlot(m.RF.1, type = 1)

#Trainset Error 
# RF at default settings trained on 70% split train data
m.RF.test <- randomForest(Fatality_Rate ~ . , data = trainset, 
                       na.action = na.omit, 
                       importance = T)
m.RF.test

###Variable Importance on Trainset
var.impt <- importance(m.RF.test)

varImpPlot(m.RF.test, type = 1)

#Test
m.RF.test.yhat <- predict(m.RF.test, newdata = testset)
RMSE.test.RF <- round(sqrt(mean((testset$Fatality_Rate - m.RF.test.yhat)^2)))


