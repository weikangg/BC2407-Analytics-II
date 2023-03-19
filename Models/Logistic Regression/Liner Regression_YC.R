library(data.table)
library(caTools)
library(car) #for vif function

#set working directory
setwd("C:/Users/tohyi/Documents/NBS/Year 3/Sem 2/BC2407 Analytics II/BC2407 Course Materials/Group Project/BC2407-Analytics-II-Project/Dataset")


#Read & Summarize Data
data <- fread("trafficAccident2020_cleaned.csv", stringsAsFactors = T)
summary(data)
View(data)
dim(data) #39 columns & 20203 rows


# To check if there are any NA values in our dataset
sum(is.na(data))
## 0 NAs in the dataset

#?????? Factorize all categorical variables [Do we need to factorize still?]


### Linear Regression Modelling ================================================

### lm1; Linear Regression Model with all variables
lm1 <- lm(Fatality_Rate ~ ., data = data) 
summary(lm1) 
## R-squared: 0.6343, Adjusted R-squared: 0.6322
vif (lm1) 
## Vehicle_Configuration, Critical_Activity, Vehicle_Classification, Harmful_Event, Relation_To_Junction & Type_Of_Intersection
## have VIFs > 10 [6 Variables]

#===============================================================================

### lm1.BE; Backward Elimination on lm1
lm1.BE <- step(lm1)
summary(lm1.BE)
## R-squared: 0.6338, Adjusted R-squared: 0.6322
vif(lm1.BE)
## Vehicle_Configuration, Critical_Activity, Vehicle_Classification & Harmful_Event have VIFs > 10 [4 Variables]

#===============================================================================

### lm2; Remove insignificant variables from lm1.BE
## Based on summary(lm1.BE), Restraint_Equipment, Drugs, Hazardous_Mat_Involvement, Driver_Height, Vehicle_Classification 
## are all insignificant variables (i.e. less than 2 *s)

# update function not working 
#lm2 = update (lm1.BE, ~. -Restraint_Equipment-Hazardous_Mat_Involvement-Driver_Height-Vehicle_Classification) 
#summary(lm2)
#?update()

lm2 <- lm(Fatality_Rate ~ Age + Airbag + Num_Of_Occupants + Hit_And_Run + Vehicle_Configuration + Trafficway_Flow + Num_Of_Lanes + Pre_Crash_Activity + Critical_Activity + Drinking + Driver_Distracted + Driver_Factors + Hour + Harmful_Event + Relation_To_Junction + Light_Condition + Weather, data = data)
summary(lm2)
## no insignificant variables (i.e. all variables have >= 2 *s)
vif(lm2)
## Critical_Activity & Harmful_Event have VIFs > 10 [2 variables]

#===============================================================================

### Model Diagnostic Plots for lm2
par(mfrow = c(2,2)) # Plot 4 charts in one plot - 2 by 2.
plot(lm2)  # Plot model 4 diagnostics
par(mfrow = c(1,1)) # Reset plot options to 1 chart in one plot.
# However, as we can see from the model diagnostic plots for lm2.
# the red trend line in our Residual vs Fitted Graph is not 
# close to zero and it is not horizontal, although it ought to be. Furthermore, 
# as evident in the graph, we are seeing all the points converging at the right side.
# Moreover, the scale-location plot may also present  some problems of heteroscedasticity as suggested by the sloping line due to the
# non-constant variance in the residual errors.  
# Thus, this may potentially violate the assumption of linearity between the predictors and outcome variables.

#===============================================================================

### Train-test Split
set.seed(2407) # Generate a random number sequence that can be reproduced to verify results.

## 70% trainset. Stratify on Y = Fatality_Rate
train <- sample.split(Y = data$Fatality_Rate, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

## Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$Fatality_Rate)
summary(testset$Fatality_Rate)
dim(trainset) # 14142 observations, 39 columns
dim(testset) # 6061 observations, 39 columns

#===============================================================================

### train_lm2; Develop Model on trainset
train_lm2 <- lm(Fatality_Rate ~ Age + Airbag + Num_Of_Occupants + Hit_And_Run + Vehicle_Configuration + Trafficway_Flow + Num_Of_Lanes + Pre_Crash_Activity + Critical_Activity + Drinking + Driver_Distracted + Driver_Factors + Hour + Harmful_Event + Relation_To_Junction + Light_Condition + Weather, data = trainset)
summary(train_lm2)
residuals(train_lm2) 

# Residuals = Error = Actual Fatality Rate - Model Predicted Fatality Rate
RMSE.train_lm2 <- sqrt(mean(residuals(train_lm2)^2))  # RMSE on trainset based on train_lm2 model
RMSE.train_lm2 #16.37726
summary(abs(residuals(train_lm2)))  # Check Min Abs Error and Max Abs Error.

#===============================================================================

### Apply model from trainset to predict on testset.
predict.train_lm2.test <- predict(train_lm2, newdata = testset)
testset.error <- testset$Fatality_Rate - predict.train_lm2.test

# Testset Errors
RMSE.train_lm2.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

#===============================================================================

### Linear Regression Model Results
RMSE.train_lm2 #16.37726
RMSE.train_lm2.test #16.62142

