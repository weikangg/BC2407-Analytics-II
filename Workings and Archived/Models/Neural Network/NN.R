# ========================================================================================================
# Purpose:      Rscript for Neuralnet demo.
# Author:       Neumann
# DOC:          15-03-2018
# Topics:       Neural Network with scaled continuous variables and dummied categorical X
# Data Source:  infert in base package datasets
#=========================================================================================================

library(caTools)            # Train-Test Split
library(neuralnet)
library(fastDummies)
library(tidyverse)
set.seed(1)  # for random starting weights


setwd("C:/Users/Loy/Documents/GitHub/BC2407-Analytics-II-Project/Models")
data1 <- read.csv("C:/Users/Loy/Documents/GitHub/BC2407-Analytics-II-Project/Dataset/trafficAccident2020_cleaned.csv", stringsAsFactors = T)
data1
# Filtering out Categorical Features. ------------------------
cat_vars <- sapply(data1, is.factor)


# Change the Baseline Reference level for All Categorical Features.
for (var in names(data1)[cat_vars]) {
  data1[[var]] <- factor(data1[[var]], levels = unique(data1[[var]]))
}


#Baselines 

cat_levels <- lapply(data1[, cat_vars], levels)
cat_levels

# Normalizing -----------------------------------------------
int_cols <- sapply(data1, is.integer)
data1[,int_cols] <- scale(data1[, int_cols])
data1[,"Driver_Height"] <- scale(data1[, "Driver_Height"])
data1[,"Driver_Weight"] <- scale(data1[, "Driver_Weight"])
data1[,"Travel_Speed"] <- scale(data1[, "Travel_Speed"])
deathdata=subset(data1, select = c('Death') )
ncol(data1)
data1 = data1[-c(42)]
deathdata$death = ifelse(deathdata$Death=="Yes",1,0)
deathdata = deathdata[-c(1)]
deathdata
# Neuralnet cannot handle factors, unlike nnet. Thus need to create dummy variables , automated using fastdummies
# Create dummy variables for all categorical columns
df_dummies <- dummy_cols(data1)
colnames(df_dummies)
df_dummies

# Dropping all factor columns
new_df = df_dummies[, sapply(df_dummies, class) != "factor"]
new_df = cbind(new_df,deathdata)

new_df_clean <- new_df %>% 
  drop_na()

# Train-test split ---------------------------------------------------------
set.seed(1)
train <- sample.split(Y=new_df_clean$death, SplitRatio = 0.7)
trainset <- subset(new_df, train==T)
testset <- subset(new_df, train==F)

#m2 <- neuralnet(Fatality_Rate~.- Fatality_Rate, data=trainset, hidden=2, err.fct="ce", linear.output=TRUE)
## Stop adjusting weights when all gradients of the error function were smaller than 0.01 (the default threshold).
summary(is.na(trainset))

# Neural Network comprising 1 hidden layer with 2 hidden nodes for binary categorical target
m1 <- neuralnet(death~.- death, data=trainset, hidden=2, err.fct="ce", linear.output=FALSE)


par(mfrow=c(1,1))
plot(m1)

m1$net.result  # predicted outputs. 

# make predictions with the trained neural network model
predictions <- predict(m1, testset)

# apply an ifelse condition to all elements in the predictions matrix
#predictions <- apply(predictions, c(1, 2), function(x) ifelse(x > 100, 100, x))
predictions<-ifelse(unlist(m1$net.result)>0.5,1,0)

predictions
# The generalized weight is defined as the contribution of the ith input variable to the log-odds:
m1$generalized.weights

# NeuralNet w 2 neurons in one hidden layers RMSE
#rmse <- sqrt(mean((testset$Fatality_Rate - predictions)^2))
#rmse
cat('Trainset Confusion Matrix')
table(infert$case,predictions)