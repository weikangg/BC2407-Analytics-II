library(earth)
library(dplyr)
library(caret)
library(vip)
library(pdp)
library(caTools)
library(data.table)

data1 = read.csv("C:/Users/ASUS/OneDrive - Nanyang Technological University/Documents/BC2407-Analytics-II-Project/Dataset/trafficAccident2020_cleaned.csv", stringsAsFactors = T)
data1=as.data.table(data1)

# Dropping Irrelevant Columns
data1 = subset(data1, select = -c(ST_CASE, VEH_NO, PER_NO))

# Categorising Relevant Numerical Columns
cols <- c("Day", "Month", "Day_Of_Week", "Hour")
data1[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
summary(data1)

#Train-Test Split
set.seed(888)
train <- sample.split(Y = data1$Death, SplitRatio = 0.7)
trainset <- subset(data1, train == T)
testset <- subset(data1, train == F)
threshold = 0.5

#Table of Results
columnName = c("Number of Degree","FPR","FNR", "Accuracy", "F1 Score", "F2 Score") 
results.df = data.frame(matrix(nrow = 0, ncol = length(columnName)))
colnames(results.df) = columnName

#1 Degree Mars Model
m.mars1 <- earth(Death ~ . , degree=1, data=trainset, glm = list(family=binomial))
summary(m.mars1)

prob.test <- predict(m.mars1, newdata = testset, type = 'response')
data.frame(prob.test)
Predicted_Testset <- ifelse(prob.test > threshold, 1, 0)
result1 <- table(Actual_Testset = testset$Death, Predicted_Testset, deparse.level = 2)
result1

fnr = (result1[2,"0"] / (result1[2,"0"] + result1[2,"1"])) * 100
fpr = (result1[1,"1"] / (result1[1,"1"] + result1[1,"0"])) * 100
acc = (result1[1,"0"] + result1[2,"1"])/ (result1[1,"0"] + result1[1,"1"] + result1[2,"0"] + result1[2,"1"]) * 100
precision = result1[2,"1"]/ (result1[2,"1"] + result1[1,"1"])
recall = result1[2,"1"]/ (result1[2,"1"] + result1[2,"0"])
f1 = 2 * ((precision*recall)/(precision+recall))
f2 = 5 * ((precision*recall)/((4*precision)+recall))
results.df[nrow(results.df)+1,] = c(1,fpr,fnr,acc,f1,f2)
results.df

#2 Degree Mars Model
m.mars2 <- earth(Death ~ . , degree=2, data=trainset, glm = list(family=binomial))
summary(m.mars2)

prob.test <- predict(m.mars2, newdata = testset, type = 'response')
data.frame(prob.test)
Predicted_Testset <- ifelse(prob.test > threshold, 1, 0)
result2 <- table(Actual_Testset = testset$Death, Predicted_Testset, deparse.level = 2)
result2

fnr = (result2[2,"0"] / (result2[2,"0"] + result2[2,"1"])) * 100
fpr = (result2[1,"1"] / (result2[1,"1"] + result2[1,"0"])) * 100
acc = (result2[1,"0"] + result2[2,"1"])/ (result2[1,"0"] + result2[1,"1"] + result2[2,"0"] + result2[2,"1"]) * 100
precision = result2[2,"1"]/ (result2[2,"1"] + result2[1,"1"])
recall = result2[2,"1"]/ (result2[2,"1"] + result2[2,"0"])
f1 = 2 * ((precision*recall)/(precision+recall))
f2 = 5 * ((precision*recall)/((4*precision)+recall))
results.df[nrow(results.df)+1,] = c(2,fpr,fnr,acc,f1,f2)
results.df

#3 Degree Mars Model
m.mars3 <- earth(Death ~ . , degree=3, data=trainset, glm = list(family=binomial))
summary(m.mars3)

prob.test <- predict(m.mars3, newdata = testset, type = 'response')
data.frame(prob.test)
Predicted_Testset <- ifelse(prob.test > threshold, 1, 0)
result3 <- table(Actual_Testset = testset$Death, Predicted_Testset, deparse.level = 2)
result3

fnr = (result3[2,"0"] / (result3[2,"0"] + result3[2,"1"])) * 100
fpr = (result3[1,"1"] / (result3[1,"1"] + result3[1,"0"])) * 100
acc = (result3[1,"0"] + result3[2,"1"])/ (result3[1,"0"] + result3[1,"1"] + result3[2,"0"] + result3[2,"1"]) * 100
precision = result3[2,"1"]/ (result3[2,"1"] + result3[1,"1"])
recall = result3[2,"1"]/ (result3[2,"1"] + result3[2,"0"])
f1 = 2 * ((precision*recall)/(precision+recall))
f2 = 5 * ((precision*recall)/((4*precision)+recall))
results.df[nrow(results.df)+1,] = c(3,fpr,fnr,acc,f1,f2)
results.df

#4 Degree Mars Model
m.mars4 <- earth(Death ~ . , degree=4, data=trainset, glm = list(family=binomial))
summary(m.mars4)

prob.test <- predict(m.mars4, newdata = testset, type = 'response')
data.frame(prob.test)
Predicted_Testset <- ifelse(prob.test > threshold, 1, 0)
result4 <- table(Actual_Testset = testset$Death, Predicted_Testset, deparse.level = 2)
result4

fnr = (result4[2,"0"] / (result4[2,"0"] + result4[2,"1"])) * 100
fpr = (result4[1,"1"] / (result4[1,"1"] + result4[1,"0"])) * 100
acc = (result4[1,"0"] + result4[2,"1"])/ (result4[1,"0"] + result4[1,"1"] + result4[2,"0"] + result4[2,"1"]) * 100
precision = result4[2,"1"]/ (result4[2,"1"] + result4[1,"1"])
recall = result4[2,"1"]/ (result4[2,"1"] + result4[2,"0"])
f1 = 2 * ((precision*recall)/(precision+recall))
f2 = 5 * ((precision*recall)/((4*precision)+recall))
results.df[nrow(results.df)+1,] = c(4,fpr,fnr,acc,f1,f2)
results.df

  #Highest F2 Score = 0.8638669, Degree = 1

#Using the optimal model, where degree=1, with Cross-Validation
m.mars1CV <- earth(Death ~ . , degree = 1, pmethod = "cv", nfold = 10, ncross = 1, data=trainset, glm = list(family=binomial))
summary(m.mars1CV)

prob.test <- predict(m.mars1CV, newdata = testset, type = 'response')
data.frame(prob.test)
Predicted_Testset <- ifelse(prob.test > threshold, 1, 0)
result1CV <- table(Actual_Testset = testset$Death, Predicted_Testset, deparse.level = 2)
result1CV

fnr = (result1CV[2,"0"] / (result1CV[2,"0"] + result1CV[2,"1"])) * 100
fpr = (result1CV[1,"1"] / (result1CV[1,"1"] + result1CV[1,"0"])) * 100
acc = (result1CV[1,"0"] + result1CV[2,"1"])/ (result1CV[1,"0"] + result1CV[1,"1"] + result1CV[2,"0"] + result1CV[2,"1"]) * 100
precision = result1CV[2,"1"]/ (result1CV[2,"1"] + result1CV[1,"1"])
recall = result1CV[2,"1"]/ (result1CV[2,"1"] + result1CV[2,"0"])
f1 = 2 * ((precision*recall)/(precision+recall))
f2 = 5 * ((precision*recall)/((4*precision)+recall))
results.df[nrow(results.df)+1,] = c(1,fpr,fnr,acc,f1,f2, "With CV")
results.df

  #No Change in F2 Score = 0.8638669, Degree = 1 (With CV)

varimpt <- evimp(m.mars1CV)
print(varimpt)


#-----------------
#Parameter Tuning Testing --> to validate the choice of degree and nprune
#-----------------
hyper_grid <- expand.grid(
  degree = 1:10,
  nprune = seq(2, 50, length.out = 10) %>% floor()
)

head(hyper_grid)

cv_mars <- train(
  x = subset(data1, select = -Death),
  y = data1$Death,
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
