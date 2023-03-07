# ========================================================================================================
# Purpose:        BC2407 Group Project Data Cleaning
# Seminar Group:  03              
# Team:           05                                         
# DOC:            06-03-2023
# Topics:         Data Cleaning
# Data:           trafficAccident.csv
#=========================================================================================================


#--------------------------------IMPORTING REQUIRED LIBRARIES-------------------------------------#

list.of.packages <- c("data.table", "RCurl", "tidyverse", "missForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(RCurl)
library(tidyverse)
library(missForest)

#-----------------------------------GETTING CSV FROM GITHUB----------------------------------------#

x <- getURL("https://raw.githubusercontent.com/weikangg/BC2407-Analytics-II-Project/main/Dataset/trafficAccident.csv")

#---------------------IMPORTING DATASET & GETTING AN OVERVIEW OF THE DATASET-----------------------#
trafficAccident2020.dt <- fread(text = x, stringsAsFactors = T)
trafficAccident2020.dt %>% glimpse()
trafficAccident2020.dt %>% head()
str(trafficAccident2020.dt)


#----------------------------------------DATA CLEANING---------------------------------------------#

# Renamed columns to names that are easily understandable 
setnames(trafficAccident2020.dt, c('AGE', 'SEX', 'PER_TYP', 'INJ_SEV', 'SEAT_POS', 'REST_USE', 'AIR_BAG', 'DRUGS', 'LOCATION', 'NUMOCCS', 'HIT_RUN', 'OWNER', 'V_CONFIG', 'HAZ_INV', 'TRAV_SP', 
                                   'DR_HGT', 'DR_WGT', 'PREV_SPD', 'SPEEDREL', 'VTRAFWAY', 'VNUM_LAN', 'VPROFILE', 'VPAVETYP', 'VSURCOND', 'P_CRASH1', 'P_CRASH2', 'DR_DRINK', 'VPICBODYCLASS',
                                   'DRDISTRACT', 'DRIVERRF', 'DAY', 'MONTH', 'DAY_WEEK', 'HOUR', 'HARM_EV', 'RELJCT2', 'TYP_INT', 'REL_ROAD', 'LGT_COND', 'WEATHER', 'Fatality Rate (%)'), 
                                 c('Age', 'Sex', 'Person_Type', 'Injury_Severity', 'Seat_Position', 'Restraint_Equipment_Usage', 'Airbag', 'Drugs', 'Non_Motorist_Location', 'Num_Of_Occupants',
                                   'Hit_And_Run', 'Owner_Type', 'Vehicle_Configuration', 'Hazardous_Mat_Involvement', 'Travel_Speed', 'Driver_Height', 'Driver_Weight', 'Previous_Speed_Convictions',
                                   'Speeding_Related','Trafficway_Flow', 'Num_of_Lanes', 'Roadway_Profile', 'Pavement_Type', 'Roadway_Surface_Condition','Pre_Crash_Activity', 'Critical_Activity',
                                   'Drinking', 'Vehicle_Classification', 'Driver_Distracted', 'Driver_Factors', 'Day','Month', 'Day_Of_Week', 'Hour', 'Harmful_Event','Relation_To_Junction', 'Type_Of_Intersection',
                                   'Location_Of_Crash', 'Light_Condition', 'Weather','Fatality_Rate'))

# Dropped irrelevant Columns
trafficAccident2020.dt[, c('ST_CASE', 'VEH_NO', 'PER_NO', 'YEAR', 'RUR_URB'):=NULL]
str(trafficAccident2020.dt)

# Total of 34 Categorical X Variables, 6 Continuous X Variables and 1 Y Variable = 41 Variables.
sum(sapply(trafficAccident2020.dt, is.factor))
str(trafficAccident2020.dt)

# Defining the mode helper function since R doesn't have inbuilt mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#=======================#
# CLEANING 1. AGE
#=======================#

trafficAccident2020.dt[Age == 0, ] # 0 incidences

trafficAccident2020.dt[Age == 97, ] # 4 incidences (97 or older)
# Since it's only 4 incidences, we assume all 4 cases are when the individual involved is 97 years old.

trafficAccident2020.dt[Age == 99, ] # 2 incidences (Unknown)
# Since it's only 2 incidences, we will replace with the median in this case.
trafficAccident2020.dt$Age[which(trafficAccident2020.dt$Age == 99)] <- median(trafficAccident2020.dt$Age)

# Checking (No more incidences)
trafficAccident2020.dt[Age == 99, ]


#=======================#
# CLEANING 2. SEX
#=======================#

trafficAccident2020.dt[Sex %in% c(8,9), ] # 1018 incidences (Not Reported/Unknown Sex) ~= 5% of data, we choose to replace it with the mode of the column instead.
1018/nrow(trafficAccident2020.dt) * 100


# Convert to integer to get Mode first
trafficAccident2020.dt$Sex <- as.integer(trafficAccident2020.dt$Sex)

# Getting no of values for each level, most of them seem to be male in the dataset. Replace with Mode.
trafficAccident2020.dt %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex))


# Getting the Mode
mode_Sex <- mode(trafficAccident2020.dt$Sex)
mode_Sex

# Replacing with Mode and Changing 1 & 2 to Male and Female

trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 8] <- mode_Sex 
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 9] <- mode_Sex
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 1] <- 'M'
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 2] <- 'F' 


# Convert back to factor
trafficAccident2020.dt$Sex <- as.factor(trafficAccident2020.dt$Sex)

# Checking
trafficAccident2020.dt %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex))

#========================#
# CLEANING 3. Person_Type
#========================#

#============================#
# CLEANING 4. Injury_Severity
#============================#

# Getting no of values for each level, most of them seem to be male in the dataset. Replace with Mode.
trafficAccident2020.dt %>% 
  group_by(Injury_Severity) %>%
  summarise(no_rows = length(Injury_Severity))

# Reclassify level 0,1,2 as Light Injury and level 3,4,5,6 as Fatal Injury
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 0] <- 'Light'
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 1] <- 'Light' 
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 2] <- 'Light' 
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 3] <- 'Fatal' 
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 4] <- 'Fatal' 
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 5] <- 'Fatal' 
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 6] <- 'Fatal' 

# Replacing "Unknown/Not Reported" Category
trafficAccident2020.dt[Injury_Severity %in% c(9), ] # 822 incidences, < 5% of no of rows. Hence, we simply replace with mode.
822/nrow(trafficAccident2020.dt) * 100

# Getting the Mode
mode_Inj_Sev <- mode(trafficAccident2020.dt$Injury_Severity)
mode_Inj_Sev

# Replacing with mode
trafficAccident2020.dt$Injury_Severity[trafficAccident2020.dt$Injury_Severity == 9] <- 'Fatal' 

# Checking
trafficAccident2020.dt %>% 
  group_by(Injury_Severity) %>%
  summarise(no_rows = length(Injury_Severity))

#==========================#
# CLEANING 5. Seat_Position
#==========================#

#======================================#
# CLEANING 6. Restraint_Equipment_Usage
#======================================#

#========================#
# CLEANING 7. Airbag
#========================#

#========================#
# CLEANING 8. Drugs
#========================#

# Getting no of values for each level, most of them seem to be male in the dataset. Replace with Mode.
trafficAccident2020.dt %>% 
  group_by(Drugs) %>%
  summarise(no_rows = length(Drugs))

library(missForest)

missForest_imputed <- data.frame(
  original = trafficAccident2020.dt$Drugs,
  imputed_missForest = missForest(trafficAccident2020.dt)$ximp$Drugs
)
missForest_imputed
#==================================#
# CLEANING 9. Non_Motorist_Location
#==================================#

#==============================#
# CLEANING 10. Num_Of_Occupants
#==============================#

#=========================#
# CLEANING 11. Hit_And_Run
#=========================#

#========================#
# CLEANING 12. Owner_Type
#========================#

#===================================#
# CLEANING 13. Vehicle_Configuration
#===================================#

#=======================================#
# CLEANING 14. Hazardous_Mat_Involvement
#=======================================#

#==========================#
# CLEANING 15. Travel_Speed
#==========================#

#===========================#
# CLEANING 16. Driver_Height
#===========================#

#===========================#
# CLEANING 17. Driver_Weight
#===========================#

#========================================#
# CLEANING 18. Previous_Speed_Convictions
#========================================#

#==============================#
# CLEANING 19. Speeding_Related
#==============================#

#=============================#
# CLEANING 20. Trafficway_Flow
#=============================#

#==========================#
# CLEANING 21. Num_Of_Lanes
#==========================#

#=============================#
# CLEANING 22. Roadway_Profile
#=============================#

#===========================#
# CLEANING 23. Pavement_Type
#===========================#

#=======================================#
# CLEANING 24. Roadway_Surface_Condition
#=======================================#

#================================#
# CLEANING 25. Pre_Crash_Activity
#================================#

#===============================#
# CLEANING 26. Critical_Activity
#===============================#

#========================#
# CLEANING 27. Drinking
#========================#

#====================================#
# CLEANING 28. Vehicle_Classification
#====================================#

#==============================#
# CLEANING 29. Driver_Distracted
#==============================#

#============================#
# CLEANING 30. Driver_Factors
#============================#

#========================#
# CLEANING 31. Day
#========================#

#========================#
# CLEANING 32. Month
#========================#

#=========================#
# CLEANING 33. Day_Of_Week
#=========================#

#========================#
# CLEANING 34. Hour
#========================#

#===========================#
# CLEANING 35. Harmful_Event
#===========================#

#==================================#
# CLEANING 36. Relation_To_Junction
#==================================#

#==================================#
# CLEANING 37. Type_Of_Intersection
#==================================#

#===============================#
# CLEANING 38. Location_Of_Crash
#===============================#

#=============================#
# CLEANING 39. Light_Condition
#=============================#

#========================#
# CLEANING 40. Weather
#========================#

#===========================#
# CLEANING 41. Fatality_Rate
#===========================#

# Factorise Categorical Columns
cols <- c("Sex", "Person_Type", "Injury_Severity", "Seat_Position",'Restraint_Equipment_Usage', 'Airbag', 'Drugs', 'Non_Motorist_Location', 'Hit_And_Run', 'Owner_Type', 'Vehicle_Configuration',
          'Hazardous_Mat_Involvement', 'Speeding_Related','Trafficway_Flow', 'Num_of_Lanes', 'Roadway_Profile', 'Pavement_Type', 'Roadway_Surface_Condition', 'Pre_Crash_Activity', 'Critical_Activity',
          'Drinking', 'Vehicle_Classification', 'Driver_Distracted', 'Driver_Factors', 'Day', 'Month', 'Day_Of_Week', 'Hour', 'Harmful_Event', 'Relation_To_Junction', 'Type_Of_Intersection', 'Location_Of_Crash',
          'Light_Condition', 'Weather')
setDT(trafficAccident2020.dt)[, (cols):= lapply(.SD, factor), .SDcols=cols]
sapply(trafficAccident2020.dt, class)

# Save cleaned dataset (Change Working Directory to Relevant directories)
write.csv(trafficAccident2020.dt,"C:/Users/Wei Kang/OneDrive - Nanyang Technological University/Year 2/Sem 2/BC2407 Analytics II/B2407 Course Materials/Group Project/BC2407-Analytics-II-Project/Data Cleaning/trafficAccident2020_cleaned.csv", row.names = FALSE)
