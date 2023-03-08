# ========================================================================================================
# Purpose:        BC2407 Group Project Data Cleaning
# Seminar Group:  03              
# Team:           05                                         
# DOC:            06-03-2023
# Topics:         Data Cleaning
# Data:           trafficAccident.csv
#=========================================================================================================


#--------------------------------IMPORTING REQUIRED LIBRARIES-------------------------------------#

list.of.packages <- c("data.table", "RCurl", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(RCurl)
library(tidyverse)

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

# Helper not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#=======================#
# CLEANING 1. AGE
#=======================#

trafficAccident2020.dt[Age == 0, ] # 0 incidences

trafficAccident2020.dt[Age == 97, ] # 4 incidences (97 or older)
# Since it's only 4 incidences, we assume all 4 cases are when the individual involved is 97 years old.

trafficAccident2020.dt[Age == 99, ] # 2 incidences (Unknown)
# Since it's only 2 incidences, we will replace with the median in this case.
# Getting median of remaining rows.
median_Age <- median(trafficAccident2020.dt$Age[which(trafficAccident2020.dt$Age!=99)])
median_Age

trafficAccident2020.dt$Age[which(trafficAccident2020.dt$Age == 99)] <- median(trafficAccident2020.dt$Age)

# Checking (No more incidences)
trafficAccident2020.dt[Age == 99, ]


#=======================#
# CLEANING 2. SEX
#=======================#

# Getting no of values for each level, most of them seem to be male in the dataset. Replace with Mode.
trafficAccident2020.dt %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex), percentage = length(Sex)/nrow(trafficAccident2020.dt) * 100)

# 1018 incidences (Not Reported/Unknown Sex) ~= 5% of data, we choose to replace it with the mode of the column instead.

# Getting the Mode
mode_Sex <- mode(trafficAccident2020.dt$Sex)
mode_Sex

# Replacing with Mode and Changing 1 & 2 to Male and Female

trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 8] <- mode_Sex 
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 9] <- mode_Sex
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 1] <- 'M'
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 2] <- 'F' 

# Checking
trafficAccident2020.dt %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex))

#========================#
# CLEANING 3. Person_Type
#========================#

# Getting no of values for each level
trafficAccident2020.dt %>% 
  group_by(Person_Type) %>%
  summarise(no_rows = length(Person_Type), percentage = length(Person_Type)/nrow(trafficAccident2020.dt) * 100)

trafficAccident2020.dt$Person_Type[trafficAccident2020.dt$Person_Type == 1] <- 'Driver'
trafficAccident2020.dt$Person_Type[trafficAccident2020.dt$Person_Type == 2] <- 'Passenger'

mode_Person_Type <- mode(trafficAccident2020.dt$Person_Type)
mode_Person_Type

trafficAccident2020.dt$Person_Type[trafficAccident2020.dt$Person_Type == 9] <- mode_Person_Type

# Checking
trafficAccident2020.dt %>% 
  group_by(Person_Type) %>%
  summarise(no_rows = length(Person_Type), percentage = length(Person_Type)/nrow(trafficAccident2020.dt) * 100)


#============================#
# CLEANING 4. Injury_Severity
#============================#

# Getting no of values for each level, most of them seem to be male in the dataset. Replace with Mode.
trafficAccident2020.dt %>% 
  group_by(Injury_Severity) %>%
  summarise(no_rows = length(Injury_Severity), percentage = length(Injury_Severity)/nrow(trafficAccident2020.dt) * 100)

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
  summarise(no_rows = length(Injury_Severity), percentage = length(Injury_Severity)/nrow(trafficAccident2020.dt) * 100)

#==========================#
# CLEANING 5. Seat_Position
#==========================#

# Getting no of occurrences of each level
trafficAccident2020.dt %>% 
  group_by(Seat_Position) %>%
  summarise(no_rows = length(Seat_Position), percentage = length(Seat_Position)/nrow(trafficAccident2020.dt) * 100)

# Given most vehicles in Singapore only have 2 rows, we will only keep data related to front_seat and second_seat. 
# The other categories have low amount of incidences, hence we will simply replace them with the mode.

# Renaming Categories & Categories

trafficAccident2020.dt$Seat_Position[trafficAccident2020.dt$Seat_Position == 11] <- 'Front_Seat_Driver'
trafficAccident2020.dt$Seat_Position[trafficAccident2020.dt$Seat_Position == 13] <- 'Front_Seat_Right'

# Merging all second row seats into second_seat since the number of rows is not significant
trafficAccident2020.dt$Seat_Position[trafficAccident2020.dt$Seat_Position %in% c(23,29)] <- 'Second_Seat'

# Merging all other rows into the mode category 
trafficAccident2020.dt$Seat_Position[trafficAccident2020.dt$Seat_Position %in% c(18,19,33,51,55,98,99)] <- 'Front_Seat_Driver'

# Check
trafficAccident2020.dt %>% 
  group_by(Seat_Position) %>%
  summarise(no_rows = length(Seat_Position), percentage = length(Seat_Position)/nrow(trafficAccident2020.dt) * 100)

#======================================#
# CLEANING 6. Restraint_Equipment_Usage
#======================================#

# Getting no of occurrences of each level
trafficAccident2020.dt %>% 
  group_by(Restraint_Equipment_Usage) %>%
  summarise(no_rows = length(Restraint_Equipment_Usage), percentage = length(Restraint_Equipment_Usage)/nrow(trafficAccident2020.dt) * 100)

# 15% unknown, too large, leave with "unknown"
nrow(trafficAccident2020.dt[Restraint_Equipment_Usage %in% c(97,98,99), ])/nrow(trafficAccident2020.dt) * 100

# Renaming categories & cleaning

trafficAccident2020.dt$Restraint_Equipment_Usage[trafficAccident2020.dt$Restraint_Equipment_Usage == 1] <- 'Shoulder_Belt'
trafficAccident2020.dt$Restraint_Equipment_Usage[trafficAccident2020.dt$Restraint_Equipment_Usage == 2] <- 'Lap_Belt'

# Replacing '4' with mode since only 1 incidence
# Replacing '8' with mode since some sort of restraint was used and only 186 rows, safe to assume with mode.
trafficAccident2020.dt$Restraint_Equipment_Usage[trafficAccident2020.dt$Restraint_Equipment_Usage %in% c(3,4,8)] <- 'Shoulder_And_Lap_Belt'

# Renaming '20' with No restraint used
trafficAccident2020.dt$Restraint_Equipment_Usage[trafficAccident2020.dt$Restraint_Equipment_Usage == 20] <- 'No_Restraint'
trafficAccident2020.dt$Restraint_Equipment_Usage[trafficAccident2020.dt$Restraint_Equipment_Usage %in% c(97,98,99)] <- 'Unknown'

# Checking
trafficAccident2020.dt %>% 
  group_by(Restraint_Equipment_Usage) %>%
  summarise(no_rows = length(Restraint_Equipment_Usage), percentage = length(Restraint_Equipment_Usage)/nrow(trafficAccident2020.dt) * 100)

#========================#
# CLEANING 7. Airbag
#========================#

# Getting no of occurrences of each level
trafficAccident2020.dt %>% 
  group_by(Airbag) %>%
  summarise(no_rows = length(Airbag), percentage = length(Airbag)/nrow(trafficAccident2020.dt) * 100)

# 8% unknown, too large, leave with "unknown"
nrow(trafficAccident2020.dt[Airbag %in% c(98,99), ])/nrow(trafficAccident2020.dt) * 100

# Renaming and cleaning data

# Replacing '1', '2', '3', '7', '8', '9' with Deployed since all of them include data of the airbag being deployed. 
# The focus here is on whether the airbag was deployed or not.
trafficAccident2020.dt$Airbag[trafficAccident2020.dt$Airbag %in% c(1,2,3,7,8,9)] <- 'Deployed'

# Renaming '20' with not deployed
trafficAccident2020.dt$Airbag[trafficAccident2020.dt$Airbag == 20] <- 'Not_Deployed'

# Renaming '97' with Not_Vehicle_Occupant
trafficAccident2020.dt$Airbag[trafficAccident2020.dt$Airbag == 97] <- 'Not_Vehicle_Occupant'

# Replacing '98' and '99' with unknown
trafficAccident2020.dt$Airbag[trafficAccident2020.dt$Airbag %in% c(98,99)] <- 'Unknown'

# Checking
trafficAccident2020.dt %>% 
  group_by(Airbag) %>%
  summarise(no_rows = length(Airbag), percentage = length(Airbag)/nrow(trafficAccident2020.dt) * 100)

#========================#
# CLEANING 8. Drugs
#========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Drugs) %>%
  summarise(no_rows = length(Drugs), percentage = length(Drugs)/nrow(trafficAccident2020.dt) * 100)
# 44% of data, too many unknowns, leave with "unknown" category

# Replacing and renaming columns

# Replacing '0' with 'No'
trafficAccident2020.dt$Drugs[trafficAccident2020.dt$Drugs == 0] <- 'No'

# Replacing '1' with 'Yes'
trafficAccident2020.dt$Drugs[trafficAccident2020.dt$Drugs == 1] <- 'Yes'

# Replacing '8' and '9' with 'Unknown'
trafficAccident2020.dt$Drugs[trafficAccident2020.dt$Drugs %in% c(8,9)] <- 'Unknown'

# Checking
trafficAccident2020.dt %>% 
  group_by(Drugs) %>%
  summarise(no_rows = length(Drugs), percentage = length(Drugs)/nrow(trafficAccident2020.dt) * 100)

#==================================#
# CLEANING 9. Non_Motorist_Location
#==================================#

# Getting no of values for each level
trafficAccident2020.dt %>% 
  group_by(Non_Motorist_Location) %>%
  summarise(no_rows = length(Non_Motorist_Location))

# Useless variable

#==============================#
# CLEANING 10. Num_Of_Occupants
#==============================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Num_Of_Occupants) %>%
  summarise(no_rows = length(Num_Of_Occupants), percentage = length(Num_Of_Occupants)/nrow(trafficAccident2020.dt) * 100)

# ~= 5% , we can choose to replace with median. 
nrow(trafficAccident2020.dt[Num_Of_Occupants == 99, ])/nrow(trafficAccident2020.dt) * 100

# Getting median of remaining rows.
median_No_Occupants <- median(trafficAccident2020.dt$Num_Of_Occupants[which(trafficAccident2020.dt$Num_Of_Occupants!=99)])
median_No_Occupants

# Replacing with median of remaining rows
trafficAccident2020.dt$Num_Of_Occupants[which(trafficAccident2020.dt$Num_Of_Occupants == 99)] <- median_No_Occupants

# Checking
trafficAccident2020.dt %>% 
  group_by(Num_Of_Occupants) %>%
  summarise(no_rows = length(Num_Of_Occupants), percentage = length(Num_Of_Occupants)/nrow(trafficAccident2020.dt) * 100)

#=========================#
# CLEANING 11. Hit_And_Run
#=========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Hit_And_Run) %>%
  summarise(no_rows = length(Hit_And_Run), percentage = length(Hit_And_Run)/nrow(trafficAccident2020.dt) * 100)

# Renaming values of 0 and 1 with 'No' and 'Yes'
trafficAccident2020.dt$Hit_And_Run[trafficAccident2020.dt$Hit_And_Run == 0] <- 'No'
trafficAccident2020.dt$Hit_And_Run[trafficAccident2020.dt$Hit_And_Run == 1] <- 'Yes'

# Checking
trafficAccident2020.dt %>% 
  group_by(Hit_And_Run) %>%
  summarise(no_rows = length(Hit_And_Run), percentage = length(Hit_And_Run)/nrow(trafficAccident2020.dt) * 100)

#========================#
# CLEANING 12. Owner_Type
#========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Owner_Type) %>%
  summarise(no_rows = length(Owner_Type), percentage = length(Owner_Type)/nrow(trafficAccident2020.dt) * 100)

# ~= 6% of data
nrow(trafficAccident2020.dt[Owner_Type %in% c(6,9), ])/nrow(trafficAccident2020.dt) * 100

# Renaming and cleaning data

# Renaming '0' with unregistered vehicle
trafficAccident2020.dt$Owner_Type[trafficAccident2020.dt$Owner_Type == 0] <- 'Unregistered_Vehicle'

# Renaming '1' with registered owner
trafficAccident2020.dt$Owner_Type[trafficAccident2020.dt$Owner_Type == 1] <- 'Registered_Owner'

# Replacing '2' and '3' with not_registered_Owner since they fall into the same category when the driver is not the registered owner
trafficAccident2020.dt$Owner_Type[trafficAccident2020.dt$Owner_Type %in% c(2,3)] <- 'Not_Registered_Owner'

# Renaming '4' with rental vehicle
trafficAccident2020.dt$Owner_Type[trafficAccident2020.dt$Owner_Type == 4] <- 'Rental_Vehicle'

# Renaming '5' with stolen vehicle
trafficAccident2020.dt$Owner_Type[trafficAccident2020.dt$Owner_Type == 5] <- 'Stolen_Vehicle'

# Replacing '6' with mode since not much incidences
# Replacing '9' with mode since approximately 6% of data, still within acceptable range
trafficAccident2020.dt$Owner_Type[trafficAccident2020.dt$Owner_Type %in% c(6,9)] <- 'Registered_Owner'

# Checking
trafficAccident2020.dt %>% 
  group_by(Owner_Type) %>%
  summarise(no_rows = length(Owner_Type), percentage = length(Owner_Type)/nrow(trafficAccident2020.dt) * 100)

#===================================#
# CLEANING 13. Vehicle_Configuration
#===================================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Vehicle_Configuration) %>%
  summarise(no_rows = length(Vehicle_Configuration), percentage = length(Vehicle_Configuration)/nrow(trafficAccident2020.dt) * 100)


# Renaming and replacing columns

# Replacing 0 
# This variable's focus  is on medium and heavy trucks / buses
# The '0' category, i.e. not Applicable applies when the vehicle is not medium/heavy truck or bus according to prev years definitions
# Here, we make the assumption that it's a Light Vehicle.

trafficAccident2020.dt$Vehicle_Configuration[trafficAccident2020.dt$Vehicle_Configuration == 0] <- 'Light_Vehicle'

# Replacing '1' and '2' with Single_Unit_Truck since they are similar
trafficAccident2020.dt$Vehicle_Configuration[trafficAccident2020.dt$Vehicle_Configuration %in% c(1,2)] <- 'Single_Unit_Truck'

# Replacing '4','5','6','7' with Truck_Tractor, since they are similar
trafficAccident2020.dt$Vehicle_Configuration[trafficAccident2020.dt$Vehicle_Configuration %in% c(4,5,6,7)] <- 'Truck_Tractor'

# Replacing '10','19','20','21' with Large_Heavy_Vehicles, these vehicles have a common feature which is that they are very large and heavy
# Feature: Weight > 10,000 lbs or has more than 9 occupants.

trafficAccident2020.dt$Vehicle_Configuration[trafficAccident2020.dt$Vehicle_Configuration %in% c(10,19,20,21)] <- 'Large_Heavy_Vehicles'

# Replacing '99' i.e. Unknowns with Mode, since the no of rows is < 5%
trafficAccident2020.dt$Vehicle_Configuration[trafficAccident2020.dt$Vehicle_Configuration == 99] <- 'Light_Vehicle'

# Checking
trafficAccident2020.dt %>% 
  group_by(Vehicle_Configuration) %>%
  summarise(no_rows = length(Vehicle_Configuration), percentage = length(Vehicle_Configuration)/nrow(trafficAccident2020.dt) * 100)

#=======================================#
# CLEANING 14. Hazardous_Mat_Involvement
#=======================================#

# Getting no of values for each level
trafficAccident2020.dt %>% 
  group_by(Hazardous_Mat_Involvement) %>%
  summarise(no_rows = length(Hazardous_Mat_Involvement), percentage = length(Hazardous_Mat_Involvement)/nrow(trafficAccident2020.dt) * 100)

# Replacing values of 1 and 2 with 'No' and 'Yes'
trafficAccident2020.dt$Hazardous_Mat_Involvement[trafficAccident2020.dt$Hazardous_Mat_Involvement == 1] <- 'No'
trafficAccident2020.dt$Hazardous_Mat_Involvement[trafficAccident2020.dt$Hazardous_Mat_Involvement == 2] <- 'Yes'

# Checking
trafficAccident2020.dt %>% 
  group_by(Hazardous_Mat_Involvement) %>%
  summarise(no_rows = length(Hazardous_Mat_Involvement), percentage = length(Hazardous_Mat_Involvement)/nrow(trafficAccident2020.dt) * 100)


#==========================#
# CLEANING 15. Travel_Speed
#==========================#

summary(trafficAccident2020.dt$Travel_Speed)

trafficAccident2020.dt[Travel_Speed == 997, ] # 2 INCIDENCES > 151mph
trafficAccident2020.dt[Travel_Speed == 998, ] # 10919 INCIDENCES Not reported
trafficAccident2020.dt[Travel_Speed == 999, ] # 2156 INCIDENCES reported as unknown

# 64% of rows
nrow(trafficAccident2020.dt[Travel_Speed %in% c(998,999), ])/nrow(trafficAccident2020.dt) * 100

# Getting median of remaining rows
median_Travel_Speed <- median(trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Travel_Speed %!in% c(997,998,999))])
median_Travel_Speed

# Replacing travel speed of '997' with median since this means that the vehicle was travelling at > 243 km/h which is quite unrealistic in Singapore.
trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Travel_Speed %in% c(997,998,999))] <- median_Travel_Speed

# Convert to km/h
speed_converter <- function(x){
  return (1.60934 * x)
}

b <- c('Travel_Speed')
trafficAccident2020.dt[, (b) := lapply(.SD, speed_converter), .SDcols = b]
summary(trafficAccident2020.dt$Travel_Speed)

#===========================#
# CLEANING 16. Driver_Height
#===========================#


summary(trafficAccident2020.dt$Driver_Height)
nrow(trafficAccident2020.dt[Driver_Height == 998, ]) # 24 INCIDENCES no driver present
nrow(trafficAccident2020.dt[Driver_Height == 999, ]) # 4821 INCIDENCES unknown height
trafficAccident2020.dt[Driver_Height >= 24 & Driver_Height <= 107, ]


# Convert to metre
height_converter <- function(x){
  return (x/39.37)
}

b <- c('Driver_Height')
trafficAccident2020.dt[, (b) := lapply(.SD, height_converter), .SDcols = b]

summary(trafficAccident2020.dt$Driver_Height)

#===========================#
# CLEANING 17. Driver_Weight
#===========================#

summary(trafficAccident2020.dt$Driver_Weight)
nrow(trafficAccident2020.dt[Driver_Weight == 997, ]) # 24 INCIDENCES no driver present
nrow(trafficAccident2020.dt[Driver_Weight == 998, ]) # 0 INCIDENCES others
nrow(trafficAccident2020.dt[Driver_Weight == 999, ]) # 9323 INCIDENCES unknown
trafficAccident2020.dt[Driver_Weight >= 40 & Driver_Weight <= 700, ]


# Convert to kg
weight_converter <- function(x){
  return (x/2.205)
}

b <- c('Driver_Weight')
trafficAccident2020.dt[, (b) := lapply(.SD, weight_converter), .SDcols = b]

summary(trafficAccident2020.dt$Driver_Weight)

#========================================#
# CLEANING 18. Previous_Speed_Convictions
#========================================#

summary(trafficAccident2020.dt$Driver_Weight)
nrow(trafficAccident2020.dt[Previous_Speed_Convictions == 99, ]) # 1806 INCIDENCES Unknown
nrow(trafficAccident2020.dt[Previous_Speed_Convictions == 998, ]) # 24 INCIDENCES others

#==============================#
# CLEANING 19. Speeding_Related
#==============================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Speeding_Related) %>%
  summarise(no_rows = length(Speeding_Related), percentage = length(Speeding_Related)/nrow(trafficAccident2020.dt) * 100)

# Replacing values of '0' with No
trafficAccident2020.dt$Speeding_Related[trafficAccident2020.dt$Speeding_Related == 0] <- 'No'

# Replacing values of '2','3','4','5' with yes 
trafficAccident2020.dt$Speeding_Related[trafficAccident2020.dt$Speeding_Related %in% c(2,3,4,5)] <- 'Yes'

# Replacing '8' and '9' with the mode, No speeding related since the no of rows is < 7% of data.
trafficAccident2020.dt$Speeding_Related[trafficAccident2020.dt$Speeding_Related %in% c(8,9)] <- 'No'

# Checking
trafficAccident2020.dt %>% 
  group_by(Speeding_Related) %>%
  summarise(no_rows = length(Speeding_Related), percentage = length(Speeding_Related)/nrow(trafficAccident2020.dt) * 100)

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
