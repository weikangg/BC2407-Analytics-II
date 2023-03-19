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
                                   'DRDISTRACT', 'DRIVERRF', 'DAY', 'MONTH', 'DAY_WEEK', 'HOUR', 'HARM_EV', 'RELJCT2', 'TYP_INT', 'REL_ROAD', 'LGT_COND', 'WEATHER'), 
                                 c('Age', 'Sex', 'Person_Type', 'Death', 'Seat_Position', 'Restraint_Equipment_Usage', 'Airbag', 'Drugs', 'Non_Motorist_Location', 'Num_Of_Occupants',
                                   'Hit_And_Run', 'Owner_Type', 'Vehicle_Configuration', 'Hazardous_Mat_Involvement', 'Travel_Speed', 'Driver_Height', 'Driver_Weight', 'Previous_Speed_Convictions',
                                   'Speeding_Related','Trafficway_Flow', 'Num_Of_Lanes', 'Roadway_Profile', 'Pavement_Type', 'Roadway_Surface_Condition','Pre_Crash_Activity', 'Critical_Activity',
                                   'Drinking', 'Vehicle_Classification', 'Driver_Distracted', 'Driver_Factors', 'Day','Month', 'Day_Of_Week', 'Hour', 'Harmful_Event','Relation_To_Junction', 'Type_Of_Intersection',
                                   'Location_Of_Crash', 'Light_Condition', 'Weather'))

# Dropped irrelevant Columns
# trafficAccident2020.dt[, c('ST_CASE', 'VEH_NO', 'PER_NO', 'YEAR', 'RUR_URB'):=NULL]
trafficAccident2020.dt[, c('YEAR', 'RUR_URB'):=NULL]
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

# Getting summary of unique values in the column
print(n=95,trafficAccident2020.dt %>% 
  group_by(Age) %>%
  summarise(no_rows = length(Age), percentage = length(Age)/nrow(trafficAccident2020.dt) * 100))

# 5% of data, can be replaced with median
nrow(trafficAccident2020.dt[Age %in% c(998,999), ])/nrow(trafficAccident2020.dt) * 100

median_Age <- median(trafficAccident2020.dt$Age[which(trafficAccident2020.dt$Age %!in% c(998,999))])
median_Age

trafficAccident2020.dt$Age[which(trafficAccident2020.dt$Age %in% c(998,999))] <- median_Age

# Checking (No more incidences)
trafficAccident2020.dt[Age %in% c(998,999), ]



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
# CLEANING 4. Death (Y-Var)
#============================#

# Getting no of values for each level, most of them seem to be male in the dataset. Replace with Mode.
trafficAccident2020.dt %>% 
  group_by(Death) %>%
  summarise(no_rows = length(Death), percentage = length(Death)/nrow(trafficAccident2020.dt) * 100)

# Replacing "Unknown/Not Reported" Category
trafficAccident2020.dt[Death %in% c(9), ] # 822 incidences, < 5% of no of rows. Hence, we simply replace with mode.
822/nrow(trafficAccident2020.dt) * 100


# Reclassify level 0,1,2,3,5 as No and level 4,6 as Yes
trafficAccident2020.dt$Death[trafficAccident2020.dt$Death %in% c(0,1,2,3,5,9)] <- 'No'
trafficAccident2020.dt$Death[trafficAccident2020.dt$Death %in% c(4,6)] <- 'Yes'

# Checking
trafficAccident2020.dt %>% 
  group_by(Death) %>%
  summarise(no_rows = length(Death), percentage = length(Death)/nrow(trafficAccident2020.dt) * 100)

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

# Useless variable, REMOVING THIS VARIABLE.

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

#==========================#
# CLEANING 15. Travel_Speed
#==========================#

summary(trafficAccident2020.dt$Travel_Speed)

trafficAccident2020.dt[Travel_Speed == 0, ] # 158 INCIDENCES == 0 mph
trafficAccident2020.dt[Travel_Speed == 997, ] # 2 INCIDENCES > 151mph
trafficAccident2020.dt[Travel_Speed == 998, ] # 10919 INCIDENCES Not reported
trafficAccident2020.dt[Travel_Speed == 999, ] # 2156 INCIDENCES reported as unknown

# 64% of rows
nrow(trafficAccident2020.dt[Travel_Speed %in% c(998,999), ])/nrow(trafficAccident2020.dt) * 100

# Methodology: IF that particular row is associated with speeding_related cases, then , we replace the 'unknown' travel speed with median speed of speeding_related
# Else: We replace the 'unknown' travel speed with median of not speeding cases

# Getting median travelling speed of all rows where speed is known
median_Travel_Speed <-  median(trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Travel_Speed %!in% c(997,998,999))])
median_Travel_Speed

# Getting median of speeding_related cases where the travel speed is known
median_Travel_Speed_Speeding <- median(trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Speeding_Related == 'Yes' & trafficAccident2020.dt$Travel_Speed %!in% c(998,999))])
median_Travel_Speed_Speeding

# Getting median of not speeding_related cases where the travel speed is known
median_Travel_Speed_NotSpeeding <- median(trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Speeding_Related == 'No' & trafficAccident2020.dt$Travel_Speed %!in% c(998,999))])
median_Travel_Speed_NotSpeeding

# Replacing travel speed of '997' with median since this means that the vehicle was travelling at > 243 km/h which is quite unrealistic in Singapore.
trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Travel_Speed == 997)] <- median_Travel_Speed

# Replacing rows with unknown travel_Speed, and are tagged as speeding_related
trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Speeding_Related == 'Yes' & trafficAccident2020.dt$Travel_Speed %in% c(998,999))] <- median_Travel_Speed_Speeding

# Replacing rows with unknown travel_Speed and are tagged as NOT speeding_related
trafficAccident2020.dt$Travel_Speed[which(trafficAccident2020.dt$Speeding_Related == 'No' & trafficAccident2020.dt$Travel_Speed %in% c(998,999))] <- median_Travel_Speed_NotSpeeding


# Convert to km/h
speed_converter <- function(x){
  return (1.60934 * x)
}

b <- c('Travel_Speed')
trafficAccident2020.dt[, (b) := lapply(.SD, speed_converter), .SDcols = b]

# Checking
summary(trafficAccident2020.dt$Travel_Speed)
trafficAccident2020.dt[Travel_Speed == 997, ] # 2 INCIDENCES > 151mph
trafficAccident2020.dt[Travel_Speed == 998, ] # 10919 INCIDENCES Not reported
trafficAccident2020.dt[Travel_Speed == 999, ] # 2156 INCIDENCES reported as unknown

#===========================#
# CLEANING 16. Driver_Height
#===========================#


summary(trafficAccident2020.dt$Driver_Height)

# 23% of rows where height is unknown
nrow(trafficAccident2020.dt[Driver_Height %in% c(998,999), ])/nrow(trafficAccident2020.dt) * 100

# Getting median height of drivers where the heights of the driver are known
median_Driver_Height <-  median(trafficAccident2020.dt$Driver_Height[which(trafficAccident2020.dt$Driver_Height %!in% c(998,999))])
median_Driver_Height

# Replacing unknown heights with the median of the remaining drivers
trafficAccident2020.dt$Driver_Height[which(trafficAccident2020.dt$Driver_Height %in% c(998,999))] <- median_Driver_Height


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

# 46% of rows unknown
nrow(trafficAccident2020.dt[Driver_Weight %in% c(997,998,999), ])/nrow(trafficAccident2020.dt) * 100

# Getting median weight of the remaining rows where the weight of the drivers are known
median_Driver_Weight <-  median(trafficAccident2020.dt$Driver_Weight[which(trafficAccident2020.dt$Driver_Weight %!in% c(997,998,999))])
median_Driver_Weight

# Replacing all unknown rows with the median weight
trafficAccident2020.dt$Driver_Weight[which(trafficAccident2020.dt$Driver_Weight %in% c(997,998,999))] <- median_Driver_Weight

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

summary(trafficAccident2020.dt$Previous_Speed_Convictions)

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Previous_Speed_Convictions) %>%
  summarise(no_rows = length(Previous_Speed_Convictions), percentage = length(Previous_Speed_Convictions)/nrow(trafficAccident2020.dt) * 100)


# 9%, replacing with median
nrow(trafficAccident2020.dt[Previous_Speed_Convictions %in% c(99,998), ])/nrow(trafficAccident2020.dt) * 100

# Getting median Previous_Speed_Convictions speed of all rows where amount of prev spd convictions is known
median_Prev_Spd_Convictions <-  median(trafficAccident2020.dt$Previous_Speed_Convictions[which(trafficAccident2020.dt$Previous_Speed_Convictions %!in% c(99,998))])
median_Prev_Spd_Convictions

# Replacing '99' and '998' with median
trafficAccident2020.dt$Previous_Speed_Convictions[which(trafficAccident2020.dt$Previous_Speed_Convictions %in% c(99,998))] <- median_Prev_Spd_Convictions

# Checking
trafficAccident2020.dt %>% 
  group_by(Previous_Speed_Convictions) %>%
  summarise(no_rows = length(Previous_Speed_Convictions), percentage = length(Previous_Speed_Convictions)/nrow(trafficAccident2020.dt) * 100)


#=============================#
# CLEANING 20. Trafficway_Flow
#=============================#
  
# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Trafficway_Flow) %>%
  summarise(no_rows = length(Trafficway_Flow), percentage = length(Trafficway_Flow)/nrow(trafficAccident2020.dt) * 100)

# 0.8% of rows, can replace with the mode, which is 2 way not divided
nrow(trafficAccident2020.dt[Trafficway_Flow %in% c(8,9), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '0', '4', '6' with Others
trafficAccident2020.dt$Trafficway_Flow[trafficAccident2020.dt$Trafficway_Flow %in% c(0,4,6)] <- 'Others'

# Replacing '1','5', '8','9' with 2_Way_Not_Divided
trafficAccident2020.dt$Trafficway_Flow[trafficAccident2020.dt$Trafficway_Flow %in% c(1,5,8,9)] <- '2_Way_Not_Divided'

# Replacing '2' with 2_Way_Divided_Unprotected
trafficAccident2020.dt$Trafficway_Flow[trafficAccident2020.dt$Trafficway_Flow == 2] <- '2_Way_Divided_Unprotected'

# Replacing '3' with 2_Way_Divided_Protected
trafficAccident2020.dt$Trafficway_Flow[trafficAccident2020.dt$Trafficway_Flow == 3] <- '2_Way_Divided_Protected'

# Checking
trafficAccident2020.dt %>% 
  group_by(Trafficway_Flow) %>%
  summarise(no_rows = length(Trafficway_Flow), percentage = length(Trafficway_Flow)/nrow(trafficAccident2020.dt) * 100)

#==========================#
# CLEANING 21. Num_Of_Lanes
#==========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Num_Of_Lanes) %>%
  summarise(no_rows = length(Num_Of_Lanes), percentage = length(Num_Of_Lanes)/nrow(trafficAccident2020.dt) * 100)

# 1.3% of data, can replace with mode, which is 2 lanes
nrow(trafficAccident2020.dt[Num_Of_Lanes %in% c(8,9), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '0' with '1'
trafficAccident2020.dt$Num_Of_Lanes[trafficAccident2020.dt$Num_Of_Lanes == 0] <- 1

# Replacing '6' and '7' with '5'
trafficAccident2020.dt$Num_Of_Lanes[trafficAccident2020.dt$Num_Of_Lanes %in% c(6,7)] <- 5

# Replacing '8' and '9' with '2'
trafficAccident2020.dt$Num_Of_Lanes[trafficAccident2020.dt$Num_Of_Lanes %in% c(8,9)] <- 2

# Checking
trafficAccident2020.dt %>% 
  group_by(Num_Of_Lanes) %>%
  summarise(no_rows = length(Num_Of_Lanes), percentage = length(Num_Of_Lanes)/nrow(trafficAccident2020.dt) * 100)

#=============================#
# CLEANING 22. Roadway_Profile
#=============================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Roadway_Profile) %>%
  summarise(no_rows = length(Roadway_Profile), percentage = length(Roadway_Profile)/nrow(trafficAccident2020.dt) * 100)

# ~ 11% of unknowns, hence we will choose not to replace with mode, but instead create a separate "Unknown category"
nrow(trafficAccident2020.dt[Roadway_Profile %in% c(8,9), ])/nrow(trafficAccident2020.dt) * 100

# Replacing 0, 3, 4 with Others
trafficAccident2020.dt$Roadway_Profile[trafficAccident2020.dt$Roadway_Profile %in% c(0,3,4)] <- 'Others'

# Replacing 1 with Level
trafficAccident2020.dt$Roadway_Profile[trafficAccident2020.dt$Roadway_Profile == 1] <- 'Level'

# Replacing 2, 5, 6  with Grade
trafficAccident2020.dt$Roadway_Profile[trafficAccident2020.dt$Roadway_Profile %in% c(2,5,6)] <- 'Grade'

# Replacing 8,9 with Unknown
trafficAccident2020.dt$Roadway_Profile[trafficAccident2020.dt$Roadway_Profile %in% c(8,9)] <- 'Unknown'

# Checking
trafficAccident2020.dt %>% 
  group_by(Roadway_Profile) %>%
  summarise(no_rows = length(Roadway_Profile), percentage = length(Roadway_Profile)/nrow(trafficAccident2020.dt) * 100)

#===========================#
# CLEANING 23. Pavement_Type
#===========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Pavement_Type) %>%
  summarise(no_rows = length(Pavement_Type), percentage = length(Pavement_Type)/nrow(trafficAccident2020.dt) * 100)

# ~ 30% of unknowns, hence we will choose not to replace with mode, but instead create a separate "Unknown category"
nrow(trafficAccident2020.dt[Pavement_Type %in% c(8,9), ])/nrow(trafficAccident2020.dt) * 100

# Replacing 0,3,4,5,7 with Others by merging these columns together
trafficAccident2020.dt$Pavement_Type[trafficAccident2020.dt$Pavement_Type %in% c(0,3,4,5,7)] <- 'Others'

# Replace 1 with Concrete
trafficAccident2020.dt$Pavement_Type[trafficAccident2020.dt$Pavement_Type  == 1] <- 'Concrete'

# Replace 2 with Blacktop_Bituminous_Asphalt
trafficAccident2020.dt$Pavement_Type[trafficAccident2020.dt$Pavement_Type  == 2] <- 'Blacktop_Bituminous_Asphalt'

# Replace '8' and '9' with Unknown
trafficAccident2020.dt$Pavement_Type[trafficAccident2020.dt$Pavement_Type %in% c(8,9)] <- 'Unknown'

# Checking
trafficAccident2020.dt %>% 
  group_by(Pavement_Type) %>%
  summarise(no_rows = length(Pavement_Type), percentage = length(Pavement_Type)/nrow(trafficAccident2020.dt) * 100)

#=======================================#
# CLEANING 24. Roadway_Surface_Condition
#=======================================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Roadway_Surface_Condition) %>%
  summarise(no_rows = length(Roadway_Surface_Condition), percentage = length(Roadway_Surface_Condition)/nrow(trafficAccident2020.dt) * 100)

# < 2% of unknowns, relatively low number of unknowns, we choose to replace them with the mode
nrow(trafficAccident2020.dt[Roadway_Surface_Condition %in% c(98,99), ])/nrow(trafficAccident2020.dt) * 100

# Replace '0', '3','4','5','6','7','8','10','11' with Others
trafficAccident2020.dt$Roadway_Surface_Condition[trafficAccident2020.dt$Roadway_Surface_Condition %in% c(0,3,4,5,6,7,8,10,11)] <- 'Others'

# Replace '1', '98','99' with Dry, replace with mode since low amount of unknowns
trafficAccident2020.dt$Roadway_Surface_Condition[trafficAccident2020.dt$Roadway_Surface_Condition %in% c(1,98,99)] <- 'Dry'

# Replace '2' with Wet
trafficAccident2020.dt$Roadway_Surface_Condition[trafficAccident2020.dt$Roadway_Surface_Condition == 2] <- 'Wet'

# Checking
trafficAccident2020.dt %>% 
  group_by(Roadway_Surface_Condition) %>%
  summarise(no_rows = length(Roadway_Surface_Condition), percentage = length(Roadway_Surface_Condition)/nrow(trafficAccident2020.dt) * 100)

#================================#
# CLEANING 25. Pre_Crash_Activity
#================================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Pre_Crash_Activity) %>%
  summarise(no_rows = length(Pre_Crash_Activity), percentage = length(Pre_Crash_Activity)/nrow(trafficAccident2020.dt) * 100)

# < 3% of unknowns, relatively low number of unknowns, we choose to replace them with the mode
nrow(trafficAccident2020.dt[Pre_Crash_Activity == 99, ])/nrow(trafficAccident2020.dt) * 100

# Replacing '0','2','3','4','5',7','8','9','12','13','16','17','98' with Others
trafficAccident2020.dt$Pre_Crash_Activity[trafficAccident2020.dt$Pre_Crash_Activity %in% c(0,2,3,4,5,7,8,9,12,13,16,17,98)] <- 'Others'

# Replacing '1', '99' with Going_Straight
trafficAccident2020.dt$Pre_Crash_Activity[trafficAccident2020.dt$Pre_Crash_Activity %in% c(1,99)] <- 'Going_Straight'

# Replacing '6' with Overtaking_Vehicle
trafficAccident2020.dt$Pre_Crash_Activity[trafficAccident2020.dt$Pre_Crash_Activity == 6] <- 'Overtaking_Vehicle'

# Replacing '10', '11' with Turning
trafficAccident2020.dt$Pre_Crash_Activity[trafficAccident2020.dt$Pre_Crash_Activity %in% c(10,11)] <- 'Turning'

# Replacing '14' with Negotiating_Curve
trafficAccident2020.dt$Pre_Crash_Activity[trafficAccident2020.dt$Pre_Crash_Activity == 14] <- 'Negotiating_Curve'

# Replacing '15' with Changing_Lanes
trafficAccident2020.dt$Pre_Crash_Activity[trafficAccident2020.dt$Pre_Crash_Activity == 15] <- 'Changing_Lanes'

# Checking
trafficAccident2020.dt %>% 
  group_by(Pre_Crash_Activity) %>%
  summarise(no_rows = length(Pre_Crash_Activity), percentage = length(Pre_Crash_Activity)/nrow(trafficAccident2020.dt) * 100)


#===============================#
# CLEANING 26. Critical_Activity
#===============================#

# Getting no of values and percentage for each level
print(n=57,trafficAccident2020.dt %>% 
  group_by(Critical_Activity) %>%
  summarise(no_rows = length(Critical_Activity), percentage = length(Critical_Activity)/nrow(trafficAccident2020.dt) * 100))

# 1% of unknowns, can just replace with the mode
nrow(trafficAccident2020.dt[Critical_Activity == 99, ])/nrow(trafficAccident2020.dt) * 100

# Getting the mode of the column
mode(trafficAccident2020.dt$Critical_Activity)

# Replacing '1','2','3','4','5','8,'9','12,'13','18','19','20','21','55','56','59','60','61','64','70','71','72','73','74','78','87','88','89','90','91','92','98' with Others
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(1,2,3,4,5,8,9,12,13,18,19,20,21,55,56,59,60,61,64,70,71,72,73,74,78,87,88,89,90,91,92,98)] <- 'Others'

# Replacing '6' with High_Speed
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity == 6] <- 'High_Speed'

# Replacing '10', '11' with Over_Lane_Line
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(10,11)] <- 'Over_Lane_Line'

# Replacing '14' with End_Departure
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity == 14] <- 'End_Departure'

# Replacing '15' , '16' with Turning
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(15,16)] <- 'Turning'

# Replacing '17' with Crossing_Intersection
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity == 17] <- 'Crossing_Intersection'

# Replacing '50' with Other_Vehicle_Stopped
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity == 50] <- 'Other_Vehicle_Stopped'

# Replacing '51' and '52' with Same_Direction_LowerSpeed
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(51,52)] <- 'Same_Direction_LowerSpeed'

# Replacing '53' with Same_Direction_HigherSpeed
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity == 53] <- 'Same_Direction_HigherSpeed'

# Replacing '54','62','63' with Opposite_Direction
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(54,62,63)] <- 'Opposite_Direction'

# Replacing '65','66','67','68' with Crossing_Street
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(65,66,67,68)] <- 'Crossing_Street'

# Replacing '80','81','82','83','84','85' with Non_Motorists
trafficAccident2020.dt$Critical_Activity[trafficAccident2020.dt$Critical_Activity %in% c(80,81,82,83,84,85,99)] <- 'Non_Motorists'

# Checking
print(n=57,trafficAccident2020.dt %>% 
        group_by(Critical_Activity) %>%
        summarise(no_rows = length(Critical_Activity), percentage = length(Critical_Activity)/nrow(trafficAccident2020.dt) * 100))

#========================#
# CLEANING 27. Drinking
#========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Drinking) %>%
  summarise(no_rows = length(Drinking), percentage = length(Drinking)/nrow(trafficAccident2020.dt) * 100)

# Replacing '0' with No
trafficAccident2020.dt$Drinking[trafficAccident2020.dt$Drinking == 0] <- 'No'

# Replacing '1' with Yes
trafficAccident2020.dt$Drinking[trafficAccident2020.dt$Drinking == 1] <- 'Yes'

# Checking
trafficAccident2020.dt %>% 
  group_by(Drinking) %>%
  summarise(no_rows = length(Drinking), percentage = length(Drinking)/nrow(trafficAccident2020.dt) * 100)


#====================================#
# CLEANING 28. Vehicle_Classification
#====================================#

# Getting no of values and percentage for each level
print(n=57,trafficAccident2020.dt %>% 
  group_by(Vehicle_Classification) %>%
  summarise(no_rows = length(Vehicle_Classification), percentage = length(Vehicle_Classification)/nrow(trafficAccident2020.dt) * 100))

# 4% of unknowns, can just replace with the mode
nrow(trafficAccident2020.dt[Vehicle_Classification %in% c(997,998,999), ])/nrow(trafficAccident2020.dt) * 100

# Getting the mode of the column
mode(trafficAccident2020.dt$Vehicle_Classification)

# Replacing '1' with Convertible
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 1] <- 'Convertible'

# Replacing '2' with Minivan
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 2] <- 'Minivan'

# Replacing '3' with Coupe
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 3] <- 'Coupe'

# Replacing '4','6','8','9','10','15','16','62','63','64','65','67','70','71','73','74','78','95','108','111','119','996','997' with Others
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification %in% c(4,6,8,9,10,15,16,62,63,64,65,67,70,71,73,74,78,95,108,111,119,996,997)] <- 'Others'

# Replacing '5' with Hatch_Lift_Notch
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 5] <- 'Hatch_Lift_Notch'

# Replacing '7' with SUV_MPV
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 7] <- 'SUV_MPV'

# Replacing '11' with Truck
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 11] <- 'Truck'

# Replacing '12','80','81','82','83','85','87','90,'94','98','103','104','114','125' with Motorcycle
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification %in% c(12,80,81,82,83,85,87,90,94,98,103,104,114,125)] <- 'Others'

# Replacing '13' with Sedan_Saloon
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 13] <- 'Sedan_Saloon'

# Replacing '60' with Pickup
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 60] <- 'Pickup'

# Replacing '66' with Truck_Tractor
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification == 66] <- 'Truck_Tractor'

# Replacing '69','84','86','88','97','105','113','124','126','127' with Off_Road_Vehicle
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification %in% c(69,84,86,88,97,105,113,124,126,127)] <- 'Off_Road_Vehicle'


# Replacing '998','999' with Sedan_Saloon (Mode)
trafficAccident2020.dt$Vehicle_Classification[trafficAccident2020.dt$Vehicle_Classification %in% c(998,999)] <- 'Sedan_Saloon'

# Checking
print(n=57,trafficAccident2020.dt %>% 
        group_by(Vehicle_Classification) %>%
        summarise(no_rows = length(Vehicle_Classification), percentage = length(Vehicle_Classification)/nrow(trafficAccident2020.dt) * 100))


#==============================#
# CLEANING 29. Driver_Distracted
#==============================#

# Getting no of values and percentage for each level
print(n=21,trafficAccident2020.dt %>% 
  group_by(Driver_Distracted) %>%
  summarise(no_rows = length(Driver_Distracted), percentage = length(Driver_Distracted)/nrow(trafficAccident2020.dt) * 100))

# 16.5% of unknowns, cannot just replace with the mode, hence we will leave it as Unknown
nrow(trafficAccident2020.dt[Driver_Distracted == 99, ])/nrow(trafficAccident2020.dt) * 100

# Replacing '0' with Not_Distracted
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted == 0] <- 'Not_Distracted'

# Replacing '3' with Other_Occupants
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted == 3] <- 'Other_Occupants'

# Replacing '4','12' with External_Objects
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted %in% c(4,12)] <- 'External_Objects'

# Replacing '5','6','7','9','10','15' with Device
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted %in% c(5,6,7,9,10,15)] <- 'Device'

# Replacing '13','14','16' with Others
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted %in% c(13,14,16)] <- 'Others'

# Replacing '17','19','92','93','97','98' with Distraction_Inattentive
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted %in% c(17,19,92,93,97,98)] <- 'Distraction_Inattentive'

# Replacing '96' with Not_Reported
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted == 96] <- 'Not_Reported'

# Replacing '99' with Unknown
trafficAccident2020.dt$Driver_Distracted[trafficAccident2020.dt$Driver_Distracted == 99] <- 'Unknown'

# Checking
print(n=21,trafficAccident2020.dt %>% 
        group_by(Driver_Distracted) %>%
        summarise(no_rows = length(Driver_Distracted), percentage = length(Driver_Distracted)/nrow(trafficAccident2020.dt) * 100))

#============================#
# CLEANING 30. Driver_Factors
#============================#

# Getting no of values and percentage for each level
print(n=63,trafficAccident2020.dt %>% 
        group_by(Driver_Factors) %>%
        summarise(no_rows = length(Driver_Factors), percentage = length(Driver_Factors)/nrow(trafficAccident2020.dt) * 100))

# 5% of unknowns, can replace with the mode
nrow(trafficAccident2020.dt[Driver_Factors == 999, ])/nrow(trafficAccident2020.dt) * 100

# Replace '0', '45', '89', '999' with None
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(0,45,89,999)] <- 'None'

# Replace '4','32', '35', '54', '57','58', '60', '88' with negligence
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(4,32,35,54,57,58,60,88)] <- 'Negligence'

# Replace '6', '10', '20', '36', '59' with Careless
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(6,10,20,36,59)] <- 'Careless'

# Replace '8' with Road_Rage
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors == 8] <- 'Road_Rage'

# Replace '13' with Mentally_Challenged
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors == 13] <- 'Mentally_Challenged'

# Replace '16','37', '94','95','96' with Job_Related
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(16,37,94,95,96)] <- 'Job_Related'

# Replace '18', '19', '29' with Illegality
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(18,19,29)] <- 'Illegality'


# Replace '21','22','23','24','26','30','31','33','34',38','39','40','41','42','47','48','50','51','55','73','74' with Non_Compliance
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(21,22,23,24,26,30,31,33,34,38,39,40,41,42,47,48,50,51,55,73,74)] <- 'Non_Compliance'

# Replace '27','28' with Improper_Lane_Usage
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(27,28)] <- 'Improper_Lane_Usage'

# Replace '52','53' with Unfamiliarity
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(52,53)] <- 'Unfamiliarity'

# Replace '77', '78', '79' , '81', '82', '83', '84', '86', '87' with Road_Conditions
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(77,78,79,81,82,83,84,86,87)] <- 'Road_Conditions'

# Replace '80','85' with Unforeseen_Circumstances
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors %in% c(80,85)] <- 'Unforeseen_Circumstances'

# Replace '91' with Violence
trafficAccident2020.dt$Driver_Factors[trafficAccident2020.dt$Driver_Factors == 91] <- 'Violence'

# Checking
print(n=63,trafficAccident2020.dt %>% 
        group_by(Driver_Factors) %>%
        summarise(no_rows = length(Driver_Factors), percentage = length(Driver_Factors)/nrow(trafficAccident2020.dt) * 100))

#========================#
# CLEANING 31. Day
#========================#

# Getting no of values and percentage for each level
print(n=31, trafficAccident2020.dt %>% 
  group_by(Day) %>%
  summarise(no_rows = length(Day), percentage = length(Day)/nrow(trafficAccident2020.dt) * 100))

# Nothing to clean, all good.

#========================#
# CLEANING 32. Month
#========================#

# Getting no of values and percentage for each level
print(n=12, trafficAccident2020.dt %>% 
        group_by(Month) %>%
        summarise(no_rows = length(Month), percentage = length(Day)/nrow(trafficAccident2020.dt) * 100))

# Nothing to clean, all good

#=========================#
# CLEANING 33. Day_Of_Week
#=========================#

# Getting no of values and percentage for each level
print(n=12, trafficAccident2020.dt %>% 
        group_by(Day_Of_Week) %>%
        summarise(no_rows = length(Day_Of_Week), percentage = length(Day_Of_Week)/nrow(trafficAccident2020.dt) * 100))

# Nothing to clean, all good

#========================#
# CLEANING 34. Hour
#========================#

# Getting no of values and percentage for each level
print(n=25, trafficAccident2020.dt %>% 
        group_by(Hour) %>%
        summarise(no_rows = length(Hour), percentage = length(Hour)/nrow(trafficAccident2020.dt) * 100))

# 0.5% of data unknown, replace with mode (9pm).

# Replacing '99' with '21'.
trafficAccident2020.dt$Hour[trafficAccident2020.dt$Hour == 99] <- 21

# Checking
print(n=25, trafficAccident2020.dt %>% 
        group_by(Hour) %>%
        summarise(no_rows = length(Hour), percentage = length(Hour)/nrow(trafficAccident2020.dt) * 100))

#===========================#
# CLEANING 35. Harmful_Event
#===========================#

# Getting no of values and percentage for each level
print(n=53,trafficAccident2020.dt %>% 
  group_by(Harmful_Event) %>%
  summarise(no_rows = length(Harmful_Event), percentage = length(Harmful_Event)/nrow(trafficAccident2020.dt) * 100))

# Low amount of unknowns, we will simply replace them  with the mode here
nrow(trafficAccident2020.dt[Harmful_Event %in% c(98,99), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '1' with Rollover
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event == 1] <- 'Rollover'

# Replacing '2','3', '4','5' ,'6','7', '72' with Non_Collision
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(2,3,4,5,6,7,72)] <- 'Non_Collision'

# Replacing '8' with Pedestrian
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event == 8] <- 'Pedestrian'

# Replacing '9' with Pedalcyclists
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event == 9] <- 'Pedalcyclists'

# Replacing '10', '12','14','15', '45', '54','55', '74', '98','99' with Motor_Vehicle
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(10,12,14,15,45,54,55,74,98,99)] <- 'Motor_Vehicle'

# Replacing '11', '49' with Animal
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(11,49)] <- 'Animal'

# Replacing '16,'17','18','43','51', '73','91','93' with Object
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(16,17,18,43,51,73,91,93)] <- 'Object'

# Replace '19', '20','21','23','24','25','26','30','31','38','39','40','46','50','52','53','57','59' with Static_Structures
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(19,20,21,23,24,25,26,30,31,38,39,40,46,50,52,53,57,59)] <- 'Static_Structures'

# Replace '32','33','34','35', '44','48', '58' with Pavement_Irregularities
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(32,33,34,35,44,48,58)] <- 'Pavement_Irregularities'

# Replace '41','42' with Plants
trafficAccident2020.dt$Harmful_Event[trafficAccident2020.dt$Harmful_Event %in% c(41,42)] <- 'Plants'

# Checking
print(n=53,trafficAccident2020.dt %>% 
        group_by(Harmful_Event) %>%
        summarise(no_rows = length(Harmful_Event), percentage = length(Harmful_Event)/nrow(trafficAccident2020.dt) * 100))

#==================================#
# CLEANING 36. Relation_To_Junction
#==================================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Relation_To_Junction) %>%
  summarise(no_rows = length(Relation_To_Junction), percentage = length(Relation_To_Junction)/nrow(trafficAccident2020.dt) * 100)

# Low amount of unknowns, we will simply replace them  with the mode here
nrow(trafficAccident2020.dt[Relation_To_Junction %in% c(98,99), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '1', '98','99' with Non_Junction
trafficAccident2020.dt$Relation_To_Junction[trafficAccident2020.dt$Relation_To_Junction %in% c(1,98,99)] <- 'Non_Junction'

# Replacing '2','3' with Intersection
trafficAccident2020.dt$Relation_To_Junction[trafficAccident2020.dt$Relation_To_Junction %in% c(2,3)] <- 'Intersection'

# Replacing  '5' ,'6' ,'7' , '16', '17', '18', '19', '20' with Others
trafficAccident2020.dt$Relation_To_Junction[trafficAccident2020.dt$Relation_To_Junction %in% c(5,6,7,16,17,18,19,20)] <- 'Others'

# Replacing '4', '8' with Driveway_Access
trafficAccident2020.dt$Relation_To_Junction[trafficAccident2020.dt$Relation_To_Junction %in% c(4,8)] <- 'Driveway_Access'

# Checking
trafficAccident2020.dt %>% 
  group_by(Relation_To_Junction) %>%
  summarise(no_rows = length(Relation_To_Junction), percentage = length(Relation_To_Junction)/nrow(trafficAccident2020.dt) * 100)

#==================================#
# CLEANING 37. Type_Of_Intersection
#==================================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Type_Of_Intersection) %>%
  summarise(no_rows = length(Type_Of_Intersection), percentage = length(Type_Of_Intersection)/nrow(trafficAccident2020.dt) * 100)

# Low amount of unknowns, we will simply replace them  with the mode here
nrow(trafficAccident2020.dt[Type_Of_Intersection %in% c(11,98,99), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '1', '11', '98','99' with Not_Intersection
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection %in% c(1,11,98,99)] <- 'Not_Intersection'

# Replacing '2' with 4_Way
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 2] <- '4_Way'

# Replacing '3' with T
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 3] <- 'T'

# Replacing '4' with Y
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 4] <- 'Y'

# Replacing '5' with Traffic_Circle
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 5] <- 'Traffic_Circle'

# Replacing '6' with Roundabout
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 6] <- 'Roundabout'

# Replacing '7' with 5_Point
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 7] <- '5_Point'

# Replacing '10 with L
trafficAccident2020.dt$Type_Of_Intersection[trafficAccident2020.dt$Type_Of_Intersection == 10] <- 'Y'

# Checking
trafficAccident2020.dt %>% 
  group_by(Type_Of_Intersection) %>%
  summarise(no_rows = length(Type_Of_Intersection), percentage = length(Type_Of_Intersection)/nrow(trafficAccident2020.dt) * 100)

#===============================#
# CLEANING 38. Location_Of_Crash
#===============================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Location_Of_Crash) %>%
  summarise(no_rows = length(Location_Of_Crash), percentage = length(Location_Of_Crash)/nrow(trafficAccident2020.dt) * 100)

# Low amount of unknowns, we will simply replace them  with the mode here
nrow(trafficAccident2020.dt[Location_Of_Crash %in% c(98,99), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '1', '11', '98','99' with On_Roadway
trafficAccident2020.dt$Location_Of_Crash[trafficAccident2020.dt$Location_Of_Crash %in% c(1,11,98,99)] <- 'On_Roadway'

# Replacing '2','3','4','5','6','7','8','10','12' with Off_Roadway
trafficAccident2020.dt$Location_Of_Crash[trafficAccident2020.dt$Location_Of_Crash %in% c(2,3,4,5,6,7,8,10,12)] <- 'Off_Roadway'

# Checking
trafficAccident2020.dt %>% 
  group_by(Location_Of_Crash) %>%
  summarise(no_rows = length(Location_Of_Crash), percentage = length(Location_Of_Crash)/nrow(trafficAccident2020.dt) * 100)

#=============================#
# CLEANING 39. Light_Condition
#=============================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Light_Condition) %>%
  summarise(no_rows = length(Light_Condition), percentage = length(Light_Condition)/nrow(trafficAccident2020.dt) * 100)

# 0.306% of rows of data, can replace with the mode, which is high_lighting  in this case
nrow(trafficAccident2020.dt[Light_Condition %in% c(7,8,9), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '1', '7','8', '9' with High_Light
trafficAccident2020.dt$Light_Condition[trafficAccident2020.dt$Light_Condition %in% c(1,7,8,9)] <- 'High_Light'

# Replacing '2' with Low_Light
trafficAccident2020.dt$Light_Condition[trafficAccident2020.dt$Light_Condition == 2] <- 'Low_Light'

# Replacing '3','4','5' '6' with Medium_Light
trafficAccident2020.dt$Light_Condition[trafficAccident2020.dt$Light_Condition %in% c(3,4,5,6)] <- 'Medium_Light'

# Checking
trafficAccident2020.dt %>% 
  group_by(Light_Condition) %>%
  summarise(no_rows = length(Light_Condition), percentage = length(Light_Condition)/nrow(trafficAccident2020.dt) * 100)

#========================#
# CLEANING 40. Weather
#========================#

# Getting no of values and percentage for each level
trafficAccident2020.dt %>% 
  group_by(Weather) %>%
  summarise(no_rows = length(Weather), percentage = length(Weather)/nrow(trafficAccident2020.dt) * 100)

# Getting % of data for weather conditions which are not applicable in singapore/ not reported/reported as unknown
# ~ 7 %, can be replaced with mode.
nrow(trafficAccident2020.dt[Weather %in% c(3,4,6,7,8,11,98,99), ])/nrow(trafficAccident2020.dt) * 100

# Replacing '1','3','4','6','7','8','11','98','99' with Clear weather since its the mode. Some of these weather conditions are not applicable in Singapore
# hence we replace them with the mode. 

trafficAccident2020.dt$Weather[trafficAccident2020.dt$Weather %in% c(1,3,4,6,7,8,11,98,99)] <- 'Clear'

# Replacing '2' and '12' with rain
trafficAccident2020.dt$Weather[trafficAccident2020.dt$Weather %in% c(2,12)] <- 'Rain'

# Replacing '5' with Fog
trafficAccident2020.dt$Weather[trafficAccident2020.dt$Weather == 5] <- 'Fog'

# Replacing '10' with Cloudy
trafficAccident2020.dt$Weather[trafficAccident2020.dt$Weather == 10] <- 'Cloudy'

# Checking
trafficAccident2020.dt %>% 
  group_by(Weather) %>%
  summarise(no_rows = length(Weather), percentage = length(Weather)/nrow(trafficAccident2020.dt) * 100)


#==================================================================================================================================#
#                                                       END OF DATA CLEANING                                                       #
#==================================================================================================================================#

# Dropping redundant columns
trafficAccident2020.dt[, c("Non_Motorist_Location" ) := NULL]

# Factorise Categorical Columns
cols <- c("Sex", "Person_Type", "Seat_Position",'Restraint_Equipment_Usage', 'Airbag', 'Drugs',  'Hit_And_Run', 'Owner_Type', 'Vehicle_Configuration',
          'Hazardous_Mat_Involvement', 'Speeding_Related','Trafficway_Flow', 'Num_Of_Lanes', 'Roadway_Profile', 'Pavement_Type', 'Roadway_Surface_Condition', 'Pre_Crash_Activity', 'Critical_Activity',
          'Drinking', 'Vehicle_Classification', 'Driver_Distracted', 'Driver_Factors', 'Day', 'Month', 'Day_Of_Week', 'Hour', 'Harmful_Event', 'Relation_To_Junction', 'Type_Of_Intersection', 'Location_Of_Crash',
          'Light_Condition', 'Weather','Death')
setDT(trafficAccident2020.dt)[, (cols):= lapply(.SD, factor), .SDcols=cols]
sapply(trafficAccident2020.dt, class)

# Total of 33 Categorical X Variables, 6 Continuous X Variables and 1 Y Variable and 3 identifier variables = 43 Variables.
sum(sapply(trafficAccident2020.dt, is.factor))

# Checking all good
str(trafficAccident2020.dt)

#====================================================================================#
# Save cleaned dataset (Change Working Directory to Respective Relevant directories)
#====================================================================================#
# write.csv(trafficAccident2020.dt,"C:/Users/Wei Kang/OneDrive - Nanyang Technological University/Year 2/Sem 2/BC2407 Analytics II/BC2407 Course Materials/Group Project/BC2407-Analytics-II-Project/Dataset/trafficAccident2020_cleaned.csv", row.names = FALSE)
