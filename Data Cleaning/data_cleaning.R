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

# Factorise Categorical Columns
cols <- c("Sex", "Person_Type", "Injury_Severity", "Seat_Position",'Restraint_Equipment_Usage', 'Airbag', 'Drugs', 'Non_Motorist_Location', 'Hit_And_Run', 'Owner_Type', 'Vehicle_Configuration',
          'Hazardous_Mat_Involvement', 'Speeding_Related','Trafficway_Flow', 'Num_of_Lanes', 'Roadway_Profile', 'Pavement_Type', 'Roadway_Surface_Condition', 'Pre_Crash_Activity', 'Critical_Activity',
          'Drinking', 'Vehicle_Classification', 'Driver_Distracted', 'Driver_Factors', 'Day', 'Month', 'Day_Of_Week', 'Hour', 'Harmful_Event', 'Relation_To_Junction', 'Type_Of_Intersection', 'Location_Of_Crash',
          'Light_Condition', 'Weather')
setDT(trafficAccident2020.dt)[, (cols):= lapply(.SD, factor), .SDcols=cols]
sapply(trafficAccident2020.dt, class)

# Total of 34 Categorical X Variables, 6 Continuous X Variables and 1 Y Variable = 41 Variables.
sum(sapply(trafficAccident2020.dt, is.factor))
str(trafficAccident2020.dt)

#=======================#
# CLEANING 1. AGE
#=======================#

trafficAccident2020.dt[Age == 0, ] # 0 incidences

trafficAccident2020.dt[Age == 97, ] # 4 incidences (97 or older)
# Since it's only 4 incidences, we assume all 4 cases are when the individual involved is 97 years old.

trafficAccident2020.dt[Age == 99, ] # 2 incidences (Unknown)
# Since it's only 2 incidences, we will replace with the median in this case.
trafficAccident2020.dt$Age[which(trafficAccident2020.dt$Age == 99)] <- median(trafficAccident2020.dt$Age)

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


# Defining the mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Getting the Mode
mode_Sex <- mode(trafficAccident2020.dt$Sex)
mode_Sex

# Replacing with Mode and Changing 1 & 2 to Male and Female

trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 3] <- mode_Sex # (Somehow when i convert to integer, it becomes 3 and 4 instead of 8 and 9.)
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 4] <- mode_Sex
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 1] <- 'M'
trafficAccident2020.dt$Sex[trafficAccident2020.dt$Sex == 2] <- 'F' 


# Convert back to factor
trafficAccident2020.dt$Sex <- as.factor(trafficAccident2020.dt$Sex)

# Checking
trafficAccident2020.dt %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex))

