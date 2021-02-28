## DATA PRE-PROCESSING 

# imports
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)
theme_set(theme_bw())


# load dataset
dd_raw <- fread("C:/Users/jorda/iCloudDrive/Documents/BU MSBA COURSES/BA810/flights.csv")


# converting YEAR number
dd_raw$YEAR <- as.Date(as.character(dd_raw$YEAR), format = "%Y")
dd_raw$YEAR <- year(dd_raw$YEAR)


# converting INT values to NUM or Factor or Log
dd_raw$MONTH <- as.numeric(dd_raw$MONTH)
dd_raw$DAY_OF_MONTH <- as.factor(dd_raw$DAY_OF_MONTH)
dd_raw$DAY_OF_WEEK <- as.factor(dd_raw$DAY_OF_WEEK)
dd_raw$MKT_CARRIER_AIRLINE_ID <- as.numeric(dd_raw$MKT_CARRIER_AIRLINE_ID)
dd_raw$Code <- as.factor(dd_raw$Code)
dd_raw$CANCELLED <- dd_raw$CANCELLED == 1
dd_raw$DIVERTED <- dd_raw$DIVERTED == 1


# renaming columns
colnames(dd_raw)[30] <- "CODE"
colnames(dd_raw)[31] <- "AIRLINE"


# removing unneeded cols
dd_raw[, string_field_28 := NULL]


# creating columns for flight scheduled time buckets

dd_raw$EARLY_AM <- FALSE
dd_raw$EARLY_AM[dd_raw$CRS_DEP_TIME >= 1 & dd_raw$CRS_DEP_TIME <= 559] <- TRUE

dd_raw$AM <- FALSE
dd_raw$AM[dd_raw$CRS_DEP_TIME >= 600 & dd_raw$CRS_DEP_TIME <= 1159] <- TRUE

dd_raw$PM <- FALSE
dd_raw$PM[dd_raw$CRS_DEP_TIME >= 1200 & dd_raw$CRS_DEP_TIME <= 1959] <- TRUE

dd_raw$LATE_PM <- FALSE
dd_raw$LATE_PM[dd_raw$CRS_DEP_TIME >= 2000 & dd_raw$CRS_DEP_TIME <= 2400] <- TRUE


# remove rows where DEP_DELAY is null so we do not skew future results
# we have a lot of rows, we're not afraid to remove some because DEP_DELAY is too important to have missing values
dd_raw <- dd_raw[!is.na(dd_raw$DEP_DELAY),]


# create two datasets for EDA & models
# 1 - dd_MASTER : contains data for all airlines (1,259,197 rows)
dd_MASTER <- dd_raw


# 2 - dd_MASTER_3 : contains data for just American Airlines, Delta Airlines, and United Airlines (845,101 rows)
dd_MASTER_3 <- dd_raw[dd_raw$CODE == "AA" | dd_raw$CODE == "DL" | dd_raw$CODE == "UA",]


