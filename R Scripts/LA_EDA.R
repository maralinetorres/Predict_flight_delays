# load libraries
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(glmnet)
library(tidyr)
theme_set(theme_bw())

##Load dataset
df <- fread("flights-master.csv")
df[,Day_before := (DAY_OF_MONTH - 1)]
str(df)
b <- fread("total_cancellation.csv")
setkey(b, ORIGIN, ORIGIN_CITY_NAME, AIRLINE,Day_before, DAY_OF_MONTH, YEAR, DEST, CRS_DEP_TIME)
setkey(df, ORIGIN, ORIGIN_CITY_NAME, AIRLINE, Day_before, DAY_OF_MONTH, YEAR, DEST, CRS_DEP_TIME)
result <- merge(df, b, all.x =TRUE)

######CLEAN THE COLUMNS NAME ##################
result = subset(result, select = -c(38:66,68) )
names(result) <- gsub("\\.x","",names(result))
result[is.na(Total_Cancellations), Total_Cancellations := 0]
write.csv(result,"original_flights_df.csv", row.names = FALSE)

cols <- c('MONTH','MKT_CARRIER_AIRLINE_ID','MKT_CARRIER','TAIL_NUM',
          'ORIGIN_CITY_NAME','ORIGIN_STATE_ABR','DEST_CITY_NAME','DEST_STATE_ABR',
          'DEST_STATE_NM','DEP_TIME','ARR_TIME','ARR_DELAY',
          'CANCELLED','DIVERTED','ACTUAL_ELAPSED_TIME','DIV_AIRPORT_LANDINGS','CODE',
          'Day_before','CRS_DEP_TIME_Formatted','CRS_DEP_HOUR','DIV_REACHED_DEST')

r <- copy(result)
setDF(r)
r <- r[,!(names(r) %in% c(cols))]
setDT(r)
write.csv(r,"flights_model_df.csv", row.names = FALSE)

# EXTRA CLEANING #
df <- fread("flights_model_df.csv")
df[,CRS_DEP_TIME := NULL]
df[,CRS_ELAPSED_TIME := NULL]
df[,DEST := NULL]
str(df)
####################

#### CHANGE DATA TYPES ##########
df$DAY_OF_MONTH <- as.factor(df$DAY_OF_MONTH)
df$DAY_OF_WEEK <- as.factor(df$DAY_OF_WEEK)
df$ORIGIN <- as.factor(df$ORIGIN)
df$AIRLINE <- as.factor(df$AIRLINE)
df$YEAR <- as.factor(df$YEAR)
df$DEP_DELAY <- as.numeric(df$DEP_DELAY)
############################################

########## SUBSETTING TO LOS ANGELES AIRPORT ##############

la <- df[ORIGIN == 'LAX',]
la[,.N, by ='AIRLINE']
setDT((la))
###############################

# How many records for each airline
p1 <- ggplot(la) +
  geom_bar(mapping=aes(x=AIRLINE), fill="light blue")

p1 + theme(axis.text.x = element_text(angle = 20)) + xlab("Airline") + ylab("Frequency")
#American Airlines have more flights in LA
#United and Delta are very similar regarding frequency

early_am <- la[EARLY_AM == TRUE,.(.N, mean_dep = mean(DEP_DELAY)),by = .(EarlyAM = EARLY_AM)]
colnames(early_am)[1] <- 'Interval'
early_am$Interval <- as.character(early_am$Interval)
early_am[, Interval := 'EARLY AM']

am <- la[AM == TRUE,.(.N, mean_dep = mean(DEP_DELAY)),by = .(AM = AM)]
colnames(am)[1] <- 'Interval'
am$Interval <- as.character(am$Interval)
am[, Interval := 'AM']

pm <- la[PM == TRUE,.(.N, mean_dep = mean(DEP_DELAY)),by = .(PM = PM)]
colnames(pm)[1] <- 'Interval'
pm$Interval <- as.character(pm$Interval)
pm[, Interval := 'PM']

late_pm <- la[LATE_PM == TRUE,.(.N, mean_dep = mean(DEP_DELAY)),by = .(LATE_PM = LATE_PM)]
colnames(late_pm)[1] <- 'Interval'
late_pm$Interval <- as.character(late_pm$Interval)
late_pm[, Interval := 'LATE PM']

category <- rbind(early_am, am, pm, late_pm)
ggplot(category, aes(x=Interval, y=N, fill=Interval)) +
  geom_col() + ggtitle("Airline flights by interval") + ylab("Frequency")

#Most of the flights are AM and PM

head(la)
library(chron)
minutes <- la$DEP_DELAY
minutes <- substr(times((minutes%/%60 +  minutes%%60 /60)/24), 1, 5)
delayed <- df[DEP_DELAY > 0, .(number_delays = .N), by = ORIGIN]
not_delayed <- df[DEP_DELAY <= 0,.(number_on_time = .N), by= ORIGIN]
total <- merge(delayed, not_delayed, by='ORIGIN')

X <- df[DEP_DELAY > 0, .(meanDelay = mean(DEP_DELAY)), by =ORIGIN]
#df[DEP_DELAY > 0, .(meanDelay = mean(DEP_DELAY)), by =ORIGIN][order(-meanDelay)][1:10]

total$ORIGIN <- as.factor(total$ORIGIN)
dat_long <- total %>%
  gather("Stat", "Value", -ORIGIN)

setDT(dat_long)

ggplot(dat_long[ORIGIN == 'LAX',], aes(x = ORIGIN, y = Value, fill = Stat)) +
  geom_col(position = "dodge")

#We have more on time flights than delay flights in Los Angeles airport

weekly_delay <- la[,.(mean_delay = mean(DEP_DELAY)), by ='DAY_OF_WEEK']
weekly_delay
ggplot(weekly_delay,aes(x = DAY_OF_WEEK, y = mean_delay)) + geom_point() + geom_smooth(method = "glm")

#It shows increase from Wednesday to Friday and then it decreases in the weekend
#Tuesday show less delays in average compare to other days

day_month_delay <-  la[,.(mean_delay = mean(DEP_DELAY)), by ='DAY_OF_MONTH']
day_month_delay
ggplot(day_month_delay,aes(x = DAY_OF_MONTH, y = mean_delay)) + geom_point() + geom_smooth(method = "lm")

ggplot(la[Total_Cancellations > 0 & DEP_DELAY > 0, ],aes(x = Total_Cancellations, y = DEP_DELAY)) + geom_point() + geom_smooth(method = "lm")

#WHEN THE DELAY IS MORE THAN 400 MINUTES (6~7 hours) we start to see a relationship... 
#For delays of more than 6 hours, there is slightly positive relationship with cancellations

ggplot(la[Total_Cancellations > 0 & DEP_DELAY > 400, ],aes(x = Total_Cancellations, y = DEP_DELAY)) + geom_point() + geom_smooth(method = "lm")


####### See delays by departure categories #############
category
ggplot(category, aes(x=Interval, y=mean_dep, fill=Interval)) +
  geom_col() + ggtitle("Departure average delay by interval") + ylab("AVERAGE")


## In average, there are more delays in the afternoon (pm and late pm)
# Early AM has the lowest average delay
# ORDER -> PM, LATE PM, AM , EARLY AM


####### DISTANCE and DELAYS #######
distance_delay_weekly <- la[,.(delay = mean(DEP_DELAY), dist = mean(DISTANCE)), by ='DAY_OF_WEEK']
ggplot(distance_delay_weekly,aes(x = dist, y = delay)) + geom_point() + geom_smooth(method = "lm")

distance_delay_airline <- la[,.( delay = mean(DEP_DELAY), dist = mean(DISTANCE)), by ='AIRLINE']
ggplot(distance_delay_airline,aes(x = dist, y = delay)) + geom_point() + geom_smooth(method = "lm")

#If we consider the distance of each flight every day of the week, we see a positive relationship between Delay and Distance
# What if we dont filter by 3 airlines? Use all of it?

ggplot(la) +
  geom_bar(mapping=aes(YEAR), fill="light blue")

#MORE records for 2019 than 2020


############ WEATHER API ##########################

weather <- fread("los_angeles_weather.csv")
weather$day <- as.factor(weather$day)
weather$year <- as.factor(weather$year)

los_angeles <- merge(la, weather, all.x =TRUE, by.x = c('DAY_OF_MONTH','YEAR'), by.y = c('day','year'))
los_angeles[,.(location := NULL, date_time := NULL)]
write.csv(los_angeles,"la_flights.csv", row.names = FALSE)

str(los_angeles)
