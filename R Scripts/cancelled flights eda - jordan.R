library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)
theme_set(theme_bw())


dd <- fread("C:/Users/jorda/iCloudDrive/Documents/BU MSBA COURSES/BA810/flights_delay_model.csv")


#### looking at relationship between departure delay and cancellations ####


# dd where cancellations > 0

dd_cancelled <- dd[dd$Total_Cancellations > 0]

# canceled:       320,678 flights
# Not canceled:   524,429 flights
# 61% of flights saw a cancellation at the origin airport the day before




# looking at sum of cancellations and average dep_delay by airport for all obs, 2019, 2020


airports_19 <- dd[YEAR==2019,.(sum(Total_Cancellations), mean(DEP_DELAY)), by=ORIGIN][order(-V2)]
setnames(airports_19, c("V1", "V2"), c("Sum Cancellations", "Average Departure Delay"))

airports_20 <- dd[YEAR==2020,.(sum(Total_Cancellations), mean(DEP_DELAY)), by=ORIGIN][order(-V2)]
setnames(airports_20, c("V1", "V2"), c("Sum Cancellations", "Average Departure Delay"))


airports = merge(airports_19, airports_20, by="ORIGIN", all.x=TRUE)

airports[, change := `Average Departure Delay.y` - `Average Departure Delay.x`]

ggplot(airports, aes(change)) + geom_histogram()

#
# ggplot(airports, aes("Average Departure Delay.y", fill="Average Departure Delay.x" )) +
#  geom_bar(position = "dodge")
#
  
  
  
  
  

# some simple stats

dd[,mean(DEP_DELAY)]
# 5.7742

dd_cancelled[,mean(DEP_DELAY)]
# 6.6609

dd[Total_Cancellations==0, mean(DEP_DELAY)]
# 5.2319

dd_cancelled[,max(Total_Cancellations)]
# 174



# looking at cancellations instead by year, 2019 vs 2020 to see what impact covid might have had
ggplot(dd_cancelled, aes(x=YEAR, y=Total_Cancellations)) +
  geom_bar(stat="identity")
#there were fewer cancellations in march 2020 likely because there were just fewer flights due to covid





# plotting departure delay where canceled > 0
ggplot(dd_cancelled, aes(x=Total_Cancellations, y=DEP_DELAY)) +
  geom_point(alpha=.5)


ggplot(dd_cancelled, aes(x=Total_Cancellations)) +
  geom_bar()


# plotting departure delay for all canceled values
ggplot(dd, aes(x=Total_Cancellations, y=DEP_DELAY)) +
  geom_point(alpha=.5)


ggplot(dd, aes(x=Total_Cancellations)) +
  geom_bar()









ggplot(dd, aes(x=DISTANCE, y=DEP_DELAY)) + geom_point()










#
# plotting departure delay where canceled = 0
ggplot(dd[dd$Total_Cancellations=0], aes(x=))


##########################################################################


# vars and dtypes

dd <- fread("C:/Users/jorda/iCloudDrive/Documents/BU MSBA COURSES/BA810/flights_delay_model.csv")

str(dd)

dd$DAY_OF_MONTH <- as.factor(dd$DAY_OF_MONTH)
dd$YEAR <- as.factor(dd$YEAR)
dd$DAY_OF_WEEK <- as.factor(dd$DAY_OF_WEEK)

# FEATURES FOR MODEL BY ORIGIN (BOS, LAX, ATL)

# DEP_DELAY
# AIRLINE
# DAY OF MONTH
# YEAR
# DAY OF WEEK
# AIR TIME
# DISTANCE
# EARLY AM
# AM
# PM
# LATE PM
# Total Cancellations



















