# load libraries
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(glmnet)
library(tidyr)
library(chron)
library(hms)

theme_set(theme_bw())



##Load dataset
df <- fread("C:/Users/jorda/iCloudDrive/Documents/BU MSBA COURSES/BA810/flights-All_airlines.csv")
df$DAY_OF_MONTH <- as.factor(df$DAY_OF_MONTH)
df$YEAR <- as.factor(df$YEAR)
df$DAY_OF_WEEK <- as.factor(df$DAY_OF_WEEK)



# dropping columns for models
df[,MKT_CARRIER_AIRLINE_ID := NULL]
df[,MKT_CARRIER := NULL]
df[,TAIL_NUM := NULL]
df[,ORIGIN_CITY_NAME := NULL]
df[,ORIGIN_STATE_ABR := NULL]
df[,DEST_CITY_NAME := NULL]
df[,DEST_STATE_ABR := NULL]
df[,DEST_STATE_NM := NULL]
df[,DEP_TIME := NULL]
df[,CRS_ARR_TIME := NULL]
df[,ARR_TIME := NULL]
df[,ARR_DELAY := NULL]
df[,CANCELLED := NULL]
df[,DIVERTED := NULL]
df[,CRS_ELAPSED_TIME := NULL]
df[,ACTUAL_ELAPSED_TIME := NULL]
df[,AIR_TIME := NULL]
df[,DIV_AIRPORT_LANDINGS := NULL]
df[,DIV_REACHED_DEST := NULL]
df[,CODE := NULL]
df[,EARLY_AM := NULL]
df[,AM := NULL]
df[,PM := NULL]
df[,LATE_PM := NULL]



# subsetting for just Boston flights
bos <- df[ORIGIN == "BOS"]



# getting hour buckets from scheduled flight departure
bos$CRS_DEP_TIME <- as.character(bos$CRS_DEP_TIME)
for( i in 1:nrow(bos)){
  x <- bos[i,]
  number_string = nchar(x$CRS_DEP_TIME)
  if(number_string == 3){
    bos[i,CRS_DEP_TIME := paste0("0", CRS_DEP_TIME)]
  }else if(number_string == 2){
    bos[i,CRS_DEP_TIME := paste0("0", CRS_DEP_TIME)]
    bos[i,CRS_DEP_TIME := paste0("0", CRS_DEP_TIME)]
  }else if(number_string == 1){
    bos[i,CRS_DEP_TIME := paste0("0", CRS_DEP_TIME)]
    bos[i,CRS_DEP_TIME := paste0("0", CRS_DEP_TIME)]
    bos[i,CRS_DEP_TIME := paste0("0", CRS_DEP_TIME)]
  }
}

d1 <- strptime(bos$CRS_DEP_TIME, format = "%H%M")
d2 <- format(d1, format = "%H:%M:%S")
bos[,CRS_DEP_TIME_Formatted := d2]
bos$CRS_DEP_TIME_Formatted <- as.factor(bos$CRS_DEP_TIME_Formatted)
bos[,CRS_DEP_HOUR := hour(as.hms(as.character(bos$CRS_DEP_TIME_Formatted)))]



### Adding in weather data ###
weather_bos <- fread("C:/Users/jorda/iCloudDrive/Documents/BU MSBA COURSES/BA810/boston-weather.csv")


weather_bos$day <- as.factor(weather_bos$day)
weather_bos$year <- as.factor(weather_bos$year)

boston <- merge(bos, weather_bos, all.x =TRUE, by.x = c('DAY_OF_MONTH','YEAR'), by.y = c('day','year'))
boston[,location := NULL]
boston[, date_time := NULL]






#####
### EDA for BOS ###
p1 <- ggplot(boston) +
  geom_bar(mapping=aes(x=AIRLINE), fill="light blue")

p1 + theme(axis.text.x = element_text(angle = 20)) + xlab("Airline") + ylab("Frequency")

# Jetblue is the most popular airline out of Boston


# Looking at how distance of flight relates to departure delay
p2 <- ggplot(boston, aes(x=DISTANCE, y=DEP_DELAY)) + geom_point()
p2

# Looks like shorter distances have higher delays with a few outliers










### NAIVE LINEAR REGRESSION ###

yhat <- mean(boston$DEP_DELAY)
boston_NLR <- boston
boston_NLR[, yhat := yhat]

mse.NLR <- mean((boston_NLR$DEP_DELAY - boston_NLR$yhat)^2)
# 1894.26




### LINEAR REGRESSION ###

set.seed(810)
boston_LM1 <- boston

# train/test split
test_index <- sample(nrow(boston_LM1), 4678) # assign 100K random rows to the test set
# now split
dd.test <- boston_LM1[test_index,]
dd.train <- boston_LM1[-test_index,]


# LM Formulas
f1 <- as.formula(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DEST + DISTANCE + AIRLINE + CRS_DEP_HOUR + humidity + precipMM + pressure + tempC + visibility + windspeedKmph)

f2 <- as.formula(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DEST + DISTANCE + AIRLINE + CRS_DEP_HOUR + visibility + windspeedKmph)

f3 <- as.formula(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DISTANCE + CRS_DEP_HOUR + humidity + precipMM + pressure + tempC + visibility + windspeedKmph)



y.train <- dd.train$DEP_DELAY
y.test <- dd.test$DEP_DELAY


# Fitting the LM model
fit.lm1 <- lm(f1, dd.train)   # all predictors
fit.lm2 <- lm(f2, dd.train)   # removing humidity, precip, pressure, temp
fit.lm3 <- lm(f3, dd.train)   # removing airline and destination as we don't think these are great predictors


# compute MSEs for training LMs
# LM1
yhat.train.lm1 <- predict(fit.lm1)
mse.train.lm1 <- mean((y.train - yhat.train.lm1)^2)
# 1483.93 


# LM2
yhat.train.lm2 <- predict(fit.lm2)
mse.train.lm2 <- mean((y.train - yhat.train.lm2)^2)
# 1547.80


# LM3
yhat.train.lm3 <- predict(fit.lm3)
mse.train.lm3 <- mean((y.train - yhat.train.lm3)^2)
# 1497.89

### LM1 with all predictors is the best model and is significantly better than baseline NLR MSE of 1894 (mse = 1483)



# Test MSE
# LM1
yhat.test.lm1 <- predict(fit.lm1, dd.test)
mse.test.lm1 <- mean((y.test - yhat.test.lm1)^2)
# 1803.11


# LM2
yhat.test.lm2 <- predict(fit.lm2, dd.test)
mse.test.lm2 <- mean((y.test - yhat.test.lm2)^2)
# 1891.55


# LM3
yhat.test.lm3 <- predict(fit.lm3, dd.test)
mse.test.lm3 <- mean((y.test - yhat.test.lm3)^2)
# 1806.89  (42 mins lowest test MSE for LM with f3 formula/predictors)





#### RIDGE REGRESSION ####

boston <- boston[, yhat := NULL]
boston_RR <- boston



boston_RR[, test:=0]
boston_RR[sample(nrow(boston_RR), 4678), test:=1]

RR.test <- boston_RR[test==1]
RR.train <- boston_RR[test==0]


x1.train <- model.matrix(f1, RR.train)[, -1]
y.train <- RR.train$DEP_DELAY


x1.test <- model.matrix(f1, RR.test)[, -1]
y.test <- RR.test$DEP_DELAY



fit.ridge <- cv.glmnet(x1.train, y.train, alpha = 0, nfolds = 10)


# Ridge Train MSE
yhat.train.ridge <- predict(fit.ridge, x1.train, s = fit.ridge$lambda.min)
mse.train.ridge <- mean((y.train - yhat.train.ridge)^2)
# 1588.59


# Ridge Test MSE
yhat.test.ridge <- predict(fit.ridge, x1.test, s = fit.ridge$lambda.min)
mse.test.ridge <- mean((y.test - yhat.test.ridge)^2)
# 1389.71





#### LASSO REGRESSION ####


fit.lasso <- cv.glmnet(x1.train, y.train, alpha = 1, nfolds = 10)


# Lasso Train MSE
yhat.train.lasso <- predict(fit.lasso, x1.train, s = fit.lasso$lambda.min)
mse.train.lasso <- mean((y.train - yhat.train.lasso)^2)
# 1587.73


# Lasso Test MSE
yhat.test.lasso <- predict(fit.lasso, x1.test, s = fit.lasso$lambda.min)
mse.test.lasso <- mean((y.test - yhat.test.lasso)^2)
# 1386.19




#### ELASTIC NET ####


fit.net <- cv.glmnet(x1.train, y.train, alpha = 0.5, nfolds = 10)


# Elastic Net Train MSE
yhat.train.net <- predict(fit.net, x1.train, s = fit.net$lambda.min)
mse.train.net <- mean((y.train - yhat.train.net)^2)
# 1587.84


# Elastic Net Test MSE
yhat.test.net <- predict(fit.net, x1.test, s = fit.net$lambda.min)
mse.test.net <- mean((y.test - yhat.test.net)^2)
# 1386.29















