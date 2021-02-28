##classification tree

set.seed(2021)
library(rpart)
library(caret)
library(rpart.plot)
library(hms)


df <- fread("/Users/cc/Desktop/BA810/Predict_flight_delays/Dataset/flights-master.csv")
head(df,10)
df$DAY_OF_MONTH <- as.factor(df$DAY_OF_MONTH)
df$DAY_OF_WEEK <- as.factor(df$DAY_OF_WEEK)
df$ORIGIN <- as.factor(df$ORIGIN)
df$AIRLINE <- as.factor(df$AIRLINE)
df$YEAR <- as.factor(df$YEAR)
df$DEP_DELAY <- as.numeric(df$DEP_DELAY)

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

bos <- df[ORIGIN == 'BOS']
head(bos)
str(bos)

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


weather <- fread("/Users/cc/Desktop/BA810/Predict_flight_delays/Dataset/boston-weather.csv")



weather$day <- as.factor(weather$day)
weather$year <- as.factor(weather$year)

boston <- merge(bos, weather, all.x =TRUE, by.x = c('DAY_OF_MONTH','YEAR'), by.y = c('day','year'))
head(boston)

index=sample(2,nrow(boston),replace = TRUE,prob=c(0.8,0.2))
trainData<-boston[index==1,]  
testData<-boston[index==2,]

tree <- rpart(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DEST + DISTANCE + AIRLINE + CRS_DEP_HOUR + humidity + precipMM + pressure + tempC + visibility + windspeedKmph, data = trainData, method = "anova")
tree
rpart.plot(tree)
plotcp(tree)
tree$cptable

prune.tree <- prune(tree, cp = 0.01)
prp(prune.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)


pred <- predict(prune.tree, newdata = testData)

mse.tree <- mean((pred - testData$DEP_DELAY ) ^ 2)
print(mse.tree)

Values1 <- data.frame(obs = testData$DEP_DELAY, pred = pred)
defaultSummary(Values1)



install.packages("randomForest")
library(randomForest)
set.seed(2021)
index=sample(2,nrow(boston),replace = TRUE,prob=c(0.7,0.3))
trainData=boston[index==1,]  
testData=boston[index==2,]

trainData <- na.omit(trainData)
rf_ntree <- randomForest(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DEST + DISTANCE + AIRLINE + CRS_DEP_HOUR + humidity + precipMM + pressure + tempC + visibility + windspeedKmph,data = trainData,ntree=500, proximity=TRUE) 
plot(rf_ntree)

rsample.rf=randomForest(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DEST + DISTANCE + AIRLINE + CRS_DEP_HOUR + humidity + precipMM + pressure + tempC + visibility + windspeedKmph,data = trainData,ntree=60,mtry=2, proximity=TRUE) 
print(rsample.rf)

rsample.rf=randomForest(DEP_DELAY ~ DAY_OF_MONTH + DAY_OF_WEEK + DEST + DISTANCE + AIRLINE + CRS_DEP_HOUR + humidity + precipMM + pressure + tempC + visibility + windspeedKmph,data = trainData,ntree=100,mtry=3, proximity=TRUE)
print(rsample.rf)

importance(rsample.rf, type=2)
varImpPlot(rsample.rf)

rsample_pred=predict(rsample.rf,testData)







