##Load required packages
library(ggplot2)
library(caret)
library(ggmap)

#Read in data
wheat13 <- read.csv("C:/Users/Zhiyu/Documents/Aerial_intel/wheat-2013-supervised.csv",
                    header = TRUE, stringsAsFactors = FALSE)
head(wheat13)

wheat14 <- read.csv("C:/Users/Zhiyu/Documents/Aerial_intel/wheat-2014-supervised.csv",
                    header = TRUE, stringsAsFactors = FALSE)
head(wheat14)

#Check some general information
table(wheat13$CountyName, useNA = "always")
sapply(wheat13, function(x) sum(is.na(x)))
sapply(wheat13, function(x) class(x))


##Check date range of both datasets
aaa <- substr(wheat13$Date, 1,10)
range(as.Date(aaa, "%m/%d/%Y"))

##Check the list of counties for wheat13 and wheat14, there are 6 more counties
length(unique(wheat13$CountyName))
length(unique(wheat14$CountyName))

##Check the latitude and longitude for both datasets
##get a aggregate level of location levels
loc.rd1 <- as.factor(paste(round(wheat13$Latitude, digits = 0), ":",
                           round(wheat13$Longitude, digits = 0), sep = ""))
head(loc.rd1)

loc.rd1.14 <- as.factor(paste(round(wheat14$Latitude, digits = 0), ":",
                              round(wheat14$Longitude, digits = 0), sep = ""))
head(loc.rd1.14)

loc.levels <- unique(c(levels(loc.rd1),levels(loc.rd1.14)))
length(loc.levels)


###Write a function to transform the data with new variables
Tranformdata <- function(inData, location.levels){
  ##Change the date to Date format
  datetemp <- as.Date(substr(inData[["Date"]], 1, 10), "%m/%d/%Y")
  # month of a time
  inData[["month"]] <- months(datetemp)
  
  ##Use round information of longitude and latitude as follows
  inData[["location.rd1"]] <- factor(paste(round(inData[["Latitude"]], digits = 0), ":",
                                           round(inData[["Longitude"]], digits = 0), sep = ""),
                                          levels = location.levels)
  
  return (inData)
  
}



#function to calculate the mean squared error for evaluation purpose
MeansqError <- function(var.pred, var.actual){
  M.S.error <- mean((var.pred - var.actual)^2)
  
  return (M.S.error)
}

###Do some visualization of the data
length(unique(wheat13$State))
length(unique(wheat13$CountyName))

#boxplot
boxplot(Yield ~ State, data = wheat13)

##density plot
map <- get_map(location = 'USA', zoom = 4, 
               maptype="terrain",
               color="bw")
mapPointsDA <- ggmap(map) +
  geom_point(aes(x = Longitude, y = Latitude, z = Yield), col = "blue",  
             data = wheat13, alpha = .3) 

mapPointsDA



##change some variables to factor
wheat13.trs <- Tranformdata(wheat13, location.levels = loc.levels)
names(wheat13.trs)
sapply(wheat13.trs, function(x) class(x))

##remove the missing values since they only take a very tiny proportion of total
#number of obs
summary(wheat13.trs)
wheat13.nmiss <- wheat13.trs[!is.na(wheat13.trs$precipIntensity) & !is.na(wheat13.trs$pressure)
            & !is.na(wheat13.trs$visibility), ]

##Change the following variables into factor type
tofacvar <- c("precipTypeIsRain", "precipTypeIsSnow", "precipTypeIsOther", "month")
for (i in 1:length(tofacvar)){
  wheat13.nmiss[[tofacvar[i]]] <- as.factor(wheat13.nmiss[[tofacvar[i]]])
}

##subset the dataset for modeling
trainData <- subset(wheat13.nmiss, 
                    select = -c(CountyName, State, Latitude, Longitude, Date, precipTypeIsOther))

#p <- 0.3
#Select 10% of the original dataset to train the model
p <- 0.1
sampleidx <- sample(1:nrow(trainData), p*nrow(trainData), replace = FALSE)
trainData.sp <- trainData[sampleidx, ]
hist(trainData.sp$Yield)


##Run two different models: Gradient boosting model and Random forests model
gbm.fit <- train(Yield ~ ., data = trainData.sp,
                trControl = trainControl(method = "cv", number = 3), 
                verbose = FALSE, method = "gbm")
saveRDS(gbm.fit, "C:/Users/Zhiyu/Documents/Aerial_intel/gbmfit_v1.rds")
varImp(gbm.fit)

gbm.fit <- readRDS("C:/Users/Zhiyu/Documents/Aerial_intel/gbmfit_v2.rds")
varImp(gbm.fit)

rf.fit <- train(Yield ~ ., data = trainData.sp,
                trControl = trainControl(method = "cv", number = 3), 
                verbose = FALSE, method = "rf")
saveRDS(rf.fit, "C:/Users/Zhiyu/Documents/Aerial_intel/rffit.rds")

#################################################################
###Test dataset - use wheat14 to test the prediction performance
summary(wheat14)
wheat14.trs <- Tranformdata(wheat14, location.levels = loc.levels)

wheat14.nmiss <- wheat14.trs[!is.na(wheat14.trs$pressure) & !is.na(wheat14.trs$visibility), ]
##Change the following variables into factor type
tofacvar <- c("precipTypeIsRain", "precipTypeIsSnow", "precipTypeIsOther", "month")
for (i in 1:length(tofacvar)){
  wheat14.nmiss[[tofacvar[i]]] <- as.factor(wheat14.nmiss[[tofacvar[i]]])
}

#predict on 2014 data
pred14.prod <- predict(gbm.fit, newdata = wheat14.nmiss)
pred14.prod.v1 <- predict(gbm.fit, newdata = wheat14.nmiss)

head(pred14.prod)
head(wheat14$Yield)

#Calculate the MSE
MeansqError(pred14.prod, wheat14.nmiss$Yield)
MeansqError(pred14.prod.v1, wheat14.nmiss$Yield)

pred14.prod.rf <- predict(rf.fit, newdata = wheat14.nmiss)
MeansqError(pred14.prod.rf, wheat14.nmiss$Yield)

####predictions for county level and calculate the MSE using the average of predictions
#based on the county level.
eval.data <- data.frame(county = wheat14.nmiss$CountyName, 
                        act.val = wheat14.nmiss$Yield,
                        pred.val = pred14.prod)

saveRDS(eval.data, "C:/Users/Zhiyu/Documents/Aerial_intel/eval_data.rds")

#by county level
eval.pred <- eval.data %>% group_by(county) %>%
  summarize(pred.mean = mean(pred.val))

eval.pred <- as.data.frame(eval.pred)

eval.act1 <- subset(eval.data, select = c(county, act.val))
eval.act2 <- unique(eval.act1)

eval.total <- merge(eval.pred, eval.act2, by = "county")
mean((eval.total$pred.mean - eval.total$act.val)^2)

