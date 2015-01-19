train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# data preprocessing - feature engineering, output pdf
fit <- rpart(count ~ atemp + temp , data=train, method="anova", control=rpart.control(minsplit=2, cp=0.0005, cex= 0.15))
fancyRpartPlot(fit)

fit <- rpart(count ~ humidity , data=train, method="anova", control=rpart.control(minsplit=2, cp=0.0005, cex= 0.15))
fancyRpartPlot(fit)

fit <- rpart(count ~ season + weather + windspeed , data=train, method="anova", control=rpart.control(minsplit=2, cp=0.0005, cex= 0.15))
fancyRpartPlot(fit)

#Following can use is.na for check null value
#is.na(test$first) ... etc
 
#feature temp & atemp
test$first[test$atemp  <  30  & test$temp < 11] <- 1
test$first[test$atemp  <  30  & 11 <= test$temp & test$temp < 13] <- 2
test$first[test$atemp  <  30  & 13 <= test$temp & test$temp < 15] <- 3
test$first[test$atemp  <  30  &  15 <= test$temp & test$temp < 20] <- 4
test$first[test$atemp  <  30  &  20 <= test$temp & test$temp < 23] <- 5
test$first[test$atemp  <  30  &  23 <= test$temp & test$temp < 24] <- 6
test$first[test$atemp  <  30  & 24 <= test$temp] <- 7

test$first[33 > test$atemp & test$atemp >=  30  & test$temp >= 41] <- 8
test$first[33 > test$atemp & test$atemp >=  30  & 31 <= test$temp & test$temp < 41] <- 9
test$first[33 > test$atemp & test$atemp >=  30  & 27 <= test$temp & test$temp 31] <- 10
test$first[33 > test$atemp & test$atemp >=  30  & 25 <= test$temp & test$temp < 27] <- 11
test$first[33 > test$atemp & test$atemp >=  30  & test$temp < 25] <- 12

test$first[33 <= test$atemp & test$atemp < 34 &  test$temp < 29] <- 13
test$first[33 <= test$atemp & test$atemp < 34  & 29 <= test$temp & test$temp < 30] <- 14
test$first[33 <= test$atemp & test$atemp < 34  & 30 <= test$temp & test$temp < 31] <- 15
test$first[33 <= test$atemp & test$atemp < 34  & 31 <= test$temp] <- 16

test$first[34 <= test$atemp & test$atemp < 41 & test$temp < 29] <- 17
test$first[34 <= test$atemp & test$atemp < 41 & 29 <= test$temp & test$temp < 30] <- 18
test$first[34 <= test$atemp & test$atemp < 41 & 30 <= test$temp & test$temp < 31] <- 19

test$first[41 <= test$atemp & test$temp < 31] <- 20
test$first[41 <= test$atemp & test$temp >= 31] <- 21

#feature humidity
test$second[humidity >= 92] <- 22
test$second[84 <= humidity & humidity < 92] <- 23
test$second[82 <= humidity & humidity < 84] <- 24
test$second[80 <= humidity & humidity < 82] <- 25
test$second[76 <= humidity & humidity < 80] <- 26
test$second[74 <= humidity & humidity < 76] <- 27
test$second[66 <= humidity & humidity < 74] <- 28
test$second[62 <= humidity & humidity < 66] <- 29
test$second[58 <= humidity & humidity < 62] <- 30
test$second[56 <= humidity & humidity < 58] <- 31
test$second[48 <= humidity & humidity < 56] <- 32
test$second[46 <= humidity & humidity < 48] <- 33
test$second[40 <= humidity & humidity < 46] <- 34
test$second[16 <= humidity & humidity < 40] <- 35
test$second[humidity < 16] <- 36

#feature season & weather & windspeed
test$third[season < 1.5 & weather >= 2.5] <- 37
test$third[season < 1.5 & weather < 2.5] <- 38
test$third[season >= 1.5 & winspeed < 10 & weather >= 2.5] <- 39
test$third[season >= 1.5 & winspeed < 6.6 & weather < 2.5] <- 40
test$third[season >= 1.5 & 6.6 <= winspeed & winspeed < 10 & weather < 2.5 ] <- 41
test$third[season >= 1.5 & winspeed >= 10 & weather >= 2.5] <- 42
test$third[season >= 1.5 & winspeed >= 10 & 1.5 <= weather & weather < 2.5] <- 43
test$third[season >= 3.5 & 10 < winspeed & winspeed < 25 & weather < 1.5] <- 44
test$third[season >= 3.5 & 25 <= winspeed  & weather < 1.5] <- 45
test$third[1.5 < season & season < 3.5 & 10 < winspeed & weather < 16 & weather < 1.5] <- 46
test$third[1.5 < season & season < 3.5 & 16 <= winspeed & weather < 1.5] <- 47
# Random Forest

library(randomForest)
set.seed(10886)
fit <- randomForest(count ~ season + weather + temp + atemp + humidity + windspeed + first + second + third + detail, data=train, ntree = 100, importance=TRUE)
varImpPlot(fit)

# Conditional Random Forest
library(party)
set.seed(10886)

fit <- cforest(count ~ season + weather + temp + atemp + humidity + windspeed + first + second + third + detail , data = train, controls=cforest_unbiased(ntree=2, mtry=1))
submit <- data.frame(PassengerId = test$count, Survived = Prediction)
write.csv(submit, file = "submission.csv", row.names = FALSE)

submission <- read.csv("submission.csv", stringsAsFactors=FALSE)
# predict equation - data postprocessing
submission$equation[submission$count <= 90 & submission$count >= 55] <- sqrt(submission$count)+20
submission$equation[submission$count <= 165 & submission$count >= 91] <- sqrt(submission$count)+35
submission$equation[submission$count <= 220 & submission$count >= 166] <- sqrt(submission$count)+40
submission$equation[submission$count <= 270 & submission$count >= 221] <- sqrt(submission$count)+45
submission$equation[submission$count <= 330 & submission$count >= 271] <- sqrt(submission$count)+50
submission$equation[submission$count <= 400 & submission$count >= 331] <- sqrt(submission$count)+60
submission$equation[submission$count <= 500 & submission$count >= 401] <- sqrt(submission$count)+65
submission$equation[submission$count >= 501] <- sqrt(submission$count)+70