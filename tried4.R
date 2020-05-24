final <- read.csv("final.csv")
final$cost <- sub(",","",final$cost)
str(final)
final$cost <- as.factor(final$cost)
str(final)
final$cost <- sub(".", "", final$cost, fixed=TRUE)
str(final)
final$cost <- as.factor(final$cost)
str(final)
final$cost <- as.numeric(levels(final$cost))[final$cost]
str(final)
colSums(is.na(final))
final <- na.omit(final)
colSums(is.na(final))
str(final)
final$date <- as.POSIXlt(final$date, format="%d/%m/%Y")
final$date <- format(as.Date(final$date), "%Y-%m")
final$date <- as.factor(final$date)
final$date <- gsub("-","", final$date)
final$date <- as.numeric(final$date)
library(lubridate)

str(final)

final$X <- NULL
# try some different models
 # remove columns
# outlier detection
install.packages(outlier)
source("http://goo.gl/UUyEzD")
outlier(final, Pop_In_Good_Health)
# try Stepwise Regression
library(MASS)
fit <- lm(cost~.,data=final)
step <- stepAIC(fit, direction="both")
step$anova # display results


#MLR modelling
m1=lm(No_of_Offences~. -1,data=model1)
plot(model1$No_of_Offences,predict(m1,data=model1),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m1)
plot(m1)
rms=sqrt((sum((crime$No_of_Offences-predict(m1,data=crime))^2))/length(crime$No_of_Offences))

m2=lm(No_of_Offences~. -1,data=model2)
plot(model2$No_of_Offences,predict(m2,data=model2),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m2)
plot(m2)
rms=sqrt((sum((crime$No_of_Offences-predict(m2,data=crime))^2))/length(crime$No_of_Offences))

m3=lm(No_of_Offences~. -1,data=model3)
plot(model3$No_of_Offences,predict(m3,data=model3),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m3)
plot(m3)
rms=sqrt((sum((crime$No_of_Offences-predict(m3,data=crime))^2))/length(crime$No_of_Offences))

# setting seed to reproduce results of random sampling
set.seed(100)
# row indices for training data
trainingRowIndex <- sample(1:nrow(model3), 0.8*nrow(model3))
trainingData <- model3[trainingRowIndex, ]  # model training data
testData  <- model3[-trainingRowIndex, ]   # test data

lmModel3 <- lm(No_of_Offences~. -1, data=trainingData)  # build the model
offencesPred <- predict(lmModel3, testData)  # predict No_of_Offences

summary(lmModel3)
AIC(lmModel3)

# check actuals vs predicted values
actuals_preds <- data.frame(cbind(actual=testData$No_of_Offences, predicted=offencesPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)


correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape