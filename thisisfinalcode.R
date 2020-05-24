
# reading the data from the dataset.

final <- read.csv("final.csv")

# structure of the dataset

str(final)

# editing the cost variable by removing commas

final$cost <- sub(",","",final$cost)

# structure of the datasets

str(final)

# changing the structure of cost variable to factor
final$cost <- as.factor(final$cost)

str(final)

# editing the cost variable as their are dots

final$cost <- sub(".", "", final$cost, fixed=TRUE)

str(final)

# when editing it will defaultly changes into character 
# variable so we need to change it structure

final$cost <- as.factor(final$cost)

str(final)

# their are extra commas in the middle of cost variable
final$cost <- sub(",","",final$cost)

final$cost <- as.numeric(final$cost)

str(final)

# looking for any NA values
colSums(is.na(final))

# omitting the NA values
final <- na.omit(final)

str(final)

# changing the date formta of the date variable
final$date <- as.POSIXlt(final$date, format="%d/%m/%Y")

# adding a month variable by extracting the existing date variable
final$month <- format(final$date,"%m")

str(final)

# changing the date format and extracting the date only
final$date <- format(as.Date(final$date), "%Y")

str(final)

# changing the date structure into numeric
final$date <- as.numeric(final$date)

str(final)

# changing the month structure in the month variable to numeric
final$month <- as.numeric(final$month)

str(final)

# finding the variables which are in characters and which are not useable and removing them
final$X <- NULL
final$Address <- NULL
final$market <- NULL
final$vat <- NULL
final$des <- NULL
final$pcode <- NULL

# only extracting the data of county dublin.
final <- subset(final, final$County == "Dublin", select = c(date, County, cost, month))

# aggregating the datasets sum
ultimate <- aggregate(x=final$cost, by = list( final$County,final$date,final$month) , FUN = sum)

# aggregating the datasets mean
ultimate <- aggregate(x=final$cost, by = list( final$County,final$date,final$month) , FUN = mean)

# dropping the column which is not useful for timeseries
ultimate$Group.1 <- NULL

# installing packages for reshaping the dataset
install.packages('reshape2')
library(reshape2)

# using the dcast function for reshaping the dataset
ultimate <-  dcast(ultimate,Group.2~Group.3,value.var = "x")

# removing the variable which is not useful
ultimate$Group.2 <- NULL

# creating a time series based upon the extracted data by months from 2017 january to 2018 december

time <- c(40209323,38799848,42223753,37717863,37954448,40183585,
          39497231,39921765,40411281,45136519,42199956,43532473,
          40030747,43816351,38678936,41175229,44929096,77560130,
          52908801,43389193,44405573,49551367,41948834,46047196)

# Frequency refers to the number of observations
# per unit of time
# In this example, each time unit is a year
# and we use frequency = 12 to represent monthly data

myts <- ts(time, start=c(2017, 1), end=c(2018, 12), frequency=12)

plot(myts)

# We can use functions to determine various properties of the time series object.

start(myts)

end(myts)

frequency(myts)

class(myts)

summary(myts)

# installing the forecast library

library(forecast)


# Describing a time series numerically and visually should
# be the first step before attempting to build complex models. 
# The first step when investigating a time series is to plot it. 
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

ylim <- c(min(myts), max(myts))
plot(myts, main="Raw time series")
# ma() function used to smooth the Nile time series
plot(ma(myts, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(myts, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(myts, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)


# installing the timeseries library

library(tseries)

# We can apply the adf.test() function to apply the Augmented Dickey-Fuller Test.
adf.test(myts)

# checking the class of the dataset

class(myts)

# We can lok at some information on the time series object though
cat("Start of property prices : ", start(myts), "\n")
cat("End of property prices : ", end(myts), "\n")
cat("Frequency of prices : ", frequency(myts), "\n")
print(summary(myts))


# ## Exploratory data analysis
# frequency of the dataset

frequency(myts)

# cycle of the dataset
cycle(myts)

# View the records with NA

na_records <- myts[!complete.cases(myts)]
sum(na_records)

options(repr.plot.width=14, repr.plot.height=6)

# Show data using a plot() function
plot(myts,
     xlab="Date", 
     ylab = "cost",
     main="residential properties 2017 and 2018")
# Add a straight line shwing the linear relationship
# between passenger numbers and time
abline(reg=lm(myts~time(myts)))

#we can examine any seasonal effects within the data using a boxplot()
boxplot(myts ~ cycle(myts),
        xlab="Date", 
        ylab = "cost" ,
        main =
          "Monthly residential properties price from 2017 to 2018")

# In order to test the stationarity of the time series, 
# let’s run the Augmented Dickey-Fuller Test using 
# the adf.test() function from the tseries package.

library(tseries)
suggested_k <- trunc((length(myts)-1)^(1/3))
suggested_k


adf.test(myts, alternative = "stationary")

adf.test(myts, alternative = "stationary", k = 5)

# Autocorrelation refers to how correlated a time series,
# is with its past values whereas the ACF is the plot used to see 
# the correlation between the points, up to and including the lag unit. 
# In ACF, the correlation coefficient is in the x-axis whereas
# the number of lags is shown in the y-axis.

library(forecast)
acf(myts)

pacf(myts)


library(forecast)
nsdiffs(myts)


log_myts <- log(myts)

options(repr.plot.width=16, repr.plot.height=10)
# Show both side-by-side for comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(myts, main = "Original residential properties dataset")
plot(log_myts, main = "Differenced residential properties dataset")
par(opar)

diff_myts <- diff(log_myts, lag = 12, differences = 2)

fit <- stl(ts(myts,freq=10), t.window=15, s.window="per", robust=TRUE)
plot(fit)

# additional plots
monthplot(myts)
seasonplot(myts)

# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)

# predictive accuracy
accuracy(forecast(fit))

# automated forecasting based on the data

fit <- ets(myts)

accuracy(fit)

# training and testing the dataset.
# we will aproach 70:30 approach
# as we see by deafult out of 24 months
 # 70 percent will be 17 months
# 30 percent will be 7 months.

myts_train <- window(x = myts, start=c(2017, 1), end=c(2018, 5))

myts_test <- window(x = myts, start=c(2018, 6))

myts_train

myts_test

# training the data

fit <- HoltWinters(myts_train, beta=FALSE, gamma=FALSE)

fit <- HoltWinters(myts_train, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components

# predictive accuracy
library(forecast)
accuracy(forecast(fit))

forecast(fit, 3)
plot(forecast(fit, 3))

# testing the data

fit <- HoltWinters(myts_test, beta=FALSE, gamma=FALSE)
fit <- HoltWinters(myts_test, gamma=FALSE)

accuracy(forecast(fit))

forecast(fit, 3)
plot(forecast(fit, 3))

# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)

# predictive accuracy
accuracy(forecast(fit))

# predict next three future values
forecast(fit, 12)
plot(forecast(fit, 12))
