# importing the dataset into the r studio
# we have added all the csv files into the ca4 folder for further use.

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
final$date <- format(final$date,"%m")
library(lubridate)
final$Address <- NULL
final$County <- NULL
final$market <- NULL
final$vat <- NULL
final$des <- NULL
final$X <- NULL
final$pcode <- gsub(" ","", final$pcode)
str(final)
colSums(is.na(final))
str(final)
final$postalcode <- final$pcode

ultimate <- aggregate(x=final$cost, by = list( final$pcode, final$date) , FUN = mean)

ultimate$postalcode <- ultimate$Group.1

ultimate$Group.1 <- gsub("Dublin","", ultimate$Group.1)
ultimate$Group.1 <- gsub("BaileÃthaCliath", "2", ultimate$Group.1)
ultimate$Group.1 <- gsub("BaileÁthaCliath", "3", ultimate$Group.1)
ultimate$Group.1 <- gsub("w", "0", ultimate$Group.1)

str(ultimate)
ultimate$Group.1 <- as.integer(ultimate$Group.1)
str(ultimate)
ultimate$Group.2 <- as.integer(ultimate$Group.2)
str(ultimate)
library(plyr)
ultimate <- rename(ultimate, c("Group.1"="pcode"))
ultimate <- rename(ultimate, c("Group.2"="year"))
ultimate <- rename(ultimate, c("x"="price"))
ultimate$postalcode <- NULL

colSums(is.na(ultimate))
t_sales <- ts(ultimate, start=c(2017, 1), frequency=12)
t_sales
str(ultimate)
plot(t_sales)

colSums(is.na(ultimate))

start(t_sales)
end(t_sales)
frequency(t_sales)

ultimate$pcode <- NULL
ultimate$year <- NULL
class(ultimate)

install.packages("forecast")
library(forecast)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

ylim <- c(min(ultimate), max(ultimate))
plot(ultimate, main="Raw time series")
# ma() function used to smooth the Nile time series
plot(ma(ultimate, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(ultimate, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(ultimate, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)
# As k increases, plot becomes increasingly smooth

library(dplyr)                                        
ultimate %>% distinct(ultimate$price, keep_all = TRUE)

library(tseries)
# p-value < 0.05 indicates the TS is stationary
# In this eample, Nile data is not stationary
adf.test(ultimate$price)

class(ultimate)

air_passengers <- ts(ultimate)

class(air_passengers)


cat("Start of air passengers : ", start(air_passengers), "\n")
cat("End of air passengers : ", end(air_passengers), "\n")
cat("Frequency of air passengers : ", frequency(air_passengers), "\n")
print(summary(air_passengers))

frequency(air_passengers)
cycle(air_passengers)
air_passengers <- ts(AirPassengers, start=c(2017, 1), frequency=123)
cycle(air_passengers)
frequency(air_passengers)
na_records <- air_passengers[!complete.cases(air_passengers)]
sum(na_records)
options(repr.plot.width=14, repr.plot.height=6)

# Show data using a plot() function
plot(air_passengers,
     xlab="Date", 
     ylab = "Passenger numbers (1000's)",
     main="Air Passenger numbers from 1949 to 1961")
# Add a straight line shwing the linear relationship
# between passenger numbers and time
abline(reg=lm(air_passengers~time(air_passengers)))
plot(aggregate(air_passengers,FUN=mean))

library(tseries)
suggested_k <- trunc((length(air_passengers)-1)^(1/3))
suggested_k
adf.test(air_passengers, alternative = "stationary")
adf.test(air_passengers, alternative = "stationary", k = 12)
library(forecast)
acf(air_passengers)
pacf(air_passengers)
