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
final$date <- as.character(final$date)
str(final)
final$date <- as.Date.character(final$date, format = "%d/%m/%Y")
final$date <- format(as.Date(final$date), "%Y-%m")
final$date <- as.factor(final$date)
final$date <- gsub("-","", final$date)
final$date <- as.numeric(final$date)
str(final)
str(final$date)
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

library(plyr)
ultimate <- rename(ultimate, c("Group.1"="pcode"))
ultimate <- rename(ultimate, c("Group.2"="year"))
ultimate <- rename(ultimate, c("x"="price"))


simple_linear_model <- lm(year ~ price, data=ultimate)
simple_linear_model

plot(ultimate$price,ultimate$year,
     xlab="postal code",
     ylab="price of property",
     main = "Scatter plot showing regression line")
abline(simple_linear_model)


summary(simple_linear_model)

confint(simple_linear_model)

cor(ultimate$pcode, ultimate$price)

