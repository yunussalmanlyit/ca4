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

install.packages('reshape2')
library(reshape2)

ultimate <-  dcast(ultimate,Group.1~Group.2,value.var = "x")

library(tidyr)
colSums(is.na(ultimate))
ultimate <- na.omit(ultimate)

ultimate <- rename(ultimate, c("Group.1"="postcode"))

