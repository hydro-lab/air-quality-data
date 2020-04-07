# This code will extract, sort, and plot data from air quality sensors extracted from .csv file.

# set working directory, if needed:
setwd("/Users/hydro/Documents/airquality/")

# install.packages("dplyr")
library(dplyr)

x = read.csv("aq31mar2020.csv")
# Subset data frame for visualization.  Guidance from: https://www.r-bloggers.com/5-ways-to-subset-a-data-frame-in-r/
# SELECT SITE HERE !!!!!!!!!!!!!!!!!!!!!!!!!
sitename <- "North Braddock"
y <- x[which(x$site == sitename), names(x) %in% c("datetime", "stat", "pm10","pm10_fl", "pm10b", "pm25", "pm25.2.", "pm25_fl", "pm25b", "pm25t")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "pm10","pm10_fl", "pm10b", "pm25", "pm25.2.", "pm25_fl", "pm25b", "pm25t")]

day <- array("", dim = c(nrow(z),1))
mes <- array(0, dim = c(nrow(z),1))
# Parse the date and time variables.  Guidance from: https://www.dummies.com/programming/r/how-to-split-strings-in-r/ and https://www.dummies.com/programming/r/how-to-convert-a-factor-in-r/
for (i in 1:(nrow(z))) {
  p <- strsplit(as.character(z$datetime[i]), "T")
  q <- tolower(p[[1]])
  day[i,1] <- q[1]
  # Insert data next to date.  SELECT MEASUREMENT HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
  mes[i,1] <- c(z$pm10b[i])
}

quality <- data.frame(as.Date(day), mes)
sorted_quality <- quality[order(day),]

plot(sorted_quality, type = "l", main = paste(sitename, "Air Quality", delim = " "), xlab = "date", ylab = "particulate matter, PM10b")
