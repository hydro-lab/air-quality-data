# This code will extract, sort, and plot data from air quality sensors extracted from .csv file.

# set working directory, if needed:
setwd("/Users/davidkahler/Documents/R/airquality/")

# install.packages("dplyr")
library(dplyr)
library(latex2exp) # for using TeX commands in plotting, https://cran.r-project.org/web/packages/latex2exp/vignettes/using-latex2exp.html 

# Locations of sites:
# https://data.wprdc.org/dataset/c7b3266c-adc6-41c0-b19a-8d4353bfcdaf/resource/7f7072ce-7c19-4813-a45c-6135cf4505bb/download/aqsensors.geojson

x = read.csv("aq30apr2020.csv")
# Subset data frame for visualization.  Guidance from: https://www.r-bloggers.com/5-ways-to-subset-a-data-frame-in-r/
# SELECT SITE HERE !!!!!!!!!!!!!!!!!!!!!!!!!
sitename <- "Harrison Township"
y <- x[which(x$site == sitename), names(x) %in% c("datetime", "stat", "pm10", "pm10b", "pm25", "pm25.2.", "pm25b", "pm25t", "so2", "no2")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "pm10","pm10b", "pm25", "pm25.2.", "pm25b", "pm25t", "so2", "no2")]
# Pull solar radiation
y <- x[which(x$site == "Lawrenceville"), names(x) %in% c("datetime", "stat", "solarrad")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "solarrad")]

day <- array("", dim = c(nrow(z),1))
mes <- array(0, dim = c(nrow(z),1))
# Parse the date and time variables.  Guidance from: https://www.dummies.com/programming/r/how-to-split-strings-in-r/ and https://www.dummies.com/programming/r/how-to-convert-a-factor-in-r/
for (i in 1:(nrow(z))) {
  p <- strsplit(as.character(z$datetime[i]), "T")
  q <- tolower(p[[1]]) # extracts the character vector from the list
  day[i,1] <- q[1] # stores the date in "day"
  # Insert data next to date.  SELECT MEASUREMENT HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
  mes[i,1] <- c(z$no2[i])
}

quality <- data.frame(as.Date(day), mes)
sorted_quality <- quality[order(day),]

plot(sorted_quality, type = "l", xlim = as.Date(c("2020-01-01", "2020-04-30")), main = paste(sitename, "Air Quality", delim = " "), xlab = "date", ylab = TeX('$Solar Radiation (W/m^{2})$'))
plot(sorted_quality, type = "l", xlim = as.Date(c("2020-01-01", "2020-04-30")), main = paste(sitename, "Air Quality", delim = " "), xlab = "date (Jan 2020 to Apr 2020)", ylab = TeX('$NO_{2} (ppm)$'))

     