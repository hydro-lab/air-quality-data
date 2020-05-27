# This code will extract, sort, and plot data from air quality sensors extracted from .csv file.

# set working directory, if needed:
setwd("/Users/davidkahler/Documents/R/airquality/")

# install.packages("dplyr")
library(dplyr)
library(latex2exp) # for using TeX commands in plotting, https://cran.r-project.org/web/packages/latex2exp/vignettes/using-latex2exp.html 
library(e1071) # includes skewness and kurtosis functions

# Locations of sites:
# https://data.wprdc.org/dataset/c7b3266c-adc6-41c0-b19a-8d4353bfcdaf/resource/7f7072ce-7c19-4813-a45c-6135cf4505bb/download/aqsensors.geojson

x = read.csv("aq_jan-apr.csv")
# Subset data frame for visualization.  Guidance from: https://www.r-bloggers.com/5-ways-to-subset-a-data-frame-in-r/
# SELECT SITE HERE !!!!!!!!!!!!!!!!!!!!!!!!!
# Solar radiation is at Lawrenceville
# NO2 from Harrison Township and Parkway East
sitename <- "Parkway East"
y <- x[which(x$site == sitename), names(x) %in% c("datetime", "stat", "pm10", "pm10b", "pm25", "pm25.2.", "pm25b", "pm25t", "so2", "no2")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "pm10","pm10b", "pm25", "pm25.2.", "pm25b", "pm25t", "so2", "no2")]
# Pull solar radiation
y <- x[which(x$site == "Lawrenceville"), names(x) %in% c("datetime", "stat", "solarrad")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "solarrad")]

day <- array("", dim = c(nrow(z),1))
yea <- array("", dim = c(nrow(z),1))
mon <- array("", dim = c(nrow(z),1))
dom <- array("", dim = c(nrow(z),1)) # day of month
mes <- array(0, dim = c(nrow(z),1))
# Parse the date and time variables.  Guidance from: https://www.dummies.com/programming/r/how-to-split-strings-in-r/ and https://www.dummies.com/programming/r/how-to-convert-a-factor-in-r/
for (i in 1:(nrow(z))) {
  p <- strsplit(as.character(z$datetime[i]), "T")
  q <- tolower(p[[1]]) # extracts the character vector from the list
  day[i,1] <- q[1] # stores the date in "day"
  tmp1 <- as.Date(q[1])
  tmp2 <- as.Date(tmp1, '%Y/%m/%d')
  yea[i] <- format(tmp2, '%Y')
  mon[i] <- format(tmp2, '%m')
  dom[i] <- format(tmp2, '%d')
  # Insert data next to date.  SELECT MEASUREMENT HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
  mes[i,1] <- c(z$no2[i])
}

quality <- data.frame(as.Date(day), yea, mon, dom, mes)
sorted_quality <- quality[order(day),]

plot(sorted_quality, type = "l", xlim = as.Date(c("2020-01-01", "2020-05-15")), main = paste(sitename, "Air Quality", delim = " "), xlab = "date", ylab = TeX('$Solar Radiation (W/m^{2})$'))
plot(sorted_quality, type = "l", xlim = as.Date(c("2020-01-01", "2020-05-15")), main = paste(sitename, "Air Quality", delim = " "), xlab = "date", ylab = TeX('$NO_{2} (ppm)$'))

cor(sol_rad$mes, no2$mes, method = "pearson", use = "pairwise.complete.obs")

no2 <- sorted_quality
# hist(no2$mes)
mean(no2$mes, na.rm = TRUE)
sd(no2$mes, na.rm = TRUE)
skewness(no2$mes, na.rm = TRUE)
kurtosis(no2$mes, na.rm = TRUE)
log_no2 = log(no2$mes) # no2 data appear to be log-normal, log defaults to natural log
# hist(log_no2)

# April of each year
no2_april <- no2[which(no2$mon == "04"), names(no2) %in% c("yea", "dom", "mes")]
no2_2016 <- no2_april[which(no2_april$yea == 2016), names(no2_april) %in% c("dom", "mes")]
no2_2016_ln <- log(no2_2016$mes)
no2_2017 <- no2_april[which(no2_april$yea == 2017), names(no2_april) %in% c("dom", "mes")]
no2_2017_ln <- log(no2_2017$mes)
no2_2018 <- no2_april[which(no2_april$yea == 2018), names(no2_april) %in% c("dom", "mes")]
no2_2018_ln <- log(no2_2018$mes)
no2_2019 <- no2_april[which(no2_april$yea == 2019), names(no2_april) %in% c("dom", "mes")]
no2_2019_ln <- log(no2_2019$mes)
no2_2020 <- no2_april[which(no2_april$yea == 2020), names(no2_april) %in% c("dom", "mes")]
no2_2020_ln <- log(no2_2020$mes)

library(ggpubr)
ggboxplot(no2_april, x = "yea", y = "mes")
t.test(no2_2017_ln,no2_2020_ln, alternative = "two.sided", var.equal = FALSE)
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
# https://www.statsdirect.co.uk/help/parametric_methods/utt.htm
# https://towardsdatascience.com/how-to-compare-two-distributions-in-practice-8c676904a285



