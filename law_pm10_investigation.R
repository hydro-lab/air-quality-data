# This code is to examine the unusual distribution found at Lawranceville, PM10
# also need to examine North Braddock and Liberty for SO2 -- extreme distributions.
library(e1071) # includes skewness and kurtosis functions

x <- read.csv("Raw_1.csv")
sitename <- "Lawrenceville"
y <- x[which(x$site == sitename), names(x) %in% c("datetime", "stat", "pm10b")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "pm10b")]

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
      mes[i,1] <- c(z$pm10b[i])
}

quality <- data.frame(as.Date(day), yea, mon, dom, mes)
sorted_quality <- quality[order(day),]
law_pm10 <- sorted_quality
rm(x, y, z, quality, sorted_quality)
hist(law_pm10$mes)
skewness(law_pm10$mes, na.rm = TRUE)
