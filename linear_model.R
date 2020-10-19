library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(e1071)
library(latex2exp)
library(nlme)

file <- file.choose()
parkway_daily <- read.csv(file)

head(parkway_daily)
str(parkway_daily)
plot(parkway_daily)

date <- as.Date(parkway_daily$date, format = "%m/%d/%y", origin = "01/01/10")
temp <- as.numeric(parkway_daily$temp)
pm25t <- parkway_daily$pm25t
precip <- parkway_daily$precip

k <- 0.1 # weighting factor, must be less than unity
d <- 3 # days
n <- length(temp) # enter daily precip here, may need to use nrow
API <- array(0, dim = c(n))
for (i in 1:(d-1)) {
  API[i] <- temp[i] # this fills the days prior to the start with the current precip, not an API
}
for (i in d:n) {
  for (j in 1:d) {
    t <- j-1
    API[i] <- API[i]+temp[i-t]*(k^t)
  }
}

april_2020 <- array(0,dim = c(nrow(parkway_daily)))
for (i in 1:(nrow(parkway_daily))) {
  if ((date[i] > as.Date("03/31/20", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/20", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2020[i] <- 1
  }
}

parkway_daily <- data.frame(date, pm25t, precip, API, temp, april_2020)

#linear model y = a + bx
model1=lm(pm25t~april_2020*precip*temp, data = parkway_daily) #temp and precip values change when run individually
summary(model1)

model2=lm(pm25t~API, data = parkway_daily) #temp and precip values change when run individually
summary(model2)


#mixed model y = a + bx + cz (cz = random variables)
model3=lme(pm25t~precip*temp, data = parkway_daily, random = ~1 | april_2020)
summary(model3)

#visual
ggplot() +
  geom_point(data = parkway_daily, mapping = aes(x = date, y = pm25t))

