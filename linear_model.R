library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(e1071)
library(latex2exp)
library(nlme)

file <- file.choose()
lincoln_daily <- read.csv(file, stringsAsFactors=FALSE)

head(lincoln_daily)
str(lincoln_daily)

date <- as.Date(lincoln_daily$date, format = "%m/%d/%y", origin = "01/01/10")
temp <- as.numeric(lincoln_daily$temp)
pm25 <- lincoln_daily$pm25
precip <- lincoln_daily$precip

# k <- 0.1 # weighting factor, must be less than unity
# d <- 3 # days
# n <- length(temp) # enter daily precip here, may need to use nrow
# API <- array(0, dim = c(n))
# for (i in 1:(d-1)) {
#   API[i] <- temp[i] # this fills the days prior to the start with the current precip, not an API
# }
# for (i in d:n) {
#   for (j in 1:d) {
#     t <- j-1
#     API[i] <- API[i]+temp[i-t]*(k^t)
#   }
# }

april_2020 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/20", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/20", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2020[i] <- 1
  }
}

april_2019 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/19", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/19", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2019[i] <- 1
  }
}

april_2018 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/18", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/18", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2018[i] <- 1
  }
}

april_2017 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/17", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/17", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2017[i] <- 1
  }
}

april_2016 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/16", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/16", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2016[i] <- 1
  }
}

lincoln_daily <- data.frame(date, pm25, precip, temp, april_2020, april_2019, april_2018, april_2017, april_2016)
lincoln_daily$month <- format(lincoln_daily$date, '%m')
pm25_april <- lincoln_daily[which(lincoln_daily$month == "04"), names(lincoln_daily) %in% c("date", "pm25", "temp", "precip", "april_2020", "april_2019", "april_2018", "april_2017", "april_2016")]

#checking for normal distribution
hist(pm25_april$pm25)

#log transforming the data for normal distribution (if not already normal)
pm25_april$pm25log <- log(pm25_april$pm25)
lincoln_daily$pm25log <- log(lincoln_daily$pm25)
hist(pm25_april$pm25log)

# fixing zero values
j <- 0
for (i in 1:nrow(lincoln_daily)) {
  if (is.na(lincoln_daily$pm25[i]) == FALSE) {
    if (lincoln_daily$pm25[i] == 0) {
      lincoln_daily$pm25[i] <- NA
      lincoln_daily$pm25log[i] <- NA
      j <- j + 1
    }
  }
}

#april 2020 v. all previous months
model1=lm(pm25log~april_2020 + precip, data = lincoln_daily) 
summary(model1)
confint(model1)

#april 2020 v. previous aprils
model2=lm(pm25log~april_2020 + precip, data = pm25_april)
summary(model2)
confint(model2)

#april 2020 v. previous aprils with indicators
model3=lm(pm25log~april_2019 + april_2018 + april_2017 + april_2016 + precip, data = pm25_april)
summary(model3)
confint(model3)
hist(residuals(model3), xlab = "LM Residuals", ylab = "Count", main = "")
plot(residuals(model3), xlab = "Sequential Aprils", ylab = "LM Residual")

#over time
model4=lm(pm25log~date + precip, data = lincoln_daily)
summary(model4)
confint(model4)

# Using generallized linear model:
model5 <- glm(pm25~april_2020 + precip, data = pm25_april, family = gaussian(link = "log"))
summary(model5)
confint(model5)

model6 <- glm(pm25~april_2019 + april_2018 + april_2017 + april_2016 + precip, data = pm25_april, family = gaussian(link = "log"))
summary(model6)
confint(model6)
hist(residuals(model6), xlab = "GLM Residuals", ylab = "Count", main = "")
plot(residuals(model6), xlab = "Sequential Aprils", ylab = "GLM Residual")

lm_pm25 <- array(0, dim = c(nrow(pm25_april)))
for (i in 1:nrow(pm25_april)) {
  lm_pm25[i] = 12.3680 - 3.7896*pm25_april$april_2020[i] - 6.9520*pm25_april$precip[i] }
r <- residuals(model2)
r2 <- lm_pm25 - pm25_april$pm25
plot(r,r2[1:148], type = "p")

#visual
ggplot() +
  geom_point(data = lincoln_daily, mapping = aes(x = date, y = pm25))

