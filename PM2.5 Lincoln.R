# install.packages("latex2exp")
# install.packages("e1071")
# install.packages("ggpubr")
# install.packages("dplyr")

setwd("/Users/carissalange/Documents/Duquesne/Thesis/")

library(dplyr) # a grammar of data manipulation
library(latex2exp) # for using TeX commands in plotting, https://cran.r-project.org/web/packages/latex2exp/vignettes/using-latex2exp.html 
library(e1071) # includes skewness and kurtosis functions

x = read.csv("AQD_Pitt_April_Comparison.csv")

qq <- x[which(x$site == "Lawrenceville"), names(x) %in% c("datetime", "stat", "solarrad", "rainfall", "out_t")]
temperature <- qq[which(qq$stat == "Avg"), names(y) %in% c("datetime", "out_t")]
rainfall <- qq[which(qq$stat == "Total"), names(y) %in% c("datetime", "rainfall")]
solarrad <- qq[which(qq$stat == "Avg"), names(y) %in% c("datetime", "solarrad")]

sitename <- "Lincoln"
y <- x[which(x$site == sitename), names(x) %in% c("datetime", "stat", "pm25", "pm25.2.", "pm25b", "pm25t", "so2", "no2", "ozone")]
z <- y[which(y$stat == "Avg"), names(y) %in% c("datetime", "pm25.2.", "pm25", "pm25b", "pm25t", "so2", "no2", "ozone")]

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
  mes[i,1] <- c(z$pm25[i])
}

quality <- data.frame(as.Date(day), yea, mon, dom, mes)
sorted_quality <- quality[order(day),]

cor(sol_rad$mes, pm25$mes, method = "pearson", use = "pairwise.complete.obs")

pm25 <- sorted_quality
# hist(ozone$mes)
mean(pm25$mes, na.rm = TRUE)
sd(pm25$mes, na.rm = TRUE)
skewness(pm25$mes, na.rm = TRUE)
kurtosis(pm25$mes, na.rm = TRUE)
log_pm25 = log(pm25$mes) # no2 data appear to be log-normal, log defaults to natural log
# hist(log_no2)

test <- ozone_2018_ln
good <- array(-9, dim = c(length(test), 1))
j <- 0
for (i in 1:(length(test))) {
  if (is.na(test[i]) == FALSE){
    if (test[i] > (-1e6)) {
      if (test[i] < (1e6)) {
        j <- j + 1
        good[j] <- test[i]
      }
    }
  }
}
rm(ozone_2018_ln)
ozone_2018_ln <- good[1:j,1]
rm(test)
rm(good)

# April of each year
pm25_april <- pm25[which(pm25$mon == "04"), names(pm25) %in% c("yea", "dom", "mes")]
pm25_2016 <- pm25_april[which(pm25_april$yea == 2016), names(pm25_april) %in% c("dom", "mes")]
pm25_2016_ln <- log(pm25_2016$mes)
pm25_2017 <- pm25_april[which(pm25_april$yea == 2017), names(pm25_april) %in% c("dom", "mes")]
pm25_2017_ln <- log(pm25_2017$mes)
pm25_2018 <- pm25_april[which(pm25_april$yea == 2018), names(pm25_april) %in% c("dom", "mes")]
pm25_2018_ln <- log(pm25_2018$mes)
pm25_2019 <- pm25_april[which(pm25_april$yea == 2019), names(pm25_april) %in% c("dom", "mes")]
pm25_2019_ln <- log(pm25_2019$mes)
pm25_2020 <- pm25_april[which(pm25_april$yea == 2020), names(pm25_april) %in% c("dom", "mes")]
pm25_2020_ln <- log(pm25_2020$mes)

library(ggpubr)
library(latex2exp)
ggboxplot(pm25_april, x = "yea", y = "mes", ylab = TeX('$PM 2.5 (ug/m^{3})$'), xlab = "", title = "Lincoln", ylim = c(0,40)) +
  ggtitle("Lincoln") +
  theme(plot.title = element_text(hjust = 0.5))
t.test(pm25_2019_ln,pm25_2018_ln, paired = FALSE, alternative = "two.sided", var.equal = FALSE)
mean(pm25_2018$mes)
sd(pm25_2018$mes)
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
# https://www.statsdirect.co.uk/help/parametric_methods/utt.htm
# https://towardsdatascience.com/how-to-compare-two-distributions-in-practice-8c676904a285
