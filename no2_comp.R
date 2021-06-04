library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)

x <- read_csv("https://duq.box.com/shared/static/e1d14vfnqibsjs7gljr3f0vqsj03py2y.csv")
x$date <- as_date(x$datetime)
x$year <- year(x$date)
x$month <- month(x$date)

y <- x %>% 
      filter(site == "Harrison Township") %>% 
      filter(stat == "Avg") %>% 
      filter(month == 4) %>% 
      group_by(year) %>% 
      summarize(nitdio = mean(no2, na.rm = TRUE)) %>% 
      arrange(year) 

plot(y$year, y$nitdio)
      