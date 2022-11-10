library(readr)
birth <- read.csv("data/Population_dynamics__month_and_year_23062022_151805.csv")
birth$total <- birth$Live.births..aantal.
birth$Periods <-str_replace(birth$Periods, "[*]","")
library(tidyverse)
library(lubridate)
library(zoo)


birth <- birth %>% 
  mutate(
    day = "01", #Create a day column just to get a full date format. The day will be dropped in the following step
    Date = as.Date(paste0(Periods,day), "%Y %b %d") #use the zoo as.yearmon() to get the year and month only
  )  %>%
  filter(Date>="1999-01-01")

df.xts <- xts(birth$total,order.by = birth$Date)
weektime <- zoo(NA, order.by=seq(start(df.xts), end(df.xts),
                                 "week",drop=T))
birth_month <- na.spline(merge(df.xts, foo=zoo(NA, order.by=seq(start(df.xts), end(df.xts),
                                                                "week",drop=T)))[, 1])
birth_month <- birth_month[index(birth_month)%in%index(weektime)]
birth_month <- birth_month[unique(index(birth_month))]
birth_month <- ifelse(birth_month>0,birth_month,0)
birth_week <- birth_month/4.33