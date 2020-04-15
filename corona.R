###### set working directory
setwd("~/Documents/corona-tracker/")

######  tbd load data to macbook
###from https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases

######  load data to R
corona_infections_total <- read.csv2("data/time_series_covid19_20200413.csv", sep = ",")

dim(corona_infections_total)
str(corona_infections_total)
head(corona_infections_total)

library(dplyr)
####### reshape data
cov_inf <- reshape(corona_infections_total, 
        idvar = c("Province.State","Country.Region", "Lat", "Long"), 
        varying = list(5:ncol(corona_infections_total)), 
        v.names = "TotalInfections", 
        timevar = "Day", 
        times = (Sys.Date() - (ncol(corona_infections_total) -4) ):(Sys.Date() - 1),
        direction = "long")

####### manipulation
cov_inf2 <- 
  cov_inf %>% 
  select(-c("Lat", "Long")) %>% 
  group_by(Province.State, Country.Region) %>%  
  mutate("LastDay" = lag(TotalInfections, order_by = Day)) %>% 
  mutate("NewInfections" = TotalInfections - LastDay) %>% 
  mutate("NewInfectionsDayBefore" = lag(NewInfections, order_by = Day)) %>% 
  mutate("Times2" = TotalInfections * 2) %>% 
  mutate("NewInfectionsOverDayBefore" = NewInfections / NewInfectionsDayBefore ) %>% 
  mutate("Growth" = TotalInfections / LastDay) %>% 
  as.data.frame() %>% 
  identity

head(cov_inf2)
rownames(cov_inf2) <- c()

library(ggplot2)
for_plot <- cov_inf2 %>% filter(Country.Region %in% c("Germany", "Italy", "Spain")) 
ggplot(for_plot, aes(x = Day, y = TotalInfections, color= Country.Region)) + geom_line()
ggplot(for_plot, aes(x = Day, y = NewInfections, color= Country.Region)) + geom_line()
for_plot %>% 
  filter (Day > as.Date("2020-03-01")) %>% 
  ggplot(aes(x = Day, y = Growth, color= Country.Region)) + geom_line()
for_plot %>% 
  filter (Day > as.Date("2020-03-15")) %>% 
  ggplot(aes(x = Day, y = NewInfectionsOverDayBefore, color= Country.Region)) + geom_line()


########2nd try with data.table/melt
library(data.table)
library(dplyr)
library(ggplot2)
long <- 
  melt(
    setDT(corona_infections_total), 
    id.vars = c("Province.State","Country.Region"), 
    variable.name = "day"
    )
long_2 <- long %>% mutate(day = as.Date(day ,"X%m.%d.%y") )
head(long_2)
str(long_2)

