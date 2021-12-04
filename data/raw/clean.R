# Loading data
library(dplyr)
library(data.table)
library(lubridate)


setwd("C:/dev/mids/w203/lab2/data/raw")
cpi <- read.csv('cpi_fnb.csv', header=T)
unemployment <- read.csv('unemployment.csv', header=T)
approval <- read.csv('approval_topline.csv', header=T)
gas <- read.csv('wti.csv')



# Easier names
names(cpi) <- tolower(names(cpi))
names(unemployment) <- tolower(names(unemployment))
names(gas) <- tolower(names(gas))

# Correct col formats
approval$modeldate <- as.Date(approval$modeldate, format='%m/%d/%Y')
cpi$date <- as.Date(cpi$date, format='%Y-%m-%d')
unemployment$date <- as.Date(unemployment$date, format='%Y-%m-%d')
gas$date <- as.Date(gas$date, format='%Y-%m-%d')
gas$dcoilwtico <- as.numeric(gas$dcoilwtico)

lapply(c(gas, unemployment, cpi, approval), summary)


# Looks good, not a lot of NAs



## Approval

range(approval$modeldate) # we got from 2021-01-23 to 2021-12-03

## Gas


filter(gas, month(date) == 10, year(date)==2021) %>% arrange(date)

# Looks like gas is only reported for weekdays. We can use the previous friday for a proxy value. DOCUMENT

# Remove the unneeded dates
gas <- filter(gas, date >= '2021-01-23')
unemployment <- filter(unemployment, date >= '2021-01-23')
cpi <- filter(cpi, date >= '2021-01-23')


# For each missing date, replace the missing value with the previous value
ts <- seq(ymd("2021-01-23"), ymd("2021-12-03"), by="day")
gas <- full_join(gas, data.frame(date=ts)) %>% arrange(date)

tt <- data.table(gas)
setnafill(tt, type="locf", cols='dcoilwtico')
gas <- data.frame(tt)


## EDA
