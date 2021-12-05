# Loading data
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)

options(max.print = 999999999)

#setwd("C:/dev/mids/w203/lab2/data/raw")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

cpi <- read.csv('cpi_fnb.csv', header=T)
unemployment <- read.csv('unemployment.csv', header=T)
approval <- read.csv('approval_topline.csv', header=T)
#gas <- read.csv('wti.csv')
gas <- read.csv('dcoilwtico.csv')



# Easier names
names(cpi) <- tolower(names(cpi))
names(unemployment) <- tolower(names(unemployment))
names(gas) <- tolower(names(gas))

# Correct col formats
approval$date <- as.Date(approval$modeldate, format='%m/%d/%Y')
cpi$date <- as.Date(cpi$date, format='%Y-%m-%d')
unemployment$date <- as.Date(unemployment$date, format='%Y-%m-%d')
gas$date <- as.Date(gas$date, format='%Y-%m-%d')
gas$dcoilwtico <- as.numeric(gas$dcoilwtico)

lapply(c(gas, unemployment, cpi, approval), summary)

summary(approval)
# Looks good, not a lot of NAs



## Approval

range(approval$date) # we got from 2021-01-23 to 2021-12-03

## Gas


filter(gas, month(date) == 10, year(date)==2021) %>% arrange(date)

# Looks like gas is only reported for weekdays. We can use the previous friday for a proxy value. DOCUMENT
# Remove the unneeded dates



# For each missing date, replace the missing value with the previous value
ts <- seq(ymd("2021-01-01"), ymd("2021-12-03"), by="day")
gas <- full_join(gas, data.frame(date=ts)) %>% arrange(date) %>% 
  mutate(
    year_month = as.yearmon(date, "%Y-%m")
  )


tt <- data.table(gas)
setnafill(tt, type="locf", cols='dcoilwtico')
gas <- data.frame(tt)


approval <- filter(approval, date >= '2021-01-23', subgroup == "All polls")
gas <- filter(gas, date >= '2021-01-23')
unemployment <- filter(unemployment, date >= '2021-01-01')
cpi <- filter(cpi, date >= '2021-01-01')

# Merge the unemployment data and cpi
unemp_join_cpi <- merge(unemployment, cpi, by = "date") %>%
  mutate(year_month = as.yearmon(date, "%Y-%m")) %>%
  select(-date)

# Create a single dataframe of the all 3 independent variables - gas, cpi, and unemployment
# Since unemployment is monthly data, we explode it to daily sequence 
# with same value for each day in the respective month.
combined_indvars <- full_join(gas, unemp_join_cpi, by="year_month", all = TRUE) %>%
  select(-year_month)

df <- inner_join(approval, combined_indvars, by = "date")

#NOTE: There are still NA in unemployment rate and cpifabsl.

## Afghanistan (August, September)
df <-  mutate(df, afghanistan = if_else(lubridate::month(date) %in% c(8, 9), 1, 0))


## EDA

covid <- read.csv('United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')
covid$date <- as.Date(covid$submission_date, format='%m/%d/%Y')

# we'll use the new_case variable as covid cases

df <- inner_join(df, select(covid, new_case, date), by='date')
df <- df %>% select(approve_estimate, disapprove_estimate, timestamp, date, dcoilwtico, unrate, cpifabsl, afghanistan, new_case)
