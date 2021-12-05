# Clean the initial approval and economic data
# 
# Saves the final combined dataset to a file called df_minus_events.RData under
# data/interim

# Libraries
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)


# setwd('C:/dev/mids/w203/lab2/data/raw')
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read data
cpi <- read.csv('cpi_fnb.csv')
unemployment <- read.csv('unemployment.csv')
gas <- read.csv('wti.csv')

## Aproval data

### Biden
approval.biden <- read.csv('approval_biden_538.csv')
approval.biden$president <- 'Joe Biden' # Sometimes reads 'Joe Biden' sometimes 'Joseph R. Biden Jr.' so let's standardize
approval.biden$date <- as.Date(approval.biden$modeldate, format='%m/%d/%Y')

### Trump
approval.trump <- read.csv('approval_trump_538.csv')
approval.trump$date <- as.Date(approval.trump$modeldate, format='%m/%d/%Y')

# Filter out unneded polls
approval.biden <- filter(approval.biden, subgroup == 'All polls')
approval.trump <- filter(approval.trump, subgroup == 'All polls')


### Obama
approval.obama <- read.csv('approval_obama_gallup.csv')
approval.obama$date <- as.Date(approval.obama$date, format='%m/%d/%Y')
approval.obama$president <- 'Barack Obama'
names(approval.obama)[2] <- 'disapprove_estimate'
names(approval.obama)[3] <- 'approve_estimate'

### George Bush
approval.bush <- read.csv('approval_georgewbush_gallup.csv')
approval.bush$date <- as.Date(approval.bush$date, format='%Y/%m/%d')
approval.bush$president <- 'George W. Bush'
names(approval.bush)[2] <- 'approve_estimate'
names(approval.bush)[3] <- 'disapprove_estimate'


# Merge all approvals
approval.all <- bind_rows(approval.biden, approval.obama, approval.trump, approval.bush)
approval.all <- select(approval.all, date, president, approve_estimate, disapprove_estimate)
approval.all$president <- as.factor(approval.all$president)


# Easier names
names(cpi) <- tolower(names(cpi))
names(unemployment) <- tolower(names(unemployment))
names(gas) <- tolower(names(gas))


# Format data types
cpi$date <- as.Date(cpi$date, format='%Y-%m-%d')
unemployment$date <- as.Date(unemployment$date, format='%Y-%m-%d')
gas$date <- as.Date(gas$date, format='%Y-%m-%d')
gas$dcoilwtico <- as.numeric(gas$dcoilwtico)



start.date <- '2000-01-01'
end.date <- '2021-12-03' # TODO: maybe we remove this because we have no data for CPI/Unemployemnt for Nov. 2021 yet


# ---------------- FIX GAS ---------------------------------------------

# filter(gas, month(date) == 10, year(date)==2021) %>% arrange(date)
# Looks like gas is only reported for weekdays. We can use the previous Friday for a proxy value. [TODO: DOCUMENT]

# For each missing date, replace the missing value with the previous value
ts <- seq(ymd(start.date), ymd(end.date), by='day')
gas <- full_join(gas, data.frame(date=ts)) %>% arrange(date) %>% 
  mutate(
    year_month = as.yearmon(date, '%Y-%m')
  )

tt <- data.table(gas)
setnafill(tt, type='locf', cols='dcoilwtico')
gas <- data.frame(tt)

# ---------------------------------------------------------------------- 



# -- Filter out unneeded dates
approval.all <- filter(approval.all, date >= start.date)
gas <- filter(gas, date >= start.date)
unemployment <- filter(unemployment, date >= start.date)
cpi <- filter(cpi, date >= start.date)

# Merge the unemployment data and cpi
unemp_join_cpi <- merge(unemployment, cpi, by = 'date') %>%
	mutate(year_month = as.yearmon(date, '%Y-%m')) %>%
	select(-date)

# Create a single dataframe of the all 3 independent variables - gas, cpi, and unemployment
# Since unemployment is monthly data, we explode it to daily sequence 
# with same value for each day in the respective month.
combined_indvars <- full_join(gas, unemp_join_cpi, by='year_month', all = TRUE) %>%
  select(-year_month)



# Create final dataset
df <- inner_join(approval.all, combined_indvars, by = 'date')



# Save the dataset to data/interim
save(df, file='../interim/df_minus_events.RData')
