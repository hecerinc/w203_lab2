# Libraries
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)


# setwd("C:/dev/mids/w203/lab2/data/raw")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read data
cpi <- read.csv('cpi_fnb.csv')
unemployment <- read.csv('unemployment.csv')
gas <- read.csv('wti.csv')

## Aproval data
approval <- read.csv('approval_topline.csv')
approval$president <- 'Joe Biden' # Sometimes reads 'Joe Biden' sometimes 'Joseph R. Biden Jr.' so let's standardize



# Easier names
names(cpi) <- tolower(names(cpi))
names(unemployment) <- tolower(names(unemployment))
names(gas) <- tolower(names(gas))


# Format data types
approval$date <- as.Date(approval$modeldate, format='%m/%d/%Y')
cpi$date <- as.Date(cpi$date, format='%Y-%m-%d')
unemployment$date <- as.Date(unemployment$date, format='%Y-%m-%d')
gas$date <- as.Date(gas$date, format='%Y-%m-%d')
gas$dcoilwtico <- as.numeric(gas$dcoilwtico)



start.date <- '2000-01-01'


# ---------------- FIX GAS ---------------------------------------------

# filter(gas, month(date) == 10, year(date)==2021) %>% arrange(date)
# Looks like gas is only reported for weekdays. We can use the previous Friday for a proxy value. [TODO: DOCUMENT]

# For each missing date, replace the missing value with the previous value
ts <- seq(ymd(start.date), ymd("2021-12-03"), by="day")
gas <- full_join(gas, data.frame(date=ts)) %>% arrange(date) %>% 
  mutate(
    year_month = as.yearmon(date, "%Y-%m")
  )

tt <- data.table(gas)
setnafill(tt, type="locf", cols='dcoilwtico')
gas <- data.frame(tt)

# ---------------------------------------------------------------------- 


# Filter out unneded polls
approval <- filter(approval, subgroup == "All polls")

# -- Filter out unneeded dates
approval <- filter(approval, date >= start.date)
gas <- filter(gas, date >= start.date)
unemployment <- filter(unemployment, date >= start.date)
cpi <- filter(cpi, date >= start.date)

# Merge the unemployment data and cpi
unemp_join_cpi <- merge(unemployment, cpi, by = "date") %>%
	mutate(year_month = as.yearmon(date, "%Y-%m")) %>%
	select(-date)

# Create a single dataframe of the all 3 independent variables - gas, cpi, and unemployment
# Since unemployment is monthly data, we explode it to daily sequence 
# with same value for each day in the respective month.
combined_indvars <- full_join(gas, unemp_join_cpi, by="year_month", all = TRUE) %>%
  select(-year_month)



# Merge all approvals
approval.all <- rbind(approval, approval.trump)

# Create final dataset
df <- inner_join(approval.all, combined_indvars, by = "date")



## Afghanistan (August, September)
df <-  mutate(df, afghanistan = if_else(lubridate::month(date) %in% c(8, 9) & lubridate::year(date) == 2021, 1, 0))
df  <- filter(df, !is.na(unrate), !is.na(cpifabsl))

## --------------- COVID ---------------------------------------- 

covid <- read.csv('United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')
covid$date <- as.Date(covid$submission_date, format='%m/%d/%Y')

covid <- group_by(covid, date) %>% summarise(new_cases = sum(new_case))

# we'll use the new_case variable as covid cases

df <- left_join(df, select(covid, new_cases, date), by='date')

# --------------------------------------------------------------- 


# Select only cols we need
df <- df %>% select(date, approve_estimate, disapprove_estimate, timestamp, dcoilwtico, unrate, cpifabsl, afghanistan, new_cases)


# MODEL
df$president <- as.factor(df$president)
lm(approve_estimate ~ dcoilwtico  + cpifabsl + president + unrate,data=df)
