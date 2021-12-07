
# Here's the list of events we're considering:

# - COVID March 2020 - Today
# - Iraq War - March to May 2003
# - 9/11 September - December 2001
# - Katrina August Sep 2005
# - Great Recession 2008
# - ObamaCare March April 2010
# - Osama Bin Laden May 2011
# - Impeachment ukraine scandal dec 2019
# - January 6 2021

library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load('../../data/interim/df_minus_events.RData')

df <- mutate(df, covid=if_else(date >= '2020-03-01', 1, 0))
df <- mutate(df, iraq=if_else(date >= '2003-03-01' & date < '2003-06-01', 1, 0))
df <- mutate(df, sep11=if_else(date >= '2001-09-11' & date < '2001-12-31', 1, 0))
df <- mutate(df, katrina=if_else(date >= '2005-08-23' & date < '2005-10-01', 1, 0))
df <- mutate(df, recession=if_else(date >= '2008-01-01' & date < '2009-01-01', 1, 0))
df <- mutate(df, osama=if_else(date >= '2011-05-02' & date < '2011-06-01', 1, 0))
df <- mutate(df, ukraine=if_else(date >= '2019-12-01' & date < '2020-01-01', 1, 0))
df <- mutate(df, riot=if_else(date >= '2021-01-06' & date < '2021-01-20', 1, 0))

save(df, file='../../data/interim/df_with_events.RData')

