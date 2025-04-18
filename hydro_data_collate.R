#Load Libraries ----
library(tidyverse)
library(lubridate)

#A. OVERVIEW OF SCRIPT----
### this script serves to read in raw data (wse, discharge) that has been downloaded... ###
### from usace as a .csv file and is then... ###
### given some new column (variable) names during import as well as... ###
### some new date related columns and whereby... ###
### the "collated" output file can be used for other scripts and analysis ### 

#B. WSE DATA----

### WSE data from L&D3 tailwaters downloaded from: ###
### https://www.mvp-wc.usace.army.mil/data/LockDam_03.Data.shtml ###


#__1. Read Orig .csv data ----
#____a) Lock and Dam 3 Tailwaters----
dat_wse_ld3tail_orig <- read_csv('data/LockDam_03-Tailwater_Elev_1Day_merged-MSL1912.csv', skip = 8, col_select = 1:2, col_names=c("Date.Time", "wse"))

#__2. Format Data ----

### Add some date columns ###

### 'date' column added to original dataframe ###
dat_wse_ld3tail_orig$date <-as.POSIXct(dat_wse_ld3tail_orig$Date.Time, format = "%m/%d/%Y")

### year, month, day, julian added to "collated" dataframe using Lubridate package ###
dat_wse_ld3tail_collate <- dat_wse_ld3tail_orig  %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(julian = yday(date))

#__3. Output Collated Data----
write_csv(dat_wse_ld3tail_collate, 'data/wse_ld3tail_collated.csv')


# C. DISCHAGE DATA----

### Discharge (i.e. Flow) data from L&D4  downloaded from: ###
### https://www.mvp-wc.usace.army.mil/data/LockDam_04.Data.shtml ###

#__1. Read Orig .csv Data ----

#____a) Lock and Dam 4----
dat_q_ld4_orig <- read_csv('data/LockDam_04_Flow_1Day_merged.csv', skip=8, col_select = 1:2, col_names=c("Date.Time", "cfs" ))

#____b) Lock and Dam 8----
#dat_q_ld8_orig <- read_csv('data/LockDam_08_Flow_1Day_merged.csv', skip=8, col_select = 1:2, col_names=c("Date.Time", "cfs" ))

#__2. Format Data ----

### Add some date columns ###

### 'date' column added to original dataframe ###
dat_q_ld4_orig$date <-as.POSIXct(dat_q_ld4_orig$Date.Time, format = "%m/%d/%Y")

### year, month, day, julian added to "collated" dataframe using Lubridate package ###
dat_q_ld4_collate <- dat_q_ld4_orig  %>%
  mutate(year = year(date) ) %>%
  mutate(month = month(date) ) %>%
  mutate(day = day(date) ) %>%
  mutate(julian = yday(date))

#__3. Output Collated Data----
write_csv(dat_q_ld4_collate, 'data/q_ld4_collated.csv')


