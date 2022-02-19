##########################################################################
# Load libraries
##########################################################################
library(tidyverse)
library(lubridate)
library(dplyr)
library(reshape)
library(weathermetrics)

##########################################################################
# Import and clean mesowest files into hourly climate data into
# central .csv and export to "data_sheets" folder
##########################################################################
## Import mesowest files
file.list.mesowest <- list.files(path = "../climate_data/new_mesowest",
                        recursive = TRUE,
                        pattern = "\\.csv$",
                        full.names = TRUE)
file.list.mesowest <- setNames(file.list.mesowest, 
                               str_extract(basename(file.list.mesowest), 
                                           '.*(?=\\.csv)'))
df.mesowest <- lapply(file.list.mesowest, read.csv)

##########################################################################
# For-loop to convert UTC time to local time, use lubridate to account 
# for daylight savings. Then, summarize hourly temp, rh, precip, and 
# pressure data. Finally, calculate additive precipitation based on lag() 
# from the previous row and sum precipitation types to get hourly 
# precipitation data. Tested to make sure time correction was accurate 
# and that no precip data spills to subsequent day
##########################################################################
for(i in seq_along(df.mesowest)) {
  df.mesowest[[i]] <- df.mesowest[[i]] %>%
    mutate(utc.datetime = mdy_hm(date.time),
           local.datetime = with_tz(mdy_hm(date.time), tz = "America/Chicago"),
           local.date = date(local.datetime),
           local.hour = hour(local.datetime)) %>%
    group_by(local.date, local.hour, site) %>%
    dplyr::summarize(temp = mean(air.temp, na.rm = TRUE),
                     relative.humidity = mean(relative.humidity, na.rm = TRUE),
                     incremental.precip = sum(incremental.precip, na.rm = TRUE),
                     accumulated.precip = max(accumulated.precip, na.rm = TRUE),
                     additive.precip = max(additive.precip, na.rm = TRUE),
                     sea.level.pressure = mean(sea.level.pressure, na.rm = TRUE),
                     atm.pressure = mean(atm.pressure, na.rm = TRUE)) %>%
    mutate(accumulated.precip = ifelse(accumulated.precip == "-Inf", 0,
                                       accumulated.precip),
           additive.precip = ifelse(additive.precip == "-Inf", 0,
                                    additive.precip))
  
  df.mesowest[[i]]$additive.precip <- df.mesowest[[i]]$additive.precip - lag(df.mesowest[[i]]$additive.precip)
  df.mesowest[[i]]$accumulated.precip <- df.mesowest[[i]]$accumulated.precip - lag(df.mesowest[[i]]$accumulated.precip)
  df.mesowest[[i]]$accumulated.precip <- ifelse(df.mesowest[[i]]$accumulated.precip < 0, 0, df.mesowest[[i]]$accumulated.precip)
  df.mesowest[[i]]$additive.precip <- ifelse(df.mesowest[[i]]$additive.precip < 0 | df.mesowest[[i]]$additive.precip > 50, 0, df.mesowest[[i]]$additive.precip)
  df.mesowest[[i]]$hourly.precip <- df.mesowest[[i]]$incremental.precip + df.mesowest[[i]]$additive.precip + df.mesowest[[i]]$accumulated.precip

  }


##########################################################################
# For-loop to calculate daily mean, min, and max temps, precip totals,
# relative humidity, and pressure data from hourly data. This file will
# be saved as a .csv to calculate short term weather variables that will
# be used in statistical models. It will also be used in this file to 
# calculate monthly means to pass on to drought and aridity indices
##########################################################################
daily.mesowest <- df.mesowest

for(i in seq_along(daily.mesowest)) {
  daily.mesowest[[i]] <- daily.mesowest[[i]] %>%
    group_by(local.date, site) %>%
    dplyr::summarize(t.mean = mean(temp, na.rm = TRUE),
                     t.max = max(temp, na.rm = TRUE),
                     t.min = min(temp, na.rm = TRUE),
                     relative.humidity = mean(relative.humidity, na.rm = TRUE),
                     daily.precip = sum(hourly.precip, na.rm = TRUE),
                     sea.level.pressure = mean(sea.level.pressure, na.rm = TRUE),
                     atm.pressure = mean(atm.pressure, na.rm = TRUE))
}

daily.mesowest <- merge_all(daily.mesowest)
daily.mesowest <- arrange(daily.mesowest, site)

write.csv(daily.mesowest, "../data_sheets/TXeco_climate_dailymesowest.csv")


##########################################################################
# Calculate monthly temperature min, max, and mean, mean rh, total precip,
# and atmospheric pressure
##########################################################################
monthly.mesowest <- daily.mesowest %>%
  mutate(year = year(local.date),
         month = month(local.date)) %>%
  group_by(site, year, month) %>%
  dplyr::summarize(month.tmean = mean(t.mean, na.rm = TRUE),
                   month.tmax = mean(t.max, na.rm = TRUE),
                   month.tmin = mean(t.min, na.rm = TRUE),
                   month.rh = mean(relative.humidity, na.rm = TRUE),
                   month.precip = sum(daily.precip, na.rm = TRUE),
                   month.sea.pressure = mean(sea.level.pressure, na.rm = TRUE),
                   month.atm.pressure = mean(atm.pressure, na.rm = TRUE))
write.csv(monthly.mesowest, "../data_sheets/TXeco_climate_monthlymesowest.csv")


################################################################
# Import and clean NOAA 2006-2020 climate data into central .csv
################################################################
## Import NOAA 2006-2020 climate normals files
file.list.normals <- list.files(path = "../climate_data/normals/",
                                 recursive = TRUE,
                                 pattern = "\\.csv$",
                                 full.names = TRUE)
file.list.normals <- setNames(file.list.normals, file.list.normals)
df.normals <- lapply(file.list.normals, read.csv)

## Convert to SI units
normals <- df.normals %>%
  merge_all() %>%
  mutate(norm.precip = norm.precip * 25.4,
         norm.tavg = fahrenheit.to.celsius(norm.tavg),
         norm.tavg.sd = norm.tavg.sd * 0.556,
         norm.tmax = fahrenheit.to.celsius(norm.tmax),
         norm.tmax.sd = norm.tmax.sd * 0.556,
         norm.tmin = fahrenheit.to.celsius(norm.tmin),
         norm.tmin.sd = norm.tmin.sd * 0.556)

## Write .csv to "data_sheets" folder
write.csv(normals, "../data_sheets/TXeco_climate_normals.csv", row.names = FALSE)

