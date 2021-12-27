################################################################
# Load libraries
################################################################
library(tidyverse)
library(lubridate)
library(dplyr)
library(reshape)
library(weathermetrics)

################################################################
# Import and clean mesowest files into hourly climate data into
# central .csv and export to "data_sheets" folder
################################################################
## Import mesowest files
file.list.mesowest <- list.files(path = "../climate_data/mesowest/",
                        recursive = TRUE,
                        pattern = "\\.csv$",
                        full.names = TRUE)
file.list.mesowest <- setNames(file.list.mesowest, file.list.mesowest)
df.mesowest <- lapply(file.list.mesowest, read.csv)

## Merge mesowest climate data from all properties and both sampling years into 
## single df, calculate daily estimates
mesowest.hourly <- df.mesowest %>%
  merge_all() %>%
  mutate(date.time = as.POSIXct(strptime(date.time, 
                                         format = "%m/%d/%Y %H:%M CDT"))) %>%
  mutate(date.time.hr = floor_date(date.time, unit = "hour")) %>%
  separate(date.time.hr, into = c("date", "time.round"), sep = " ", remove = FALSE) %>%
  dplyr::group_by(site, sampling.year, visit.type, date, time.round) %>%
  dplyr::summarize(air.temp = mean(air.temp, na.rm = TRUE),
                   relative.humidity = mean(relative.humidity, na.rm = TRUE),
                   wind.speed = mean(wind.speed, na.rm = TRUE),
                   wind.gust = max(wind.gust, na.rm = TRUE),
                   solar.rad = max(solar.rad, na.rm = TRUE),
                   incremental.precip = max(incremental.precip, na.rm = TRUE),
                   accumulated.precip = max(accumulated.precip, na.rm = TRUE),
                   additive.precip = max(additive.precip, na.rm = TRUE),
                   sea.level.pressure = mean(sea.level.pressure, na.rm = TRUE),
                   atm.pressure = mean(atm.pressure, na.rm = TRUE)) %>%
  data.frame()

## Convert all -Inf values to NA
short.merged[short.merged == "-Inf"] <- NA

## Write .csv to "data_sheets" folder
write.csv

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

normals <- df.normals %>%
  merge_all() %>%
  mutate(norm.precip = norm.precip * 2.54,
            norm.tavg = fahrenheit.to.celsius(norm.tavg),
            norm.tavg.sd = norm.tavg.sd * 0.556,
            norm.tmax = fahrenheit.to.celsius(norm.tmax),
            norm.tmax.sd = norm.tmax.sd * 0.556,
            norm.tmin = fahrenheit.to.celsius(norm.tmin),
            norm.tmin.sd = norm.tmin.sd * 0.556)

write.csv(normals, "../data_sheets/TXeco_climate_normals.csv")

