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
file.list.mesowest <- list.files(path = "../climate_data/mesowest",
                        recursive = TRUE,
                        pattern = "\\.csv$",
                        full.names = TRUE)
file.list.mesowest <- setNames(file.list.mesowest, 
                               str_extract(basename(file.list.mesowest), 
                                           '.*(?=\\.csv)'))

df.mesowest <- lapply(file.list.mesowest, read.csv)

## Merge mesowest climate data from all properties and both sampling years into 
## single df, calculate hourly estimates

## Note to self: would it be easier to do a for loop and iteratively calculate 
## these hourly estimates? You could then just calculate daily averages

mesowest.hourly <- df.mesowest %>%
  merge_all() %>%
  filter(sampling.year == "2020") %>%
  mutate(date.time.round = floor_date(as.POSIXct(strptime(date.time,
                                                          format = "%m/%d/%Y %H:%M UTC")), 
                                      unit = "hour")) %>%
  separate(date.time.round, into = c("date", "time.round"), 
           sep = " ", remove = FALSE)

## STOPPED HERE 02-18-22 ##

test <- mesowest.hourly %>%
  group_by(site, sampling.year, sampling.date, visit.type, date, time.round) %>%
  dplyr::summarize(air.temp = mean(air.temp, na.rm = TRUE),
                   relative.humidity = mean(relative.humidity, na.rm = TRUE),
                   incremental.precip = sum(incremental.precip, na.rm = TRUE),
                   accumulated.precip = max(accumulated.precip, na.rm = TRUE),
                   additive.precip = max(additive.precip, na.rm = TRUE),
                   sea.level.pressure = mean(sea.level.pressure, na.rm = TRUE),
                   atm.pressure = mean(atm.pressure, na.rm = TRUE)) %>%
  dplyr::mutate(additive.precip = additive.precip - lag(additive.precip),
                additive.precip = ifelse(additive.precip == "NaN" | 
                                           is.na(additive.precip) == TRUE, 
                                         0, additive.precip),
                accumulated.precip = ifelse(accumulated.precip == "-Inf", 
                                            0, accumulated.precip),
                accum.precip = accumulated.precip - lag(accumulated.precip),
                accum.precip = ifelse(time.round == "00:00:00", 
                                      accumulated.precip, accum.precip),
                hourly.precip = incremental.precip + accum.precip + additive.precip,
                hourly.precip = ifelse(hourly.precip == "Inf", 0, hourly.precip)) %>%
  dplyr::select(-c(incremental.precip:additive.precip, accum.precip)) %>%
  data.frame()

## Rename "site" column to "property
names(mesowest.hourly)[names(mesowest.hourly) == "site"] <- "property"

## Convert all -Inf values to NA
mesowest.hourly[mesowest.hourly == "-Inf"] <- NA

## Write .csv to "data_sheets" folder
write.csv(mesowest.hourly, "../data_sheets/TXeco_mesowest_hourly.csv", 
          row.names = FALSE)

################################################################
# Calculate daily means for evapotranspiration estimate and 
# SPEI/AI calculations. Hargreaves requires Tmax, Tmin
################################################################
mesowest.daily <- mesowest.hourly %>%
  group_by(property, sampling.year, sampling.date, visit.type, date) %>%
  summarize(mean.temp = mean(air.temp, na.rm = TRUE),
            sd.temp = sd(air.temp, na.rm = TRUE),
            max.temp = max(air.temp, na.rm = TRUE),
            min.temp = min(air.temp, na.rm = TRUE),
            daily.precip = sum(hourly.precip, na.rm = TRUE))

## Write .csv to "data_sheets" folder
write.csv(mesowest.daily, "../data_sheets/TXeco_mesowest_daily.csv", row.names = FALSE)

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

