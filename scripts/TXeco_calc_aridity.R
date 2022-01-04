################################################################
# Load libraries
################################################################
## Load libraries
library(SPEI)
library(tidyverse)
library(lubridate)
library(reshape)

################################################################
# Import daily weather means, 2006-2020 Normals csv files,
# and site coordinates
################################################################
daily.means <- read.csv("../data_sheets/TXeco_mesowest_daily.csv",
                        na.strings = "NA", stringsAsFactors = FALSE)
normals <- read.csv("../data_sheets/TXeco_climate_normals.csv",
                    na.strings = "NA", stringsAsFactors = FALSE)
site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv",
                        na.strings = "NA", stringsAsFactors = FALSE)

################################################################
# Subset daily means to 30 days leading up to sampling date and 
# append site coordinates
################################################################
# Format sampling.date and date to similar date class
daily.means$sampling.date <- mdy(daily.means$sampling.date)
daily.means$date <- ymd(daily.means$date)

# Subset data frame to only include 30 days leading up to sampling 
# date
daily.means <- daily.means %>%
  full_join(site.coords) %>%
  group_by(property, sampling.year, visit.type) %>%
  filter(date < sampling.date & date > sampling.date - 30) %>%
  unite("site.long", c(property, sampling.year, visit.type), sep = " ")


################################################################
# Separate properties into list of data frames. This is necessary
# because SPEI requires a time series notation that can only
# be accomplished through a series of unique lists
################################################################
daily.means <- setNames(split(x = daily.means,
                              f = daily.means$site.long),
                        paste0(unique(daily.means$site.long)))

################################################################
# Use Hargreaves equation to estimate 30-day reference 
# evapotranspiration, merge back to central data frame
################################################################
for(i in seq_along(daily.means)) {
  daily.means[[i]]$et0 <- as.numeric(hargreaves(Tmin = as.numeric(daily.means[[i]]$min.temp),
                                                Tmax = as.numeric(daily.means[[i]]$max.temp),
                                                lat = unique(daily.means[[i]]$latitude)))
}

## Merge list of data.frames into single data.frame
daily.means <- merge_all(daily.means)

## Separate concatenated "site.long" back to three separate columns
daily.means <- separate(daily.means, site.long, 
                        into = c("property", "sampling.year", "visit.type"),
                        sep = " ")

################################################################
# Calculate 30-day SPEI and AI for all site visits
################################################################
## Determine average monthly precipitation, et0 by visit type and property
monthly.mean <- daily.means %>%
  group_by(property, sampling.year, visit.type, latitude, longitude) %>%
  summarize(monthly.precip = sum(daily.precip, na.rm = TRUE),
            monthly.et0 = mean(et0, na.rm = TRUE)) %>%
  mutate(monthly.water.balance = as.numeric(monthly.precip - monthly.et0)) %>%
  select(site = property, everything())

## Calculate SPEI and aridity index values
monthly.spei <- spei(data = as.ts(monthly.mean$monthly.water.balance), scale = 1)

## Add SPEI and aridity index values into monthly mean data frame
monthly.mean$spei <- monthly.spei$fitted
monthly.mean$aridity <- monthly.mean$monthly.precip / monthly.mean$monthly.et0

################################################################
# Visualize 30-day aridity index
################################################################
# Initial site visits for 2020 and 2021
ggplot(data = subset(monthly.mean, visit.type == "i" & 
                       sampling.year == "2021" & site != "Menard_2020_01"), 
       aes(x = reorder(site, longitude), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "SPEI") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
ggplot(data = subset(monthly.mean, visit.type == "i" & 
                       sampling.year == "2020"), 
       aes(x = reorder(site, longitude), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "SPEI") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# Primary site visits for 2020 and 2021
ggplot(data = subset(monthly.mean, visit.type == "p" & 
                       sampling.year == "2021"), 
       aes(x = reorder(site, longitude), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "SPEI") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
ggplot(data = subset(monthly.mean, visit.type == "p" & 
                       sampling.year == "2020"), 
       aes(x = reorder(site, longitude), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "SPEI") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) 

################################################################
# Clean normals data sheet and put in list of data frames by
# site to pass Hargreaves function through
################################################################
normals <- normals %>%
  select(property, sampling.year, visit.type, month, 
         norm.precip, norm.tmax, norm.tmin) %>%
  full_join(site.coords) %>%
  group_by(property, sampling.year, visit.type, month, latitude, longitude) %>%
  summarize(norm.precip = mean(norm.precip, na.rm = TRUE),
            norm.tmax = mean(norm.tmax, na.rm = TRUE),
            norm.tmin = mean(norm.tmin, na.rm = TRUE)) %>%
  unite("site.long", c(property, sampling.year, visit.type), sep = " ")

normals <- setNames(split(x = normals,
                               f = normals$site.long),
                         paste0(unique(normals$site.long)))

################################################################
# Use Hargreaves equation to estimate 30-day reference 
# evapotranspiration, merge back to central data frame
################################################################
for(i in seq_along(normals)) {
  normals[[i]]$et0 <- as.numeric(hargreaves(Tmin = as.numeric(normals[[i]]$norm.tmin),
                                            Tmax = as.numeric(normals[[i]]$norm.tmax),
                                            lat = unique(normals[[i]]$latitude)))
}

## Merge list of data.frames into single data.frame
normals <- merge_all(normals)


## Separate concatenated "site.long" back to three separate columns
normals <- separate(normals, site.long, 
                    into = c("site", "sampling.year", "visit.type"),
                    sep = " ")

################################################################
# Calculate 15-yr normal SPEI and AI for all sites
################################################################
## Determine average monthly precipitation, et0 by visit type and property
normals <- normals %>%
  group_by(site, sampling.year, visit.type) %>%
  mutate(water.balance = as.numeric(norm.precip - et0))

## Calculate SPEI and aridity index values
normals.spei <- spei(data = as.ts(normals$water.balance), scale = 1)

## Add SPEI and aridity index values into monthly mean data frame
normals$spei <- normals.spei$fitted
normals$aridity <- normals$norm.precip / normals$et0

normals.gs <- normals %>%
  #filter(month == "5" | month == "6" | month == "7") %>%
  group_by(site, visit.type, sampling.year, longitude) %>%
  summarize(mean.aridity = mean(aridity, na.rm = TRUE),
            mean.spei = mean(spei, na.rm = TRUE))

## 2021 field sites
ggplot(data = subset(normals.gs, sampling.year == "2021" & visit.type == "i"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
  
ggplot(data = subset(normals.gs, sampling.year == "2021" & visit.type == "p"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## 2020 field sites
ggplot(data = subset(normals.gs, sampling.year == "2020" & visit.type == "i"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
ggplot(data = subset(normals.gs, sampling.year == "2020" & visit.type == "p"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


normal.summary <- normals %>%
  filter(sampling.year == "2021") %>%
  group_by(site, visit.type) %>%
  summarize(spei = mean(spei, na.rm = TRUE),
            aridity = mean(aridity, na.rm = TRUE))
