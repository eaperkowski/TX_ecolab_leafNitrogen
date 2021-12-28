################################################################
# Load libraries
################################################################
## Load libraries
library(SPEI)
library(tidyverse)
library(lubridate)

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
  group_by(site, sampling.year, visit.type) %>%
  subset(., date < sampling.date & date > sampling.date - 30) %>%
  unite("site.long", c(site, sampling.year, visit.type), sep = " ")

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
                        into = c("site", "sampling.year", "visit.type"),
                        sep = " ")

################################################################
# Calculate 30-day SPEI and AI for all site visits
################################################################
## Determine average monthly precipitation, et0 by visit type and property
monthly.mean <- daily.means %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(monthly.precip = sum(daily.precip, na.rm = TRUE),
            monthly.et0 = mean(et0, na.rm = TRUE)) %>%
  mutate(monthly.water.balance = as.numeric(monthly.precip - monthly.et0))

## Calculate SPEI and aridity index values
monthly.spei <- spei(data = as.ts(monthly.mean$monthly.water.balance), scale = 1)
data.frame(monthly.spei$fitted)

## Add SPEI and aridity index values into monthly mean data frame
monthly.mean$spei <- monthly.spei$fitted
monthly.mean$aridity <- monthly.mean$monthly.precip / monthly.mean$monthly.et0

################################################################
# Visualize 30-day aridity index
################################################################
ggplot(data = subset(monthly.mean, visit.type == "i" & 
                       sampling.year == "2021" & site != "Menard_2020_01"), 
       aes(x = reorder(site, -spei), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(sampling.year~.) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = subset(monthly.mean, visit.type == "i" & 
                       sampling.year == "2020"), 
       aes(x = reorder(site, -spei), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(sampling.year~.) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = subset(monthly.mean, visit.type == "p" & 
                       sampling.year == "2021" & site != "Menard_2020_01"), 
       aes(x = reorder(site, -spei), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(sampling.year~.) +
  theme(axis.text.x = element_text(angle = 45))



