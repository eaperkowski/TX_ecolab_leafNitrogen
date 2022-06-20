###############################################################################
## Load libraries, Ecolab site coordinates, set PRISM directory, add pubtheme
###############################################################################
# Load libraries
library(dplyr)
library(stringr)
library(lubridate)
library(raster)
library(prism)
library(ggplot2)
library(ggpubr)
library(terra)
library(suncalc)

# Load Ecolab site coords
site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  dplyr::select(site = property, property = site, latitude:elevation.m)

ecosites <- site.coords %>%
  dplyr::select(site, latitude, longitude) %>%
  dplyr::distinct()

eco.coords <- dplyr::select(ecosites, x = longitude, y = latitude) # select only lat/long data, code as xy

###############################################################################
###############################################################################
## Daily PRISM climate by site (1991-2021)
###############################################################################
###############################################################################

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("../climate_data/prism/prism_daily/")

###############################################################################
## Stack raster files, select grid cells by site
###############################################################################
# Note: pd_stack stacks all daily climate data into single RasterStack, 
# terra:extract extracts all data from single grid cell for each Ecolab site

################
# Precipitation
################
pd.daily.prcp <- pd_stack(prism_archive_subset("ppt", "daily", 
                                               minDate = "1991-01-01",
                                               maxDate = "2021-07-31"))
sites.daily.prcp <- as.data.frame(terra::extract(pd.daily.prcp,
                                                 SpatialPoints(eco.coords),
                                                 sp = F))

# Add precipitation site, latitude, longitude data
sites.daily.prcp$latitude = eco.coords$y
sites.daily.prcp$longitude = eco.coords$x
sites.daily.prcp$site = ecosites$site

sites.daily.prcp <- sites.daily.prcp %>%
  dplyr::select(site, latitude, longitude, everything())

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
names(sites.daily.prcp) <- c("site", "latitude", "longitude",
                             str_c("prcp_", 
                                   str_extract(names(sites.daily.prcp[4:11172]),
                                               "[0-9]{8}")))

# Pivot to long data format
sites.daily.prcp <- sites.daily.prcp %>%
  tidyr::pivot_longer(cols = prcp_19910101:prcp_20210730,
                      names_to = "date", values_to = "daily.prcp") %>%
  mutate(date = ymd(str_replace(date, "prcp_", "")))

write.csv(sites.daily.prcp, "/Users/eaperkowski/Desktop/precip_daily_ecoSites.csv")


################
# Tmean
################
pd.daily.tmean <- pd_stack(prism_archive_subset("tmean", "daily", 
                                                minDate = "1991-01-01",
                                                maxDate = "2021-07-31"))
sites.daily.tmean <- as.data.frame(terra::extract(pd.daily.tmean,
                                                  SpatialPoints(eco.coords),
                                                  sp = F))

# Add  site, latitude, longitude data
sites.daily.tmean$latitude = eco.coords$y
sites.daily.tmean$longitude = eco.coords$x
sites.daily.tmean$site = ecosites$site

sites.daily.tmean <- sites.daily.tmean %>%
  dplyr::select(site, latitude, longitude, everything())

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
names(sites.daily.tmean) <- c("site", "latitude", "longitude",
                              str_c("tmean_", 
                                    str_extract(names(sites.daily.tmean[4:11172]),
                                                "[0-9]{8}")))


# Pivot to long data format
sites.daily.tmean <- sites.daily.tmean %>%
  tidyr::pivot_longer(cols = tmean_19910101:tmean_20210730,
                      names_to = "date", values_to = "daily.tmean") %>%
  mutate(date = ymd(str_replace(date, "tmean_", "")))


################
# Tmax
################
pd.daily.tmax <- pd_stack(prism_archive_subset("tmax", "daily", 
                                         minDate = "1991-01-01",
                                         maxDate = "2021-07-31"))
sites.daily.tmax <- as.data.frame(terra::extract(pd.daily.tmax,
                                                 SpatialPoints(eco.coords),
                                                 sp = F))

# Add  site, latitude, longitude data
sites.daily.tmax$latitude = eco.coords$y
sites.daily.tmax$longitude = eco.coords$x
sites.daily.tmax$site = ecosites$site

sites.daily.tmax <- sites.daily.tmax %>%
  dplyr::select(site, latitude, longitude, everything())


# Rename columns to have climate data indicator as prefix with month/year
# as suffix
names(sites.daily.tmax) <- c("site", "latitude", "longitude",
                             str_c("tmax_", 
                                   str_extract(names(sites.daily.tmax[4:11172]),
                                               "[0-9]{8}")))


# Pivot to long data format
sites.daily.tmax <- sites.daily.tmax %>%
  tidyr::pivot_longer(cols = tmax_19910101:tmax_20210730,
                      names_to = "date", values_to = "daily.tmax") %>%
  mutate(date = ymd(str_replace(date, "tmax_", "")))


################
# Tmin
################
pd.daily.tmin <- pd_stack(prism_archive_subset("tmin", "daily", 
                                         minDate = "1991-01-01",
                                         maxDate = "2021-07-31"))

sites.daily.tmin <- as.data.frame(terra::extract(pd.daily.tmin,
                                                 SpatialPoints(eco.coords),
                                                 sp = F))

# Add  site, latitude, longitude data
sites.daily.tmin$latitude = eco.coords$y
sites.daily.tmin$longitude = eco.coords$x
sites.daily.tmin$site = ecosites$site

sites.daily.tmin <- sites.daily.tmin %>%
  dplyr::select(site, latitude, longitude, everything())

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
names(sites.daily.tmin) <- c("site", "latitude", "longitude",
                             str_c("tmin_", 
                                   str_extract(names(sites.daily.tmin[4:11172]),
                                               "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.tmin <- sites.daily.tmin %>%
  tidyr::pivot_longer(cols = tmin_19910101:tmin_20210730,
                      names_to = "date", values_to = "daily.tmin") %>%
  mutate(date = ymd(str_replace(date, "tmin_", "")))


################
# VPDmax
################
pd.daily.vpdmax <- pd_stack(prism_archive_subset("vpdmax", "daily", 
                                           minDate = "1991-01-01",
                                           maxDate = "2021-07-31"))

sites.daily.vpdmax <- as.data.frame(terra::extract(pd.daily.vpdmax,
                                                   SpatialPoints(eco.coords),
                                                   sp = F))

# Add  site, latitude, longitude data
sites.daily.vpdmax$latitude = eco.coords$y
sites.daily.vpdmax$longitude = eco.coords$x
sites.daily.vpdmax$site = ecosites$site

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
sites.daily.vpdmax <- sites.daily.vpdmax %>%
  dplyr::select(site, latitude, longitude, everything())

names(sites.daily.vpdmax) <- c("site", "latitude", "longitude",
                               str_c("vpdmax_", 
                                     str_extract(names(sites.daily.vpdmax[4:11172]),
                                                 "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.vpdmax <- sites.daily.vpdmax %>%
  tidyr::pivot_longer(cols = vpdmax_19910101:vpdmax_20210730,
                      names_to = "date", values_to = "daily.vpdmax") %>%
  mutate(date = ymd(str_replace(date, "vpdmax_", "")))


################
# VPDmin
################
pd.daily.vpdmin <- pd_stack(prism_archive_subset("vpdmin", "daily", 
                                           minDate = "1991-01-01",
                                           maxDate = "2021-07-31"))

sites.daily.vpdmin <- as.data.frame(terra::extract(pd.daily.vpdmin,
                                                   SpatialPoints(eco.coords),
                                                   sp = F))

# Add  site, latitude, longitude data
sites.daily.vpdmin$latitude = eco.coords$y
sites.daily.vpdmin$longitude = eco.coords$x
sites.daily.vpdmin$site = ecosites$site

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
sites.daily.vpdmin <- sites.daily.vpdmin %>%
  dplyr::select(site, latitude, longitude, everything())

names(sites.daily.vpdmin) <- c("site", "latitude", "longitude",
                               str_c("vpdmin_", 
                                     str_extract(names(sites.daily.vpdmin[4:11172]),
                                                 "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.vpdmin <- sites.daily.vpdmin %>%
  tidyr::pivot_longer(cols = vpdmin_19910101:vpdmin_20210730,
                      names_to = "date", values_to = "daily.vpdmin") %>%
  mutate(date = ymd(str_replace(date, "vpdmin_", "")))

## Join normals data into single dataset
daily.clim <- sites.daily.prcp %>%
  left_join(sites.daily.tmean) %>%
  left_join(sites.daily.tmax) %>%
  full_join(sites.daily.tmin) %>%
  full_join(sites.daily.vpdmax) %>%
  full_join(sites.daily.vpdmin)

###############################################################################
## Determine sunrise/sunset using 'suncalc' package, then calculate fraction
## of sunlight hours
###############################################################################

## Change latitude/longitude colnames to "lat" and "long" to make easier
## data loading into "getSunlightTimes" function
names(daily.clim)[2:3] <- c("lat", "lon")

## Visualize dataset
head(daily.clim)

## Obtain sunrise/sunset times given lat/long and date
sunlight <- getSunlightTimes(data = daily.clim, tz = "America/Chicago")

## Merge sunlight df with daily.clim, select sunrise and sunset data from
## sunlight call, then calculate the time difference between sunset and
## sunrise. Then, calculate fraction of sunlight given 24 hours in day.

## NOTE: Using sunriseEnd and sunsetStart instead of sunset because SPLASH model uses fraction
## of "bright" sunshine hours
daily.clim <- daily.clim %>%
  coalesce(sunlight) %>%
  dplyr::select(site:daily.vpdmin, sunriseEnd, sunsetStart) %>%
  mutate(sun.hours = as.double(difftime(sunsetStart, sunriseEnd, units = "hours")),
         sf = sun.hours/24,
         m = month(date),
         i = day(date),
         year = year(date)) %>%
  full_join(site.coords) %>%
  dplyr::select(site:date, m:year, elevation.m, 
                daily.prcp:daily.vpdmin, sun.hours, sf)

## Create 2019 files for for splash prep
test.splash2019 <- daily.clim %>%
  filter(year == 2019) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp)

test.splash2019 <- lapply(split(test.splash2019, test.splash2019$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2019, 
       file = paste(paste("../climate_data/splash_2019/", 
                          names(test.splash2019), sep = ""),
                    "2019splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))


## Create 2020 files for for splash prep
test.splash2020 <- daily.clim %>%
  filter(year == 2020) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp)

test.splash2020 <- lapply(split(test.splash2020, test.splash2020$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2020, 
       file = paste(paste("../climate_data/splash_2020/", 
                          names(test.splash2019), sep = ""),
                    "2020splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## Create 2021 files for for splash prep
test.splash2021 <- daily.clim %>%
  filter(year == 2021) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp)

test.splash2021 <- lapply(split(test.splash2021, test.splash2021$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2021, 
       file = paste(paste("../climate_data/splash_2021/", 
                          names(test.splash2021), sep = ""),
                    "2021splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))


## Write daily climate .csv
write.csv(daily.clim, "../climate_data/TXeco_PRISM_daily.csv",
          row.names = FALSE)

