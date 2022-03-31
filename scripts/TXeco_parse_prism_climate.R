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
ecosites <- read.csv("../data_sheets/TXeco_sitecoords.csv") # add path to site coords file here
eco.coords <- dplyr::select(ecosites, x = longitude, y = latitude) # select only lat/long data, code as xy

# Change visit dates to mdy format
ecosites$initial.2020 <- mdy(ecosites$initial.2020)
ecosites$primary.2020 <- mdy(ecosites$primary.2020)
ecosites$initial.2021 <- mdy(ecosites$initial.2021)
ecosites$primary.2021 <- mdy(ecosites$primary.2021)

###############################################################################
###############################################################################
##  PRISM climate normals (1991-2020) by site
###############################################################################
###############################################################################

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("../climate_data/prism/prism_monthly/")

###############################################################################
## Stack raster files, select grid cells by site
###############################################################################
# Note: pd_stack stacks all monthly climate data into single RasterStack, 
# terra:extract extracts all data from single grid cell for each EcoLab site

################
# Precipitation
################
pd.precip <- pd_stack(prism_archive_subset("ppt", "monthly", 
                                           years = 1991:2020, mon = 1:12))
norm.sites.prcp <- as.data.frame(terra::extract(pd.precip,
                                                SpatialPoints(eco.coords),
                                                sp = F))

# Add precipitation site, latitude, longitude data
norm.sites.prcp$latitude = eco.coords$y
norm.sites.prcp$longitude = eco.coords$x
norm.sites.prcp$site <- ecosites$site

norm.sites.prcp <- norm.sites.prcp %>%
  dplyr::select(site, latitude, longitude, everything())

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
names(norm.sites.prcp) <- c("site", "latitude", "longitude",
                             str_c("prcp_", 
                                   str_extract(names(norm.sites.prcp[4:363]),
                                               "[0-9]{6}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
norm.sites.prcp <- norm.sites.prcp %>%
  tidyr::pivot_longer(cols = prcp_199101:prcp_202012,
                      names_to = "date", values_to = "monthly.prcp") %>%
  mutate(date = ym(str_replace(date, "prcp_", "")),
         month = month(date),
         year = year(date)) %>%
  group_by(site, latitude, longitude, month) %>%
  summarize(norm.prcp = mean(monthly.prcp, na.rm = TRUE),
            sd.prcp = sd(monthly.prcp, na.rm = TRUE))


# Calculate mean annual precipitation
annual.prcp <- norm.sites.prcp %>%
  group_by(site) %>%
  summarize(map.mm = sum(norm.prcp, na.rm = TRUE))


################
# Tmean
################
pd.tmean <- pd_stack(prism_archive_subset("tmean", "monthly", 
                                          years = 1991:2020, mon = 1:12))
norm.sites.tmean <- as.data.frame(terra::extract(pd.tmean,
                                                 SpatialPoints(eco.coords),
                                                 sp = F))

# Add  site, latitude, longitude data
norm.sites.tmean$latitude = eco.coords$y
norm.sites.tmean$longitude = eco.coords$x
norm.sites.tmean$site = ecosites$site


# Rename columns to have climate data indicator as prefix with month/year
# as suffix
norm.sites.tmean <- norm.sites.tmean %>%
  dplyr::select(site, latitude, longitude, everything())

names(norm.sites.tmean) <- c("site", "latitude", "longitude",
                            str_c("tmean_", 
                                  str_extract(names(norm.sites.tmean[4:363]),
                                              "[0-9]{6}")))


# Pivot to long data format and summarize mean temperature by month (1991-2020)
# to simulate 1991-2020 climate normals
norm.sites.tmean <- norm.sites.tmean %>%
  tidyr::pivot_longer(cols = tmean_199101:tmean_202012,
                      names_to = "date", values_to = "monthly.tmean") %>%
  mutate(date = ym(str_replace(date, "tmean_", "")),
         month = month(date),
         year = year(date)) %>%
  group_by(site, latitude, longitude, month) %>%
  summarize(norm.tmean = mean(monthly.tmean, na.rm = TRUE),
            sd.tmean = sd(monthly.tmean, na.rm = TRUE))


# Calculate mean annual precipitation
annual.tmean <- norm.sites.tmean %>%
  group_by(site) %>%
  summarize(mat.c = mean(norm.tmean, na.rm = TRUE))


################
# Tmax
################
pd.tmax <- pd_stack(prism_archive_subset("tmax", "monthly", 
                                         years = 1991:2020, mon = 1:12))
norm.sites.tmax <- as.data.frame(terra::extract(pd.tmax,
                                                SpatialPoints(eco.coords),
                                                sp = F))

# Add  site, latitude, longitude data
norm.sites.tmax$latitude = eco.coords$y
norm.sites.tmax$longitude = eco.coords$x
norm.sites.tmax$site = ecosites$site


# Rename columns to have climate data indicator as prefix with month/year
# as suffix
norm.sites.tmax <- norm.sites.tmax %>%
  dplyr::select(site, latitude, longitude, everything())


names(norm.sites.tmax) <- c("site", "latitude", "longitude",
                            str_c("tmax_", 
                                  str_extract(names(norm.sites.tmax[4:363]),
                                              "[0-9]{6}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
norm.sites.tmax <- norm.sites.tmax %>%
  tidyr::pivot_longer(cols = tmax_199101:tmax_202012,
                      names_to = "date", values_to = "monthly.tmax") %>%
  mutate(date = ym(str_replace(date, "tmax_", "")),
         month = month(date),
         year = year(date)) %>%
  group_by(site, latitude, longitude, month) %>%
  summarize(norm.tmax = mean(monthly.tmax, na.rm = TRUE),
            sd.tmax = sd(monthly.tmax, na.rm = TRUE))

annual.tmax <- norm.sites.tmax %>%
  group_by(site) %>%
  summarize(tmax = mean(norm.tmax, na.rm = TRUE))

################
# Tmin
################
pd.tmin <- pd_stack(prism_archive_subset("tmin", "monthly", 
                                         years = 1991:2020, mon = 1:12))
norm.sites.tmin <- as.data.frame(terra::extract(pd.tmin,
                                                SpatialPoints(eco.coords),
                                                sp = F))

# Add  site, latitude, longitude data
norm.sites.tmin$latitude = eco.coords$y
norm.sites.tmin$longitude = eco.coords$x
norm.sites.tmin$site = ecosites$site

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
norm.sites.tmin <- norm.sites.tmin %>%
  dplyr::select(site, latitude, longitude, everything())

names(norm.sites.tmin) <- c("site", "latitude", "longitude",
                            str_c("tmin_", 
                                  str_extract(names(norm.sites.tmin[4:363]),
                                              "[0-9]{6}")))

# Pivot to long data format and summarize by month (1991-2020)
# to simulate 1991-2020 climate normals
norm.sites.tmin <- norm.sites.tmin %>%
  tidyr::pivot_longer(cols = tmin_199101:tmin_202012,
                      names_to = "date", values_to = "monthly.tmin") %>%
  mutate(date = ym(str_replace(date, "tmin_", "")),
         month = month(date),
         year = year(date)) %>%
  group_by(site, latitude, longitude, month) %>%
  summarize(norm.tmin = mean(monthly.tmin, na.rm = TRUE),
            sd.tmin = sd(monthly.tmin, na.rm = TRUE))

annual.tmin <- norm.sites.tmin %>%
  group_by(site) %>%
  summarize(tmin = mean(norm.tmin, na.rm = TRUE))



################
# VPDmax
################
pd.vpdmax <- pd_stack(prism_archive_subset("vpdmax", "monthly", 
                                           years = 1991:2020, mon = 1:12))
norm.sites.vpdmax<- as.data.frame(terra::extract(pd.vpdmax,
                                                 SpatialPoints(eco.coords),
                                                 sp = F))

# Add  site, latitude, longitude data
norm.sites.vpdmax$latitude = eco.coords$y
norm.sites.vpdmax$longitude = eco.coords$x
norm.sites.vpdmax$site = ecosites$site

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
norm.sites.vpdmax <- norm.sites.vpdmax %>%
  dplyr::select(site, latitude, longitude, everything())

names(norm.sites.vpdmax) <- c("site", "latitude", "longitude",
                            str_c("vpdmax_", 
                                  str_extract(names(norm.sites.vpdmax[4:363]),
                                              "[0-9]{6}")))

# Pivot to long data format and summarize by month (1991-2020)
# to simulate 1991-2020 climate normals
norm.sites.vpdmax <- norm.sites.vpdmax %>%
  tidyr::pivot_longer(cols = vpdmax_199101:vpdmax_202012,
                      names_to = "date", values_to = "monthly.vpdmax") %>%
  mutate(date = ym(str_replace(date, "vpdmax_", "")),
         month = month(date),
         year = year(date)) %>%
  group_by(site, latitude, longitude, month) %>%
  summarize(norm.vpdmax = mean(monthly.vpdmax, na.rm = TRUE),
            sd.vpdmax = sd(monthly.vpdmax, na.rm = TRUE))

annual.vpdmax <- norm.sites.vpdmax %>%
  group_by(site) %>%
  summarize(vpdmax = mean(norm.vpdmax, na.rm = TRUE))

################
# VPDmin
################
pd.vpdmin <- pd_stack(prism_archive_subset("vpdmin", "monthly", 
                                           years = 1991:2020, mon = 1:12))
norm.sites.vpdmin <- as.data.frame(terra::extract(pd.vpdmin,
                                                  SpatialPoints(eco.coords),
                                                  sp = F))

# Add  site, latitude, longitude data
norm.sites.vpdmin$latitude = eco.coords$y
norm.sites.vpdmin$longitude = eco.coords$x
norm.sites.vpdmin$site = ecosites$site

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
norm.sites.vpdmin <- norm.sites.vpdmin %>%
  dplyr::select(site, latitude, longitude, everything())

names(norm.sites.vpdmin) <- c("site", "latitude", "longitude",
                              str_c("vpdmin_", 
                                    str_extract(names(norm.sites.vpdmin[4:363]),
                                                "[0-9]{6}")))

# Pivot to long data format and summarize by month (1991-2020)
# to simulate 1991-2020 climate normals
norm.sites.vpdmin <- norm.sites.vpdmin %>%
  tidyr::pivot_longer(cols = vpdmin_199101:vpdmin_202012,
                      names_to = "date", values_to = "monthly.vpdmin") %>%
  mutate(date = ym(str_replace(date, "vpdmin_", "")),
         month = month(date),
         year = year(date)) %>%
  group_by(site, latitude, longitude, month) %>%
  summarize(norm.vpdmin = mean(monthly.vpdmin, na.rm = TRUE),
            sd.vpdmin = sd(monthly.vpdmin, na.rm = TRUE))

annual.vpdmin <- norm.sites.vpdmin %>%
  group_by(site) %>%
  summarize(vpdmin = mean(norm.vpdmin, na.rm = TRUE))

## Join normals data into single dataset
monthly.normals <- norm.sites.prcp %>%
  full_join(norm.sites.tmean) %>%
  full_join(norm.sites.tmax) %>%
  full_join(norm.sites.tmin) %>%
  full_join(norm.sites.vpdmax) %>%
  full_join(norm.sites.vpdmin)

annual.normals <- annual.prcp %>%
  full_join(annual.tmean) %>%
  full_join(annual.tmax) %>%
  full_join(annual.tmin) %>%
  full_join(annual.vpdmax) %>%
  full_join(annual.vpdmin)

## Write climate normals .csv
write.csv(annual.normals, "../climate_data/TXeco_PRISM_19912020_annualNorms.csv",
          row.names = FALSE)
write.csv(monthly.normals, "../climate_data/TXeco_PRISM_19912020_monthlyNorms.csv",
          row.names = FALSE)

###############################################################################
###############################################################################
## Daily PRISM climate by site
###############################################################################
###############################################################################

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("../climate_data/prism/prism_daily/")

###############################################################################
## Stack raster files, select grid cells by site
###############################################################################
# Note: pd_stack stacks all monthly climate data into single RasterStack, 
# terra:extract extracts all data from single grid cell for each Ecolab site


################
# Precipitation
################
pd.daily.prcp <- pd_stack(prism_archive_subset("ppt", "daily", 
                                               minDate = "2019-06-01",
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
                                   str_extract(names(sites.daily.prcp[4:794]),
                                               "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.prcp <- sites.daily.prcp %>%
  tidyr::pivot_longer(cols = prcp_20190601:prcp_20210730,
                      names_to = "date", values_to = "daily.prcp") %>%
  mutate(date = ymd(str_replace(date, "prcp_", "")))


################
# Tmean
################
pd.daily.tmean <- pd_stack(prism_archive_subset("tmean", "daily", 
                                          minDate = "2019-06-01",
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
                                    str_extract(names(sites.daily.tmean[4:794]),
                                                "[0-9]{8}")))


# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.tmean <- sites.daily.tmean %>%
  tidyr::pivot_longer(cols = tmean_20190601:tmean_20210730,
                      names_to = "date", values_to = "daily.tmean") %>%
  mutate(date = ymd(str_replace(date, "tmean_", "")))


################
# Tmax
################
pd.daily.tmax <- pd_stack(prism_archive_subset("tmax", "daily", 
                                         minDate = "2019-06-01",
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
                                   str_extract(names(sites.daily.tmax[4:794]),
                                               "[0-9]{8}")))



# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.tmax <- sites.daily.tmax %>%
  tidyr::pivot_longer(cols = tmax_20190601:tmax_20210730,
                      names_to = "date", values_to = "daily.tmax") %>%
  mutate(date = ymd(str_replace(date, "tmax_", "")))


################
# Tmin
################
pd.daily.tmin <- pd_stack(prism_archive_subset("tmin", "daily", 
                                         minDate = "2019-06-01",
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
                                   str_extract(names(sites.daily.tmin[4:794]),
                                               "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.tmin <- sites.daily.tmin %>%
  tidyr::pivot_longer(cols = tmin_20190601:tmin_20210730,
                      names_to = "date", values_to = "daily.tmin") %>%
  mutate(date = ymd(str_replace(date, "tmin_", "")))


################
# VPDmax
################
pd.daily.vpdmax <- pd_stack(prism_archive_subset("vpdmax", "daily", 
                                           minDate = "2019-06-01",
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
                                     str_extract(names(sites.daily.vpdmax[4:794]),
                                                 "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.vpdmax <- sites.daily.vpdmax %>%
  tidyr::pivot_longer(cols = vpdmax_20190601:vpdmax_20210730,
                      names_to = "date", values_to = "daily.vpdmax") %>%
  mutate(date = ymd(str_replace(date, "vpdmax_", "")))


################
# VPDmin
################
pd.daily.vpdmin <- pd_stack(prism_archive_subset("vpdmin", "daily", 
                                           minDate = "2019-06-01",
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
                                     str_extract(names(sites.daily.vpdmin[4:794]),
                                                 "[0-9]{8}")))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
sites.daily.vpdmin <- sites.daily.vpdmin %>%
  tidyr::pivot_longer(cols = vpdmin_20190601:vpdmin_20210730,
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
  full_join(ecosites) %>%
  dplyr::select(site:date, m:year, elevation.m, initial.2020:primary.2021, 
                daily.prcp:daily.vpdmin, sun.hours, sf)

## Write daily climate .csv
write.csv(daily.clim, "../climate_data/TXeco_PRISM_daily.csv",
          row.names = FALSE)
