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

# Load SoilGrids datafile
soilgrids <- read.csv("../data_sheets/TXeco_soilgrid_data.csv")

# Load calc_soil_water custom fxn
source("../../r_functions/calc_soil_water.R")

###############################################################################
###############################################################################
## Daily PRISM climate by site (1991-2021)
###############################################################################
###############################################################################

# Set PRISM directory (put folder where prism data will be stored here)
# prism_set_dl_dir("../climate_data/prism/prism_daily/")
# 
# ###############################################################################
# ## Stack raster files, select grid cells by site
# ###############################################################################
# # Note: pd_stack stacks all daily climate data into single RasterStack, 
# # terra:extract extracts all data from single grid cell for each Ecolab site
# 
# 
# ################
# # Precipitation
# ################
# pd.daily.prcp <- pd_stack(prism_archive_subset("ppt", "daily", 
#                                                minDate = "1991-01-01",
#                                                maxDate = "2021-07-31"))
# sites.daily.prcp <- as.data.frame(terra::extract(pd.daily.prcp,
#                                                  SpatialPoints(eco.coords),
#                                                  sp = F))
# 
# # Add precipitation site, latitude, longitude data
# sites.daily.prcp$latitude = eco.coords$y
# sites.daily.prcp$longitude = eco.coords$x
# sites.daily.prcp$site = ecosites$site
# 
# sites.daily.prcp <- sites.daily.prcp %>%
#   dplyr::select(site, latitude, longitude, everything())
# 
# # Rename columns to have climate data indicator as prefix with month/year
# # as suffix
# names(sites.daily.prcp) <- c("site", "latitude", "longitude",
#                              str_c("prcp_", 
#                                    str_extract(names(sites.daily.prcp[4:11172]),
#                                                "[0-9]{8}")))
# 
# # Pivot to long data format
# sites.daily.prcp <- sites.daily.prcp %>%
#   tidyr::pivot_longer(cols = prcp_19910101:prcp_20210730,
#                       names_to = "date", values_to = "daily.prcp") %>%
#   mutate(date = ymd(str_replace(date, "prcp_", "")))
# 
# ################
# # Tmean
# ################
# pd.daily.tmean <- pd_stack(prism_archive_subset("tmean", "daily", 
#                                                 minDate = "1991-01-01",
#                                                 maxDate = "2021-07-31"))
# sites.daily.tmean <- as.data.frame(terra::extract(pd.daily.tmean,
#                                                   SpatialPoints(eco.coords),
#                                                   sp = F))
# 
# # Add  site, latitude, longitude data
# sites.daily.tmean$latitude = eco.coords$y
# sites.daily.tmean$longitude = eco.coords$x
# sites.daily.tmean$site = ecosites$site
# 
# sites.daily.tmean <- sites.daily.tmean %>%
#   dplyr::select(site, latitude, longitude, everything())
# 
# # Rename columns to have climate data indicator as prefix with month/year
# # as suffix
# names(sites.daily.tmean) <- c("site", "latitude", "longitude",
#                               str_c("tmean_", 
#                                     str_extract(names(sites.daily.tmean[4:11172]),
#                                                 "[0-9]{8}")))
# 
# # Pivot to long data format
# sites.daily.tmean <- sites.daily.tmean %>%
#   tidyr::pivot_longer(cols = tmean_19910101:tmean_20210730,
#                       names_to = "date", values_to = "daily.tmean") %>%
#   mutate(date = ymd(str_replace(date, "tmean_", "")))
# 
# 
# ################
# # Tmax
# ################
# pd.daily.tmax <- pd_stack(prism_archive_subset("tmax", "daily", 
#                                          minDate = "1991-01-01",
#                                          maxDate = "2021-07-31"))
# sites.daily.tmax <- as.data.frame(terra::extract(pd.daily.tmax,
#                                                  SpatialPoints(eco.coords),
#                                                  sp = F))
# 
# # Add  site, latitude, longitude data
# sites.daily.tmax$latitude = eco.coords$y
# sites.daily.tmax$longitude = eco.coords$x
# sites.daily.tmax$site = ecosites$site
# 
# sites.daily.tmax <- sites.daily.tmax %>%
#   dplyr::select(site, latitude, longitude, everything())
# 
# 
# # Rename columns to have climate data indicator as prefix with month/year
# # as suffix
# names(sites.daily.tmax) <- c("site", "latitude", "longitude",
#                              str_c("tmax_", 
#                                    str_extract(names(sites.daily.tmax[4:11172]),
#                                                "[0-9]{8}")))
# 
# # Pivot to long data format
# sites.daily.tmax <- sites.daily.tmax %>%
#   tidyr::pivot_longer(cols = tmax_19910101:tmax_20210730,
#                       names_to = "date", values_to = "daily.tmax") %>%
#   mutate(date = ymd(str_replace(date, "tmax_", "")))
# 
# ################
# # Tmin
# ################
# pd.daily.tmin <- pd_stack(prism_archive_subset("tmin", "daily", 
#                                          minDate = "1991-01-01",
#                                          maxDate = "2021-07-31"))
# 
# sites.daily.tmin <- as.data.frame(terra::extract(pd.daily.tmin,
#                                                  SpatialPoints(eco.coords),
#                                                  sp = F))
# 
# # Add  site, latitude, longitude data
# sites.daily.tmin$latitude = eco.coords$y
# sites.daily.tmin$longitude = eco.coords$x
# sites.daily.tmin$site = ecosites$site
# 
# sites.daily.tmin <- sites.daily.tmin %>%
#   dplyr::select(site, latitude, longitude, everything())
# 
# # Rename columns to have climate data indicator as prefix with month/year
# # as suffix
# names(sites.daily.tmin) <- c("site", "latitude", "longitude",
#                              str_c("tmin_", 
#                                    str_extract(names(sites.daily.tmin[4:11172]),
#                                                "[0-9]{8}")))
# 
# # Pivot to long data format and summarize precipitation by month (1991-2020)
# # to simulate 1991-2020 climate normals
# sites.daily.tmin <- sites.daily.tmin %>%
#   tidyr::pivot_longer(cols = tmin_19910101:tmin_20210730,
#                       names_to = "date", values_to = "daily.tmin") %>%
#   mutate(date = ymd(str_replace(date, "tmin_", "")))
# 
# ################
# # VPDmax
# ################
# pd.daily.vpdmax <- pd_stack(prism_archive_subset("vpdmax", "daily", 
#                                            minDate = "1991-01-01",
#                                            maxDate = "2021-07-31"))
# 
# sites.daily.vpdmax <- as.data.frame(terra::extract(pd.daily.vpdmax,
#                                                    SpatialPoints(eco.coords),
#                                                    sp = F))
# 
# # Add  site, latitude, longitude data
# sites.daily.vpdmax$latitude = eco.coords$y
# sites.daily.vpdmax$longitude = eco.coords$x
# sites.daily.vpdmax$site = ecosites$site
# 
# # Rename columns to have climate data indicator as prefix with month/year
# # as suffix
# sites.daily.vpdmax <- sites.daily.vpdmax %>%
#   dplyr::select(site, latitude, longitude, everything())
# 
# names(sites.daily.vpdmax) <- c("site", "latitude", "longitude",
#                                str_c("vpdmax_", 
#                                      str_extract(names(sites.daily.vpdmax[4:11172]),
#                                                  "[0-9]{8}")))
# 
# # Pivot to long data format and summarize precipitation by month (1991-2020)
# # to simulate 1991-2020 climate normals
# sites.daily.vpdmax <- sites.daily.vpdmax %>%
#   tidyr::pivot_longer(cols = vpdmax_19910101:vpdmax_20210730,
#                       names_to = "date", values_to = "daily.vpdmax") %>%
#   mutate(date = ymd(str_replace(date, "vpdmax_", "")))
# 
# ################
# # VPDmin
# ################
# pd.daily.vpdmin <- pd_stack(prism_archive_subset("vpdmin", "daily", 
#                                            minDate = "1991-01-01",
#                                            maxDate = "2021-07-31"))
# 
# sites.daily.vpdmin <- as.data.frame(terra::extract(pd.daily.vpdmin,
#                                                    SpatialPoints(eco.coords),
#                                                    sp = F))
# 
# # Add  site, latitude, longitude data
# sites.daily.vpdmin$latitude = eco.coords$y
# sites.daily.vpdmin$longitude = eco.coords$x
# sites.daily.vpdmin$site = ecosites$site
# 
# # Rename columns to have climate data indicator as prefix with month/year
# # as suffix
# sites.daily.vpdmin <- sites.daily.vpdmin %>%
#   dplyr::select(site, latitude, longitude, everything())
# 
# names(sites.daily.vpdmin) <- c("site", "latitude", "longitude",
#                                str_c("vpdmin_", 
#                                      str_extract(names(sites.daily.vpdmin[4:11171]),
#                                                  "[0-9]{8}")))
# 
# # Pivot to long data format and summarize precipitation by month (1991-2020)
# # to simulate 1991-2020 climate normals
# sites.daily.vpdmin <- sites.daily.vpdmin %>%
#   tidyr::pivot_longer(cols = vpdmin_19910101:vpdmin_20210730,
#                       names_to = "date", values_to = "daily.vpdmin") %>%
#   mutate(date = ymd(str_replace(date, "vpdmin_", "")))
# 
# ## Join normals data into single dataset
# daily.clim <- sites.daily.prcp %>%
#   left_join(sites.daily.tmean) %>%
#   left_join(sites.daily.tmax) %>%
#   full_join(sites.daily.tmin) %>%
#   full_join(sites.daily.vpdmax) %>%
#   full_join(sites.daily.vpdmin)
# 
# write.csv(daily.clim, "../climate_data/TXeco_PRISM_daily.csv")

###############################################################################
## Determine sunrise/sunset using 'suncalc' package, then calculate fraction
## of sunlight hours
###############################################################################

daily.clim <- read.csv("../climate_data/TXeco_PRISM_daily.csv") %>%
  mutate(date = lubridate::ymd(date))

## Change latitude/longitude colnames to "lat" and "long" to make easier
## data loading into "getSunlightTimes" function
# names(daily.clim)[2:3] <- c("lat", "lon")

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

###############################################################################
## Calculate water holding capacity of each site
###############################################################################
whc.data <- soilgrids %>%
  mutate(calc_soil_water(id = id, fsand = perc.sand/100,
                         fclay = perc.clay/100,
                         fom = om/1000,
                         fgravel = perc.gravel/100,
                         zbed = bedrock/100),
         wfc = wfc * 1000, 
         pwp = pwp * 1000,
         whc = whc * 1000) %>%
  dplyr::select(site = id, whc)

## Join water holding capacity data with compiled daily climate dataframe
daily.clim <- daily.clim %>%
  full_join(whc.data)

###############################################################################
## Write daily climate .csv
###############################################################################
## Write daily climate .csv
write.csv(daily.clim, "../climate_data/TXeco_PRISM_daily.csv",
          row.names = FALSE)

daily.clim <- read.csv("../climate_data/TXeco_PRISM_daily.csv")

###############################################################################
## SPLASH file prep
###############################################################################
## 1991 splash prep
test.splash1991 <- daily.clim %>%
  filter(year == 1991) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1991 <- lapply(split(test.splash1991, test.splash1991$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1991, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1991/", 
                          names(test.splash1991), sep = ""),
                    "1991splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1992 splash prep
test.splash1992 <- daily.clim %>%
  filter(year == 1992) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1992 <- lapply(split(test.splash1992, test.splash1992$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1992, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1992/", 
                          names(test.splash1992), sep = ""),
                    "1992splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1993 splash prep
test.splash1993 <- daily.clim %>%
  filter(year == 1993) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1993 <- lapply(split(test.splash1993, test.splash1993$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1993, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1993/", 
                          names(test.splash1993), sep = ""),
                    "1993splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1994 splash prep
test.splash1994 <- daily.clim %>%
  filter(year == 1994) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1994 <- lapply(split(test.splash1994, test.splash1994$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1994, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1994/", 
                          names(test.splash1994), sep = ""),
                    "1994splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1995 splash prep
test.splash1995 <- daily.clim %>%
  filter(year == 1995) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1995 <- lapply(split(test.splash1995, test.splash1995$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1995, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1995/", 
                          names(test.splash1995), sep = ""),
                    "1995splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1996 splash prep
test.splash1996 <- daily.clim %>%
  filter(year == 1996) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1996 <- lapply(split(test.splash1996, test.splash1996$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1996, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1996/", 
                          names(test.splash1996), sep = ""),
                    "1996splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1997 splash prep
test.splash1997 <- daily.clim %>%
  filter(year == 1997) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1997 <- lapply(split(test.splash1997, test.splash1997$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1997, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1997/", 
                          names(test.splash1997), sep = ""),
                    "1997splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1998 splash prep
test.splash1998 <- daily.clim %>%
  filter(year == 1998) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1998 <- lapply(split(test.splash1998, test.splash1998$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1998, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1998/", 
                          names(test.splash1998), sep = ""),
                    "1998splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 1999 splash prep
test.splash1999 <- daily.clim %>%
  filter(year == 1999) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash1999 <- lapply(split(test.splash1999, test.splash1999$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash1999, 
       file = paste(paste("../climate_data/splash_prep_files/splash_1999/", 
                          names(test.splash1999), sep = ""),
                    "1999splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2000 splash prep
test.splash2000 <- daily.clim %>%
  filter(year == 2000) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2000 <- lapply(split(test.splash2000, test.splash2000$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2000, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2000/", 
                          names(test.splash2000), sep = ""),
                    "2000splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2001 splash prep
test.splash2001 <- daily.clim %>%
  filter(year == 2001) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2001 <- lapply(split(test.splash2001, test.splash2001$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2001, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2001/", 
                          names(test.splash2001), sep = ""),
                    "2001splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2002 splash prep
test.splash2002 <- daily.clim %>%
  filter(year == 2002) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2002 <- lapply(split(test.splash2002, test.splash2002$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2002, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2002/", 
                          names(test.splash2002), sep = ""),
                    "2002splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2003 splash prep
test.splash2003 <- daily.clim %>%
  filter(year == 2003) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2003 <- lapply(split(test.splash2003, test.splash2003$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2003, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2003/", 
                          names(test.splash2003), sep = ""),
                    "2003splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2004 splash prep
test.splash2004 <- daily.clim %>%
  filter(year == 2004) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2004 <- lapply(split(test.splash2004, test.splash2004$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2004, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2004/", 
                          names(test.splash2004), sep = ""),
                    "2004splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2005 splash prep
test.splash2005 <- daily.clim %>%
  filter(year == 2005) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2005 <- lapply(split(test.splash2005, test.splash2005$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2005, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2005/", 
                          names(test.splash2005), sep = ""),
                    "2005splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2006 splash prep
test.splash2006 <- daily.clim %>%
  filter(year == 2006) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2006 <- lapply(split(test.splash2006, test.splash2006$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2006, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2006/", 
                          names(test.splash2006), sep = ""),
                    "2006splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2007 splash prep
test.splash2007 <- daily.clim %>%
  filter(year == 2007) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2007 <- lapply(split(test.splash2007, test.splash2007$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2007, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2007/", 
                          names(test.splash2007), sep = ""),
                    "2007splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2008 splash prep
test.splash2008 <- daily.clim %>%
  filter(year == 2008) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2008 <- lapply(split(test.splash2008, test.splash2008$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2008, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2008/", 
                          names(test.splash2008), sep = ""),
                    "2008splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2009 splash prep
test.splash2009 <- daily.clim %>%
  filter(year == 2009) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2009 <- lapply(split(test.splash2009, test.splash2009$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2009, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2009/", 
                          names(test.splash2009), sep = ""),
                    "2009splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2010 splash prep
test.splash2010 <- daily.clim %>%
  filter(year == 2010) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2010 <- lapply(split(test.splash2010, test.splash2010$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2010, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2010/", 
                          names(test.splash2010), sep = ""),
                    "2010splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2011 splash prep
test.splash2011 <- daily.clim %>%
  filter(year == 2011) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2011 <- lapply(split(test.splash2011, test.splash2011$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2011, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2011/", 
                          names(test.splash2011), sep = ""),
                    "2011splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2012 splash prep
test.splash2012 <- daily.clim %>%
  filter(year == 2012) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2012 <- lapply(split(test.splash2012, test.splash2012$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2012, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2012/", 
                          names(test.splash2012), sep = ""),
                    "2012splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2013 splash prep
test.splash2013 <- daily.clim %>%
  filter(year == 2013) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2013 <- lapply(split(test.splash2013, test.splash2013$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2013, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2013/", 
                          names(test.splash2013), sep = ""),
                    "2013splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2014 splash prep
test.splash2014 <- daily.clim %>%
  filter(year == 2014) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2014 <- lapply(split(test.splash2014, test.splash2014$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2014, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2014/", 
                          names(test.splash2014), sep = ""),
                    "2014splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2015 splash prep
test.splash2015 <- daily.clim %>%
  filter(year == 2015) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2015 <- lapply(split(test.splash2015, test.splash2015$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2015, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2015/", 
                          names(test.splash2015), sep = ""),
                    "2015splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2016 splash prep
test.splash2016 <- daily.clim %>%
  filter(year == 2016) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2016 <- lapply(split(test.splash2016, test.splash2016$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2016, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2016/", 
                          names(test.splash2016), sep = ""),
                    "2016splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2017 splash prep
test.splash2017 <- daily.clim %>%
  filter(year == 2017) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2017 <- lapply(split(test.splash2017, test.splash2017$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2017, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2017/", 
                          names(test.splash2017), sep = ""),
                    "2017splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2018 splash prep
test.splash2018 <- daily.clim %>%
  filter(year == 2018) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2018 <- lapply(split(test.splash2018, test.splash2018$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2018, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2018/", 
                          names(test.splash2017), sep = ""),
                    "2018splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2019 splash prep
test.splash2019 <- daily.clim %>%
  filter(year == 2019) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2019 <- lapply(split(test.splash2019, test.splash2019$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2019, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2019/", 
                          names(test.splash2019), sep = ""),
                    "2019splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))


## 2020 splash prep
test.splash2020 <- daily.clim %>%
  filter(year == 2020) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2020 <- lapply(split(test.splash2020, test.splash2020$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2020, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2020/", 
                          names(test.splash2020), sep = ""),
                    "2020splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))

## 2021 splash prep
test.splash2021 <- daily.clim %>%
  filter(year == 2021) %>%
  dplyr::select(site, lat_deg = lat, elv_m = elevation.m, date, m, i, year,
                sf, tair = daily.tmean, pn = daily.prcp, kWm = whc)

test.splash2021 <- lapply(split(test.splash2021, test.splash2021$site, 
                                drop = TRUE), as.list)

mapply(write.table,
       x = test.splash2021, 
       file = paste(paste("../climate_data/splash_prep_files/splash_2021/", 
                          names(test.splash2021), sep = ""),
                    "2021splash", "csv", sep="."),
       MoreArgs = list(row.names = FALSE, sep = ","))
