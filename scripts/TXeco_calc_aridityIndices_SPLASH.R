###############################################################################
## Load libraries
###############################################################################
library(splash)
library(dplyr)
library(jsonlite)
library(purrr)
library(data.table)
library(stringr)
library(ggplot2)
library(lubridate)

###############################################################################
## Load site name file with visit date info
###############################################################################
sites <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  dplyr::select(-site) %>%
  dplyr::select(site = property, everything())

###############################################################################
## Run SPLASH model with 1991 data
###############################################################################
file.list.1991 <- list.files("../climate_data/splash_prep_files/splash_1991",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1991 <- setNames(file.list.1991, 
                           str_extract(basename(file.list.1991), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1991 <- lapply(file.list.1991, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1991$Brazos_2020_18.1991splash$sf <- splash.readin.1991$Brazos_2020_18.1991splash$sf[seq_along(splash.readin.1991$Brazos_2020_18.1991splash$sf) %% 2 > 0]
splash.readin.1991$Brazos_2020_18.1991splash$tair <- splash.readin.1991$Brazos_2020_18.1991splash$tair[seq_along(splash.readin.1991$Brazos_2020_18.1991splash$tair) %% 2 > 0]
splash.readin.1991$Brazos_2020_18.1991splash$pn <- splash.readin.1991$Brazos_2020_18.1991splash$pn[seq_along(splash.readin.1991$Brazos_2020_18.1991splash$pn) %% 2 > 0]
splash.readin.1991$Brazos_2020_18.1991splash$num_lines <- 365

# Harris 2020-03
splash.readin.1991$Harris_2020_03.1991splash$sf <- splash.readin.1991$Harris_2020_03.1991splash$sf[seq_along(splash.readin.1991$Harris_2020_03.1991splash$sf) %% 2 > 0]
splash.readin.1991$Harris_2020_03.1991splash$tair <- splash.readin.1991$Harris_2020_03.1991splash$tair[seq_along(splash.readin.1991$Harris_2020_03.1991splash$tair) %% 2 > 0]
splash.readin.1991$Harris_2020_03.1991splash$pn <- splash.readin.1991$Harris_2020_03.1991splash$pn[seq_along(splash.readin.1991$Harris_2020_03.1991splash$pn) %% 2 > 0]
splash.readin.1991$Harris_2020_03.1991splash$num_lines <- 365

# Menard 2020-01
splash.readin.1991$Menard_2020_01.1991splash$sf <- splash.readin.1991$Menard_2020_01.1991splash$sf[seq_along(splash.readin.1991$Menard_2020_01.1991splash$sf) %% 2 > 0]
splash.readin.1991$Menard_2020_01.1991splash$tair <- splash.readin.1991$Menard_2020_01.1991splash$tair[seq_along(splash.readin.1991$Menard_2020_01.1991splash$tair) %% 2 > 0]
splash.readin.1991$Menard_2020_01.1991splash$pn <- splash.readin.1991$Menard_2020_01.1991splash$pn[seq_along(splash.readin.1991$Menard_2020_01.1991splash$pn) %% 2 > 0]
splash.readin.1991$Menard_2020_01.1991splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1991$Uvalde_2020_02.1991splash$sf <- splash.readin.1991$Uvalde_2020_02.1991splash$sf[seq_along(splash.readin.1991$Uvalde_2020_02.1991splash$sf) %% 2 > 0]
splash.readin.1991$Uvalde_2020_02.1991splash$tair <- splash.readin.1991$Uvalde_2020_02.1991splash$tair[seq_along(splash.readin.1991$Uvalde_2020_02.1991splash$tair) %% 2 > 0]
splash.readin.1991$Uvalde_2020_02.1991splash$pn <- splash.readin.1991$Uvalde_2020_02.1991splash$pn[seq_along(splash.readin.1991$Uvalde_2020_02.1991splash$pn) %% 2 > 0]
splash.readin.1991$Uvalde_2020_02.1991splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1991 <- lapply(file.list.1991, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1991[["Brazos_2020_18.1991splash"]] <- splash.month.1991[["Brazos_2020_18.1991splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1991[["Harris_2020_03.1991splash"]] <- splash.month.1991[["Harris_2020_03.1991splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1991[["Menard_2020_01.1991splash"]] <- splash.month.1991[["Menard_2020_01.1991splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1991[["Uvalde_2020_02.1991splash"]] <- splash.month.1991[["Uvalde_2020_02.1991splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1991)) {
  splash.month.1991[[i]] <- splash.month.1991[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1991 <- lapply(file.list.1991, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1991 <- mapply(c, splash.readin.1991, splash.month.1991, 
                            splash.ancillary.1991, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1991)) {
  
  splash.total.1991[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1991[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1991[[i]][["num_lines"]])
  splash.total.1991[[i]][["daily_totals"]] <- as.data.frame(splash.total.1991[[i]][["daily_totals"]])
  names(splash.total.1991[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1991[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1991[[i]],
                                                      dtot = splash.total.1991[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1991 <- map(splash.total.1991, as.data.table)
splash.water.1991 <- rbindlist(splash.water.1991, fill = TRUE, idcol = TRUE)
splash.water.1991$year <- 1991
splash.water.1991$site <- str_extract(splash.water.1991$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1991 <- splash.water.1991 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1991 <- tidyr::unite(splash.water.1991, "date", year:day, sep = "-", remove = FALSE)
splash.water.1991$date <- lubridate::ymd(as.Date(splash.water.1991$date))
splash.water.1991$doy <- lubridate::yday(splash.water.1991$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1991 <- run_one_day(lat = splash.water.1991$lat_deg,
                                  elv = splash.water.1991$elv_m,
                                  n = splash.water.1991$doy,
                                  y = splash.water.1991$year,
                                  wn = splash.water.1991$soil.moisture,
                                  sf = splash.water.1991$sf,
                                  tc = splash.water.1991$tair,
                                  pn = splash.water.1991$pn,
                                  kWm = 150)

splash.oneday.1991 <- map(splash.oneday.1991, as.data.table)

## Change list column names
names(splash.oneday.1991[[1]]) <- "ho"
names(splash.oneday.1991[[2]]) <- "hn"
names(splash.oneday.1991[[3]]) <- "ppfd"
names(splash.oneday.1991[[4]]) <- "cond"
names(splash.oneday.1991[[5]]) <- "eet"
names(splash.oneday.1991[[6]]) <- "pet"
names(splash.oneday.1991[[7]]) <- "aet"
names(splash.oneday.1991[[8]]) <- "wn"
names(splash.oneday.1991[[9]]) <- "ro"

sites.daily.1991 <- splash.oneday.1991$ho %>%
  coalesce(splash.oneday.1991$hn) %>%
  coalesce(splash.oneday.1991$ppfd) %>%
  coalesce(splash.oneday.1991$cond) %>%
  coalesce(splash.oneday.1991$eet) %>%
  coalesce(splash.oneday.1991$pet) %>%
  coalesce(splash.oneday.1991$aet) %>%
  coalesce(splash.oneday.1991$wn) %>%
  coalesce(splash.oneday.1991$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1991) %>% 
  dplyr::select(site:soil.moisture, ho:aet)


###############################################################################
## Run SPLASH model with 1992 data
###############################################################################
file.list.1992 <- list.files("../climate_data/splash_prep_files/splash_1992",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1992 <- setNames(file.list.1992, 
                           str_extract(basename(file.list.1992), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1992 <- lapply(file.list.1992, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1992$Brazos_2020_18.1992splash$sf <- splash.readin.1992$Brazos_2020_18.1992splash$sf[seq_along(splash.readin.1992$Brazos_2020_18.1992splash$sf) %% 2 > 0]
splash.readin.1992$Brazos_2020_18.1992splash$tair <- splash.readin.1992$Brazos_2020_18.1992splash$tair[seq_along(splash.readin.1992$Brazos_2020_18.1992splash$tair) %% 2 > 0]
splash.readin.1992$Brazos_2020_18.1992splash$pn <- splash.readin.1992$Brazos_2020_18.1992splash$pn[seq_along(splash.readin.1992$Brazos_2020_18.1992splash$pn) %% 2 > 0]
splash.readin.1992$Brazos_2020_18.1992splash$num_lines <- 366

# Harris 2020-03
splash.readin.1992$Harris_2020_03.1992splash$sf <- splash.readin.1992$Harris_2020_03.1992splash$sf[seq_along(splash.readin.1992$Harris_2020_03.1992splash$sf) %% 2 > 0]
splash.readin.1992$Harris_2020_03.1992splash$tair <- splash.readin.1992$Harris_2020_03.1992splash$tair[seq_along(splash.readin.1992$Harris_2020_03.1992splash$tair) %% 2 > 0]
splash.readin.1992$Harris_2020_03.1992splash$pn <- splash.readin.1992$Harris_2020_03.1992splash$pn[seq_along(splash.readin.1992$Harris_2020_03.1992splash$pn) %% 2 > 0]
splash.readin.1992$Harris_2020_03.1992splash$num_lines <- 366

# Menard 2020-01
splash.readin.1992$Menard_2020_01.1992splash$sf <- splash.readin.1992$Menard_2020_01.1992splash$sf[seq_along(splash.readin.1992$Menard_2020_01.1992splash$sf) %% 2 > 0]
splash.readin.1992$Menard_2020_01.1992splash$tair <- splash.readin.1992$Menard_2020_01.1992splash$tair[seq_along(splash.readin.1992$Menard_2020_01.1992splash$tair) %% 2 > 0]
splash.readin.1992$Menard_2020_01.1992splash$pn <- splash.readin.1992$Menard_2020_01.1992splash$pn[seq_along(splash.readin.1992$Menard_2020_01.1992splash$pn) %% 2 > 0]
splash.readin.1992$Menard_2020_01.1992splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.1992$Uvalde_2020_02.1992splash$sf <- splash.readin.1992$Uvalde_2020_02.1992splash$sf[seq_along(splash.readin.1992$Uvalde_2020_02.1992splash$sf) %% 2 > 0]
splash.readin.1992$Uvalde_2020_02.1992splash$tair <- splash.readin.1992$Uvalde_2020_02.1992splash$tair[seq_along(splash.readin.1992$Uvalde_2020_02.1992splash$tair) %% 2 > 0]
splash.readin.1992$Uvalde_2020_02.1992splash$pn <- splash.readin.1992$Uvalde_2020_02.1992splash$pn[seq_along(splash.readin.1992$Uvalde_2020_02.1992splash$pn) %% 2 > 0]
splash.readin.1992$Uvalde_2020_02.1992splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1992 <- lapply(file.list.1992, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1992[["Brazos_2020_18.1992splash"]] <- splash.month.1992[["Brazos_2020_18.1992splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1992[["Harris_2020_03.1992splash"]] <- splash.month.1992[["Harris_2020_03.1992splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1992[["Menard_2020_01.1992splash"]] <- splash.month.1992[["Menard_2020_01.1992splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1992[["Uvalde_2020_02.1992splash"]] <- splash.month.1992[["Uvalde_2020_02.1992splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1992)) {
  splash.month.1992[[i]] <- splash.month.1992[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1992 <- lapply(file.list.1992, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1992 <- mapply(c, splash.readin.1992, splash.month.1992, 
                            splash.ancillary.1992, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1992)) {
  
  splash.total.1992[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1992[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1992[[i]][["num_lines"]])
  splash.total.1992[[i]][["daily_totals"]] <- as.data.frame(splash.total.1992[[i]][["daily_totals"]])
  names(splash.total.1992[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1992[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1992[[i]],
                                                      dtot = splash.total.1992[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1992 <- map(splash.total.1992, as.data.table)
splash.water.1992 <- rbindlist(splash.water.1992, fill = TRUE, idcol = TRUE)
splash.water.1992$year <- 1992
splash.water.1992$site <- str_extract(splash.water.1992$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1992 <- splash.water.1992 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1992 <- tidyr::unite(splash.water.1992, "date", year:day, sep = "-", remove = FALSE)
splash.water.1992$date <- lubridate::ymd(as.Date(splash.water.1992$date))
splash.water.1992$doy <- lubridate::yday(splash.water.1992$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1992 <- run_one_day(lat = splash.water.1992$lat_deg,
                                  elv = splash.water.1992$elv_m,
                                  n = splash.water.1992$doy,
                                  y = splash.water.1992$year,
                                  wn = splash.water.1992$soil.moisture,
                                  sf = splash.water.1992$sf,
                                  tc = splash.water.1992$tair,
                                  pn = splash.water.1992$pn,
                                  kWm = 150)

splash.oneday.1992 <- map(splash.oneday.1992, as.data.table)

## Change list column names
names(splash.oneday.1992[[1]]) <- "ho"
names(splash.oneday.1992[[2]]) <- "hn"
names(splash.oneday.1992[[3]]) <- "ppfd"
names(splash.oneday.1992[[4]]) <- "cond"
names(splash.oneday.1992[[5]]) <- "eet"
names(splash.oneday.1992[[6]]) <- "pet"
names(splash.oneday.1992[[7]]) <- "aet"
names(splash.oneday.1992[[8]]) <- "wn"
names(splash.oneday.1992[[9]]) <- "ro"

sites.daily.1992 <- splash.oneday.1992$ho %>%
  coalesce(splash.oneday.1992$hn) %>%
  coalesce(splash.oneday.1992$ppfd) %>%
  coalesce(splash.oneday.1992$cond) %>%
  coalesce(splash.oneday.1992$eet) %>%
  coalesce(splash.oneday.1992$pet) %>%
  coalesce(splash.oneday.1992$aet) %>%
  coalesce(splash.oneday.1992$wn) %>%
  coalesce(splash.oneday.1992$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1992) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1993 data
###############################################################################
file.list.1993 <- list.files("../climate_data/splash_prep_files/splash_1993",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1993 <- setNames(file.list.1993, 
                           str_extract(basename(file.list.1993), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1993 <- lapply(file.list.1993, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1993$Brazos_2020_18.1993splash$sf <- splash.readin.1993$Brazos_2020_18.1993splash$sf[seq_along(splash.readin.1993$Brazos_2020_18.1993splash$sf) %% 2 > 0]
splash.readin.1993$Brazos_2020_18.1993splash$tair <- splash.readin.1993$Brazos_2020_18.1993splash$tair[seq_along(splash.readin.1993$Brazos_2020_18.1993splash$tair) %% 2 > 0]
splash.readin.1993$Brazos_2020_18.1993splash$pn <- splash.readin.1993$Brazos_2020_18.1993splash$pn[seq_along(splash.readin.1993$Brazos_2020_18.1993splash$pn) %% 2 > 0]
splash.readin.1993$Brazos_2020_18.1993splash$num_lines <- 365

# Harris 2020-03
splash.readin.1993$Harris_2020_03.1993splash$sf <- splash.readin.1993$Harris_2020_03.1993splash$sf[seq_along(splash.readin.1993$Harris_2020_03.1993splash$sf) %% 2 > 0]
splash.readin.1993$Harris_2020_03.1993splash$tair <- splash.readin.1993$Harris_2020_03.1993splash$tair[seq_along(splash.readin.1993$Harris_2020_03.1993splash$tair) %% 2 > 0]
splash.readin.1993$Harris_2020_03.1993splash$pn <- splash.readin.1993$Harris_2020_03.1993splash$pn[seq_along(splash.readin.1993$Harris_2020_03.1993splash$pn) %% 2 > 0]
splash.readin.1993$Harris_2020_03.1993splash$num_lines <- 365

# Menard 2020-01
splash.readin.1993$Menard_2020_01.1993splash$sf <- splash.readin.1993$Menard_2020_01.1993splash$sf[seq_along(splash.readin.1993$Menard_2020_01.1993splash$sf) %% 2 > 0]
splash.readin.1993$Menard_2020_01.1993splash$tair <- splash.readin.1993$Menard_2020_01.1993splash$tair[seq_along(splash.readin.1993$Menard_2020_01.1993splash$tair) %% 2 > 0]
splash.readin.1993$Menard_2020_01.1993splash$pn <- splash.readin.1993$Menard_2020_01.1993splash$pn[seq_along(splash.readin.1993$Menard_2020_01.1993splash$pn) %% 2 > 0]
splash.readin.1993$Menard_2020_01.1993splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1993$Uvalde_2020_02.1993splash$sf <- splash.readin.1993$Uvalde_2020_02.1993splash$sf[seq_along(splash.readin.1993$Uvalde_2020_02.1993splash$sf) %% 2 > 0]
splash.readin.1993$Uvalde_2020_02.1993splash$tair <- splash.readin.1993$Uvalde_2020_02.1993splash$tair[seq_along(splash.readin.1993$Uvalde_2020_02.1993splash$tair) %% 2 > 0]
splash.readin.1993$Uvalde_2020_02.1993splash$pn <- splash.readin.1993$Uvalde_2020_02.1993splash$pn[seq_along(splash.readin.1993$Uvalde_2020_02.1993splash$pn) %% 2 > 0]
splash.readin.1993$Uvalde_2020_02.1993splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1993 <- lapply(file.list.1993, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1993[["Brazos_2020_18.1993splash"]] <- splash.month.1993[["Brazos_2020_18.1993splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1993[["Harris_2020_03.1993splash"]] <- splash.month.1993[["Harris_2020_03.1993splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1993[["Menard_2020_01.1993splash"]] <- splash.month.1993[["Menard_2020_01.1993splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1993[["Uvalde_2020_02.1993splash"]] <- splash.month.1993[["Uvalde_2020_02.1993splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1993)) {
  splash.month.1993[[i]] <- splash.month.1993[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1993 <- lapply(file.list.1993, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1993 <- mapply(c, splash.readin.1993, splash.month.1993, 
                            splash.ancillary.1993, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1993)) {
  
  splash.total.1993[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1993[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1993[[i]][["num_lines"]])
  splash.total.1993[[i]][["daily_totals"]] <- as.data.frame(splash.total.1993[[i]][["daily_totals"]])
  names(splash.total.1993[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1993[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1993[[i]],
                                                      dtot = splash.total.1993[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1993 <- map(splash.total.1993, as.data.table)
splash.water.1993 <- rbindlist(splash.water.1993, fill = TRUE, idcol = TRUE)
splash.water.1993$year <- 1993
splash.water.1993$site <- str_extract(splash.water.1993$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1993 <- splash.water.1993 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1993 <- tidyr::unite(splash.water.1993, "date", year:day, sep = "-", remove = FALSE)
splash.water.1993$date <- lubridate::ymd(as.Date(splash.water.1993$date))
splash.water.1993$doy <- lubridate::yday(splash.water.1993$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1993 <- run_one_day(lat = splash.water.1993$lat_deg,
                                  elv = splash.water.1993$elv_m,
                                  n = splash.water.1993$doy,
                                  y = splash.water.1993$year,
                                  wn = splash.water.1993$soil.moisture,
                                  sf = splash.water.1993$sf,
                                  tc = splash.water.1993$tair,
                                  pn = splash.water.1993$pn,
                                  kWm = 150)

splash.oneday.1993 <- map(splash.oneday.1993, as.data.table)

## Change list column names
names(splash.oneday.1993[[1]]) <- "ho"
names(splash.oneday.1993[[2]]) <- "hn"
names(splash.oneday.1993[[3]]) <- "ppfd"
names(splash.oneday.1993[[4]]) <- "cond"
names(splash.oneday.1993[[5]]) <- "eet"
names(splash.oneday.1993[[6]]) <- "pet"
names(splash.oneday.1993[[7]]) <- "aet"
names(splash.oneday.1993[[8]]) <- "wn"
names(splash.oneday.1993[[9]]) <- "ro"

sites.daily.1993 <- splash.oneday.1993$ho %>%
  coalesce(splash.oneday.1993$hn) %>%
  coalesce(splash.oneday.1993$ppfd) %>%
  coalesce(splash.oneday.1993$cond) %>%
  coalesce(splash.oneday.1993$eet) %>%
  coalesce(splash.oneday.1993$pet) %>%
  coalesce(splash.oneday.1993$aet) %>%
  coalesce(splash.oneday.1993$wn) %>%
  coalesce(splash.oneday.1993$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1993) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1994 data
###############################################################################
file.list.1994 <- list.files("../climate_data/splash_prep_files/splash_1994",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1994 <- setNames(file.list.1994, 
                           str_extract(basename(file.list.1994), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1994 <- lapply(file.list.1994, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1994$Brazos_2020_18.1994splash$sf <- splash.readin.1994$Brazos_2020_18.1994splash$sf[seq_along(splash.readin.1994$Brazos_2020_18.1994splash$sf) %% 2 > 0]
splash.readin.1994$Brazos_2020_18.1994splash$tair <- splash.readin.1994$Brazos_2020_18.1994splash$tair[seq_along(splash.readin.1994$Brazos_2020_18.1994splash$tair) %% 2 > 0]
splash.readin.1994$Brazos_2020_18.1994splash$pn <- splash.readin.1994$Brazos_2020_18.1994splash$pn[seq_along(splash.readin.1994$Brazos_2020_18.1994splash$pn) %% 2 > 0]
splash.readin.1994$Brazos_2020_18.1994splash$num_lines <- 365

# Harris 2020-03
splash.readin.1994$Harris_2020_03.1994splash$sf <- splash.readin.1994$Harris_2020_03.1994splash$sf[seq_along(splash.readin.1994$Harris_2020_03.1994splash$sf) %% 2 > 0]
splash.readin.1994$Harris_2020_03.1994splash$tair <- splash.readin.1994$Harris_2020_03.1994splash$tair[seq_along(splash.readin.1994$Harris_2020_03.1994splash$tair) %% 2 > 0]
splash.readin.1994$Harris_2020_03.1994splash$pn <- splash.readin.1994$Harris_2020_03.1994splash$pn[seq_along(splash.readin.1994$Harris_2020_03.1994splash$pn) %% 2 > 0]
splash.readin.1994$Harris_2020_03.1994splash$num_lines <- 365

# Menard 2020-01
splash.readin.1994$Menard_2020_01.1994splash$sf <- splash.readin.1994$Menard_2020_01.1994splash$sf[seq_along(splash.readin.1994$Menard_2020_01.1994splash$sf) %% 2 > 0]
splash.readin.1994$Menard_2020_01.1994splash$tair <- splash.readin.1994$Menard_2020_01.1994splash$tair[seq_along(splash.readin.1994$Menard_2020_01.1994splash$tair) %% 2 > 0]
splash.readin.1994$Menard_2020_01.1994splash$pn <- splash.readin.1994$Menard_2020_01.1994splash$pn[seq_along(splash.readin.1994$Menard_2020_01.1994splash$pn) %% 2 > 0]
splash.readin.1994$Menard_2020_01.1994splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1994$Uvalde_2020_02.1994splash$sf <- splash.readin.1994$Uvalde_2020_02.1994splash$sf[seq_along(splash.readin.1994$Uvalde_2020_02.1994splash$sf) %% 2 > 0]
splash.readin.1994$Uvalde_2020_02.1994splash$tair <- splash.readin.1994$Uvalde_2020_02.1994splash$tair[seq_along(splash.readin.1994$Uvalde_2020_02.1994splash$tair) %% 2 > 0]
splash.readin.1994$Uvalde_2020_02.1994splash$pn <- splash.readin.1994$Uvalde_2020_02.1994splash$pn[seq_along(splash.readin.1994$Uvalde_2020_02.1994splash$pn) %% 2 > 0]
splash.readin.1994$Uvalde_2020_02.1994splash$num_lines <- 365

## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1994 <- lapply(file.list.1994, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1994[["Brazos_2020_18.1994splash"]] <- splash.month.1994[["Brazos_2020_18.1994splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1994[["Harris_2020_03.1994splash"]] <- splash.month.1994[["Harris_2020_03.1994splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1994[["Menard_2020_01.1994splash"]] <- splash.month.1994[["Menard_2020_01.1994splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1994[["Uvalde_2020_02.1994splash"]] <- splash.month.1994[["Uvalde_2020_02.1994splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1994)) {
  splash.month.1994[[i]] <- splash.month.1994[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1994 <- lapply(file.list.1994, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1994 <- mapply(c, splash.readin.1994, splash.month.1994, 
                            splash.ancillary.1994, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1994)) {
  
  splash.total.1994[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1994[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1994[[i]][["num_lines"]])
  splash.total.1994[[i]][["daily_totals"]] <- as.data.frame(splash.total.1994[[i]][["daily_totals"]])
  names(splash.total.1994[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1994[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1994[[i]],
                                                      dtot = splash.total.1994[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1994 <- map(splash.total.1994, as.data.table)
splash.water.1994 <- rbindlist(splash.water.1994, fill = TRUE, idcol = TRUE)
splash.water.1994$year <- 1994
splash.water.1994$site <- str_extract(splash.water.1994$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1994 <- splash.water.1994 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1994 <- tidyr::unite(splash.water.1994, "date", year:day, sep = "-", remove = FALSE)
splash.water.1994$date <- lubridate::ymd(as.Date(splash.water.1994$date))
splash.water.1994$doy <- lubridate::yday(splash.water.1994$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1994 <- run_one_day(lat = splash.water.1994$lat_deg,
                                  elv = splash.water.1994$elv_m,
                                  n = splash.water.1994$doy,
                                  y = splash.water.1994$year,
                                  wn = splash.water.1994$soil.moisture,
                                  sf = splash.water.1994$sf,
                                  tc = splash.water.1994$tair,
                                  pn = splash.water.1994$pn,
                                  kWm = 150)

splash.oneday.1994 <- map(splash.oneday.1994, as.data.table)

## Change list column names
names(splash.oneday.1994[[1]]) <- "ho"
names(splash.oneday.1994[[2]]) <- "hn"
names(splash.oneday.1994[[3]]) <- "ppfd"
names(splash.oneday.1994[[4]]) <- "cond"
names(splash.oneday.1994[[5]]) <- "eet"
names(splash.oneday.1994[[6]]) <- "pet"
names(splash.oneday.1994[[7]]) <- "aet"
names(splash.oneday.1994[[8]]) <- "wn"
names(splash.oneday.1994[[9]]) <- "ro"

sites.daily.1994 <- splash.oneday.1994$ho %>%
  coalesce(splash.oneday.1994$hn) %>%
  coalesce(splash.oneday.1994$ppfd) %>%
  coalesce(splash.oneday.1994$cond) %>%
  coalesce(splash.oneday.1994$eet) %>%
  coalesce(splash.oneday.1994$pet) %>%
  coalesce(splash.oneday.1994$aet) %>%
  coalesce(splash.oneday.1994$wn) %>%
  coalesce(splash.oneday.1994$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1994) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1995 data
###############################################################################
file.list.1995 <- list.files("../climate_data/splash_prep_files/splash_1995",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1995 <- setNames(file.list.1995, 
                           str_extract(basename(file.list.1995), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1995 <- lapply(file.list.1995, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1995$Brazos_2020_18.1995splash$sf <- splash.readin.1995$Brazos_2020_18.1995splash$sf[seq_along(splash.readin.1995$Brazos_2020_18.1995splash$sf) %% 2 > 0]
splash.readin.1995$Brazos_2020_18.1995splash$tair <- splash.readin.1995$Brazos_2020_18.1995splash$tair[seq_along(splash.readin.1995$Brazos_2020_18.1995splash$tair) %% 2 > 0]
splash.readin.1995$Brazos_2020_18.1995splash$pn <- splash.readin.1995$Brazos_2020_18.1995splash$pn[seq_along(splash.readin.1995$Brazos_2020_18.1995splash$pn) %% 2 > 0]
splash.readin.1995$Brazos_2020_18.1995splash$num_lines <- 365

# Harris 2020-03
splash.readin.1995$Harris_2020_03.1995splash$sf <- splash.readin.1995$Harris_2020_03.1995splash$sf[seq_along(splash.readin.1995$Harris_2020_03.1995splash$sf) %% 2 > 0]
splash.readin.1995$Harris_2020_03.1995splash$tair <- splash.readin.1995$Harris_2020_03.1995splash$tair[seq_along(splash.readin.1995$Harris_2020_03.1995splash$tair) %% 2 > 0]
splash.readin.1995$Harris_2020_03.1995splash$pn <- splash.readin.1995$Harris_2020_03.1995splash$pn[seq_along(splash.readin.1995$Harris_2020_03.1995splash$pn) %% 2 > 0]
splash.readin.1995$Harris_2020_03.1995splash$num_lines <- 365

# Menard 2020-01
splash.readin.1995$Menard_2020_01.1995splash$sf <- splash.readin.1995$Menard_2020_01.1995splash$sf[seq_along(splash.readin.1995$Menard_2020_01.1995splash$sf) %% 2 > 0]
splash.readin.1995$Menard_2020_01.1995splash$tair <- splash.readin.1995$Menard_2020_01.1995splash$tair[seq_along(splash.readin.1995$Menard_2020_01.1995splash$tair) %% 2 > 0]
splash.readin.1995$Menard_2020_01.1995splash$pn <- splash.readin.1995$Menard_2020_01.1995splash$pn[seq_along(splash.readin.1995$Menard_2020_01.1995splash$pn) %% 2 > 0]
splash.readin.1995$Menard_2020_01.1995splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1995$Uvalde_2020_02.1995splash$sf <- splash.readin.1995$Uvalde_2020_02.1995splash$sf[seq_along(splash.readin.1995$Uvalde_2020_02.1995splash$sf) %% 2 > 0]
splash.readin.1995$Uvalde_2020_02.1995splash$tair <- splash.readin.1995$Uvalde_2020_02.1995splash$tair[seq_along(splash.readin.1995$Uvalde_2020_02.1995splash$tair) %% 2 > 0]
splash.readin.1995$Uvalde_2020_02.1995splash$pn <- splash.readin.1995$Uvalde_2020_02.1995splash$pn[seq_along(splash.readin.1995$Uvalde_2020_02.1995splash$pn) %% 2 > 0]
splash.readin.1995$Uvalde_2020_02.1995splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1995 <- lapply(file.list.1995, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1995[["Brazos_2020_18.1995splash"]] <- splash.month.1995[["Brazos_2020_18.1995splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1995[["Harris_2020_03.1995splash"]] <- splash.month.1995[["Harris_2020_03.1995splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1995[["Menard_2020_01.1995splash"]] <- splash.month.1995[["Menard_2020_01.1995splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1995[["Uvalde_2020_02.1995splash"]] <- splash.month.1995[["Uvalde_2020_02.1995splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1995)) {
  splash.month.1995[[i]] <- splash.month.1995[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1995 <- lapply(file.list.1995, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1995 <- mapply(c, splash.readin.1995, splash.month.1995, 
                            splash.ancillary.1995, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1995)) {
  
  splash.total.1995[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1995[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1995[[i]][["num_lines"]])
  splash.total.1995[[i]][["daily_totals"]] <- as.data.frame(splash.total.1995[[i]][["daily_totals"]])
  names(splash.total.1995[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1995[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1995[[i]],
                                                      dtot = splash.total.1995[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1995 <- map(splash.total.1995, as.data.table)
splash.water.1995 <- rbindlist(splash.water.1995, fill = TRUE, idcol = TRUE)
splash.water.1995$year <- 1995
splash.water.1995$site <- str_extract(splash.water.1995$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1995 <- splash.water.1995 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1995 <- tidyr::unite(splash.water.1995, "date", year:day, sep = "-", remove = FALSE)
splash.water.1995$date <- lubridate::ymd(as.Date(splash.water.1995$date))
splash.water.1995$doy <- lubridate::yday(splash.water.1995$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1995 <- run_one_day(lat = splash.water.1995$lat_deg,
                                  elv = splash.water.1995$elv_m,
                                  n = splash.water.1995$doy,
                                  y = splash.water.1995$year,
                                  wn = splash.water.1995$soil.moisture,
                                  sf = splash.water.1995$sf,
                                  tc = splash.water.1995$tair,
                                  pn = splash.water.1995$pn,
                                  kWm = 150)

splash.oneday.1995 <- map(splash.oneday.1995, as.data.table)

## Change list column names
names(splash.oneday.1995[[1]]) <- "ho"
names(splash.oneday.1995[[2]]) <- "hn"
names(splash.oneday.1995[[3]]) <- "ppfd"
names(splash.oneday.1995[[4]]) <- "cond"
names(splash.oneday.1995[[5]]) <- "eet"
names(splash.oneday.1995[[6]]) <- "pet"
names(splash.oneday.1995[[7]]) <- "aet"
names(splash.oneday.1995[[8]]) <- "wn"
names(splash.oneday.1995[[9]]) <- "ro"

sites.daily.1995 <- splash.oneday.1995$ho %>%
  coalesce(splash.oneday.1995$hn) %>%
  coalesce(splash.oneday.1995$ppfd) %>%
  coalesce(splash.oneday.1995$cond) %>%
  coalesce(splash.oneday.1995$eet) %>%
  coalesce(splash.oneday.1995$pet) %>%
  coalesce(splash.oneday.1995$aet) %>%
  coalesce(splash.oneday.1995$wn) %>%
  coalesce(splash.oneday.1995$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1995) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1996 data
###############################################################################
file.list.1996 <- list.files("../climate_data/splash_prep_files/splash_1996",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1996 <- setNames(file.list.1996, 
                           str_extract(basename(file.list.1996), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1996 <- lapply(file.list.1996, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1996$Brazos_2020_18.1996splash$sf <- splash.readin.1996$Brazos_2020_18.1996splash$sf[seq_along(splash.readin.1996$Brazos_2020_18.1996splash$sf) %% 2 > 0]
splash.readin.1996$Brazos_2020_18.1996splash$tair <- splash.readin.1996$Brazos_2020_18.1996splash$tair[seq_along(splash.readin.1996$Brazos_2020_18.1996splash$tair) %% 2 > 0]
splash.readin.1996$Brazos_2020_18.1996splash$pn <- splash.readin.1996$Brazos_2020_18.1996splash$pn[seq_along(splash.readin.1996$Brazos_2020_18.1996splash$pn) %% 2 > 0]
splash.readin.1996$Brazos_2020_18.1996splash$num_lines <- 366

# Harris 2020-03
splash.readin.1996$Harris_2020_03.1996splash$sf <- splash.readin.1996$Harris_2020_03.1996splash$sf[seq_along(splash.readin.1996$Harris_2020_03.1996splash$sf) %% 2 > 0]
splash.readin.1996$Harris_2020_03.1996splash$tair <- splash.readin.1996$Harris_2020_03.1996splash$tair[seq_along(splash.readin.1996$Harris_2020_03.1996splash$tair) %% 2 > 0]
splash.readin.1996$Harris_2020_03.1996splash$pn <- splash.readin.1996$Harris_2020_03.1996splash$pn[seq_along(splash.readin.1996$Harris_2020_03.1996splash$pn) %% 2 > 0]
splash.readin.1996$Harris_2020_03.1996splash$num_lines <- 366

# Menard 2020-01
splash.readin.1996$Menard_2020_01.1996splash$sf <- splash.readin.1996$Menard_2020_01.1996splash$sf[seq_along(splash.readin.1996$Menard_2020_01.1996splash$sf) %% 2 > 0]
splash.readin.1996$Menard_2020_01.1996splash$tair <- splash.readin.1996$Menard_2020_01.1996splash$tair[seq_along(splash.readin.1996$Menard_2020_01.1996splash$tair) %% 2 > 0]
splash.readin.1996$Menard_2020_01.1996splash$pn <- splash.readin.1996$Menard_2020_01.1996splash$pn[seq_along(splash.readin.1996$Menard_2020_01.1996splash$pn) %% 2 > 0]
splash.readin.1996$Menard_2020_01.1996splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.1996$Uvalde_2020_02.1996splash$sf <- splash.readin.1996$Uvalde_2020_02.1996splash$sf[seq_along(splash.readin.1996$Uvalde_2020_02.1996splash$sf) %% 2 > 0]
splash.readin.1996$Uvalde_2020_02.1996splash$tair <- splash.readin.1996$Uvalde_2020_02.1996splash$tair[seq_along(splash.readin.1996$Uvalde_2020_02.1996splash$tair) %% 2 > 0]
splash.readin.1996$Uvalde_2020_02.1996splash$pn <- splash.readin.1996$Uvalde_2020_02.1996splash$pn[seq_along(splash.readin.1996$Uvalde_2020_02.1996splash$pn) %% 2 > 0]
splash.readin.1996$Uvalde_2020_02.1996splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1996 <- lapply(file.list.1996, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1996[["Brazos_2020_18.1996splash"]] <- splash.month.1996[["Brazos_2020_18.1996splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1996[["Harris_2020_03.1996splash"]] <- splash.month.1996[["Harris_2020_03.1996splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1996[["Menard_2020_01.1996splash"]] <- splash.month.1996[["Menard_2020_01.1996splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1996[["Uvalde_2020_02.1996splash"]] <- splash.month.1996[["Uvalde_2020_02.1996splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1996)) {
  splash.month.1996[[i]] <- splash.month.1996[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1996 <- lapply(file.list.1996, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1996 <- mapply(c, splash.readin.1996, splash.month.1996, 
                            splash.ancillary.1996, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1996)) {
  
  splash.total.1996[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1996[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1996[[i]][["num_lines"]])
  splash.total.1996[[i]][["daily_totals"]] <- as.data.frame(splash.total.1996[[i]][["daily_totals"]])
  names(splash.total.1996[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1996[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1996[[i]],
                                                      dtot = splash.total.1996[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1996 <- map(splash.total.1996, as.data.table)
splash.water.1996 <- rbindlist(splash.water.1996, fill = TRUE, idcol = TRUE)
splash.water.1996$year <- 1996
splash.water.1996$site <- str_extract(splash.water.1996$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1996 <- splash.water.1996 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1996 <- tidyr::unite(splash.water.1996, "date", year:day, sep = "-", remove = FALSE)
splash.water.1996$date <- lubridate::ymd(as.Date(splash.water.1996$date))
splash.water.1996$doy <- lubridate::yday(splash.water.1996$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1996 <- run_one_day(lat = splash.water.1996$lat_deg,
                                  elv = splash.water.1996$elv_m,
                                  n = splash.water.1996$doy,
                                  y = splash.water.1996$year,
                                  wn = splash.water.1996$soil.moisture,
                                  sf = splash.water.1996$sf,
                                  tc = splash.water.1996$tair,
                                  pn = splash.water.1996$pn,
                                  kWm = 150)

splash.oneday.1996 <- map(splash.oneday.1996, as.data.table)

## Change list column names
names(splash.oneday.1996[[1]]) <- "ho"
names(splash.oneday.1996[[2]]) <- "hn"
names(splash.oneday.1996[[3]]) <- "ppfd"
names(splash.oneday.1996[[4]]) <- "cond"
names(splash.oneday.1996[[5]]) <- "eet"
names(splash.oneday.1996[[6]]) <- "pet"
names(splash.oneday.1996[[7]]) <- "aet"
names(splash.oneday.1996[[8]]) <- "wn"
names(splash.oneday.1996[[9]]) <- "ro"

sites.daily.1996 <- splash.oneday.1996$ho %>%
  coalesce(splash.oneday.1996$hn) %>%
  coalesce(splash.oneday.1996$ppfd) %>%
  coalesce(splash.oneday.1996$cond) %>%
  coalesce(splash.oneday.1996$eet) %>%
  coalesce(splash.oneday.1996$pet) %>%
  coalesce(splash.oneday.1996$aet) %>%
  coalesce(splash.oneday.1996$wn) %>%
  coalesce(splash.oneday.1996$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1996) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1997 data
###############################################################################
file.list.1997 <- list.files("../climate_data/splash_prep_files/splash_1997",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1997 <- setNames(file.list.1997, 
                           str_extract(basename(file.list.1997), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1997 <- lapply(file.list.1997, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1997$Brazos_2020_18.1997splash$sf <- splash.readin.1997$Brazos_2020_18.1997splash$sf[seq_along(splash.readin.1997$Brazos_2020_18.1997splash$sf) %% 2 > 0]
splash.readin.1997$Brazos_2020_18.1997splash$tair <- splash.readin.1997$Brazos_2020_18.1997splash$tair[seq_along(splash.readin.1997$Brazos_2020_18.1997splash$tair) %% 2 > 0]
splash.readin.1997$Brazos_2020_18.1997splash$pn <- splash.readin.1997$Brazos_2020_18.1997splash$pn[seq_along(splash.readin.1997$Brazos_2020_18.1997splash$pn) %% 2 > 0]
splash.readin.1997$Brazos_2020_18.1997splash$num_lines <- 365

# Harris 2020-03
splash.readin.1997$Harris_2020_03.1997splash$sf <- splash.readin.1997$Harris_2020_03.1997splash$sf[seq_along(splash.readin.1997$Harris_2020_03.1997splash$sf) %% 2 > 0]
splash.readin.1997$Harris_2020_03.1997splash$tair <- splash.readin.1997$Harris_2020_03.1997splash$tair[seq_along(splash.readin.1997$Harris_2020_03.1997splash$tair) %% 2 > 0]
splash.readin.1997$Harris_2020_03.1997splash$pn <- splash.readin.1997$Harris_2020_03.1997splash$pn[seq_along(splash.readin.1997$Harris_2020_03.1997splash$pn) %% 2 > 0]
splash.readin.1997$Harris_2020_03.1997splash$num_lines <- 365

# Menard 2020-01
splash.readin.1997$Menard_2020_01.1997splash$sf <- splash.readin.1997$Menard_2020_01.1997splash$sf[seq_along(splash.readin.1997$Menard_2020_01.1997splash$sf) %% 2 > 0]
splash.readin.1997$Menard_2020_01.1997splash$tair <- splash.readin.1997$Menard_2020_01.1997splash$tair[seq_along(splash.readin.1997$Menard_2020_01.1997splash$tair) %% 2 > 0]
splash.readin.1997$Menard_2020_01.1997splash$pn <- splash.readin.1997$Menard_2020_01.1997splash$pn[seq_along(splash.readin.1997$Menard_2020_01.1997splash$pn) %% 2 > 0]
splash.readin.1997$Menard_2020_01.1997splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1997$Uvalde_2020_02.1997splash$sf <- splash.readin.1997$Uvalde_2020_02.1997splash$sf[seq_along(splash.readin.1997$Uvalde_2020_02.1997splash$sf) %% 2 > 0]
splash.readin.1997$Uvalde_2020_02.1997splash$tair <- splash.readin.1997$Uvalde_2020_02.1997splash$tair[seq_along(splash.readin.1997$Uvalde_2020_02.1997splash$tair) %% 2 > 0]
splash.readin.1997$Uvalde_2020_02.1997splash$pn <- splash.readin.1997$Uvalde_2020_02.1997splash$pn[seq_along(splash.readin.1997$Uvalde_2020_02.1997splash$pn) %% 2 > 0]
splash.readin.1997$Uvalde_2020_02.1997splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1997 <- lapply(file.list.1997, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1997[["Brazos_2020_18.1997splash"]] <- splash.month.1997[["Brazos_2020_18.1997splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1997[["Harris_2020_03.1997splash"]] <- splash.month.1997[["Harris_2020_03.1997splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1997[["Menard_2020_01.1997splash"]] <- splash.month.1997[["Menard_2020_01.1997splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1997[["Uvalde_2020_02.1997splash"]] <- splash.month.1997[["Uvalde_2020_02.1997splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1997)) {
  splash.month.1997[[i]] <- splash.month.1997[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1997 <- lapply(file.list.1997, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1997 <- mapply(c, splash.readin.1997, splash.month.1997, 
                            splash.ancillary.1997, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1997)) {
  
  splash.total.1997[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1997[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1997[[i]][["num_lines"]])
  splash.total.1997[[i]][["daily_totals"]] <- as.data.frame(splash.total.1997[[i]][["daily_totals"]])
  names(splash.total.1997[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1997[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1997[[i]],
                                                      dtot = splash.total.1997[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1997 <- map(splash.total.1997, as.data.table)
splash.water.1997 <- rbindlist(splash.water.1997, fill = TRUE, idcol = TRUE)
splash.water.1997$year <- 1997
splash.water.1997$site <- str_extract(splash.water.1997$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1997 <- splash.water.1997 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1997 <- tidyr::unite(splash.water.1997, "date", year:day, sep = "-", remove = FALSE)
splash.water.1997$date <- lubridate::ymd(as.Date(splash.water.1997$date))
splash.water.1997$doy <- lubridate::yday(splash.water.1997$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1997 <- run_one_day(lat = splash.water.1997$lat_deg,
                                  elv = splash.water.1997$elv_m,
                                  n = splash.water.1997$doy,
                                  y = splash.water.1997$year,
                                  wn = splash.water.1997$soil.moisture,
                                  sf = splash.water.1997$sf,
                                  tc = splash.water.1997$tair,
                                  pn = splash.water.1997$pn,
                                  kWm = 150)

splash.oneday.1997 <- map(splash.oneday.1997, as.data.table)

## Change list column names
names(splash.oneday.1997[[1]]) <- "ho"
names(splash.oneday.1997[[2]]) <- "hn"
names(splash.oneday.1997[[3]]) <- "ppfd"
names(splash.oneday.1997[[4]]) <- "cond"
names(splash.oneday.1997[[5]]) <- "eet"
names(splash.oneday.1997[[6]]) <- "pet"
names(splash.oneday.1997[[7]]) <- "aet"
names(splash.oneday.1997[[8]]) <- "wn"
names(splash.oneday.1997[[9]]) <- "ro"

sites.daily.1997 <- splash.oneday.1997$ho %>%
  coalesce(splash.oneday.1997$hn) %>%
  coalesce(splash.oneday.1997$ppfd) %>%
  coalesce(splash.oneday.1997$cond) %>%
  coalesce(splash.oneday.1997$eet) %>%
  coalesce(splash.oneday.1997$pet) %>%
  coalesce(splash.oneday.1997$aet) %>%
  coalesce(splash.oneday.1997$wn) %>%
  coalesce(splash.oneday.1997$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1997) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1998 data
###############################################################################
file.list.1998 <- list.files("../climate_data/splash_prep_files/splash_1998",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1998 <- setNames(file.list.1998, 
                           str_extract(basename(file.list.1998), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1998 <- lapply(file.list.1998, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1998$Brazos_2020_18.1998splash$sf <- splash.readin.1998$Brazos_2020_18.1998splash$sf[seq_along(splash.readin.1998$Brazos_2020_18.1998splash$sf) %% 2 > 0]
splash.readin.1998$Brazos_2020_18.1998splash$tair <- splash.readin.1998$Brazos_2020_18.1998splash$tair[seq_along(splash.readin.1998$Brazos_2020_18.1998splash$tair) %% 2 > 0]
splash.readin.1998$Brazos_2020_18.1998splash$pn <- splash.readin.1998$Brazos_2020_18.1998splash$pn[seq_along(splash.readin.1998$Brazos_2020_18.1998splash$pn) %% 2 > 0]
splash.readin.1998$Brazos_2020_18.1998splash$num_lines <- 365

# Harris 2020-03
splash.readin.1998$Harris_2020_03.1998splash$sf <- splash.readin.1998$Harris_2020_03.1998splash$sf[seq_along(splash.readin.1998$Harris_2020_03.1998splash$sf) %% 2 > 0]
splash.readin.1998$Harris_2020_03.1998splash$tair <- splash.readin.1998$Harris_2020_03.1998splash$tair[seq_along(splash.readin.1998$Harris_2020_03.1998splash$tair) %% 2 > 0]
splash.readin.1998$Harris_2020_03.1998splash$pn <- splash.readin.1998$Harris_2020_03.1998splash$pn[seq_along(splash.readin.1998$Harris_2020_03.1998splash$pn) %% 2 > 0]
splash.readin.1998$Harris_2020_03.1998splash$num_lines <- 365

# Menard 2020-01
splash.readin.1998$Menard_2020_01.1998splash$sf <- splash.readin.1998$Menard_2020_01.1998splash$sf[seq_along(splash.readin.1998$Menard_2020_01.1998splash$sf) %% 2 > 0]
splash.readin.1998$Menard_2020_01.1998splash$tair <- splash.readin.1998$Menard_2020_01.1998splash$tair[seq_along(splash.readin.1998$Menard_2020_01.1998splash$tair) %% 2 > 0]
splash.readin.1998$Menard_2020_01.1998splash$pn <- splash.readin.1998$Menard_2020_01.1998splash$pn[seq_along(splash.readin.1998$Menard_2020_01.1998splash$pn) %% 2 > 0]
splash.readin.1998$Menard_2020_01.1998splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1998$Uvalde_2020_02.1998splash$sf <- splash.readin.1998$Uvalde_2020_02.1998splash$sf[seq_along(splash.readin.1998$Uvalde_2020_02.1998splash$sf) %% 2 > 0]
splash.readin.1998$Uvalde_2020_02.1998splash$tair <- splash.readin.1998$Uvalde_2020_02.1998splash$tair[seq_along(splash.readin.1998$Uvalde_2020_02.1998splash$tair) %% 2 > 0]
splash.readin.1998$Uvalde_2020_02.1998splash$pn <- splash.readin.1998$Uvalde_2020_02.1998splash$pn[seq_along(splash.readin.1998$Uvalde_2020_02.1998splash$pn) %% 2 > 0]
splash.readin.1998$Uvalde_2020_02.1998splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1998 <- lapply(file.list.1998, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1998[["Brazos_2020_18.1998splash"]] <- splash.month.1998[["Brazos_2020_18.1998splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1998[["Harris_2020_03.1998splash"]] <- splash.month.1998[["Harris_2020_03.1998splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1998[["Menard_2020_01.1998splash"]] <- splash.month.1998[["Menard_2020_01.1998splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1998[["Uvalde_2020_02.1998splash"]] <- splash.month.1998[["Uvalde_2020_02.1998splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1998)) {
  splash.month.1998[[i]] <- splash.month.1998[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1998 <- lapply(file.list.1998, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1998 <- mapply(c, splash.readin.1998, splash.month.1998, 
                            splash.ancillary.1998, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1998)) {
  
  splash.total.1998[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1998[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1998[[i]][["num_lines"]])
  splash.total.1998[[i]][["daily_totals"]] <- as.data.frame(splash.total.1998[[i]][["daily_totals"]])
  names(splash.total.1998[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1998[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1998[[i]],
                                                      dtot = splash.total.1998[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1998 <- map(splash.total.1998, as.data.table)
splash.water.1998 <- rbindlist(splash.water.1998, fill = TRUE, idcol = TRUE)
splash.water.1998$year <- 1998
splash.water.1998$site <- str_extract(splash.water.1998$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1998 <- splash.water.1998 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1998 <- tidyr::unite(splash.water.1998, "date", year:day, sep = "-", remove = FALSE)
splash.water.1998$date <- lubridate::ymd(as.Date(splash.water.1998$date))
splash.water.1998$doy <- lubridate::yday(splash.water.1998$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1998 <- run_one_day(lat = splash.water.1998$lat_deg,
                                  elv = splash.water.1998$elv_m,
                                  n = splash.water.1998$doy,
                                  y = splash.water.1998$year,
                                  wn = splash.water.1998$soil.moisture,
                                  sf = splash.water.1998$sf,
                                  tc = splash.water.1998$tair,
                                  pn = splash.water.1998$pn,
                                  kWm = 150)

splash.oneday.1998 <- map(splash.oneday.1998, as.data.table)

## Change list column names
names(splash.oneday.1998[[1]]) <- "ho"
names(splash.oneday.1998[[2]]) <- "hn"
names(splash.oneday.1998[[3]]) <- "ppfd"
names(splash.oneday.1998[[4]]) <- "cond"
names(splash.oneday.1998[[5]]) <- "eet"
names(splash.oneday.1998[[6]]) <- "pet"
names(splash.oneday.1998[[7]]) <- "aet"
names(splash.oneday.1998[[8]]) <- "wn"
names(splash.oneday.1998[[9]]) <- "ro"

sites.daily.1998 <- splash.oneday.1998$ho %>%
  coalesce(splash.oneday.1998$hn) %>%
  coalesce(splash.oneday.1998$ppfd) %>%
  coalesce(splash.oneday.1998$cond) %>%
  coalesce(splash.oneday.1998$eet) %>%
  coalesce(splash.oneday.1998$pet) %>%
  coalesce(splash.oneday.1998$aet) %>%
  coalesce(splash.oneday.1998$wn) %>%
  coalesce(splash.oneday.1998$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1998) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 1999 data
###############################################################################
file.list.1999 <- list.files("../climate_data/splash_prep_files/splash_1999",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.1999 <- setNames(file.list.1999, 
                           str_extract(basename(file.list.1999), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.1999 <- lapply(file.list.1999, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.1999$Brazos_2020_18.1999splash$sf <- splash.readin.1999$Brazos_2020_18.1999splash$sf[seq_along(splash.readin.1999$Brazos_2020_18.1999splash$sf) %% 2 > 0]
splash.readin.1999$Brazos_2020_18.1999splash$tair <- splash.readin.1999$Brazos_2020_18.1999splash$tair[seq_along(splash.readin.1999$Brazos_2020_18.1999splash$tair) %% 2 > 0]
splash.readin.1999$Brazos_2020_18.1999splash$pn <- splash.readin.1999$Brazos_2020_18.1999splash$pn[seq_along(splash.readin.1999$Brazos_2020_18.1999splash$pn) %% 2 > 0]
splash.readin.1999$Brazos_2020_18.1999splash$num_lines <- 365

# Harris 2020-03
splash.readin.1999$Harris_2020_03.1999splash$sf <- splash.readin.1999$Harris_2020_03.1999splash$sf[seq_along(splash.readin.1999$Harris_2020_03.1999splash$sf) %% 2 > 0]
splash.readin.1999$Harris_2020_03.1999splash$tair <- splash.readin.1999$Harris_2020_03.1999splash$tair[seq_along(splash.readin.1999$Harris_2020_03.1999splash$tair) %% 2 > 0]
splash.readin.1999$Harris_2020_03.1999splash$pn <- splash.readin.1999$Harris_2020_03.1999splash$pn[seq_along(splash.readin.1999$Harris_2020_03.1999splash$pn) %% 2 > 0]
splash.readin.1999$Harris_2020_03.1999splash$num_lines <- 365

# Menard 2020-01
splash.readin.1999$Menard_2020_01.1999splash$sf <- splash.readin.1999$Menard_2020_01.1999splash$sf[seq_along(splash.readin.1999$Menard_2020_01.1999splash$sf) %% 2 > 0]
splash.readin.1999$Menard_2020_01.1999splash$tair <- splash.readin.1999$Menard_2020_01.1999splash$tair[seq_along(splash.readin.1999$Menard_2020_01.1999splash$tair) %% 2 > 0]
splash.readin.1999$Menard_2020_01.1999splash$pn <- splash.readin.1999$Menard_2020_01.1999splash$pn[seq_along(splash.readin.1999$Menard_2020_01.1999splash$pn) %% 2 > 0]
splash.readin.1999$Menard_2020_01.1999splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1999$Uvalde_2020_02.1999splash$sf <- splash.readin.1999$Uvalde_2020_02.1999splash$sf[seq_along(splash.readin.1999$Uvalde_2020_02.1999splash$sf) %% 2 > 0]
splash.readin.1999$Uvalde_2020_02.1999splash$tair <- splash.readin.1999$Uvalde_2020_02.1999splash$tair[seq_along(splash.readin.1999$Uvalde_2020_02.1999splash$tair) %% 2 > 0]
splash.readin.1999$Uvalde_2020_02.1999splash$pn <- splash.readin.1999$Uvalde_2020_02.1999splash$pn[seq_along(splash.readin.1999$Uvalde_2020_02.1999splash$pn) %% 2 > 0]
splash.readin.1999$Uvalde_2020_02.1999splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.1999 <- lapply(file.list.1999, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.1999[["Brazos_2020_18.1999splash"]] <- splash.month.1999[["Brazos_2020_18.1999splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.1999[["Harris_2020_03.1999splash"]] <- splash.month.1999[["Harris_2020_03.1999splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.1999[["Menard_2020_01.1999splash"]] <- splash.month.1999[["Menard_2020_01.1999splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.1999[["Uvalde_2020_02.1999splash"]] <- splash.month.1999[["Uvalde_2020_02.1999splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.1999)) {
  splash.month.1999[[i]] <- splash.month.1999[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.1999 <- lapply(file.list.1999, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.1999 <- mapply(c, splash.readin.1999, splash.month.1999, 
                            splash.ancillary.1999, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.1999)) {
  
  splash.total.1999[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.1999[[i]][["num_lines"]]), 
                                                     nrow = splash.total.1999[[i]][["num_lines"]])
  splash.total.1999[[i]][["daily_totals"]] <- as.data.frame(splash.total.1999[[i]][["daily_totals"]])
  names(splash.total.1999[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.1999[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.1999[[i]],
                                                      dtot = splash.total.1999[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.1999 <- map(splash.total.1999, as.data.table)
splash.water.1999 <- rbindlist(splash.water.1999, fill = TRUE, idcol = TRUE)
splash.water.1999$year <- 1999
splash.water.1999$site <- str_extract(splash.water.1999$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.1999 <- splash.water.1999 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.1999 <- tidyr::unite(splash.water.1999, "date", year:day, sep = "-", remove = FALSE)
splash.water.1999$date <- lubridate::ymd(as.Date(splash.water.1999$date))
splash.water.1999$doy <- lubridate::yday(splash.water.1999$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.1999 <- run_one_day(lat = splash.water.1999$lat_deg,
                                  elv = splash.water.1999$elv_m,
                                  n = splash.water.1999$doy,
                                  y = splash.water.1999$year,
                                  wn = splash.water.1999$soil.moisture,
                                  sf = splash.water.1999$sf,
                                  tc = splash.water.1999$tair,
                                  pn = splash.water.1999$pn,
                                  kWm = 150)

splash.oneday.1999 <- map(splash.oneday.1999, as.data.table)

## Change list column names
names(splash.oneday.1999[[1]]) <- "ho"
names(splash.oneday.1999[[2]]) <- "hn"
names(splash.oneday.1999[[3]]) <- "ppfd"
names(splash.oneday.1999[[4]]) <- "cond"
names(splash.oneday.1999[[5]]) <- "eet"
names(splash.oneday.1999[[6]]) <- "pet"
names(splash.oneday.1999[[7]]) <- "aet"
names(splash.oneday.1999[[8]]) <- "wn"
names(splash.oneday.1999[[9]]) <- "ro"

sites.daily.1999 <- splash.oneday.1999$ho %>%
  coalesce(splash.oneday.1999$hn) %>%
  coalesce(splash.oneday.1999$ppfd) %>%
  coalesce(splash.oneday.1999$cond) %>%
  coalesce(splash.oneday.1999$eet) %>%
  coalesce(splash.oneday.1999$pet) %>%
  coalesce(splash.oneday.1999$aet) %>%
  coalesce(splash.oneday.1999$wn) %>%
  coalesce(splash.oneday.1999$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.1999) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2000 data
###############################################################################
file.list.2000 <- list.files("../climate_data/splash_prep_files/splash_2000",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2000 <- setNames(file.list.2000, 
                           str_extract(basename(file.list.2000), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2000 <- lapply(file.list.2000, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2000$Brazos_2020_18.2000splash$sf <- splash.readin.2000$Brazos_2020_18.2000splash$sf[seq_along(splash.readin.2000$Brazos_2020_18.2000splash$sf) %% 2 > 0]
splash.readin.2000$Brazos_2020_18.2000splash$tair <- splash.readin.2000$Brazos_2020_18.2000splash$tair[seq_along(splash.readin.2000$Brazos_2020_18.2000splash$tair) %% 2 > 0]
splash.readin.2000$Brazos_2020_18.2000splash$pn <- splash.readin.2000$Brazos_2020_18.2000splash$pn[seq_along(splash.readin.2000$Brazos_2020_18.2000splash$pn) %% 2 > 0]
splash.readin.2000$Brazos_2020_18.2000splash$num_lines <- 366

# Harris 2020-03
splash.readin.2000$Harris_2020_03.2000splash$sf <- splash.readin.2000$Harris_2020_03.2000splash$sf[seq_along(splash.readin.2000$Harris_2020_03.2000splash$sf) %% 2 > 0]
splash.readin.2000$Harris_2020_03.2000splash$tair <- splash.readin.2000$Harris_2020_03.2000splash$tair[seq_along(splash.readin.2000$Harris_2020_03.2000splash$tair) %% 2 > 0]
splash.readin.2000$Harris_2020_03.2000splash$pn <- splash.readin.2000$Harris_2020_03.2000splash$pn[seq_along(splash.readin.2000$Harris_2020_03.2000splash$pn) %% 2 > 0]
splash.readin.2000$Harris_2020_03.2000splash$num_lines <- 366

# Menard 2020-01
splash.readin.2000$Menard_2020_01.2000splash$sf <- splash.readin.2000$Menard_2020_01.2000splash$sf[seq_along(splash.readin.2000$Menard_2020_01.2000splash$sf) %% 2 > 0]
splash.readin.2000$Menard_2020_01.2000splash$tair <- splash.readin.2000$Menard_2020_01.2000splash$tair[seq_along(splash.readin.2000$Menard_2020_01.2000splash$tair) %% 2 > 0]
splash.readin.2000$Menard_2020_01.2000splash$pn <- splash.readin.2000$Menard_2020_01.2000splash$pn[seq_along(splash.readin.2000$Menard_2020_01.2000splash$pn) %% 2 > 0]
splash.readin.2000$Menard_2020_01.2000splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.2000$Uvalde_2020_02.2000splash$sf <- splash.readin.2000$Uvalde_2020_02.2000splash$sf[seq_along(splash.readin.2000$Uvalde_2020_02.2000splash$sf) %% 2 > 0]
splash.readin.2000$Uvalde_2020_02.2000splash$tair <- splash.readin.2000$Uvalde_2020_02.2000splash$tair[seq_along(splash.readin.2000$Uvalde_2020_02.2000splash$tair) %% 2 > 0]
splash.readin.2000$Uvalde_2020_02.2000splash$pn <- splash.readin.2000$Uvalde_2020_02.2000splash$pn[seq_along(splash.readin.2000$Uvalde_2020_02.2000splash$pn) %% 2 > 0]
splash.readin.2000$Uvalde_2020_02.2000splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2000 <- lapply(file.list.2000, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2000[["Brazos_2020_18.2000splash"]] <- splash.month.2000[["Brazos_2020_18.2000splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2000[["Harris_2020_03.2000splash"]] <- splash.month.2000[["Harris_2020_03.2000splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2000[["Menard_2020_01.2000splash"]] <- splash.month.2000[["Menard_2020_01.2000splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2000[["Uvalde_2020_02.2000splash"]] <- splash.month.2000[["Uvalde_2020_02.2000splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2000)) {
  splash.month.2000[[i]] <- splash.month.2000[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2000 <- lapply(file.list.2000, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2000 <- mapply(c, splash.readin.2000, splash.month.2000, 
                            splash.ancillary.2000, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2000)) {
  
  splash.total.2000[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2000[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2000[[i]][["num_lines"]])
  splash.total.2000[[i]][["daily_totals"]] <- as.data.frame(splash.total.2000[[i]][["daily_totals"]])
  names(splash.total.2000[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2000[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2000[[i]],
                                                      dtot = splash.total.2000[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2000 <- map(splash.total.2000, as.data.table)
splash.water.2000 <- rbindlist(splash.water.2000, fill = TRUE, idcol = TRUE)
splash.water.2000$year <- 2000
splash.water.2000$site <- str_extract(splash.water.2000$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2000 <- splash.water.2000 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2000 <- tidyr::unite(splash.water.2000, "date", year:day, sep = "-", remove = FALSE)
splash.water.2000$date <- lubridate::ymd(as.Date(splash.water.2000$date))
splash.water.2000$doy <- lubridate::yday(splash.water.2000$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2000 <- run_one_day(lat = splash.water.2000$lat_deg,
                                  elv = splash.water.2000$elv_m,
                                  n = splash.water.2000$doy,
                                  y = splash.water.2000$year,
                                  wn = splash.water.2000$soil.moisture,
                                  sf = splash.water.2000$sf,
                                  tc = splash.water.2000$tair,
                                  pn = splash.water.2000$pn,
                                  kWm = 150)

splash.oneday.2000 <- map(splash.oneday.2000, as.data.table)

## Change list column names
names(splash.oneday.2000[[1]]) <- "ho"
names(splash.oneday.2000[[2]]) <- "hn"
names(splash.oneday.2000[[3]]) <- "ppfd"
names(splash.oneday.2000[[4]]) <- "cond"
names(splash.oneday.2000[[5]]) <- "eet"
names(splash.oneday.2000[[6]]) <- "pet"
names(splash.oneday.2000[[7]]) <- "aet"
names(splash.oneday.2000[[8]]) <- "wn"
names(splash.oneday.2000[[9]]) <- "ro"

sites.daily.2000 <- splash.oneday.2000$ho %>%
  coalesce(splash.oneday.2000$hn) %>%
  coalesce(splash.oneday.2000$ppfd) %>%
  coalesce(splash.oneday.2000$cond) %>%
  coalesce(splash.oneday.2000$eet) %>%
  coalesce(splash.oneday.2000$pet) %>%
  coalesce(splash.oneday.2000$aet) %>%
  coalesce(splash.oneday.2000$wn) %>%
  coalesce(splash.oneday.2000$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2000) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2001 data
###############################################################################
file.list.2001 <- list.files("../climate_data/splash_prep_files/splash_2001",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2001 <- setNames(file.list.2001, 
                           str_extract(basename(file.list.2001), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2001 <- lapply(file.list.2001, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2001$Brazos_2020_18.2001splash$sf <- splash.readin.2001$Brazos_2020_18.2001splash$sf[seq_along(splash.readin.2001$Brazos_2020_18.2001splash$sf) %% 2 > 0]
splash.readin.2001$Brazos_2020_18.2001splash$tair <- splash.readin.2001$Brazos_2020_18.2001splash$tair[seq_along(splash.readin.2001$Brazos_2020_18.2001splash$tair) %% 2 > 0]
splash.readin.2001$Brazos_2020_18.2001splash$pn <- splash.readin.2001$Brazos_2020_18.2001splash$pn[seq_along(splash.readin.2001$Brazos_2020_18.2001splash$pn) %% 2 > 0]
splash.readin.2001$Brazos_2020_18.2001splash$num_lines <- 365

# Harris 2020-03
splash.readin.2001$Harris_2020_03.2001splash$sf <- splash.readin.2001$Harris_2020_03.2001splash$sf[seq_along(splash.readin.2001$Harris_2020_03.2001splash$sf) %% 2 > 0]
splash.readin.2001$Harris_2020_03.2001splash$tair <- splash.readin.2001$Harris_2020_03.2001splash$tair[seq_along(splash.readin.2001$Harris_2020_03.2001splash$tair) %% 2 > 0]
splash.readin.2001$Harris_2020_03.2001splash$pn <- splash.readin.2001$Harris_2020_03.2001splash$pn[seq_along(splash.readin.2001$Harris_2020_03.2001splash$pn) %% 2 > 0]
splash.readin.2001$Harris_2020_03.2001splash$num_lines <- 365

# Menard 2020-01
splash.readin.2001$Menard_2020_01.2001splash$sf <- splash.readin.2001$Menard_2020_01.2001splash$sf[seq_along(splash.readin.2001$Menard_2020_01.2001splash$sf) %% 2 > 0]
splash.readin.2001$Menard_2020_01.2001splash$tair <- splash.readin.2001$Menard_2020_01.2001splash$tair[seq_along(splash.readin.2001$Menard_2020_01.2001splash$tair) %% 2 > 0]
splash.readin.2001$Menard_2020_01.2001splash$pn <- splash.readin.2001$Menard_2020_01.2001splash$pn[seq_along(splash.readin.2001$Menard_2020_01.2001splash$pn) %% 2 > 0]
splash.readin.2001$Menard_2020_01.2001splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2001$Uvalde_2020_02.2001splash$sf <- splash.readin.2001$Uvalde_2020_02.2001splash$sf[seq_along(splash.readin.2001$Uvalde_2020_02.2001splash$sf) %% 2 > 0]
splash.readin.2001$Uvalde_2020_02.2001splash$tair <- splash.readin.2001$Uvalde_2020_02.2001splash$tair[seq_along(splash.readin.2001$Uvalde_2020_02.2001splash$tair) %% 2 > 0]
splash.readin.2001$Uvalde_2020_02.2001splash$pn <- splash.readin.2001$Uvalde_2020_02.2001splash$pn[seq_along(splash.readin.2001$Uvalde_2020_02.2001splash$pn) %% 2 > 0]
splash.readin.2001$Uvalde_2020_02.2001splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2001 <- lapply(file.list.2001, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2001[["Brazos_2020_18.2001splash"]] <- splash.month.2001[["Brazos_2020_18.2001splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2001[["Harris_2020_03.2001splash"]] <- splash.month.2001[["Harris_2020_03.2001splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2001[["Menard_2020_01.2001splash"]] <- splash.month.2001[["Menard_2020_01.2001splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2001[["Uvalde_2020_02.2001splash"]] <- splash.month.2001[["Uvalde_2020_02.2001splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2001)) {
  splash.month.2001[[i]] <- splash.month.2001[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2001 <- lapply(file.list.2001, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2001 <- mapply(c, splash.readin.2001, splash.month.2001, 
                            splash.ancillary.2001, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2001)) {
  
  splash.total.2001[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2001[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2001[[i]][["num_lines"]])
  splash.total.2001[[i]][["daily_totals"]] <- as.data.frame(splash.total.2001[[i]][["daily_totals"]])
  names(splash.total.2001[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2001[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2001[[i]],
                                                      dtot = splash.total.2001[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2001 <- map(splash.total.2001, as.data.table)
splash.water.2001 <- rbindlist(splash.water.2001, fill = TRUE, idcol = TRUE)
splash.water.2001$year <- 2001
splash.water.2001$site <- str_extract(splash.water.2001$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2001 <- splash.water.2001 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2001 <- tidyr::unite(splash.water.2001, "date", year:day, sep = "-", remove = FALSE)
splash.water.2001$date <- lubridate::ymd(as.Date(splash.water.2001$date))
splash.water.2001$doy <- lubridate::yday(splash.water.2001$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2001 <- run_one_day(lat = splash.water.2001$lat_deg,
                                  elv = splash.water.2001$elv_m,
                                  n = splash.water.2001$doy,
                                  y = splash.water.2001$year,
                                  wn = splash.water.2001$soil.moisture,
                                  sf = splash.water.2001$sf,
                                  tc = splash.water.2001$tair,
                                  pn = splash.water.2001$pn,
                                  kWm = 150)

splash.oneday.2001 <- map(splash.oneday.2001, as.data.table)

## Change list column names
names(splash.oneday.2001[[1]]) <- "ho"
names(splash.oneday.2001[[2]]) <- "hn"
names(splash.oneday.2001[[3]]) <- "ppfd"
names(splash.oneday.2001[[4]]) <- "cond"
names(splash.oneday.2001[[5]]) <- "eet"
names(splash.oneday.2001[[6]]) <- "pet"
names(splash.oneday.2001[[7]]) <- "aet"
names(splash.oneday.2001[[8]]) <- "wn"
names(splash.oneday.2001[[9]]) <- "ro"

sites.daily.2001 <- splash.oneday.2001$ho %>%
  coalesce(splash.oneday.2001$hn) %>%
  coalesce(splash.oneday.2001$ppfd) %>%
  coalesce(splash.oneday.2001$cond) %>%
  coalesce(splash.oneday.2001$eet) %>%
  coalesce(splash.oneday.2001$pet) %>%
  coalesce(splash.oneday.2001$aet) %>%
  coalesce(splash.oneday.2001$wn) %>%
  coalesce(splash.oneday.2001$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2001) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2002 data
###############################################################################
file.list.2002 <- list.files("../climate_data/splash_prep_files/splash_2002",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2002 <- setNames(file.list.2002, 
                           str_extract(basename(file.list.2002), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2002 <- lapply(file.list.2002, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2002$Brazos_2020_18.2002splash$sf <- splash.readin.2002$Brazos_2020_18.2002splash$sf[seq_along(splash.readin.2002$Brazos_2020_18.2002splash$sf) %% 2 > 0]
splash.readin.2002$Brazos_2020_18.2002splash$tair <- splash.readin.2002$Brazos_2020_18.2002splash$tair[seq_along(splash.readin.2002$Brazos_2020_18.2002splash$tair) %% 2 > 0]
splash.readin.2002$Brazos_2020_18.2002splash$pn <- splash.readin.2002$Brazos_2020_18.2002splash$pn[seq_along(splash.readin.2002$Brazos_2020_18.2002splash$pn) %% 2 > 0]
splash.readin.2002$Brazos_2020_18.2002splash$num_lines <- 365

# Harris 2020-03
splash.readin.2002$Harris_2020_03.2002splash$sf <- splash.readin.2002$Harris_2020_03.2002splash$sf[seq_along(splash.readin.2002$Harris_2020_03.2002splash$sf) %% 2 > 0]
splash.readin.2002$Harris_2020_03.2002splash$tair <- splash.readin.2002$Harris_2020_03.2002splash$tair[seq_along(splash.readin.2002$Harris_2020_03.2002splash$tair) %% 2 > 0]
splash.readin.2002$Harris_2020_03.2002splash$pn <- splash.readin.2002$Harris_2020_03.2002splash$pn[seq_along(splash.readin.2002$Harris_2020_03.2002splash$pn) %% 2 > 0]
splash.readin.2002$Harris_2020_03.2002splash$num_lines <- 365

# Menard 2020-01
splash.readin.2002$Menard_2020_01.2002splash$sf <- splash.readin.2002$Menard_2020_01.2002splash$sf[seq_along(splash.readin.2002$Menard_2020_01.2002splash$sf) %% 2 > 0]
splash.readin.2002$Menard_2020_01.2002splash$tair <- splash.readin.2002$Menard_2020_01.2002splash$tair[seq_along(splash.readin.2002$Menard_2020_01.2002splash$tair) %% 2 > 0]
splash.readin.2002$Menard_2020_01.2002splash$pn <- splash.readin.2002$Menard_2020_01.2002splash$pn[seq_along(splash.readin.2002$Menard_2020_01.2002splash$pn) %% 2 > 0]
splash.readin.2002$Menard_2020_01.2002splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2002$Uvalde_2020_02.2002splash$sf <- splash.readin.2002$Uvalde_2020_02.2002splash$sf[seq_along(splash.readin.2002$Uvalde_2020_02.2002splash$sf) %% 2 > 0]
splash.readin.2002$Uvalde_2020_02.2002splash$tair <- splash.readin.2002$Uvalde_2020_02.2002splash$tair[seq_along(splash.readin.2002$Uvalde_2020_02.2002splash$tair) %% 2 > 0]
splash.readin.2002$Uvalde_2020_02.2002splash$pn <- splash.readin.2002$Uvalde_2020_02.2002splash$pn[seq_along(splash.readin.2002$Uvalde_2020_02.2002splash$pn) %% 2 > 0]
splash.readin.2002$Uvalde_2020_02.2002splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2002 <- lapply(file.list.2002, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2002[["Brazos_2020_18.2002splash"]] <- splash.month.2002[["Brazos_2020_18.2002splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2002[["Harris_2020_03.2002splash"]] <- splash.month.2002[["Harris_2020_03.2002splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2002[["Menard_2020_01.2002splash"]] <- splash.month.2002[["Menard_2020_01.2002splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2002[["Uvalde_2020_02.2002splash"]] <- splash.month.2002[["Uvalde_2020_02.2002splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2002)) {
  splash.month.2002[[i]] <- splash.month.2002[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2002 <- lapply(file.list.2002, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2002 <- mapply(c, splash.readin.2002, splash.month.2002, 
                            splash.ancillary.2002, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2002)) {
  
  splash.total.2002[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2002[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2002[[i]][["num_lines"]])
  splash.total.2002[[i]][["daily_totals"]] <- as.data.frame(splash.total.2002[[i]][["daily_totals"]])
  names(splash.total.2002[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2002[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2002[[i]],
                                                      dtot = splash.total.2002[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2002 <- map(splash.total.2002, as.data.table)
splash.water.2002 <- rbindlist(splash.water.2002, fill = TRUE, idcol = TRUE)
splash.water.2002$year <- 2002
splash.water.2002$site <- str_extract(splash.water.2002$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2002 <- splash.water.2002 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2002 <- tidyr::unite(splash.water.2002, "date", year:day, sep = "-", remove = FALSE)
splash.water.2002$date <- lubridate::ymd(as.Date(splash.water.2002$date))
splash.water.2002$doy <- lubridate::yday(splash.water.2002$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2002 <- run_one_day(lat = splash.water.2002$lat_deg,
                                  elv = splash.water.2002$elv_m,
                                  n = splash.water.2002$doy,
                                  y = splash.water.2002$year,
                                  wn = splash.water.2002$soil.moisture,
                                  sf = splash.water.2002$sf,
                                  tc = splash.water.2002$tair,
                                  pn = splash.water.2002$pn,
                                  kWm = 150)

splash.oneday.2002 <- map(splash.oneday.2002, as.data.table)

## Change list column names
names(splash.oneday.2002[[1]]) <- "ho"
names(splash.oneday.2002[[2]]) <- "hn"
names(splash.oneday.2002[[3]]) <- "ppfd"
names(splash.oneday.2002[[4]]) <- "cond"
names(splash.oneday.2002[[5]]) <- "eet"
names(splash.oneday.2002[[6]]) <- "pet"
names(splash.oneday.2002[[7]]) <- "aet"
names(splash.oneday.2002[[8]]) <- "wn"
names(splash.oneday.2002[[9]]) <- "ro"

sites.daily.2002 <- splash.oneday.2002$ho %>%
  coalesce(splash.oneday.2002$hn) %>%
  coalesce(splash.oneday.2002$ppfd) %>%
  coalesce(splash.oneday.2002$cond) %>%
  coalesce(splash.oneday.2002$eet) %>%
  coalesce(splash.oneday.2002$pet) %>%
  coalesce(splash.oneday.2002$aet) %>%
  coalesce(splash.oneday.2002$wn) %>%
  coalesce(splash.oneday.2002$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2002) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2003 data
###############################################################################
file.list.2003 <- list.files("../climate_data/splash_prep_files/splash_2003",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2003 <- setNames(file.list.2003, 
                           str_extract(basename(file.list.2003), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2003 <- lapply(file.list.2003, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2003$Brazos_2020_18.2003splash$sf <- splash.readin.2003$Brazos_2020_18.2003splash$sf[seq_along(splash.readin.2003$Brazos_2020_18.2003splash$sf) %% 2 > 0]
splash.readin.2003$Brazos_2020_18.2003splash$tair <- splash.readin.2003$Brazos_2020_18.2003splash$tair[seq_along(splash.readin.2003$Brazos_2020_18.2003splash$tair) %% 2 > 0]
splash.readin.2003$Brazos_2020_18.2003splash$pn <- splash.readin.2003$Brazos_2020_18.2003splash$pn[seq_along(splash.readin.2003$Brazos_2020_18.2003splash$pn) %% 2 > 0]
splash.readin.2003$Brazos_2020_18.2003splash$num_lines <- 365

# Harris 2020-03
splash.readin.2003$Harris_2020_03.2003splash$sf <- splash.readin.2003$Harris_2020_03.2003splash$sf[seq_along(splash.readin.2003$Harris_2020_03.2003splash$sf) %% 2 > 0]
splash.readin.2003$Harris_2020_03.2003splash$tair <- splash.readin.2003$Harris_2020_03.2003splash$tair[seq_along(splash.readin.2003$Harris_2020_03.2003splash$tair) %% 2 > 0]
splash.readin.2003$Harris_2020_03.2003splash$pn <- splash.readin.2003$Harris_2020_03.2003splash$pn[seq_along(splash.readin.2003$Harris_2020_03.2003splash$pn) %% 2 > 0]
splash.readin.2003$Harris_2020_03.2003splash$num_lines <- 365

# Menard 2020-01
splash.readin.2003$Menard_2020_01.2003splash$sf <- splash.readin.2003$Menard_2020_01.2003splash$sf[seq_along(splash.readin.2003$Menard_2020_01.2003splash$sf) %% 2 > 0]
splash.readin.2003$Menard_2020_01.2003splash$tair <- splash.readin.2003$Menard_2020_01.2003splash$tair[seq_along(splash.readin.2003$Menard_2020_01.2003splash$tair) %% 2 > 0]
splash.readin.2003$Menard_2020_01.2003splash$pn <- splash.readin.2003$Menard_2020_01.2003splash$pn[seq_along(splash.readin.2003$Menard_2020_01.2003splash$pn) %% 2 > 0]
splash.readin.2003$Menard_2020_01.2003splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2003$Uvalde_2020_02.2003splash$sf <- splash.readin.2003$Uvalde_2020_02.2003splash$sf[seq_along(splash.readin.2003$Uvalde_2020_02.2003splash$sf) %% 2 > 0]
splash.readin.2003$Uvalde_2020_02.2003splash$tair <- splash.readin.2003$Uvalde_2020_02.2003splash$tair[seq_along(splash.readin.2003$Uvalde_2020_02.2003splash$tair) %% 2 > 0]
splash.readin.2003$Uvalde_2020_02.2003splash$pn <- splash.readin.2003$Uvalde_2020_02.2003splash$pn[seq_along(splash.readin.2003$Uvalde_2020_02.2003splash$pn) %% 2 > 0]
splash.readin.2003$Uvalde_2020_02.2003splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2003 <- lapply(file.list.2003, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2003[["Brazos_2020_18.2003splash"]] <- splash.month.2003[["Brazos_2020_18.2003splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2003[["Harris_2020_03.2003splash"]] <- splash.month.2003[["Harris_2020_03.2003splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2003[["Menard_2020_01.2003splash"]] <- splash.month.2003[["Menard_2020_01.2003splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2003[["Uvalde_2020_02.2003splash"]] <- splash.month.2003[["Uvalde_2020_02.2003splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2003)) {
  splash.month.2003[[i]] <- splash.month.2003[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2003 <- lapply(file.list.2003, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2003 <- mapply(c, splash.readin.2003, splash.month.2003, 
                            splash.ancillary.2003, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2003)) {
  
  splash.total.2003[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2003[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2003[[i]][["num_lines"]])
  splash.total.2003[[i]][["daily_totals"]] <- as.data.frame(splash.total.2003[[i]][["daily_totals"]])
  names(splash.total.2003[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2003[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2003[[i]],
                                                      dtot = splash.total.2003[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2003 <- map(splash.total.2003, as.data.table)
splash.water.2003 <- rbindlist(splash.water.2003, fill = TRUE, idcol = TRUE)
splash.water.2003$year <- 2003
splash.water.2003$site <- str_extract(splash.water.2003$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2003 <- splash.water.2003 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2003 <- tidyr::unite(splash.water.2003, "date", year:day, sep = "-", remove = FALSE)
splash.water.2003$date <- lubridate::ymd(as.Date(splash.water.2003$date))
splash.water.2003$doy <- lubridate::yday(splash.water.2003$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2003 <- run_one_day(lat = splash.water.2003$lat_deg,
                                  elv = splash.water.2003$elv_m,
                                  n = splash.water.2003$doy,
                                  y = splash.water.2003$year,
                                  wn = splash.water.2003$soil.moisture,
                                  sf = splash.water.2003$sf,
                                  tc = splash.water.2003$tair,
                                  pn = splash.water.2003$pn,
                                  kWm = 150)

splash.oneday.2003 <- map(splash.oneday.2003, as.data.table)

## Change list column names
names(splash.oneday.2003[[1]]) <- "ho"
names(splash.oneday.2003[[2]]) <- "hn"
names(splash.oneday.2003[[3]]) <- "ppfd"
names(splash.oneday.2003[[4]]) <- "cond"
names(splash.oneday.2003[[5]]) <- "eet"
names(splash.oneday.2003[[6]]) <- "pet"
names(splash.oneday.2003[[7]]) <- "aet"
names(splash.oneday.2003[[8]]) <- "wn"
names(splash.oneday.2003[[9]]) <- "ro"

sites.daily.2003 <- splash.oneday.2003$ho %>%
  coalesce(splash.oneday.2003$hn) %>%
  coalesce(splash.oneday.2003$ppfd) %>%
  coalesce(splash.oneday.2003$cond) %>%
  coalesce(splash.oneday.2003$eet) %>%
  coalesce(splash.oneday.2003$pet) %>%
  coalesce(splash.oneday.2003$aet) %>%
  coalesce(splash.oneday.2003$wn) %>%
  coalesce(splash.oneday.2003$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2003) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2004 data
###############################################################################
file.list.2004 <- list.files("../climate_data/splash_prep_files/splash_2004",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2004 <- setNames(file.list.2004, 
                           str_extract(basename(file.list.2004), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2004 <- lapply(file.list.2004, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2004$Brazos_2020_18.2004splash$sf <- splash.readin.2004$Brazos_2020_18.2004splash$sf[seq_along(splash.readin.2004$Brazos_2020_18.2004splash$sf) %% 2 > 0]
splash.readin.2004$Brazos_2020_18.2004splash$tair <- splash.readin.2004$Brazos_2020_18.2004splash$tair[seq_along(splash.readin.2004$Brazos_2020_18.2004splash$tair) %% 2 > 0]
splash.readin.2004$Brazos_2020_18.2004splash$pn <- splash.readin.2004$Brazos_2020_18.2004splash$pn[seq_along(splash.readin.2004$Brazos_2020_18.2004splash$pn) %% 2 > 0]
splash.readin.2004$Brazos_2020_18.2004splash$num_lines <- 366

# Harris 2020-03
splash.readin.2004$Harris_2020_03.2004splash$sf <- splash.readin.2004$Harris_2020_03.2004splash$sf[seq_along(splash.readin.2004$Harris_2020_03.2004splash$sf) %% 2 > 0]
splash.readin.2004$Harris_2020_03.2004splash$tair <- splash.readin.2004$Harris_2020_03.2004splash$tair[seq_along(splash.readin.2004$Harris_2020_03.2004splash$tair) %% 2 > 0]
splash.readin.2004$Harris_2020_03.2004splash$pn <- splash.readin.2004$Harris_2020_03.2004splash$pn[seq_along(splash.readin.2004$Harris_2020_03.2004splash$pn) %% 2 > 0]
splash.readin.2004$Harris_2020_03.2004splash$num_lines <- 366

# Menard 2020-01
splash.readin.2004$Menard_2020_01.2004splash$sf <- splash.readin.2004$Menard_2020_01.2004splash$sf[seq_along(splash.readin.2004$Menard_2020_01.2004splash$sf) %% 2 > 0]
splash.readin.2004$Menard_2020_01.2004splash$tair <- splash.readin.2004$Menard_2020_01.2004splash$tair[seq_along(splash.readin.2004$Menard_2020_01.2004splash$tair) %% 2 > 0]
splash.readin.2004$Menard_2020_01.2004splash$pn <- splash.readin.2004$Menard_2020_01.2004splash$pn[seq_along(splash.readin.2004$Menard_2020_01.2004splash$pn) %% 2 > 0]
splash.readin.2004$Menard_2020_01.2004splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.2004$Uvalde_2020_02.2004splash$sf <- splash.readin.2004$Uvalde_2020_02.2004splash$sf[seq_along(splash.readin.2004$Uvalde_2020_02.2004splash$sf) %% 2 > 0]
splash.readin.2004$Uvalde_2020_02.2004splash$tair <- splash.readin.2004$Uvalde_2020_02.2004splash$tair[seq_along(splash.readin.2004$Uvalde_2020_02.2004splash$tair) %% 2 > 0]
splash.readin.2004$Uvalde_2020_02.2004splash$pn <- splash.readin.2004$Uvalde_2020_02.2004splash$pn[seq_along(splash.readin.2004$Uvalde_2020_02.2004splash$pn) %% 2 > 0]
splash.readin.2004$Uvalde_2020_02.2004splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2004 <- lapply(file.list.2004, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2004[["Brazos_2020_18.2004splash"]] <- splash.month.2004[["Brazos_2020_18.2004splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2004[["Harris_2020_03.2004splash"]] <- splash.month.2004[["Harris_2020_03.2004splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2004[["Menard_2020_01.2004splash"]] <- splash.month.2004[["Menard_2020_01.2004splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2004[["Uvalde_2020_02.2004splash"]] <- splash.month.2004[["Uvalde_2020_02.2004splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2004)) {
  splash.month.2004[[i]] <- splash.month.2004[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2004 <- lapply(file.list.2004, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2004 <- mapply(c, splash.readin.2004, splash.month.2004, 
                            splash.ancillary.2004, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2004)) {
  
  splash.total.2004[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2004[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2004[[i]][["num_lines"]])
  splash.total.2004[[i]][["daily_totals"]] <- as.data.frame(splash.total.2004[[i]][["daily_totals"]])
  names(splash.total.2004[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2004[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2004[[i]],
                                                      dtot = splash.total.2004[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2004 <- map(splash.total.2004, as.data.table)
splash.water.2004 <- rbindlist(splash.water.2004, fill = TRUE, idcol = TRUE)
splash.water.2004$year <- 2004
splash.water.2004$site <- str_extract(splash.water.2004$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2004 <- splash.water.2004 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2004 <- tidyr::unite(splash.water.2004, "date", year:day, sep = "-", remove = FALSE)
splash.water.2004$date <- lubridate::ymd(as.Date(splash.water.2004$date))
splash.water.2004$doy <- lubridate::yday(splash.water.2004$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2004 <- run_one_day(lat = splash.water.2004$lat_deg,
                                  elv = splash.water.2004$elv_m,
                                  n = splash.water.2004$doy,
                                  y = splash.water.2004$year,
                                  wn = splash.water.2004$soil.moisture,
                                  sf = splash.water.2004$sf,
                                  tc = splash.water.2004$tair,
                                  pn = splash.water.2004$pn,
                                  kWm = 150)

splash.oneday.2004 <- map(splash.oneday.2004, as.data.table)

## Change list column names
names(splash.oneday.2004[[1]]) <- "ho"
names(splash.oneday.2004[[2]]) <- "hn"
names(splash.oneday.2004[[3]]) <- "ppfd"
names(splash.oneday.2004[[4]]) <- "cond"
names(splash.oneday.2004[[5]]) <- "eet"
names(splash.oneday.2004[[6]]) <- "pet"
names(splash.oneday.2004[[7]]) <- "aet"
names(splash.oneday.2004[[8]]) <- "wn"
names(splash.oneday.2004[[9]]) <- "ro"

sites.daily.2004 <- splash.oneday.2004$ho %>%
  coalesce(splash.oneday.2004$hn) %>%
  coalesce(splash.oneday.2004$ppfd) %>%
  coalesce(splash.oneday.2004$cond) %>%
  coalesce(splash.oneday.2004$eet) %>%
  coalesce(splash.oneday.2004$pet) %>%
  coalesce(splash.oneday.2004$aet) %>%
  coalesce(splash.oneday.2004$wn) %>%
  coalesce(splash.oneday.2004$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2004) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2005 data
###############################################################################
file.list.2005 <- list.files("../climate_data/splash_prep_files/splash_2005",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2005 <- setNames(file.list.2005, 
                           str_extract(basename(file.list.2005), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2005 <- lapply(file.list.2005, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2005$Brazos_2020_18.2005splash$sf <- splash.readin.2005$Brazos_2020_18.2005splash$sf[seq_along(splash.readin.2005$Brazos_2020_18.2005splash$sf) %% 2 > 0]
splash.readin.2005$Brazos_2020_18.2005splash$tair <- splash.readin.2005$Brazos_2020_18.2005splash$tair[seq_along(splash.readin.2005$Brazos_2020_18.2005splash$tair) %% 2 > 0]
splash.readin.2005$Brazos_2020_18.2005splash$pn <- splash.readin.2005$Brazos_2020_18.2005splash$pn[seq_along(splash.readin.2005$Brazos_2020_18.2005splash$pn) %% 2 > 0]
splash.readin.2005$Brazos_2020_18.2005splash$num_lines <- 365

# Harris 2020-03
splash.readin.2005$Harris_2020_03.2005splash$sf <- splash.readin.2005$Harris_2020_03.2005splash$sf[seq_along(splash.readin.2005$Harris_2020_03.2005splash$sf) %% 2 > 0]
splash.readin.2005$Harris_2020_03.2005splash$tair <- splash.readin.2005$Harris_2020_03.2005splash$tair[seq_along(splash.readin.2005$Harris_2020_03.2005splash$tair) %% 2 > 0]
splash.readin.2005$Harris_2020_03.2005splash$pn <- splash.readin.2005$Harris_2020_03.2005splash$pn[seq_along(splash.readin.2005$Harris_2020_03.2005splash$pn) %% 2 > 0]
splash.readin.2005$Harris_2020_03.2005splash$num_lines <- 365

# Menard 2020-01
splash.readin.2005$Menard_2020_01.2005splash$sf <- splash.readin.2005$Menard_2020_01.2005splash$sf[seq_along(splash.readin.2005$Menard_2020_01.2005splash$sf) %% 2 > 0]
splash.readin.2005$Menard_2020_01.2005splash$tair <- splash.readin.2005$Menard_2020_01.2005splash$tair[seq_along(splash.readin.2005$Menard_2020_01.2005splash$tair) %% 2 > 0]
splash.readin.2005$Menard_2020_01.2005splash$pn <- splash.readin.2005$Menard_2020_01.2005splash$pn[seq_along(splash.readin.2005$Menard_2020_01.2005splash$pn) %% 2 > 0]
splash.readin.2005$Menard_2020_01.2005splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2005$Uvalde_2020_02.2005splash$sf <- splash.readin.2005$Uvalde_2020_02.2005splash$sf[seq_along(splash.readin.2005$Uvalde_2020_02.2005splash$sf) %% 2 > 0]
splash.readin.2005$Uvalde_2020_02.2005splash$tair <- splash.readin.2005$Uvalde_2020_02.2005splash$tair[seq_along(splash.readin.2005$Uvalde_2020_02.2005splash$tair) %% 2 > 0]
splash.readin.2005$Uvalde_2020_02.2005splash$pn <- splash.readin.2005$Uvalde_2020_02.2005splash$pn[seq_along(splash.readin.2005$Uvalde_2020_02.2005splash$pn) %% 2 > 0]
splash.readin.2005$Uvalde_2020_02.2005splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2005 <- lapply(file.list.2005, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2005[["Brazos_2020_18.2005splash"]] <- splash.month.2005[["Brazos_2020_18.2005splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2005[["Harris_2020_03.2005splash"]] <- splash.month.2005[["Harris_2020_03.2005splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2005[["Menard_2020_01.2005splash"]] <- splash.month.2005[["Menard_2020_01.2005splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2005[["Uvalde_2020_02.2005splash"]] <- splash.month.2005[["Uvalde_2020_02.2005splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2005)) {
  splash.month.2005[[i]] <- splash.month.2005[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2005 <- lapply(file.list.2005, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2005 <- mapply(c, splash.readin.2005, splash.month.2005, 
                            splash.ancillary.2005, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2005)) {
  
  splash.total.2005[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2005[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2005[[i]][["num_lines"]])
  splash.total.2005[[i]][["daily_totals"]] <- as.data.frame(splash.total.2005[[i]][["daily_totals"]])
  names(splash.total.2005[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2005[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2005[[i]],
                                                      dtot = splash.total.2005[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2005 <- map(splash.total.2005, as.data.table)
splash.water.2005 <- rbindlist(splash.water.2005, fill = TRUE, idcol = TRUE)
splash.water.2005$year <- 2005
splash.water.2005$site <- str_extract(splash.water.2005$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2005 <- splash.water.2005 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2005 <- tidyr::unite(splash.water.2005, "date", year:day, sep = "-", remove = FALSE)
splash.water.2005$date <- lubridate::ymd(as.Date(splash.water.2005$date))
splash.water.2005$doy <- lubridate::yday(splash.water.2005$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2005 <- run_one_day(lat = splash.water.2005$lat_deg,
                                  elv = splash.water.2005$elv_m,
                                  n = splash.water.2005$doy,
                                  y = splash.water.2005$year,
                                  wn = splash.water.2005$soil.moisture,
                                  sf = splash.water.2005$sf,
                                  tc = splash.water.2005$tair,
                                  pn = splash.water.2005$pn,
                                  kWm = 150)

splash.oneday.2005 <- map(splash.oneday.2005, as.data.table)

## Change list column names
names(splash.oneday.2005[[1]]) <- "ho"
names(splash.oneday.2005[[2]]) <- "hn"
names(splash.oneday.2005[[3]]) <- "ppfd"
names(splash.oneday.2005[[4]]) <- "cond"
names(splash.oneday.2005[[5]]) <- "eet"
names(splash.oneday.2005[[6]]) <- "pet"
names(splash.oneday.2005[[7]]) <- "aet"
names(splash.oneday.2005[[8]]) <- "wn"
names(splash.oneday.2005[[9]]) <- "ro"

sites.daily.2005 <- splash.oneday.2005$ho %>%
  coalesce(splash.oneday.2005$hn) %>%
  coalesce(splash.oneday.2005$ppfd) %>%
  coalesce(splash.oneday.2005$cond) %>%
  coalesce(splash.oneday.2005$eet) %>%
  coalesce(splash.oneday.2005$pet) %>%
  coalesce(splash.oneday.2005$aet) %>%
  coalesce(splash.oneday.2005$wn) %>%
  coalesce(splash.oneday.2005$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2005) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2006 data
###############################################################################
file.list.2006 <- list.files("../climate_data/splash_prep_files/splash_2006",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2006 <- setNames(file.list.2006, 
                           str_extract(basename(file.list.2006), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2006 <- lapply(file.list.2006, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2006$Brazos_2020_18.2006splash$sf <- splash.readin.2006$Brazos_2020_18.2006splash$sf[seq_along(splash.readin.2006$Brazos_2020_18.2006splash$sf) %% 2 > 0]
splash.readin.2006$Brazos_2020_18.2006splash$tair <- splash.readin.2006$Brazos_2020_18.2006splash$tair[seq_along(splash.readin.2006$Brazos_2020_18.2006splash$tair) %% 2 > 0]
splash.readin.2006$Brazos_2020_18.2006splash$pn <- splash.readin.2006$Brazos_2020_18.2006splash$pn[seq_along(splash.readin.2006$Brazos_2020_18.2006splash$pn) %% 2 > 0]
splash.readin.2006$Brazos_2020_18.2006splash$num_lines <- 365

# Harris 2020-03
splash.readin.2006$Harris_2020_03.2006splash$sf <- splash.readin.2006$Harris_2020_03.2006splash$sf[seq_along(splash.readin.2006$Harris_2020_03.2006splash$sf) %% 2 > 0]
splash.readin.2006$Harris_2020_03.2006splash$tair <- splash.readin.2006$Harris_2020_03.2006splash$tair[seq_along(splash.readin.2006$Harris_2020_03.2006splash$tair) %% 2 > 0]
splash.readin.2006$Harris_2020_03.2006splash$pn <- splash.readin.2006$Harris_2020_03.2006splash$pn[seq_along(splash.readin.2006$Harris_2020_03.2006splash$pn) %% 2 > 0]
splash.readin.2006$Harris_2020_03.2006splash$num_lines <- 365

# Menard 2020-01
splash.readin.2006$Menard_2020_01.2006splash$sf <- splash.readin.2006$Menard_2020_01.2006splash$sf[seq_along(splash.readin.2006$Menard_2020_01.2006splash$sf) %% 2 > 0]
splash.readin.2006$Menard_2020_01.2006splash$tair <- splash.readin.2006$Menard_2020_01.2006splash$tair[seq_along(splash.readin.2006$Menard_2020_01.2006splash$tair) %% 2 > 0]
splash.readin.2006$Menard_2020_01.2006splash$pn <- splash.readin.2006$Menard_2020_01.2006splash$pn[seq_along(splash.readin.2006$Menard_2020_01.2006splash$pn) %% 2 > 0]
splash.readin.2006$Menard_2020_01.2006splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2006$Uvalde_2020_02.2006splash$sf <- splash.readin.2006$Uvalde_2020_02.2006splash$sf[seq_along(splash.readin.2006$Uvalde_2020_02.2006splash$sf) %% 2 > 0]
splash.readin.2006$Uvalde_2020_02.2006splash$tair <- splash.readin.2006$Uvalde_2020_02.2006splash$tair[seq_along(splash.readin.2006$Uvalde_2020_02.2006splash$tair) %% 2 > 0]
splash.readin.2006$Uvalde_2020_02.2006splash$pn <- splash.readin.2006$Uvalde_2020_02.2006splash$pn[seq_along(splash.readin.2006$Uvalde_2020_02.2006splash$pn) %% 2 > 0]
splash.readin.2006$Uvalde_2020_02.2006splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2006 <- lapply(file.list.2006, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2006[["Brazos_2020_18.2006splash"]] <- splash.month.2006[["Brazos_2020_18.2006splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2006[["Harris_2020_03.2006splash"]] <- splash.month.2006[["Harris_2020_03.2006splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2006[["Menard_2020_01.2006splash"]] <- splash.month.2006[["Menard_2020_01.2006splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2006[["Uvalde_2020_02.2006splash"]] <- splash.month.2006[["Uvalde_2020_02.2006splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2006)) {
  splash.month.2006[[i]] <- splash.month.2006[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2006 <- lapply(file.list.2006, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2006 <- mapply(c, splash.readin.2006, splash.month.2006, 
                            splash.ancillary.2006, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2006)) {
  
  splash.total.2006[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2006[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2006[[i]][["num_lines"]])
  splash.total.2006[[i]][["daily_totals"]] <- as.data.frame(splash.total.2006[[i]][["daily_totals"]])
  names(splash.total.2006[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2006[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2006[[i]],
                                                      dtot = splash.total.2006[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2006 <- map(splash.total.2006, as.data.table)
splash.water.2006 <- rbindlist(splash.water.2006, fill = TRUE, idcol = TRUE)
splash.water.2006$year <- 2006
splash.water.2006$site <- str_extract(splash.water.2006$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2006 <- splash.water.2006 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2006 <- tidyr::unite(splash.water.2006, "date", year:day, sep = "-", remove = FALSE)
splash.water.2006$date <- lubridate::ymd(as.Date(splash.water.2006$date))
splash.water.2006$doy <- lubridate::yday(splash.water.2006$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2006 <- run_one_day(lat = splash.water.2006$lat_deg,
                                  elv = splash.water.2006$elv_m,
                                  n = splash.water.2006$doy,
                                  y = splash.water.2006$year,
                                  wn = splash.water.2006$soil.moisture,
                                  sf = splash.water.2006$sf,
                                  tc = splash.water.2006$tair,
                                  pn = splash.water.2006$pn,
                                  kWm = 150)

splash.oneday.2006 <- map(splash.oneday.2006, as.data.table)

## Change list column names
names(splash.oneday.2006[[1]]) <- "ho"
names(splash.oneday.2006[[2]]) <- "hn"
names(splash.oneday.2006[[3]]) <- "ppfd"
names(splash.oneday.2006[[4]]) <- "cond"
names(splash.oneday.2006[[5]]) <- "eet"
names(splash.oneday.2006[[6]]) <- "pet"
names(splash.oneday.2006[[7]]) <- "aet"
names(splash.oneday.2006[[8]]) <- "wn"
names(splash.oneday.2006[[9]]) <- "ro"

sites.daily.2006 <- splash.oneday.2006$ho %>%
  coalesce(splash.oneday.2006$hn) %>%
  coalesce(splash.oneday.2006$ppfd) %>%
  coalesce(splash.oneday.2006$cond) %>%
  coalesce(splash.oneday.2006$eet) %>%
  coalesce(splash.oneday.2006$pet) %>%
  coalesce(splash.oneday.2006$aet) %>%
  coalesce(splash.oneday.2006$wn) %>%
  coalesce(splash.oneday.2006$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2006) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2007 data
###############################################################################
file.list.2007 <- list.files("../climate_data/splash_prep_files/splash_2007",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2007 <- setNames(file.list.2007, 
                           str_extract(basename(file.list.2007), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2007 <- lapply(file.list.2007, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2007$Brazos_2020_18.2007splash$sf <- splash.readin.2007$Brazos_2020_18.2007splash$sf[seq_along(splash.readin.2007$Brazos_2020_18.2007splash$sf) %% 2 > 0]
splash.readin.2007$Brazos_2020_18.2007splash$tair <- splash.readin.2007$Brazos_2020_18.2007splash$tair[seq_along(splash.readin.2007$Brazos_2020_18.2007splash$tair) %% 2 > 0]
splash.readin.2007$Brazos_2020_18.2007splash$pn <- splash.readin.2007$Brazos_2020_18.2007splash$pn[seq_along(splash.readin.2007$Brazos_2020_18.2007splash$pn) %% 2 > 0]
splash.readin.2007$Brazos_2020_18.2007splash$num_lines <- 365

# Harris 2020-03
splash.readin.2007$Harris_2020_03.2007splash$sf <- splash.readin.2007$Harris_2020_03.2007splash$sf[seq_along(splash.readin.2007$Harris_2020_03.2007splash$sf) %% 2 > 0]
splash.readin.2007$Harris_2020_03.2007splash$tair <- splash.readin.2007$Harris_2020_03.2007splash$tair[seq_along(splash.readin.2007$Harris_2020_03.2007splash$tair) %% 2 > 0]
splash.readin.2007$Harris_2020_03.2007splash$pn <- splash.readin.2007$Harris_2020_03.2007splash$pn[seq_along(splash.readin.2007$Harris_2020_03.2007splash$pn) %% 2 > 0]
splash.readin.2007$Harris_2020_03.2007splash$num_lines <- 365

# Menard 2020-01
splash.readin.2007$Menard_2020_01.2007splash$sf <- splash.readin.2007$Menard_2020_01.2007splash$sf[seq_along(splash.readin.2007$Menard_2020_01.2007splash$sf) %% 2 > 0]
splash.readin.2007$Menard_2020_01.2007splash$tair <- splash.readin.2007$Menard_2020_01.2007splash$tair[seq_along(splash.readin.2007$Menard_2020_01.2007splash$tair) %% 2 > 0]
splash.readin.2007$Menard_2020_01.2007splash$pn <- splash.readin.2007$Menard_2020_01.2007splash$pn[seq_along(splash.readin.2007$Menard_2020_01.2007splash$pn) %% 2 > 0]
splash.readin.2007$Menard_2020_01.2007splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2007$Uvalde_2020_02.2007splash$sf <- splash.readin.2007$Uvalde_2020_02.2007splash$sf[seq_along(splash.readin.2007$Uvalde_2020_02.2007splash$sf) %% 2 > 0]
splash.readin.2007$Uvalde_2020_02.2007splash$tair <- splash.readin.2007$Uvalde_2020_02.2007splash$tair[seq_along(splash.readin.2007$Uvalde_2020_02.2007splash$tair) %% 2 > 0]
splash.readin.2007$Uvalde_2020_02.2007splash$pn <- splash.readin.2007$Uvalde_2020_02.2007splash$pn[seq_along(splash.readin.2007$Uvalde_2020_02.2007splash$pn) %% 2 > 0]
splash.readin.2007$Uvalde_2020_02.2007splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2007 <- lapply(file.list.2007, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2007[["Brazos_2020_18.2007splash"]] <- splash.month.2007[["Brazos_2020_18.2007splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2007[["Harris_2020_03.2007splash"]] <- splash.month.2007[["Harris_2020_03.2007splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2007[["Menard_2020_01.2007splash"]] <- splash.month.2007[["Menard_2020_01.2007splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2007[["Uvalde_2020_02.2007splash"]] <- splash.month.2007[["Uvalde_2020_02.2007splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2007)) {
  splash.month.2007[[i]] <- splash.month.2007[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2007 <- lapply(file.list.2007, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2007 <- mapply(c, splash.readin.2007, splash.month.2007, 
                            splash.ancillary.2007, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2007)) {
  
  splash.total.2007[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2007[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2007[[i]][["num_lines"]])
  splash.total.2007[[i]][["daily_totals"]] <- as.data.frame(splash.total.2007[[i]][["daily_totals"]])
  names(splash.total.2007[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2007[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2007[[i]],
                                                      dtot = splash.total.2007[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2007 <- map(splash.total.2007, as.data.table)
splash.water.2007 <- rbindlist(splash.water.2007, fill = TRUE, idcol = TRUE)
splash.water.2007$year <- 2007
splash.water.2007$site <- str_extract(splash.water.2007$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2007 <- splash.water.2007 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2007 <- tidyr::unite(splash.water.2007, "date", year:day, sep = "-", remove = FALSE)
splash.water.2007$date <- lubridate::ymd(as.Date(splash.water.2007$date))
splash.water.2007$doy <- lubridate::yday(splash.water.2007$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2007 <- run_one_day(lat = splash.water.2007$lat_deg,
                                  elv = splash.water.2007$elv_m,
                                  n = splash.water.2007$doy,
                                  y = splash.water.2007$year,
                                  wn = splash.water.2007$soil.moisture,
                                  sf = splash.water.2007$sf,
                                  tc = splash.water.2007$tair,
                                  pn = splash.water.2007$pn,
                                  kWm = 150)

splash.oneday.2007 <- map(splash.oneday.2007, as.data.table)

## Change list column names
names(splash.oneday.2007[[1]]) <- "ho"
names(splash.oneday.2007[[2]]) <- "hn"
names(splash.oneday.2007[[3]]) <- "ppfd"
names(splash.oneday.2007[[4]]) <- "cond"
names(splash.oneday.2007[[5]]) <- "eet"
names(splash.oneday.2007[[6]]) <- "pet"
names(splash.oneday.2007[[7]]) <- "aet"
names(splash.oneday.2007[[8]]) <- "wn"
names(splash.oneday.2007[[9]]) <- "ro"

sites.daily.2007 <- splash.oneday.2007$ho %>%
  coalesce(splash.oneday.2007$hn) %>%
  coalesce(splash.oneday.2007$ppfd) %>%
  coalesce(splash.oneday.2007$cond) %>%
  coalesce(splash.oneday.2007$eet) %>%
  coalesce(splash.oneday.2007$pet) %>%
  coalesce(splash.oneday.2007$aet) %>%
  coalesce(splash.oneday.2007$wn) %>%
  coalesce(splash.oneday.2007$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2007) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2008 data
###############################################################################
file.list.2008 <- list.files("../climate_data/splash_prep_files/splash_2008",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2008 <- setNames(file.list.2008, 
                           str_extract(basename(file.list.2008), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2008 <- lapply(file.list.2008, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2008$Brazos_2020_18.2008splash$sf <- splash.readin.2008$Brazos_2020_18.2008splash$sf[seq_along(splash.readin.2008$Brazos_2020_18.2008splash$sf) %% 2 > 0]
splash.readin.2008$Brazos_2020_18.2008splash$tair <- splash.readin.2008$Brazos_2020_18.2008splash$tair[seq_along(splash.readin.2008$Brazos_2020_18.2008splash$tair) %% 2 > 0]
splash.readin.2008$Brazos_2020_18.2008splash$pn <- splash.readin.2008$Brazos_2020_18.2008splash$pn[seq_along(splash.readin.2008$Brazos_2020_18.2008splash$pn) %% 2 > 0]
splash.readin.2008$Brazos_2020_18.2008splash$num_lines <- 366

# Harris 2020-03
splash.readin.2008$Harris_2020_03.2008splash$sf <- splash.readin.2008$Harris_2020_03.2008splash$sf[seq_along(splash.readin.2008$Harris_2020_03.2008splash$sf) %% 2 > 0]
splash.readin.2008$Harris_2020_03.2008splash$tair <- splash.readin.2008$Harris_2020_03.2008splash$tair[seq_along(splash.readin.2008$Harris_2020_03.2008splash$tair) %% 2 > 0]
splash.readin.2008$Harris_2020_03.2008splash$pn <- splash.readin.2008$Harris_2020_03.2008splash$pn[seq_along(splash.readin.2008$Harris_2020_03.2008splash$pn) %% 2 > 0]
splash.readin.2008$Harris_2020_03.2008splash$num_lines <- 366

# Menard 2020-01
splash.readin.2008$Menard_2020_01.2008splash$sf <- splash.readin.2008$Menard_2020_01.2008splash$sf[seq_along(splash.readin.2008$Menard_2020_01.2008splash$sf) %% 2 > 0]
splash.readin.2008$Menard_2020_01.2008splash$tair <- splash.readin.2008$Menard_2020_01.2008splash$tair[seq_along(splash.readin.2008$Menard_2020_01.2008splash$tair) %% 2 > 0]
splash.readin.2008$Menard_2020_01.2008splash$pn <- splash.readin.2008$Menard_2020_01.2008splash$pn[seq_along(splash.readin.2008$Menard_2020_01.2008splash$pn) %% 2 > 0]
splash.readin.2008$Menard_2020_01.2008splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.2008$Uvalde_2020_02.2008splash$sf <- splash.readin.2008$Uvalde_2020_02.2008splash$sf[seq_along(splash.readin.2008$Uvalde_2020_02.2008splash$sf) %% 2 > 0]
splash.readin.2008$Uvalde_2020_02.2008splash$tair <- splash.readin.2008$Uvalde_2020_02.2008splash$tair[seq_along(splash.readin.2008$Uvalde_2020_02.2008splash$tair) %% 2 > 0]
splash.readin.2008$Uvalde_2020_02.2008splash$pn <- splash.readin.2008$Uvalde_2020_02.2008splash$pn[seq_along(splash.readin.2008$Uvalde_2020_02.2008splash$pn) %% 2 > 0]
splash.readin.2008$Uvalde_2020_02.2008splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2008 <- lapply(file.list.2008, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2008[["Brazos_2020_18.2008splash"]] <- splash.month.2008[["Brazos_2020_18.2008splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2008[["Harris_2020_03.2008splash"]] <- splash.month.2008[["Harris_2020_03.2008splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2008[["Menard_2020_01.2008splash"]] <- splash.month.2008[["Menard_2020_01.2008splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2008[["Uvalde_2020_02.2008splash"]] <- splash.month.2008[["Uvalde_2020_02.2008splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2008)) {
  splash.month.2008[[i]] <- splash.month.2008[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2008 <- lapply(file.list.2008, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2008 <- mapply(c, splash.readin.2008, splash.month.2008, 
                            splash.ancillary.2008, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2008)) {
  
  splash.total.2008[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2008[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2008[[i]][["num_lines"]])
  splash.total.2008[[i]][["daily_totals"]] <- as.data.frame(splash.total.2008[[i]][["daily_totals"]])
  names(splash.total.2008[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2008[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2008[[i]],
                                                      dtot = splash.total.2008[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2008 <- map(splash.total.2008, as.data.table)
splash.water.2008 <- rbindlist(splash.water.2008, fill = TRUE, idcol = TRUE)
splash.water.2008$year <- 2008
splash.water.2008$site <- str_extract(splash.water.2008$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2008 <- splash.water.2008 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2008 <- tidyr::unite(splash.water.2008, "date", year:day, sep = "-", remove = FALSE)
splash.water.2008$date <- lubridate::ymd(as.Date(splash.water.2008$date))
splash.water.2008$doy <- lubridate::yday(splash.water.2008$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2008 <- run_one_day(lat = splash.water.2008$lat_deg,
                                  elv = splash.water.2008$elv_m,
                                  n = splash.water.2008$doy,
                                  y = splash.water.2008$year,
                                  wn = splash.water.2008$soil.moisture,
                                  sf = splash.water.2008$sf,
                                  tc = splash.water.2008$tair,
                                  pn = splash.water.2008$pn,
                                  kWm = 150)

splash.oneday.2008 <- map(splash.oneday.2008, as.data.table)

## Change list column names
names(splash.oneday.2008[[1]]) <- "ho"
names(splash.oneday.2008[[2]]) <- "hn"
names(splash.oneday.2008[[3]]) <- "ppfd"
names(splash.oneday.2008[[4]]) <- "cond"
names(splash.oneday.2008[[5]]) <- "eet"
names(splash.oneday.2008[[6]]) <- "pet"
names(splash.oneday.2008[[7]]) <- "aet"
names(splash.oneday.2008[[8]]) <- "wn"
names(splash.oneday.2008[[9]]) <- "ro"

sites.daily.2008 <- splash.oneday.2008$ho %>%
  coalesce(splash.oneday.2008$hn) %>%
  coalesce(splash.oneday.2008$ppfd) %>%
  coalesce(splash.oneday.2008$cond) %>%
  coalesce(splash.oneday.2008$eet) %>%
  coalesce(splash.oneday.2008$pet) %>%
  coalesce(splash.oneday.2008$aet) %>%
  coalesce(splash.oneday.2008$wn) %>%
  coalesce(splash.oneday.2008$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2008) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2009 data
###############################################################################
file.list.2009 <- list.files("../climate_data/splash_prep_files/splash_2009",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2009 <- setNames(file.list.2009, 
                           str_extract(basename(file.list.2009), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2009 <- lapply(file.list.2009, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2009$Brazos_2020_18.2009splash$sf <- splash.readin.2009$Brazos_2020_18.2009splash$sf[seq_along(splash.readin.2009$Brazos_2020_18.2009splash$sf) %% 2 > 0]
splash.readin.2009$Brazos_2020_18.2009splash$tair <- splash.readin.2009$Brazos_2020_18.2009splash$tair[seq_along(splash.readin.2009$Brazos_2020_18.2009splash$tair) %% 2 > 0]
splash.readin.2009$Brazos_2020_18.2009splash$pn <- splash.readin.2009$Brazos_2020_18.2009splash$pn[seq_along(splash.readin.2009$Brazos_2020_18.2009splash$pn) %% 2 > 0]
splash.readin.2009$Brazos_2020_18.2009splash$num_lines <- 365

# Harris 2020-03
splash.readin.2009$Harris_2020_03.2009splash$sf <- splash.readin.2009$Harris_2020_03.2009splash$sf[seq_along(splash.readin.2009$Harris_2020_03.2009splash$sf) %% 2 > 0]
splash.readin.2009$Harris_2020_03.2009splash$tair <- splash.readin.2009$Harris_2020_03.2009splash$tair[seq_along(splash.readin.2009$Harris_2020_03.2009splash$tair) %% 2 > 0]
splash.readin.2009$Harris_2020_03.2009splash$pn <- splash.readin.2009$Harris_2020_03.2009splash$pn[seq_along(splash.readin.2009$Harris_2020_03.2009splash$pn) %% 2 > 0]
splash.readin.2009$Harris_2020_03.2009splash$num_lines <- 365

# Menard 2020-01
splash.readin.2009$Menard_2020_01.2009splash$sf <- splash.readin.2009$Menard_2020_01.2009splash$sf[seq_along(splash.readin.2009$Menard_2020_01.2009splash$sf) %% 2 > 0]
splash.readin.2009$Menard_2020_01.2009splash$tair <- splash.readin.2009$Menard_2020_01.2009splash$tair[seq_along(splash.readin.2009$Menard_2020_01.2009splash$tair) %% 2 > 0]
splash.readin.2009$Menard_2020_01.2009splash$pn <- splash.readin.2009$Menard_2020_01.2009splash$pn[seq_along(splash.readin.2009$Menard_2020_01.2009splash$pn) %% 2 > 0]
splash.readin.2009$Menard_2020_01.2009splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2009$Uvalde_2020_02.2009splash$sf <- splash.readin.2009$Uvalde_2020_02.2009splash$sf[seq_along(splash.readin.2009$Uvalde_2020_02.2009splash$sf) %% 2 > 0]
splash.readin.2009$Uvalde_2020_02.2009splash$tair <- splash.readin.2009$Uvalde_2020_02.2009splash$tair[seq_along(splash.readin.2009$Uvalde_2020_02.2009splash$tair) %% 2 > 0]
splash.readin.2009$Uvalde_2020_02.2009splash$pn <- splash.readin.2009$Uvalde_2020_02.2009splash$pn[seq_along(splash.readin.2009$Uvalde_2020_02.2009splash$pn) %% 2 > 0]
splash.readin.2009$Uvalde_2020_02.2009splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2009 <- lapply(file.list.2009, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2009[["Brazos_2020_18.2009splash"]] <- splash.month.2009[["Brazos_2020_18.2009splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2009[["Harris_2020_03.2009splash"]] <- splash.month.2009[["Harris_2020_03.2009splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2009[["Menard_2020_01.2009splash"]] <- splash.month.2009[["Menard_2020_01.2009splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2009[["Uvalde_2020_02.2009splash"]] <- splash.month.2009[["Uvalde_2020_02.2009splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2009)) {
  splash.month.2009[[i]] <- splash.month.2009[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2009 <- lapply(file.list.2009, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2009 <- mapply(c, splash.readin.2009, splash.month.2009, 
                            splash.ancillary.2009, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2009)) {
  
  splash.total.2009[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2009[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2009[[i]][["num_lines"]])
  splash.total.2009[[i]][["daily_totals"]] <- as.data.frame(splash.total.2009[[i]][["daily_totals"]])
  names(splash.total.2009[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2009[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2009[[i]],
                                                      dtot = splash.total.2009[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2009 <- map(splash.total.2009, as.data.table)
splash.water.2009 <- rbindlist(splash.water.2009, fill = TRUE, idcol = TRUE)
splash.water.2009$year <- 2009
splash.water.2009$site <- str_extract(splash.water.2009$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2009 <- splash.water.2009 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2009 <- tidyr::unite(splash.water.2009, "date", year:day, sep = "-", remove = FALSE)
splash.water.2009$date <- lubridate::ymd(as.Date(splash.water.2009$date))
splash.water.2009$doy <- lubridate::yday(splash.water.2009$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2009 <- run_one_day(lat = splash.water.2009$lat_deg,
                                  elv = splash.water.2009$elv_m,
                                  n = splash.water.2009$doy,
                                  y = splash.water.2009$year,
                                  wn = splash.water.2009$soil.moisture,
                                  sf = splash.water.2009$sf,
                                  tc = splash.water.2009$tair,
                                  pn = splash.water.2009$pn,
                                  kWm = 150)

splash.oneday.2009 <- map(splash.oneday.2009, as.data.table)

## Change list column names
names(splash.oneday.2009[[1]]) <- "ho"
names(splash.oneday.2009[[2]]) <- "hn"
names(splash.oneday.2009[[3]]) <- "ppfd"
names(splash.oneday.2009[[4]]) <- "cond"
names(splash.oneday.2009[[5]]) <- "eet"
names(splash.oneday.2009[[6]]) <- "pet"
names(splash.oneday.2009[[7]]) <- "aet"
names(splash.oneday.2009[[8]]) <- "wn"
names(splash.oneday.2009[[9]]) <- "ro"

sites.daily.2009 <- splash.oneday.2009$ho %>%
  coalesce(splash.oneday.2009$hn) %>%
  coalesce(splash.oneday.2009$ppfd) %>%
  coalesce(splash.oneday.2009$cond) %>%
  coalesce(splash.oneday.2009$eet) %>%
  coalesce(splash.oneday.2009$pet) %>%
  coalesce(splash.oneday.2009$aet) %>%
  coalesce(splash.oneday.2009$wn) %>%
  coalesce(splash.oneday.2009$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2009) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2010 data
###############################################################################
file.list.2010 <- list.files("../climate_data/splash_prep_files/splash_2010",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2010 <- setNames(file.list.2010, 
                           str_extract(basename(file.list.2010), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2010 <- lapply(file.list.2010, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2010$Brazos_2020_18.2010splash$sf <- splash.readin.2010$Brazos_2020_18.2010splash$sf[seq_along(splash.readin.2010$Brazos_2020_18.2010splash$sf) %% 2 > 0]
splash.readin.2010$Brazos_2020_18.2010splash$tair <- splash.readin.2010$Brazos_2020_18.2010splash$tair[seq_along(splash.readin.2010$Brazos_2020_18.2010splash$tair) %% 2 > 0]
splash.readin.2010$Brazos_2020_18.2010splash$pn <- splash.readin.2010$Brazos_2020_18.2010splash$pn[seq_along(splash.readin.2010$Brazos_2020_18.2010splash$pn) %% 2 > 0]
splash.readin.2010$Brazos_2020_18.2010splash$num_lines <- 365

# Harris 2020-03
splash.readin.2010$Harris_2020_03.2010splash$sf <- splash.readin.2010$Harris_2020_03.2010splash$sf[seq_along(splash.readin.2010$Harris_2020_03.2010splash$sf) %% 2 > 0]
splash.readin.2010$Harris_2020_03.2010splash$tair <- splash.readin.2010$Harris_2020_03.2010splash$tair[seq_along(splash.readin.2010$Harris_2020_03.2010splash$tair) %% 2 > 0]
splash.readin.2010$Harris_2020_03.2010splash$pn <- splash.readin.2010$Harris_2020_03.2010splash$pn[seq_along(splash.readin.2010$Harris_2020_03.2010splash$pn) %% 2 > 0]
splash.readin.2010$Harris_2020_03.2010splash$num_lines <- 365

# Menard 2020-01
splash.readin.2010$Menard_2020_01.2010splash$sf <- splash.readin.2010$Menard_2020_01.2010splash$sf[seq_along(splash.readin.2010$Menard_2020_01.2010splash$sf) %% 2 > 0]
splash.readin.2010$Menard_2020_01.2010splash$tair <- splash.readin.2010$Menard_2020_01.2010splash$tair[seq_along(splash.readin.2010$Menard_2020_01.2010splash$tair) %% 2 > 0]
splash.readin.2010$Menard_2020_01.2010splash$pn <- splash.readin.2010$Menard_2020_01.2010splash$pn[seq_along(splash.readin.2010$Menard_2020_01.2010splash$pn) %% 2 > 0]
splash.readin.2010$Menard_2020_01.2010splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2010$Uvalde_2020_02.2010splash$sf <- splash.readin.2010$Uvalde_2020_02.2010splash$sf[seq_along(splash.readin.2010$Uvalde_2020_02.2010splash$sf) %% 2 > 0]
splash.readin.2010$Uvalde_2020_02.2010splash$tair <- splash.readin.2010$Uvalde_2020_02.2010splash$tair[seq_along(splash.readin.2010$Uvalde_2020_02.2010splash$tair) %% 2 > 0]
splash.readin.2010$Uvalde_2020_02.2010splash$pn <- splash.readin.2010$Uvalde_2020_02.2010splash$pn[seq_along(splash.readin.2010$Uvalde_2020_02.2010splash$pn) %% 2 > 0]
splash.readin.2010$Uvalde_2020_02.2010splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2010 <- lapply(file.list.2010, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2010[["Brazos_2020_18.2010splash"]] <- splash.month.2010[["Brazos_2020_18.2010splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2010[["Harris_2020_03.2010splash"]] <- splash.month.2010[["Harris_2020_03.2010splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2010[["Menard_2020_01.2010splash"]] <- splash.month.2010[["Menard_2020_01.2010splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2010[["Uvalde_2020_02.2010splash"]] <- splash.month.2010[["Uvalde_2020_02.2010splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2010)) {
  splash.month.2010[[i]] <- splash.month.2010[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2010 <- lapply(file.list.2010, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2010 <- mapply(c, splash.readin.2010, splash.month.2010, 
                            splash.ancillary.2010, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2010)) {
  
  splash.total.2010[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2010[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2010[[i]][["num_lines"]])
  splash.total.2010[[i]][["daily_totals"]] <- as.data.frame(splash.total.2010[[i]][["daily_totals"]])
  names(splash.total.2010[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2010[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2010[[i]],
                                                      dtot = splash.total.2010[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2010 <- map(splash.total.2010, as.data.table)
splash.water.2010 <- rbindlist(splash.water.2010, fill = TRUE, idcol = TRUE)
splash.water.2010$year <- 2010
splash.water.2010$site <- str_extract(splash.water.2010$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2010 <- splash.water.2010 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2010 <- tidyr::unite(splash.water.2010, "date", year:day, sep = "-", remove = FALSE)
splash.water.2010$date <- lubridate::ymd(as.Date(splash.water.2010$date))
splash.water.2010$doy <- lubridate::yday(splash.water.2010$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2010 <- run_one_day(lat = splash.water.2010$lat_deg,
                                  elv = splash.water.2010$elv_m,
                                  n = splash.water.2010$doy,
                                  y = splash.water.2010$year,
                                  wn = splash.water.2010$soil.moisture,
                                  sf = splash.water.2010$sf,
                                  tc = splash.water.2010$tair,
                                  pn = splash.water.2010$pn,
                                  kWm = 150)

splash.oneday.2010 <- map(splash.oneday.2010, as.data.table)

## Change list column names
names(splash.oneday.2010[[1]]) <- "ho"
names(splash.oneday.2010[[2]]) <- "hn"
names(splash.oneday.2010[[3]]) <- "ppfd"
names(splash.oneday.2010[[4]]) <- "cond"
names(splash.oneday.2010[[5]]) <- "eet"
names(splash.oneday.2010[[6]]) <- "pet"
names(splash.oneday.2010[[7]]) <- "aet"
names(splash.oneday.2010[[8]]) <- "wn"
names(splash.oneday.2010[[9]]) <- "ro"

sites.daily.2010 <- splash.oneday.2010$ho %>%
  coalesce(splash.oneday.2010$hn) %>%
  coalesce(splash.oneday.2010$ppfd) %>%
  coalesce(splash.oneday.2010$cond) %>%
  coalesce(splash.oneday.2010$eet) %>%
  coalesce(splash.oneday.2010$pet) %>%
  coalesce(splash.oneday.2010$aet) %>%
  coalesce(splash.oneday.2010$wn) %>%
  coalesce(splash.oneday.2010$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2010) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2011 data
###############################################################################
file.list.2011 <- list.files("../climate_data/splash_prep_files/splash_2011",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2011 <- setNames(file.list.2011, 
                           str_extract(basename(file.list.2011), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2011 <- lapply(file.list.2011, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2011$Brazos_2020_18.2011splash$sf <- splash.readin.2011$Brazos_2020_18.2011splash$sf[seq_along(splash.readin.2011$Brazos_2020_18.2011splash$sf) %% 2 > 0]
splash.readin.2011$Brazos_2020_18.2011splash$tair <- splash.readin.2011$Brazos_2020_18.2011splash$tair[seq_along(splash.readin.2011$Brazos_2020_18.2011splash$tair) %% 2 > 0]
splash.readin.2011$Brazos_2020_18.2011splash$pn <- splash.readin.2011$Brazos_2020_18.2011splash$pn[seq_along(splash.readin.2011$Brazos_2020_18.2011splash$pn) %% 2 > 0]
splash.readin.2011$Brazos_2020_18.2011splash$num_lines <- 365

# Harris 2020-03
splash.readin.2011$Harris_2020_03.2011splash$sf <- splash.readin.2011$Harris_2020_03.2011splash$sf[seq_along(splash.readin.2011$Harris_2020_03.2011splash$sf) %% 2 > 0]
splash.readin.2011$Harris_2020_03.2011splash$tair <- splash.readin.2011$Harris_2020_03.2011splash$tair[seq_along(splash.readin.2011$Harris_2020_03.2011splash$tair) %% 2 > 0]
splash.readin.2011$Harris_2020_03.2011splash$pn <- splash.readin.2011$Harris_2020_03.2011splash$pn[seq_along(splash.readin.2011$Harris_2020_03.2011splash$pn) %% 2 > 0]
splash.readin.2011$Harris_2020_03.2011splash$num_lines <- 365

# Menard 2020-01
splash.readin.2011$Menard_2020_01.2011splash$sf <- splash.readin.2011$Menard_2020_01.2011splash$sf[seq_along(splash.readin.2011$Menard_2020_01.2011splash$sf) %% 2 > 0]
splash.readin.2011$Menard_2020_01.2011splash$tair <- splash.readin.2011$Menard_2020_01.2011splash$tair[seq_along(splash.readin.2011$Menard_2020_01.2011splash$tair) %% 2 > 0]
splash.readin.2011$Menard_2020_01.2011splash$pn <- splash.readin.2011$Menard_2020_01.2011splash$pn[seq_along(splash.readin.2011$Menard_2020_01.2011splash$pn) %% 2 > 0]
splash.readin.2011$Menard_2020_01.2011splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2011$Uvalde_2020_02.2011splash$sf <- splash.readin.2011$Uvalde_2020_02.2011splash$sf[seq_along(splash.readin.2011$Uvalde_2020_02.2011splash$sf) %% 2 > 0]
splash.readin.2011$Uvalde_2020_02.2011splash$tair <- splash.readin.2011$Uvalde_2020_02.2011splash$tair[seq_along(splash.readin.2011$Uvalde_2020_02.2011splash$tair) %% 2 > 0]
splash.readin.2011$Uvalde_2020_02.2011splash$pn <- splash.readin.2011$Uvalde_2020_02.2011splash$pn[seq_along(splash.readin.2011$Uvalde_2020_02.2011splash$pn) %% 2 > 0]
splash.readin.2011$Uvalde_2020_02.2011splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2011 <- lapply(file.list.2011, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2011[["Brazos_2020_18.2011splash"]] <- splash.month.2011[["Brazos_2020_18.2011splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2011[["Harris_2020_03.2011splash"]] <- splash.month.2011[["Harris_2020_03.2011splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2011[["Menard_2020_01.2011splash"]] <- splash.month.2011[["Menard_2020_01.2011splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2011[["Uvalde_2020_02.2011splash"]] <- splash.month.2011[["Uvalde_2020_02.2011splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2011)) {
  splash.month.2011[[i]] <- splash.month.2011[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2011 <- lapply(file.list.2011, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2011 <- mapply(c, splash.readin.2011, splash.month.2011, 
                            splash.ancillary.2011, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2011)) {
  
  splash.total.2011[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2011[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2011[[i]][["num_lines"]])
  splash.total.2011[[i]][["daily_totals"]] <- as.data.frame(splash.total.2011[[i]][["daily_totals"]])
  names(splash.total.2011[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2011[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2011[[i]],
                                                      dtot = splash.total.2011[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2011 <- map(splash.total.2011, as.data.table)
splash.water.2011 <- rbindlist(splash.water.2011, fill = TRUE, idcol = TRUE)
splash.water.2011$year <- 2011
splash.water.2011$site <- str_extract(splash.water.2011$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2011 <- splash.water.2011 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2011 <- tidyr::unite(splash.water.2011, "date", year:day, sep = "-", remove = FALSE)
splash.water.2011$date <- lubridate::ymd(as.Date(splash.water.2011$date))
splash.water.2011$doy <- lubridate::yday(splash.water.2011$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2011 <- run_one_day(lat = splash.water.2011$lat_deg,
                                  elv = splash.water.2011$elv_m,
                                  n = splash.water.2011$doy,
                                  y = splash.water.2011$year,
                                  wn = splash.water.2011$soil.moisture,
                                  sf = splash.water.2011$sf,
                                  tc = splash.water.2011$tair,
                                  pn = splash.water.2011$pn,
                                  kWm = 150)

splash.oneday.2011 <- map(splash.oneday.2011, as.data.table)

## Change list column names
names(splash.oneday.2011[[1]]) <- "ho"
names(splash.oneday.2011[[2]]) <- "hn"
names(splash.oneday.2011[[3]]) <- "ppfd"
names(splash.oneday.2011[[4]]) <- "cond"
names(splash.oneday.2011[[5]]) <- "eet"
names(splash.oneday.2011[[6]]) <- "pet"
names(splash.oneday.2011[[7]]) <- "aet"
names(splash.oneday.2011[[8]]) <- "wn"
names(splash.oneday.2011[[9]]) <- "ro"

sites.daily.2011 <- splash.oneday.2011$ho %>%
  coalesce(splash.oneday.2011$hn) %>%
  coalesce(splash.oneday.2011$ppfd) %>%
  coalesce(splash.oneday.2011$cond) %>%
  coalesce(splash.oneday.2011$eet) %>%
  coalesce(splash.oneday.2011$pet) %>%
  coalesce(splash.oneday.2011$aet) %>%
  coalesce(splash.oneday.2011$wn) %>%
  coalesce(splash.oneday.2011$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2011) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2012 data
###############################################################################
file.list.2012 <- list.files("../climate_data/splash_prep_files/splash_2012",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2012 <- setNames(file.list.2012, 
                           str_extract(basename(file.list.2012), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2012 <- lapply(file.list.2012, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2012$Brazos_2020_18.2012splash$sf <- splash.readin.2012$Brazos_2020_18.2012splash$sf[seq_along(splash.readin.2012$Brazos_2020_18.2012splash$sf) %% 2 > 0]
splash.readin.2012$Brazos_2020_18.2012splash$tair <- splash.readin.2012$Brazos_2020_18.2012splash$tair[seq_along(splash.readin.2012$Brazos_2020_18.2012splash$tair) %% 2 > 0]
splash.readin.2012$Brazos_2020_18.2012splash$pn <- splash.readin.2012$Brazos_2020_18.2012splash$pn[seq_along(splash.readin.2012$Brazos_2020_18.2012splash$pn) %% 2 > 0]
splash.readin.2012$Brazos_2020_18.2012splash$num_lines <- 366

# Harris 2020-03
splash.readin.2012$Harris_2020_03.2012splash$sf <- splash.readin.2012$Harris_2020_03.2012splash$sf[seq_along(splash.readin.2012$Harris_2020_03.2012splash$sf) %% 2 > 0]
splash.readin.2012$Harris_2020_03.2012splash$tair <- splash.readin.2012$Harris_2020_03.2012splash$tair[seq_along(splash.readin.2012$Harris_2020_03.2012splash$tair) %% 2 > 0]
splash.readin.2012$Harris_2020_03.2012splash$pn <- splash.readin.2012$Harris_2020_03.2012splash$pn[seq_along(splash.readin.2012$Harris_2020_03.2012splash$pn) %% 2 > 0]
splash.readin.2012$Harris_2020_03.2012splash$num_lines <- 366

# Menard 2020-01
splash.readin.2012$Menard_2020_01.2012splash$sf <- splash.readin.2012$Menard_2020_01.2012splash$sf[seq_along(splash.readin.2012$Menard_2020_01.2012splash$sf) %% 2 > 0]
splash.readin.2012$Menard_2020_01.2012splash$tair <- splash.readin.2012$Menard_2020_01.2012splash$tair[seq_along(splash.readin.2012$Menard_2020_01.2012splash$tair) %% 2 > 0]
splash.readin.2012$Menard_2020_01.2012splash$pn <- splash.readin.2012$Menard_2020_01.2012splash$pn[seq_along(splash.readin.2012$Menard_2020_01.2012splash$pn) %% 2 > 0]
splash.readin.2012$Menard_2020_01.2012splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.2012$Uvalde_2020_02.2012splash$sf <- splash.readin.2012$Uvalde_2020_02.2012splash$sf[seq_along(splash.readin.2012$Uvalde_2020_02.2012splash$sf) %% 2 > 0]
splash.readin.2012$Uvalde_2020_02.2012splash$tair <- splash.readin.2012$Uvalde_2020_02.2012splash$tair[seq_along(splash.readin.2012$Uvalde_2020_02.2012splash$tair) %% 2 > 0]
splash.readin.2012$Uvalde_2020_02.2012splash$pn <- splash.readin.2012$Uvalde_2020_02.2012splash$pn[seq_along(splash.readin.2012$Uvalde_2020_02.2012splash$pn) %% 2 > 0]
splash.readin.2012$Uvalde_2020_02.2012splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2012 <- lapply(file.list.2012, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2012[["Brazos_2020_18.2012splash"]] <- splash.month.2012[["Brazos_2020_18.2012splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2012[["Harris_2020_03.2012splash"]] <- splash.month.2012[["Harris_2020_03.2012splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2012[["Menard_2020_01.2012splash"]] <- splash.month.2012[["Menard_2020_01.2012splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2012[["Uvalde_2020_02.2012splash"]] <- splash.month.2012[["Uvalde_2020_02.2012splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2012)) {
  splash.month.2012[[i]] <- splash.month.2012[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2012 <- lapply(file.list.2012, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2012 <- mapply(c, splash.readin.2012, splash.month.2012, 
                            splash.ancillary.2012, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2012)) {
  
  splash.total.2012[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2012[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2012[[i]][["num_lines"]])
  splash.total.2012[[i]][["daily_totals"]] <- as.data.frame(splash.total.2012[[i]][["daily_totals"]])
  names(splash.total.2012[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2012[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2012[[i]],
                                                      dtot = splash.total.2012[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2012 <- map(splash.total.2012, as.data.table)
splash.water.2012 <- rbindlist(splash.water.2012, fill = TRUE, idcol = TRUE)
splash.water.2012$year <- 2012
splash.water.2012$site <- str_extract(splash.water.2012$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2012 <- splash.water.2012 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2012 <- tidyr::unite(splash.water.2012, "date", year:day, sep = "-", remove = FALSE)
splash.water.2012$date <- lubridate::ymd(as.Date(splash.water.2012$date))
splash.water.2012$doy <- lubridate::yday(splash.water.2012$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2012 <- run_one_day(lat = splash.water.2012$lat_deg,
                                  elv = splash.water.2012$elv_m,
                                  n = splash.water.2012$doy,
                                  y = splash.water.2012$year,
                                  wn = splash.water.2012$soil.moisture,
                                  sf = splash.water.2012$sf,
                                  tc = splash.water.2012$tair,
                                  pn = splash.water.2012$pn,
                                  kWm = 150)

splash.oneday.2012 <- map(splash.oneday.2012, as.data.table)

## Change list column names
names(splash.oneday.2012[[1]]) <- "ho"
names(splash.oneday.2012[[2]]) <- "hn"
names(splash.oneday.2012[[3]]) <- "ppfd"
names(splash.oneday.2012[[4]]) <- "cond"
names(splash.oneday.2012[[5]]) <- "eet"
names(splash.oneday.2012[[6]]) <- "pet"
names(splash.oneday.2012[[7]]) <- "aet"
names(splash.oneday.2012[[8]]) <- "wn"
names(splash.oneday.2012[[9]]) <- "ro"

sites.daily.2012 <- splash.oneday.2012$ho %>%
  coalesce(splash.oneday.2012$hn) %>%
  coalesce(splash.oneday.2012$ppfd) %>%
  coalesce(splash.oneday.2012$cond) %>%
  coalesce(splash.oneday.2012$eet) %>%
  coalesce(splash.oneday.2012$pet) %>%
  coalesce(splash.oneday.2012$aet) %>%
  coalesce(splash.oneday.2012$wn) %>%
  coalesce(splash.oneday.2012$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2012) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2013 data
###############################################################################
file.list.2013 <- list.files("../climate_data/splash_prep_files/splash_2013",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2013 <- setNames(file.list.2013, 
                           str_extract(basename(file.list.2013), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2013 <- lapply(file.list.2013, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2013$Brazos_2020_18.2013splash$sf <- splash.readin.2013$Brazos_2020_18.2013splash$sf[seq_along(splash.readin.2013$Brazos_2020_18.2013splash$sf) %% 2 > 0]
splash.readin.2013$Brazos_2020_18.2013splash$tair <- splash.readin.2013$Brazos_2020_18.2013splash$tair[seq_along(splash.readin.2013$Brazos_2020_18.2013splash$tair) %% 2 > 0]
splash.readin.2013$Brazos_2020_18.2013splash$pn <- splash.readin.2013$Brazos_2020_18.2013splash$pn[seq_along(splash.readin.2013$Brazos_2020_18.2013splash$pn) %% 2 > 0]
splash.readin.2013$Brazos_2020_18.2013splash$num_lines <- 365

# Harris 2020-03
splash.readin.2013$Harris_2020_03.2013splash$sf <- splash.readin.2013$Harris_2020_03.2013splash$sf[seq_along(splash.readin.2013$Harris_2020_03.2013splash$sf) %% 2 > 0]
splash.readin.2013$Harris_2020_03.2013splash$tair <- splash.readin.2013$Harris_2020_03.2013splash$tair[seq_along(splash.readin.2013$Harris_2020_03.2013splash$tair) %% 2 > 0]
splash.readin.2013$Harris_2020_03.2013splash$pn <- splash.readin.2013$Harris_2020_03.2013splash$pn[seq_along(splash.readin.2013$Harris_2020_03.2013splash$pn) %% 2 > 0]
splash.readin.2013$Harris_2020_03.2013splash$num_lines <- 365

# Menard 2020-01
splash.readin.2013$Menard_2020_01.2013splash$sf <- splash.readin.2013$Menard_2020_01.2013splash$sf[seq_along(splash.readin.2013$Menard_2020_01.2013splash$sf) %% 2 > 0]
splash.readin.2013$Menard_2020_01.2013splash$tair <- splash.readin.2013$Menard_2020_01.2013splash$tair[seq_along(splash.readin.2013$Menard_2020_01.2013splash$tair) %% 2 > 0]
splash.readin.2013$Menard_2020_01.2013splash$pn <- splash.readin.2013$Menard_2020_01.2013splash$pn[seq_along(splash.readin.2013$Menard_2020_01.2013splash$pn) %% 2 > 0]
splash.readin.2013$Menard_2020_01.2013splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2013$Uvalde_2020_02.2013splash$sf <- splash.readin.2013$Uvalde_2020_02.2013splash$sf[seq_along(splash.readin.2013$Uvalde_2020_02.2013splash$sf) %% 2 > 0]
splash.readin.2013$Uvalde_2020_02.2013splash$tair <- splash.readin.2013$Uvalde_2020_02.2013splash$tair[seq_along(splash.readin.2013$Uvalde_2020_02.2013splash$tair) %% 2 > 0]
splash.readin.2013$Uvalde_2020_02.2013splash$pn <- splash.readin.2013$Uvalde_2020_02.2013splash$pn[seq_along(splash.readin.2013$Uvalde_2020_02.2013splash$pn) %% 2 > 0]
splash.readin.2013$Uvalde_2020_02.2013splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2013 <- lapply(file.list.2013, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2013[["Brazos_2020_18.2013splash"]] <- splash.month.2013[["Brazos_2020_18.2013splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2013[["Harris_2020_03.2013splash"]] <- splash.month.2013[["Harris_2020_03.2013splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2013[["Menard_2020_01.2013splash"]] <- splash.month.2013[["Menard_2020_01.2013splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2013[["Uvalde_2020_02.2013splash"]] <- splash.month.2013[["Uvalde_2020_02.2013splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2013)) {
  splash.month.2013[[i]] <- splash.month.2013[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2013 <- lapply(file.list.2013, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2013 <- mapply(c, splash.readin.2013, splash.month.2013, 
                            splash.ancillary.2013, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2013)) {
  
  splash.total.2013[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2013[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2013[[i]][["num_lines"]])
  splash.total.2013[[i]][["daily_totals"]] <- as.data.frame(splash.total.2013[[i]][["daily_totals"]])
  names(splash.total.2013[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2013[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2013[[i]],
                                                      dtot = splash.total.2013[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2013 <- map(splash.total.2013, as.data.table)
splash.water.2013 <- rbindlist(splash.water.2013, fill = TRUE, idcol = TRUE)
splash.water.2013$year <- 2013
splash.water.2013$site <- str_extract(splash.water.2013$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2013 <- splash.water.2013 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2013 <- tidyr::unite(splash.water.2013, "date", year:day, sep = "-", remove = FALSE)
splash.water.2013$date <- lubridate::ymd(as.Date(splash.water.2013$date))
splash.water.2013$doy <- lubridate::yday(splash.water.2013$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2013 <- run_one_day(lat = splash.water.2013$lat_deg,
                                  elv = splash.water.2013$elv_m,
                                  n = splash.water.2013$doy,
                                  y = splash.water.2013$year,
                                  wn = splash.water.2013$soil.moisture,
                                  sf = splash.water.2013$sf,
                                  tc = splash.water.2013$tair,
                                  pn = splash.water.2013$pn,
                                  kWm = 150)

splash.oneday.2013 <- map(splash.oneday.2013, as.data.table)

## Change list column names
names(splash.oneday.2013[[1]]) <- "ho"
names(splash.oneday.2013[[2]]) <- "hn"
names(splash.oneday.2013[[3]]) <- "ppfd"
names(splash.oneday.2013[[4]]) <- "cond"
names(splash.oneday.2013[[5]]) <- "eet"
names(splash.oneday.2013[[6]]) <- "pet"
names(splash.oneday.2013[[7]]) <- "aet"
names(splash.oneday.2013[[8]]) <- "wn"
names(splash.oneday.2013[[9]]) <- "ro"

sites.daily.2013 <- splash.oneday.2013$ho %>%
  coalesce(splash.oneday.2013$hn) %>%
  coalesce(splash.oneday.2013$ppfd) %>%
  coalesce(splash.oneday.2013$cond) %>%
  coalesce(splash.oneday.2013$eet) %>%
  coalesce(splash.oneday.2013$pet) %>%
  coalesce(splash.oneday.2013$aet) %>%
  coalesce(splash.oneday.2013$wn) %>%
  coalesce(splash.oneday.2013$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2013) %>% 
  dplyr::select(site:soil.moisture, ho:aet)


###############################################################################
## Run SPLASH model with 2014 data
###############################################################################
file.list.2014 <- list.files("../climate_data/splash_prep_files/splash_2014",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2014 <- setNames(file.list.2014, 
                           str_extract(basename(file.list.2014), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2014 <- lapply(file.list.2014, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2014$Brazos_2020_18.2014splash$sf <- splash.readin.2014$Brazos_2020_18.2014splash$sf[seq_along(splash.readin.2014$Brazos_2020_18.2014splash$sf) %% 2 > 0]
splash.readin.2014$Brazos_2020_18.2014splash$tair <- splash.readin.2014$Brazos_2020_18.2014splash$tair[seq_along(splash.readin.2014$Brazos_2020_18.2014splash$tair) %% 2 > 0]
splash.readin.2014$Brazos_2020_18.2014splash$pn <- splash.readin.2014$Brazos_2020_18.2014splash$pn[seq_along(splash.readin.2014$Brazos_2020_18.2014splash$pn) %% 2 > 0]
splash.readin.2014$Brazos_2020_18.2014splash$num_lines <- 365

# Harris 2020-03
splash.readin.2014$Harris_2020_03.2014splash$sf <- splash.readin.2014$Harris_2020_03.2014splash$sf[seq_along(splash.readin.2014$Harris_2020_03.2014splash$sf) %% 2 > 0]
splash.readin.2014$Harris_2020_03.2014splash$tair <- splash.readin.2014$Harris_2020_03.2014splash$tair[seq_along(splash.readin.2014$Harris_2020_03.2014splash$tair) %% 2 > 0]
splash.readin.2014$Harris_2020_03.2014splash$pn <- splash.readin.2014$Harris_2020_03.2014splash$pn[seq_along(splash.readin.2014$Harris_2020_03.2014splash$pn) %% 2 > 0]
splash.readin.2014$Harris_2020_03.2014splash$num_lines <- 365

# Menard 2020-01
splash.readin.2014$Menard_2020_01.2014splash$sf <- splash.readin.2014$Menard_2020_01.2014splash$sf[seq_along(splash.readin.2014$Menard_2020_01.2014splash$sf) %% 2 > 0]
splash.readin.2014$Menard_2020_01.2014splash$tair <- splash.readin.2014$Menard_2020_01.2014splash$tair[seq_along(splash.readin.2014$Menard_2020_01.2014splash$tair) %% 2 > 0]
splash.readin.2014$Menard_2020_01.2014splash$pn <- splash.readin.2014$Menard_2020_01.2014splash$pn[seq_along(splash.readin.2014$Menard_2020_01.2014splash$pn) %% 2 > 0]
splash.readin.2014$Menard_2020_01.2014splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2014$Uvalde_2020_02.2014splash$sf <- splash.readin.2014$Uvalde_2020_02.2014splash$sf[seq_along(splash.readin.2014$Uvalde_2020_02.2014splash$sf) %% 2 > 0]
splash.readin.2014$Uvalde_2020_02.2014splash$tair <- splash.readin.2014$Uvalde_2020_02.2014splash$tair[seq_along(splash.readin.2014$Uvalde_2020_02.2014splash$tair) %% 2 > 0]
splash.readin.2014$Uvalde_2020_02.2014splash$pn <- splash.readin.2014$Uvalde_2020_02.2014splash$pn[seq_along(splash.readin.2014$Uvalde_2020_02.2014splash$pn) %% 2 > 0]
splash.readin.2014$Uvalde_2020_02.2014splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2014 <- lapply(file.list.2014, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2014[["Brazos_2020_18.2014splash"]] <- splash.month.2014[["Brazos_2020_18.2014splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2014[["Harris_2020_03.2014splash"]] <- splash.month.2014[["Harris_2020_03.2014splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2014[["Menard_2020_01.2014splash"]] <- splash.month.2014[["Menard_2020_01.2014splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2014[["Uvalde_2020_02.2014splash"]] <- splash.month.2014[["Uvalde_2020_02.2014splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2014)) {
  splash.month.2014[[i]] <- splash.month.2014[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2014 <- lapply(file.list.2014, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2014 <- mapply(c, splash.readin.2014, splash.month.2014, 
                            splash.ancillary.2014, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2014)) {
  
  splash.total.2014[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2014[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2014[[i]][["num_lines"]])
  splash.total.2014[[i]][["daily_totals"]] <- as.data.frame(splash.total.2014[[i]][["daily_totals"]])
  names(splash.total.2014[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2014[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2014[[i]],
                                                      dtot = splash.total.2014[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2014 <- map(splash.total.2014, as.data.table)
splash.water.2014 <- rbindlist(splash.water.2014, fill = TRUE, idcol = TRUE)
splash.water.2014$year <- 2014
splash.water.2014$site <- str_extract(splash.water.2014$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2014 <- splash.water.2014 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2014 <- tidyr::unite(splash.water.2014, "date", year:day, sep = "-", remove = FALSE)
splash.water.2014$date <- lubridate::ymd(as.Date(splash.water.2014$date))
splash.water.2014$doy <- lubridate::yday(splash.water.2014$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2014 <- run_one_day(lat = splash.water.2014$lat_deg,
                                  elv = splash.water.2014$elv_m,
                                  n = splash.water.2014$doy,
                                  y = splash.water.2014$year,
                                  wn = splash.water.2014$soil.moisture,
                                  sf = splash.water.2014$sf,
                                  tc = splash.water.2014$tair,
                                  pn = splash.water.2014$pn,
                                  kWm = 150)

splash.oneday.2014 <- map(splash.oneday.2014, as.data.table)

## Change list column names
names(splash.oneday.2014[[1]]) <- "ho"
names(splash.oneday.2014[[2]]) <- "hn"
names(splash.oneday.2014[[3]]) <- "ppfd"
names(splash.oneday.2014[[4]]) <- "cond"
names(splash.oneday.2014[[5]]) <- "eet"
names(splash.oneday.2014[[6]]) <- "pet"
names(splash.oneday.2014[[7]]) <- "aet"
names(splash.oneday.2014[[8]]) <- "wn"
names(splash.oneday.2014[[9]]) <- "ro"

sites.daily.2014 <- splash.oneday.2014$ho %>%
  coalesce(splash.oneday.2014$hn) %>%
  coalesce(splash.oneday.2014$ppfd) %>%
  coalesce(splash.oneday.2014$cond) %>%
  coalesce(splash.oneday.2014$eet) %>%
  coalesce(splash.oneday.2014$pet) %>%
  coalesce(splash.oneday.2014$aet) %>%
  coalesce(splash.oneday.2014$wn) %>%
  coalesce(splash.oneday.2014$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2014) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2015 data
###############################################################################
file.list.2015 <- list.files("../climate_data/splash_prep_files/splash_2015",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2015 <- setNames(file.list.2015, 
                           str_extract(basename(file.list.2015), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2015 <- lapply(file.list.2015, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2015$Brazos_2020_18.2015splash$sf <- splash.readin.2015$Brazos_2020_18.2015splash$sf[seq_along(splash.readin.2015$Brazos_2020_18.2015splash$sf) %% 2 > 0]
splash.readin.2015$Brazos_2020_18.2015splash$tair <- splash.readin.2015$Brazos_2020_18.2015splash$tair[seq_along(splash.readin.2015$Brazos_2020_18.2015splash$tair) %% 2 > 0]
splash.readin.2015$Brazos_2020_18.2015splash$pn <- splash.readin.2015$Brazos_2020_18.2015splash$pn[seq_along(splash.readin.2015$Brazos_2020_18.2015splash$pn) %% 2 > 0]
splash.readin.2015$Brazos_2020_18.2015splash$num_lines <- 365

# Harris 2020-03
splash.readin.2015$Harris_2020_03.2015splash$sf <- splash.readin.2015$Harris_2020_03.2015splash$sf[seq_along(splash.readin.2015$Harris_2020_03.2015splash$sf) %% 2 > 0]
splash.readin.2015$Harris_2020_03.2015splash$tair <- splash.readin.2015$Harris_2020_03.2015splash$tair[seq_along(splash.readin.2015$Harris_2020_03.2015splash$tair) %% 2 > 0]
splash.readin.2015$Harris_2020_03.2015splash$pn <- splash.readin.2015$Harris_2020_03.2015splash$pn[seq_along(splash.readin.2015$Harris_2020_03.2015splash$pn) %% 2 > 0]
splash.readin.2015$Harris_2020_03.2015splash$num_lines <- 365

# Menard 2020-01
splash.readin.2015$Menard_2020_01.2015splash$sf <- splash.readin.2015$Menard_2020_01.2015splash$sf[seq_along(splash.readin.2015$Menard_2020_01.2015splash$sf) %% 2 > 0]
splash.readin.2015$Menard_2020_01.2015splash$tair <- splash.readin.2015$Menard_2020_01.2015splash$tair[seq_along(splash.readin.2015$Menard_2020_01.2015splash$tair) %% 2 > 0]
splash.readin.2015$Menard_2020_01.2015splash$pn <- splash.readin.2015$Menard_2020_01.2015splash$pn[seq_along(splash.readin.2015$Menard_2020_01.2015splash$pn) %% 2 > 0]
splash.readin.2015$Menard_2020_01.2015splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2015$Uvalde_2020_02.2015splash$sf <- splash.readin.2015$Uvalde_2020_02.2015splash$sf[seq_along(splash.readin.2015$Uvalde_2020_02.2015splash$sf) %% 2 > 0]
splash.readin.2015$Uvalde_2020_02.2015splash$tair <- splash.readin.2015$Uvalde_2020_02.2015splash$tair[seq_along(splash.readin.2015$Uvalde_2020_02.2015splash$tair) %% 2 > 0]
splash.readin.2015$Uvalde_2020_02.2015splash$pn <- splash.readin.2015$Uvalde_2020_02.2015splash$pn[seq_along(splash.readin.2015$Uvalde_2020_02.2015splash$pn) %% 2 > 0]
splash.readin.2015$Uvalde_2020_02.2015splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2015 <- lapply(file.list.2015, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2015[["Brazos_2020_18.2015splash"]] <- splash.month.2015[["Brazos_2020_18.2015splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2015[["Harris_2020_03.2015splash"]] <- splash.month.2015[["Harris_2020_03.2015splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2015[["Menard_2020_01.2015splash"]] <- splash.month.2015[["Menard_2020_01.2015splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2015[["Uvalde_2020_02.2015splash"]] <- splash.month.2015[["Uvalde_2020_02.2015splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2015)) {
  splash.month.2015[[i]] <- splash.month.2015[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2015 <- lapply(file.list.2015, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2015 <- mapply(c, splash.readin.2015, splash.month.2015, 
                            splash.ancillary.2015, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2015)) {
  
  splash.total.2015[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2015[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2015[[i]][["num_lines"]])
  splash.total.2015[[i]][["daily_totals"]] <- as.data.frame(splash.total.2015[[i]][["daily_totals"]])
  names(splash.total.2015[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2015[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2015[[i]],
                                                      dtot = splash.total.2015[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2015 <- map(splash.total.2015, as.data.table)
splash.water.2015 <- rbindlist(splash.water.2015, fill = TRUE, idcol = TRUE)
splash.water.2015$year <- 2015
splash.water.2015$site <- str_extract(splash.water.2015$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2015 <- splash.water.2015 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2015 <- tidyr::unite(splash.water.2015, "date", year:day, sep = "-", remove = FALSE)
splash.water.2015$date <- lubridate::ymd(as.Date(splash.water.2015$date))
splash.water.2015$doy <- lubridate::yday(splash.water.2015$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2015 <- run_one_day(lat = splash.water.2015$lat_deg,
                                  elv = splash.water.2015$elv_m,
                                  n = splash.water.2015$doy,
                                  y = splash.water.2015$year,
                                  wn = splash.water.2015$soil.moisture,
                                  sf = splash.water.2015$sf,
                                  tc = splash.water.2015$tair,
                                  pn = splash.water.2015$pn,
                                  kWm = 150)

splash.oneday.2015 <- map(splash.oneday.2015, as.data.table)

## Change list column names
names(splash.oneday.2015[[1]]) <- "ho"
names(splash.oneday.2015[[2]]) <- "hn"
names(splash.oneday.2015[[3]]) <- "ppfd"
names(splash.oneday.2015[[4]]) <- "cond"
names(splash.oneday.2015[[5]]) <- "eet"
names(splash.oneday.2015[[6]]) <- "pet"
names(splash.oneday.2015[[7]]) <- "aet"
names(splash.oneday.2015[[8]]) <- "wn"
names(splash.oneday.2015[[9]]) <- "ro"

sites.daily.2015 <- splash.oneday.2015$ho %>%
  coalesce(splash.oneday.2015$hn) %>%
  coalesce(splash.oneday.2015$ppfd) %>%
  coalesce(splash.oneday.2015$cond) %>%
  coalesce(splash.oneday.2015$eet) %>%
  coalesce(splash.oneday.2015$pet) %>%
  coalesce(splash.oneday.2015$aet) %>%
  coalesce(splash.oneday.2015$wn) %>%
  coalesce(splash.oneday.2015$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2015) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2016 data
###############################################################################
file.list.2016 <- list.files("../climate_data/splash_prep_files/splash_2016",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2016 <- setNames(file.list.2016, 
                           str_extract(basename(file.list.2016), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2016 <- lapply(file.list.2016, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2016$Brazos_2020_18.2016splash$sf <- splash.readin.2016$Brazos_2020_18.2016splash$sf[seq_along(splash.readin.2016$Brazos_2020_18.2016splash$sf) %% 2 > 0]
splash.readin.2016$Brazos_2020_18.2016splash$tair <- splash.readin.2016$Brazos_2020_18.2016splash$tair[seq_along(splash.readin.2016$Brazos_2020_18.2016splash$tair) %% 2 > 0]
splash.readin.2016$Brazos_2020_18.2016splash$pn <- splash.readin.2016$Brazos_2020_18.2016splash$pn[seq_along(splash.readin.2016$Brazos_2020_18.2016splash$pn) %% 2 > 0]
splash.readin.2016$Brazos_2020_18.2016splash$num_lines <- 366

# Harris 2020-03
splash.readin.2016$Harris_2020_03.2016splash$sf <- splash.readin.2016$Harris_2020_03.2016splash$sf[seq_along(splash.readin.2016$Harris_2020_03.2016splash$sf) %% 2 > 0]
splash.readin.2016$Harris_2020_03.2016splash$tair <- splash.readin.2016$Harris_2020_03.2016splash$tair[seq_along(splash.readin.2016$Harris_2020_03.2016splash$tair) %% 2 > 0]
splash.readin.2016$Harris_2020_03.2016splash$pn <- splash.readin.2016$Harris_2020_03.2016splash$pn[seq_along(splash.readin.2016$Harris_2020_03.2016splash$pn) %% 2 > 0]
splash.readin.2016$Harris_2020_03.2016splash$num_lines <- 366

# Menard 2020-01
splash.readin.2016$Menard_2020_01.2016splash$sf <- splash.readin.2016$Menard_2020_01.2016splash$sf[seq_along(splash.readin.2016$Menard_2020_01.2016splash$sf) %% 2 > 0]
splash.readin.2016$Menard_2020_01.2016splash$tair <- splash.readin.2016$Menard_2020_01.2016splash$tair[seq_along(splash.readin.2016$Menard_2020_01.2016splash$tair) %% 2 > 0]
splash.readin.2016$Menard_2020_01.2016splash$pn <- splash.readin.2016$Menard_2020_01.2016splash$pn[seq_along(splash.readin.2016$Menard_2020_01.2016splash$pn) %% 2 > 0]
splash.readin.2016$Menard_2020_01.2016splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.2016$Uvalde_2020_02.2016splash$sf <- splash.readin.2016$Uvalde_2020_02.2016splash$sf[seq_along(splash.readin.2016$Uvalde_2020_02.2016splash$sf) %% 2 > 0]
splash.readin.2016$Uvalde_2020_02.2016splash$tair <- splash.readin.2016$Uvalde_2020_02.2016splash$tair[seq_along(splash.readin.2016$Uvalde_2020_02.2016splash$tair) %% 2 > 0]
splash.readin.2016$Uvalde_2020_02.2016splash$pn <- splash.readin.2016$Uvalde_2020_02.2016splash$pn[seq_along(splash.readin.2016$Uvalde_2020_02.2016splash$pn) %% 2 > 0]
splash.readin.2016$Uvalde_2020_02.2016splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2016 <- lapply(file.list.2016, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2016[["Brazos_2020_18.2016splash"]] <- splash.month.2016[["Brazos_2020_18.2016splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2016[["Harris_2020_03.2016splash"]] <- splash.month.2016[["Harris_2020_03.2016splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2016[["Menard_2020_01.2016splash"]] <- splash.month.2016[["Menard_2020_01.2016splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2016[["Uvalde_2020_02.2016splash"]] <- splash.month.2016[["Uvalde_2020_02.2016splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2016)) {
  splash.month.2016[[i]] <- splash.month.2016[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2016 <- lapply(file.list.2016, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2016 <- mapply(c, splash.readin.2016, splash.month.2016, 
                            splash.ancillary.2016, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2016)) {
  
  splash.total.2016[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2016[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2016[[i]][["num_lines"]])
  splash.total.2016[[i]][["daily_totals"]] <- as.data.frame(splash.total.2016[[i]][["daily_totals"]])
  names(splash.total.2016[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2016[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2016[[i]],
                                                      dtot = splash.total.2016[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2016 <- map(splash.total.2016, as.data.table)
splash.water.2016 <- rbindlist(splash.water.2016, fill = TRUE, idcol = TRUE)
splash.water.2016$year <- 2016
splash.water.2016$site <- str_extract(splash.water.2016$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2016 <- splash.water.2016 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2016 <- tidyr::unite(splash.water.2016, "date", year:day, sep = "-", remove = FALSE)
splash.water.2016$date <- lubridate::ymd(as.Date(splash.water.2016$date))
splash.water.2016$doy <- lubridate::yday(splash.water.2016$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2016 <- run_one_day(lat = splash.water.2016$lat_deg,
                                  elv = splash.water.2016$elv_m,
                                  n = splash.water.2016$doy,
                                  y = splash.water.2016$year,
                                  wn = splash.water.2016$soil.moisture,
                                  sf = splash.water.2016$sf,
                                  tc = splash.water.2016$tair,
                                  pn = splash.water.2016$pn,
                                  kWm = 150)

splash.oneday.2016 <- map(splash.oneday.2016, as.data.table)

## Change list column names
names(splash.oneday.2016[[1]]) <- "ho"
names(splash.oneday.2016[[2]]) <- "hn"
names(splash.oneday.2016[[3]]) <- "ppfd"
names(splash.oneday.2016[[4]]) <- "cond"
names(splash.oneday.2016[[5]]) <- "eet"
names(splash.oneday.2016[[6]]) <- "pet"
names(splash.oneday.2016[[7]]) <- "aet"
names(splash.oneday.2016[[8]]) <- "wn"
names(splash.oneday.2016[[9]]) <- "ro"

sites.daily.2016 <- splash.oneday.2016$ho %>%
  coalesce(splash.oneday.2016$hn) %>%
  coalesce(splash.oneday.2016$ppfd) %>%
  coalesce(splash.oneday.2016$cond) %>%
  coalesce(splash.oneday.2016$eet) %>%
  coalesce(splash.oneday.2016$pet) %>%
  coalesce(splash.oneday.2016$aet) %>%
  coalesce(splash.oneday.2016$wn) %>%
  coalesce(splash.oneday.2016$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2016) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2016 data
###############################################################################
file.list.2017 <- list.files("../climate_data/splash_prep_files/splash_2017",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2017 <- setNames(file.list.2017, 
                           str_extract(basename(file.list.2017), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2017 <- lapply(file.list.2017, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2017$Brazos_2020_18.2017splash$sf <- splash.readin.2017$Brazos_2020_18.2017splash$sf[seq_along(splash.readin.2017$Brazos_2020_18.2017splash$sf) %% 2 > 0]
splash.readin.2017$Brazos_2020_18.2017splash$tair <- splash.readin.2017$Brazos_2020_18.2017splash$tair[seq_along(splash.readin.2017$Brazos_2020_18.2017splash$tair) %% 2 > 0]
splash.readin.2017$Brazos_2020_18.2017splash$pn <- splash.readin.2017$Brazos_2020_18.2017splash$pn[seq_along(splash.readin.2017$Brazos_2020_18.2017splash$pn) %% 2 > 0]
splash.readin.2017$Brazos_2020_18.2017splash$num_lines <- 365

# Harris 2020-03
splash.readin.2017$Harris_2020_03.2017splash$sf <- splash.readin.2017$Harris_2020_03.2017splash$sf[seq_along(splash.readin.2017$Harris_2020_03.2017splash$sf) %% 2 > 0]
splash.readin.2017$Harris_2020_03.2017splash$tair <- splash.readin.2017$Harris_2020_03.2017splash$tair[seq_along(splash.readin.2017$Harris_2020_03.2017splash$tair) %% 2 > 0]
splash.readin.2017$Harris_2020_03.2017splash$pn <- splash.readin.2017$Harris_2020_03.2017splash$pn[seq_along(splash.readin.2017$Harris_2020_03.2017splash$pn) %% 2 > 0]
splash.readin.2017$Harris_2020_03.2017splash$num_lines <- 365

# Menard 2020-01
splash.readin.2017$Menard_2020_01.2017splash$sf <- splash.readin.2017$Menard_2020_01.2017splash$sf[seq_along(splash.readin.2017$Menard_2020_01.2017splash$sf) %% 2 > 0]
splash.readin.2017$Menard_2020_01.2017splash$tair <- splash.readin.2017$Menard_2020_01.2017splash$tair[seq_along(splash.readin.2017$Menard_2020_01.2017splash$tair) %% 2 > 0]
splash.readin.2017$Menard_2020_01.2017splash$pn <- splash.readin.2017$Menard_2020_01.2017splash$pn[seq_along(splash.readin.2017$Menard_2020_01.2017splash$pn) %% 2 > 0]
splash.readin.2017$Menard_2020_01.2017splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2017$Uvalde_2020_02.2017splash$sf <- splash.readin.2017$Uvalde_2020_02.2017splash$sf[seq_along(splash.readin.2017$Uvalde_2020_02.2017splash$sf) %% 2 > 0]
splash.readin.2017$Uvalde_2020_02.2017splash$tair <- splash.readin.2017$Uvalde_2020_02.2017splash$tair[seq_along(splash.readin.2017$Uvalde_2020_02.2017splash$tair) %% 2 > 0]
splash.readin.2017$Uvalde_2020_02.2017splash$pn <- splash.readin.2017$Uvalde_2020_02.2017splash$pn[seq_along(splash.readin.2017$Uvalde_2020_02.2017splash$pn) %% 2 > 0]
splash.readin.2017$Uvalde_2020_02.2017splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2017 <- lapply(file.list.2017, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2017[["Brazos_2020_18.2017splash"]] <- splash.month.2017[["Brazos_2020_18.2017splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2017[["Harris_2020_03.2017splash"]] <- splash.month.2017[["Harris_2020_03.2017splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2017[["Menard_2020_01.2017splash"]] <- splash.month.2017[["Menard_2020_01.2017splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2017[["Uvalde_2020_02.2017splash"]] <- splash.month.2017[["Uvalde_2020_02.2017splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2017)) {
  splash.month.2017[[i]] <- splash.month.2017[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2017 <- lapply(file.list.2017, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2017 <- mapply(c, splash.readin.2017, splash.month.2017, 
                            splash.ancillary.2017, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2017)) {
  
  splash.total.2017[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2017[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2017[[i]][["num_lines"]])
  splash.total.2017[[i]][["daily_totals"]] <- as.data.frame(splash.total.2017[[i]][["daily_totals"]])
  names(splash.total.2017[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2017[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2017[[i]],
                                                      dtot = splash.total.2017[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2017 <- map(splash.total.2017, as.data.table)
splash.water.2017 <- rbindlist(splash.water.2017, fill = TRUE, idcol = TRUE)
splash.water.2017$year <- 2017
splash.water.2017$site <- str_extract(splash.water.2017$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2017 <- splash.water.2017 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2017 <- tidyr::unite(splash.water.2017, "date", year:day, sep = "-", remove = FALSE)
splash.water.2017$date <- lubridate::ymd(as.Date(splash.water.2017$date))
splash.water.2017$doy <- lubridate::yday(splash.water.2017$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2017 <- run_one_day(lat = splash.water.2017$lat_deg,
                                  elv = splash.water.2017$elv_m,
                                  n = splash.water.2017$doy,
                                  y = splash.water.2017$year,
                                  wn = splash.water.2017$soil.moisture,
                                  sf = splash.water.2017$sf,
                                  tc = splash.water.2017$tair,
                                  pn = splash.water.2017$pn,
                                  kWm = 150)

splash.oneday.2017 <- map(splash.oneday.2017, as.data.table)

## Change list column names
names(splash.oneday.2017[[1]]) <- "ho"
names(splash.oneday.2017[[2]]) <- "hn"
names(splash.oneday.2017[[3]]) <- "ppfd"
names(splash.oneday.2017[[4]]) <- "cond"
names(splash.oneday.2017[[5]]) <- "eet"
names(splash.oneday.2017[[6]]) <- "pet"
names(splash.oneday.2017[[7]]) <- "aet"
names(splash.oneday.2017[[8]]) <- "wn"
names(splash.oneday.2017[[9]]) <- "ro"

sites.daily.2017 <- splash.oneday.2017$ho %>%
  coalesce(splash.oneday.2017$hn) %>%
  coalesce(splash.oneday.2017$ppfd) %>%
  coalesce(splash.oneday.2017$cond) %>%
  coalesce(splash.oneday.2017$eet) %>%
  coalesce(splash.oneday.2017$pet) %>%
  coalesce(splash.oneday.2017$aet) %>%
  coalesce(splash.oneday.2017$wn) %>%
  coalesce(splash.oneday.2017$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2017) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2018 data
###############################################################################
file.list.2018 <- list.files("../climate_data/splash_prep_files/splash_2018",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2018 <- setNames(file.list.2018, 
                           str_extract(basename(file.list.2018), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2018 <- lapply(file.list.2018, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2018$Brazos_2020_18.2018splash$sf <- splash.readin.2018$Brazos_2020_18.2018splash$sf[seq_along(splash.readin.2018$Brazos_2020_18.2018splash$sf) %% 2 > 0]
splash.readin.2018$Brazos_2020_18.2018splash$tair <- splash.readin.2018$Brazos_2020_18.2018splash$tair[seq_along(splash.readin.2018$Brazos_2020_18.2018splash$tair) %% 2 > 0]
splash.readin.2018$Brazos_2020_18.2018splash$pn <- splash.readin.2018$Brazos_2020_18.2018splash$pn[seq_along(splash.readin.2018$Brazos_2020_18.2018splash$pn) %% 2 > 0]
splash.readin.2018$Brazos_2020_18.2018splash$num_lines <- 365

# Harris 2020-03
splash.readin.2018$Harris_2020_03.2018splash$sf <- splash.readin.2018$Harris_2020_03.2018splash$sf[seq_along(splash.readin.2018$Harris_2020_03.2018splash$sf) %% 2 > 0]
splash.readin.2018$Harris_2020_03.2018splash$tair <- splash.readin.2018$Harris_2020_03.2018splash$tair[seq_along(splash.readin.2018$Harris_2020_03.2018splash$tair) %% 2 > 0]
splash.readin.2018$Harris_2020_03.2018splash$pn <- splash.readin.2018$Harris_2020_03.2018splash$pn[seq_along(splash.readin.2018$Harris_2020_03.2018splash$pn) %% 2 > 0]
splash.readin.2018$Harris_2020_03.2018splash$num_lines <- 365

# Menard 2020-01
splash.readin.2018$Menard_2020_01.2018splash$sf <- splash.readin.2018$Menard_2020_01.2018splash$sf[seq_along(splash.readin.2018$Menard_2020_01.2018splash$sf) %% 2 > 0]
splash.readin.2018$Menard_2020_01.2018splash$tair <- splash.readin.2018$Menard_2020_01.2018splash$tair[seq_along(splash.readin.2018$Menard_2020_01.2018splash$tair) %% 2 > 0]
splash.readin.2018$Menard_2020_01.2018splash$pn <- splash.readin.2018$Menard_2020_01.2018splash$pn[seq_along(splash.readin.2018$Menard_2020_01.2018splash$pn) %% 2 > 0]
splash.readin.2018$Menard_2020_01.2018splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2018$Uvalde_2020_02.2018splash$sf <- splash.readin.2018$Uvalde_2020_02.2018splash$sf[seq_along(splash.readin.2018$Uvalde_2020_02.2018splash$sf) %% 2 > 0]
splash.readin.2018$Uvalde_2020_02.2018splash$tair <- splash.readin.2018$Uvalde_2020_02.2018splash$tair[seq_along(splash.readin.2018$Uvalde_2020_02.2018splash$tair) %% 2 > 0]
splash.readin.2018$Uvalde_2020_02.2018splash$pn <- splash.readin.2018$Uvalde_2020_02.2018splash$pn[seq_along(splash.readin.2018$Uvalde_2020_02.2018splash$pn) %% 2 > 0]
splash.readin.2018$Uvalde_2020_02.2018splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2018 <- lapply(file.list.2018, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2018[["Brazos_2020_18.2018splash"]] <- splash.month.2018[["Brazos_2020_18.2018splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2018[["Harris_2020_03.2018splash"]] <- splash.month.2018[["Harris_2020_03.2018splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2018[["Menard_2020_01.2018splash"]] <- splash.month.2018[["Menard_2020_01.2018splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2018[["Uvalde_2020_02.2018splash"]] <- splash.month.2018[["Uvalde_2020_02.2018splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2018)) {
  splash.month.2018[[i]] <- splash.month.2018[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2018 <- lapply(file.list.2018, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2018 <- mapply(c, splash.readin.2018, splash.month.2018, 
                            splash.ancillary.2018, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2018)) {
  
  splash.total.2018[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2018[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2018[[i]][["num_lines"]])
  splash.total.2018[[i]][["daily_totals"]] <- as.data.frame(splash.total.2018[[i]][["daily_totals"]])
  names(splash.total.2018[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2018[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2018[[i]],
                                                      dtot = splash.total.2018[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2018 <- map(splash.total.2018, as.data.table)
splash.water.2018 <- rbindlist(splash.water.2018, fill = TRUE, idcol = TRUE)
splash.water.2018$year <- 2018
splash.water.2018$site <- str_extract(splash.water.2018$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2018 <- splash.water.2018 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2018 <- tidyr::unite(splash.water.2018, "date", year:day, sep = "-", remove = FALSE)
splash.water.2018$date <- lubridate::ymd(as.Date(splash.water.2018$date))
splash.water.2018$doy <- lubridate::yday(splash.water.2018$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2018 <- run_one_day(lat = splash.water.2018$lat_deg,
                                  elv = splash.water.2018$elv_m,
                                  n = splash.water.2018$doy,
                                  y = splash.water.2018$year,
                                  wn = splash.water.2018$soil.moisture,
                                  sf = splash.water.2018$sf,
                                  tc = splash.water.2018$tair,
                                  pn = splash.water.2018$pn,
                                  kWm = 150)

splash.oneday.2018 <- map(splash.oneday.2018, as.data.table)

## Change list column names
names(splash.oneday.2018[[1]]) <- "ho"
names(splash.oneday.2018[[2]]) <- "hn"
names(splash.oneday.2018[[3]]) <- "ppfd"
names(splash.oneday.2018[[4]]) <- "cond"
names(splash.oneday.2018[[5]]) <- "eet"
names(splash.oneday.2018[[6]]) <- "pet"
names(splash.oneday.2018[[7]]) <- "aet"
names(splash.oneday.2018[[8]]) <- "wn"
names(splash.oneday.2018[[9]]) <- "ro"

sites.daily.2018 <- splash.oneday.2018$ho %>%
  coalesce(splash.oneday.2018$hn) %>%
  coalesce(splash.oneday.2018$ppfd) %>%
  coalesce(splash.oneday.2018$cond) %>%
  coalesce(splash.oneday.2018$eet) %>%
  coalesce(splash.oneday.2018$pet) %>%
  coalesce(splash.oneday.2018$aet) %>%
  coalesce(splash.oneday.2018$wn) %>%
  coalesce(splash.oneday.2018$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2018) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2019 data
###############################################################################
file.list.2019 <- list.files("../climate_data/splash_prep_files/splash_2019",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2019 <- setNames(file.list.2019, 
                           str_extract(basename(file.list.2019), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2019 <- lapply(file.list.2019, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2019$Brazos_2020_18.2019splash$sf <- splash.readin.2019$Brazos_2020_18.2019splash$sf[seq_along(splash.readin.2019$Brazos_2020_18.2019splash$sf) %% 2 > 0]
splash.readin.2019$Brazos_2020_18.2019splash$tair <- splash.readin.2019$Brazos_2020_18.2019splash$tair[seq_along(splash.readin.2019$Brazos_2020_18.2019splash$tair) %% 2 > 0]
splash.readin.2019$Brazos_2020_18.2019splash$pn <- splash.readin.2019$Brazos_2020_18.2019splash$pn[seq_along(splash.readin.2019$Brazos_2020_18.2019splash$pn) %% 2 > 0]
splash.readin.2019$Brazos_2020_18.2019splash$num_lines <- 365

# Harris 2020-03
splash.readin.2019$Harris_2020_03.2019splash$sf <- splash.readin.2019$Harris_2020_03.2019splash$sf[seq_along(splash.readin.2019$Harris_2020_03.2019splash$sf) %% 2 > 0]
splash.readin.2019$Harris_2020_03.2019splash$tair <- splash.readin.2019$Harris_2020_03.2019splash$tair[seq_along(splash.readin.2019$Harris_2020_03.2019splash$tair) %% 2 > 0]
splash.readin.2019$Harris_2020_03.2019splash$pn <- splash.readin.2019$Harris_2020_03.2019splash$pn[seq_along(splash.readin.2019$Harris_2020_03.2019splash$pn) %% 2 > 0]
splash.readin.2019$Harris_2020_03.2019splash$num_lines <- 365

# Menard 2020-01
splash.readin.2019$Menard_2020_01.2019splash$sf <- splash.readin.2019$Menard_2020_01.2019splash$sf[seq_along(splash.readin.2019$Menard_2020_01.2019splash$sf) %% 2 > 0]
splash.readin.2019$Menard_2020_01.2019splash$tair <- splash.readin.2019$Menard_2020_01.2019splash$tair[seq_along(splash.readin.2019$Menard_2020_01.2019splash$tair) %% 2 > 0]
splash.readin.2019$Menard_2020_01.2019splash$pn <- splash.readin.2019$Menard_2020_01.2019splash$pn[seq_along(splash.readin.2019$Menard_2020_01.2019splash$pn) %% 2 > 0]
splash.readin.2019$Menard_2020_01.2019splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2019$Uvalde_2020_02.2019splash$sf <- splash.readin.2019$Uvalde_2020_02.2019splash$sf[seq_along(splash.readin.2019$Uvalde_2020_02.2019splash$sf) %% 2 > 0]
splash.readin.2019$Uvalde_2020_02.2019splash$tair <- splash.readin.2019$Uvalde_2020_02.2019splash$tair[seq_along(splash.readin.2019$Uvalde_2020_02.2019splash$tair) %% 2 > 0]
splash.readin.2019$Uvalde_2020_02.2019splash$pn <- splash.readin.2019$Uvalde_2020_02.2019splash$pn[seq_along(splash.readin.2019$Uvalde_2020_02.2019splash$pn) %% 2 > 0]
splash.readin.2019$Uvalde_2020_02.2019splash$num_lines <- 365


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2019 <- lapply(file.list.2019, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2019[["Brazos_2020_18.2019splash"]] <- splash.month.2019[["Brazos_2020_18.2019splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2019[["Harris_2020_03.2019splash"]] <- splash.month.2019[["Harris_2020_03.2019splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2019[["Menard_2020_01.2019splash"]] <- splash.month.2019[["Menard_2020_01.2019splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2019[["Uvalde_2020_02.2019splash"]] <- splash.month.2019[["Uvalde_2020_02.2019splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2019)) {
  splash.month.2019[[i]] <- splash.month.2019[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2019 <- lapply(file.list.2019, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2019 <- mapply(c, splash.readin.2019, splash.month.2019, 
                            splash.ancillary.2019, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2019)) {
  
  splash.total.2019[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2019[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2019[[i]][["num_lines"]])
  splash.total.2019[[i]][["daily_totals"]] <- as.data.frame(splash.total.2019[[i]][["daily_totals"]])
  names(splash.total.2019[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2019[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2019[[i]],
                                                      dtot = splash.total.2019[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2019 <- map(splash.total.2019, as.data.table)
splash.water.2019 <- rbindlist(splash.water.2019, fill = TRUE, idcol = TRUE)
splash.water.2019$year <- 2019
splash.water.2019$site <- str_extract(splash.water.2019$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2019 <- splash.water.2019 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2019 <- tidyr::unite(splash.water.2019, "date", year:day, sep = "-", remove = FALSE)
splash.water.2019$date <- lubridate::ymd(as.Date(splash.water.2019$date))
splash.water.2019$doy <- lubridate::yday(splash.water.2019$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2019 <- run_one_day(lat = splash.water.2019$lat_deg,
                                  elv = splash.water.2019$elv_m,
                                  n = splash.water.2019$doy,
                                  y = splash.water.2019$year,
                                  wn = splash.water.2019$soil.moisture,
                                  sf = splash.water.2019$sf,
                                  tc = splash.water.2019$tair,
                                  pn = splash.water.2019$pn,
                                  kWm = 150)

splash.oneday.2019 <- map(splash.oneday.2019, as.data.table)

## Change list column names
names(splash.oneday.2019[[1]]) <- "ho"
names(splash.oneday.2019[[2]]) <- "hn"
names(splash.oneday.2019[[3]]) <- "ppfd"
names(splash.oneday.2019[[4]]) <- "cond"
names(splash.oneday.2019[[5]]) <- "eet"
names(splash.oneday.2019[[6]]) <- "pet"
names(splash.oneday.2019[[7]]) <- "aet"
names(splash.oneday.2019[[8]]) <- "wn"
names(splash.oneday.2019[[9]]) <- "ro"

sites.daily.2019 <- splash.oneday.2019$ho %>%
  coalesce(splash.oneday.2019$hn) %>%
  coalesce(splash.oneday.2019$ppfd) %>%
  coalesce(splash.oneday.2019$cond) %>%
  coalesce(splash.oneday.2019$eet) %>%
  coalesce(splash.oneday.2019$pet) %>%
  coalesce(splash.oneday.2019$aet) %>%
  coalesce(splash.oneday.2019$wn) %>%
  coalesce(splash.oneday.2019$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2019) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2020 data
###############################################################################
file.list.2020 <- list.files("../climate_data/splash_prep_files/splash_2020",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2020 <- setNames(file.list.2020, 
                           str_extract(basename(file.list.2020), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2020 <- lapply(file.list.2020, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2020$Brazos_2020_18.2020splash$sf <- splash.readin.2020$Brazos_2020_18.2020splash$sf[seq_along(splash.readin.2020$Brazos_2020_18.2020splash$sf) %% 2 > 0]
splash.readin.2020$Brazos_2020_18.2020splash$tair <- splash.readin.2020$Brazos_2020_18.2020splash$tair[seq_along(splash.readin.2020$Brazos_2020_18.2020splash$tair) %% 2 > 0]
splash.readin.2020$Brazos_2020_18.2020splash$pn <- splash.readin.2020$Brazos_2020_18.2020splash$pn[seq_along(splash.readin.2020$Brazos_2020_18.2020splash$pn) %% 2 > 0]
splash.readin.2020$Brazos_2020_18.2020splash$num_lines <- 366

# Harris 2020-03
splash.readin.2020$Harris_2020_03.2020splash$sf <- splash.readin.2020$Harris_2020_03.2020splash$sf[seq_along(splash.readin.2020$Harris_2020_03.2020splash$sf) %% 2 > 0]
splash.readin.2020$Harris_2020_03.2020splash$tair <- splash.readin.2020$Harris_2020_03.2020splash$tair[seq_along(splash.readin.2020$Harris_2020_03.2020splash$tair) %% 2 > 0]
splash.readin.2020$Harris_2020_03.2020splash$pn <- splash.readin.2020$Harris_2020_03.2020splash$pn[seq_along(splash.readin.2020$Harris_2020_03.2020splash$pn) %% 2 > 0]
splash.readin.2020$Harris_2020_03.2020splash$num_lines <- 366

# Menard 2020-01
splash.readin.2020$Menard_2020_01.2020splash$sf <- splash.readin.2020$Menard_2020_01.2020splash$sf[seq_along(splash.readin.2020$Menard_2020_01.2020splash$sf) %% 2 > 0]
splash.readin.2020$Menard_2020_01.2020splash$tair <- splash.readin.2020$Menard_2020_01.2020splash$tair[seq_along(splash.readin.2020$Menard_2020_01.2020splash$tair) %% 2 > 0]
splash.readin.2020$Menard_2020_01.2020splash$pn <- splash.readin.2020$Menard_2020_01.2020splash$pn[seq_along(splash.readin.2020$Menard_2020_01.2020splash$pn) %% 2 > 0]
splash.readin.2020$Menard_2020_01.2020splash$num_lines <- 366

# Uvalde 2020-02
splash.readin.2020$Uvalde_2020_02.2020splash$sf <- splash.readin.2020$Uvalde_2020_02.2020splash$sf[seq_along(splash.readin.2020$Uvalde_2020_02.2020splash$sf) %% 2 > 0]
splash.readin.2020$Uvalde_2020_02.2020splash$tair <- splash.readin.2020$Uvalde_2020_02.2020splash$tair[seq_along(splash.readin.2020$Uvalde_2020_02.2020splash$tair) %% 2 > 0]
splash.readin.2020$Uvalde_2020_02.2020splash$pn <- splash.readin.2020$Uvalde_2020_02.2020splash$pn[seq_along(splash.readin.2020$Uvalde_2020_02.2020splash$pn) %% 2 > 0]
splash.readin.2020$Uvalde_2020_02.2020splash$num_lines <- 366


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2020 <- lapply(file.list.2020, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2020[["Brazos_2020_18.2020splash"]] <- splash.month.2020[["Brazos_2020_18.2020splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2020[["Harris_2020_03.2020splash"]] <- splash.month.2020[["Harris_2020_03.2020splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2020[["Menard_2020_01.2020splash"]] <- splash.month.2020[["Menard_2020_01.2020splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2020[["Uvalde_2020_02.2020splash"]] <- splash.month.2020[["Uvalde_2020_02.2020splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2020)) {
  splash.month.2020[[i]] <- splash.month.2020[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2020 <- lapply(file.list.2020, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2020 <- mapply(c, splash.readin.2020, splash.month.2020, 
                            splash.ancillary.2020, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2020)) {
  
  splash.total.2020[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2020[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2020[[i]][["num_lines"]])
  splash.total.2020[[i]][["daily_totals"]] <- as.data.frame(splash.total.2020[[i]][["daily_totals"]])
  names(splash.total.2020[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2020[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2020[[i]],
                                                      dtot = splash.total.2020[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2020 <- map(splash.total.2020, as.data.table)
splash.water.2020 <- rbindlist(splash.water.2020, fill = TRUE, idcol = TRUE)
splash.water.2020$year <- 2020
splash.water.2020$site <- str_extract(splash.water.2020$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2020 <- splash.water.2020 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2020 <- tidyr::unite(splash.water.2020, "date", year:day, sep = "-", remove = FALSE)
splash.water.2020$date <- lubridate::ymd(as.Date(splash.water.2020$date))
splash.water.2020$doy <- lubridate::yday(splash.water.2020$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2020 <- run_one_day(lat = splash.water.2020$lat_deg,
                                  elv = splash.water.2020$elv_m,
                                  n = splash.water.2020$doy,
                                  y = splash.water.2020$year,
                                  wn = splash.water.2020$soil.moisture,
                                  sf = splash.water.2020$sf,
                                  tc = splash.water.2020$tair,
                                  pn = splash.water.2020$pn,
                                  kWm = 150)

splash.oneday.2020 <- map(splash.oneday.2020, as.data.table)

## Change list column names
names(splash.oneday.2020[[1]]) <- "ho"
names(splash.oneday.2020[[2]]) <- "hn"
names(splash.oneday.2020[[3]]) <- "ppfd"
names(splash.oneday.2020[[4]]) <- "cond"
names(splash.oneday.2020[[5]]) <- "eet"
names(splash.oneday.2020[[6]]) <- "pet"
names(splash.oneday.2020[[7]]) <- "aet"
names(splash.oneday.2020[[8]]) <- "wn"
names(splash.oneday.2020[[9]]) <- "ro"

sites.daily.2020 <- splash.oneday.2020$ho %>%
  coalesce(splash.oneday.2020$hn) %>%
  coalesce(splash.oneday.2020$ppfd) %>%
  coalesce(splash.oneday.2020$cond) %>%
  coalesce(splash.oneday.2020$eet) %>%
  coalesce(splash.oneday.2020$pet) %>%
  coalesce(splash.oneday.2020$aet) %>%
  coalesce(splash.oneday.2020$wn) %>%
  coalesce(splash.oneday.2020$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2020) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Run SPLASH model with 2021 data
###############################################################################
file.list.2021 <- list.files("../climate_data/splash_prep_files/splash_2021",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2021 <- setNames(file.list.2021, 
                           str_extract(basename(file.list.2021), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2021 <- lapply(file.list.2021, read_csv)

## Remove duplicate rows in sites that received more than one site visit
# Brazos 2020-18
splash.readin.2021$Brazos_2020_18.2021splash$sf <- splash.readin.2021$Brazos_2020_18.2021splash$sf[seq_along(splash.readin.2021$Brazos_2020_18.2021splash$sf) %% 2 > 0]
splash.readin.2021$Brazos_2020_18.2021splash$tair <- splash.readin.2021$Brazos_2020_18.2021splash$tair[seq_along(splash.readin.2021$Brazos_2020_18.2021splash$tair) %% 2 > 0]
splash.readin.2021$Brazos_2020_18.2021splash$pn <- splash.readin.2021$Brazos_2020_18.2021splash$pn[seq_along(splash.readin.2021$Brazos_2020_18.2021splash$pn) %% 2 > 0]
splash.readin.2021$Brazos_2020_18.2021splash$num_lines <- 211

# Harris 2020-03
splash.readin.2021$Harris_2020_03.2021splash$sf <- splash.readin.2021$Harris_2020_03.2021splash$sf[seq_along(splash.readin.2021$Harris_2020_03.2021splash$sf) %% 2 > 0]
splash.readin.2021$Harris_2020_03.2021splash$tair <- splash.readin.2021$Harris_2020_03.2021splash$tair[seq_along(splash.readin.2021$Harris_2020_03.2021splash$tair) %% 2 > 0]
splash.readin.2021$Harris_2020_03.2021splash$pn <- splash.readin.2021$Harris_2020_03.2021splash$pn[seq_along(splash.readin.2021$Harris_2020_03.2021splash$pn) %% 2 > 0]
splash.readin.2021$Harris_2020_03.2021splash$num_lines <- 211

# Menard 2020-01
splash.readin.2021$Menard_2020_01.2021splash$sf <- splash.readin.2021$Menard_2020_01.2021splash$sf[seq_along(splash.readin.2021$Menard_2020_01.2021splash$sf) %% 2 > 0]
splash.readin.2021$Menard_2020_01.2021splash$tair <- splash.readin.2021$Menard_2020_01.2021splash$tair[seq_along(splash.readin.2021$Menard_2020_01.2021splash$tair) %% 2 > 0]
splash.readin.2021$Menard_2020_01.2021splash$pn <- splash.readin.2021$Menard_2020_01.2021splash$pn[seq_along(splash.readin.2021$Menard_2020_01.2021splash$pn) %% 2 > 0]
splash.readin.2021$Menard_2020_01.2021splash$num_lines <- 211

# Uvalde 2020-02
splash.readin.2021$Uvalde_2020_02.2021splash$sf <- splash.readin.2021$Uvalde_2020_02.2021splash$sf[seq_along(splash.readin.2021$Uvalde_2020_02.2021splash$sf) %% 2 > 0]
splash.readin.2021$Uvalde_2020_02.2021splash$tair <- splash.readin.2021$Uvalde_2020_02.2021splash$tair[seq_along(splash.readin.2021$Uvalde_2020_02.2021splash$tair) %% 2 > 0]
splash.readin.2021$Uvalde_2020_02.2021splash$pn <- splash.readin.2021$Uvalde_2020_02.2021splash$pn[seq_along(splash.readin.2021$Uvalde_2020_02.2021splash$pn) %% 2 > 0]
splash.readin.2021$Uvalde_2020_02.2021splash$num_lines <- 211


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2021 <- lapply(file.list.2021, read.csv)

## Remove multiple dates for sites visited more than one growing season
# Brazos 2020-18
splash.month.2021[["Brazos_2020_18.2021splash"]] <- splash.month.2021[["Brazos_2020_18.2021splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Harris 2020-03
splash.month.2021[["Harris_2020_03.2021splash"]] <- splash.month.2021[["Harris_2020_03.2021splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Menard 2020-01
splash.month.2021[["Menard_2020_01.2021splash"]] <- splash.month.2021[["Menard_2020_01.2021splash"]] %>%
  slice(which(row_number() %% 2 == 1))

# Uvalde 2020-02
splash.month.2021[["Uvalde_2020_02.2021splash"]] <- splash.month.2021[["Uvalde_2020_02.2021splash"]] %>%
  slice(which(row_number() %% 2 == 1))


## Loop to select only month and day
for (i in seq_along(splash.month.2021)) {
  splash.month.2021[[i]] <- splash.month.2021[[i]] %>%
    dplyr::select(m, i)
}

## Extract latitude and elevation of each site
splash.ancillary.2021 <- lapply(file.list.2021, read.csv) %>%
  map(., ~ (.x %>% group_by(site) %>%
              summarize(lat_deg = unique(lat_deg),
                        elv_m = unique(elv_m)) %>%
              dplyr::select(lat_deg, elv_m)))

## Merge test.ancillary with test
splash.total.2021 <- mapply(c, splash.readin.2021, splash.month.2021, 
                            splash.ancillary.2021, SIMPLIFY = FALSE)

## Loop for adding daily_totals blank data.frame, run spin up for daily soil
## moisture
for (i in seq_along(splash.total.2021)) {
  
  splash.total.2021[[i]][["daily_totals"]] <- matrix(data = rep(0, 
                                                                splash.total.2021[[i]][["num_lines"]]), 
                                                     nrow = splash.total.2021[[i]][["num_lines"]])
  splash.total.2021[[i]][["daily_totals"]] <- as.data.frame(splash.total.2021[[i]][["daily_totals"]])
  names(splash.total.2021[[i]][["daily_totals"]]) <- c("wn")
  
  splash.total.2021[[i]][["daily_totals"]] <- spin_up(mdat = splash.total.2021[[i]],
                                                      dtot = splash.total.2021[[i]]$daily_totals)
}



## Convert list of lists to data.frame (for run_one_day), revise column names
splash.water.2021 <- map(splash.total.2021, as.data.table)
splash.water.2021 <- rbindlist(splash.water.2021, fill = TRUE, idcol = TRUE)
splash.water.2021$year <- 2021
splash.water.2021$site <- str_extract(splash.water.2021$.id, 
                                      "[A-Za-z]{4,}_[0-9]{4}_[0-9]{2}")

splash.water.2021 <- splash.water.2021 %>%
  dplyr::select(site, lat_deg, elv_m, year, month = m, 
                day = i, sf:pn, soil.moisture = daily_totals) %>%
  as.data.frame()

splash.water.2021 <- tidyr::unite(splash.water.2021, "date", year:day, sep = "-", remove = FALSE)
splash.water.2021$date <- lubridate::ymd(as.Date(splash.water.2021$date))
splash.water.2021$doy <- lubridate::yday(splash.water.2021$date)


## Run run_one_day given equilibrated soil moisture
splash.oneday.2021 <- run_one_day(lat = splash.water.2021$lat_deg,
                                  elv = splash.water.2021$elv_m,
                                  n = splash.water.2021$doy,
                                  y = splash.water.2021$year,
                                  wn = splash.water.2021$soil.moisture,
                                  sf = splash.water.2021$sf,
                                  tc = splash.water.2021$tair,
                                  pn = splash.water.2021$pn,
                                  kWm = 150)

splash.oneday.2021 <- map(splash.oneday.2021, as.data.table)

## Change list column names
names(splash.oneday.2021[[1]]) <- "ho"
names(splash.oneday.2021[[2]]) <- "hn"
names(splash.oneday.2021[[3]]) <- "ppfd"
names(splash.oneday.2021[[4]]) <- "cond"
names(splash.oneday.2021[[5]]) <- "eet"
names(splash.oneday.2021[[6]]) <- "pet"
names(splash.oneday.2021[[7]]) <- "aet"
names(splash.oneday.2021[[8]]) <- "wn"
names(splash.oneday.2021[[9]]) <- "ro"

sites.daily.2021 <- splash.oneday.2021$ho %>%
  coalesce(splash.oneday.2021$hn) %>%
  coalesce(splash.oneday.2021$ppfd) %>%
  coalesce(splash.oneday.2021$cond) %>%
  coalesce(splash.oneday.2021$eet) %>%
  coalesce(splash.oneday.2021$pet) %>%
  coalesce(splash.oneday.2021$aet) %>%
  coalesce(splash.oneday.2021$wn) %>%
  coalesce(splash.oneday.2021$ro) %>%
  as.data.frame() %>%
  coalesce(splash.water.2021) %>% 
  dplyr::select(site:soil.moisture, ho:aet)

###############################################################################
## Merge SPLASHv1.0 2019, 2020, and 2021 data into central data.frame, merge
## site visit dates to then calculate 30, 60, 90 day moisture index means
###############################################################################
daily.clim <- sites.daily.1991 %>%
  full_join(sites.daily.1992) %>% full_join(sites.daily.1993) %>%
  full_join(sites.daily.1994) %>% full_join(sites.daily.1995) %>%
  full_join(sites.daily.1996) %>% full_join(sites.daily.1997) %>%
  full_join(sites.daily.1998) %>% full_join(sites.daily.1999) %>%
  full_join(sites.daily.2000) %>% full_join(sites.daily.2001) %>%
  full_join(sites.daily.2002) %>% full_join(sites.daily.2003) %>%
  full_join(sites.daily.2004) %>% full_join(sites.daily.2005) %>%
  full_join(sites.daily.2006) %>% full_join(sites.daily.2007) %>%
  full_join(sites.daily.2008) %>% full_join(sites.daily.2009) %>%
  full_join(sites.daily.2010) %>% full_join(sites.daily.2011) %>%
  full_join(sites.daily.2012) %>% full_join(sites.daily.2013) %>%
  full_join(sites.daily.2014) %>% full_join(sites.daily.2015) %>%
  full_join(sites.daily.2016) %>% full_join(sites.daily.2017) %>%
  full_join(sites.daily.2018) %>% full_join(sites.daily.2019) %>%
  full_join(sites.daily.2020) %>% full_join(sites.daily.2021) %>%
  full_join(sites) %>%
  dplyr::select(site, date, year, month, day, latitude, longitude, elv_m,
         sampling.year:primary.2021, sf:aet) %>%
  mutate(initial.2020 = ymd(initial.2020),
         primary.2020 = ymd(primary.2020),
         initial.2021 = ymd(initial.2021),
         primary.2021 = ymd(primary.2021))

###############################################################################
## Calculate 30-day, 60-day, 90-day aridity index means (PT coef, P/PET)
###############################################################################
# 2020 initial site visits
initial.2020eco <- daily.clim %>%
  dplyr::filter(site == "Bell_2020_05" | site == "Bexar_2019_13" | 
                  site == "Blanco_2019_16" | site == "Brazos_2020_18" | 
                  site == "Comal_2020_21" | site == "Edwards_2019_17" |
                  site == "Fayette_2019_04" | site == "Harris_2020_03" | 
                  site == "Menard_2020_01" | site == "Russel_2020_01" | 
                  site == "Sansaba_2020_01" | site == "Uvalde_2020_02" |
                  site == "Williamson_2019_09" | site == "Williamson_2019_10") %>%
  dplyr::filter(sampling.year == 2020) %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date),
         sampling.date = initial.2020,
         visit.type = "initial") %>%
  group_by(site, date) %>%
  distinct() %>%
  dplyr::select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, 
                date, month, year, sf:aet)

# 2020 primary site visits
primary.2020eco <- daily.clim %>%
  dplyr::filter(site == "Bexar_2019_13" | site == "Harris_2020_03" |
                  site == "Menard_2020_01" | site == "Uvalde_2020_02" |
                  site == "Williamson_2019_10") %>%
  dplyr::filter(sampling.year == 2020) %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date),
         sampling.date = primary.2020,
         visit.type = "primary") %>%
  group_by(site, date) %>%
  distinct() %>%  ## Note duplicate values for Harris, two rows of single dates
  dplyr::select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, 
                date, month, year, sf:aet)

# 2021 initial site visits
initial.2021eco <- daily.clim %>%
  dplyr::filter(site == "Austin_2020_03" | site == "Bandera_2020_03" |
                  site == "Bell_2021_08" | site == "Brazos_2020_16" |
                  site == "Brazos_2020_18" | site == "Burnet_2020_12" |
                  site == "Burnet_2020_14" | site == "Comal_2020_19" |
                  site == "Fayette_2020_09" | site == "Fayette_2021_12" |
                  site == "Harris_2020_03" | site == "Hays_2021_54" |
                  site == "Kerr_2020_03" | site == "Uvalde_2020_02" |
                  site == "Washington_2020_08") %>%
  dplyr::filter(sampling.year == 2021) %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date),
         sampling.date = initial.2021,
         visit.type = "initial") %>%
  group_by(site, date) %>%
  distinct() %>%  ## Note duplicate values for Harris, two rows of single dates
  dplyr::select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, 
                date, month, year, sf:aet)

# 2021 primary site visits
primary.2021eco <- daily.clim %>%
  dplyr::filter(site == "Bandera_2020_03" | site == "Bell_2021_08" | 
                  site == "Burnet_2020_12" | site == "Harris_2020_03" | 
                  site == "Uvalde_2020_02") %>%
  dplyr::filter(sampling.year == 2021) %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date),
         sampling.date = primary.2021,
         visit.type = "primary") %>%
  group_by(site, date) %>%
  distinct() %>%  ## Note duplicate values for Harris, two rows of single dates
  dplyr::select(site, latitude:elv_m, sampling.date, sampling.year, visit.type,
                date, month, year, sf:aet)

###############################################################################
## Merge initial and primary site 90-day SPLASH runs into single dataframe
###############################################################################
concat.clim <- initial.2020eco %>%
  full_join(initial.2021eco) %>%
  full_join(primary.2020eco) %>%
  full_join(primary.2021eco)

###############################################################################
## Calculate 30, 60, 90 day site Priestley-Taylor coefficient and aridity
## index (P/PET)
###############################################################################

txeco.splash.90day <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  filter(date > sampling.date - 90) %>%
  summarize(ho.90 = mean(ho, na.rm = TRUE),
            hn.90 = mean(hn, na.rm = TRUE),
            ppfd.90 = mean(ppfd, na.rm = TRUE),
            cond.90 = sum(cond, na.rm = TRUE),
            eet.90 = sum(eet, na.rm = TRUE),
            pet.90 = sum(pet, na.rm = TRUE),
            aet.90 = sum(aet, na.rm = TRUE),
            wn.90 = mean(soil.moisture, na.rm = TRUE),
            ai.90 = sum(pn, na.rm = TRUE) / sum(pet, na.rm = TRUE),
            pt.90 = sum(aet, na.rm = TRUE) / sum(eet, na.rm = TRUE))

txeco.splash.60day <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  filter(date > sampling.date - 60) %>%
  summarize(ho.60 = mean(ho, na.rm = TRUE),
            hn.60 = mean(hn, na.rm = TRUE),
            ppfd.60 = mean(ppfd, na.rm = TRUE),
            cond.60 = sum(cond, na.rm = TRUE),
            eet.60 = sum(eet, na.rm = TRUE),
            pet.60 = sum(pet, na.rm = TRUE),
            aet.60 = sum(aet, na.rm = TRUE),
            wn.60 = mean(soil.moisture, na.rm = TRUE),
            ai.60 = sum(pn, na.rm = TRUE) / sum(pet, na.rm = TRUE),
            pt.60 = sum(aet, na.rm = TRUE) / sum(eet, na.rm = TRUE))

txeco.splash.30day <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  filter(date > sampling.date - 30) %>%
  summarize(ho.30 = mean(ho, na.rm = TRUE),
            hn.30 = mean(hn, na.rm = TRUE),
            ppfd.30 = mean(ppfd, na.rm = TRUE),
            cond.30 = sum(cond, na.rm = TRUE),
            eet.30 = sum(eet, na.rm = TRUE),
            pet.30 = sum(pet, na.rm = TRUE),
            aet.30 = sum(aet, na.rm = TRUE),
            wn.30 = mean(soil.moisture, na.rm = TRUE),
            ai.30 = sum(pn, na.rm = TRUE) / sum(pet, na.rm = TRUE),
            pt.30 = sum(aet, na.rm = TRUE) / sum(eet, na.rm = TRUE))

## Merge files
site.gs.aridity <- txeco.splash.30day %>%
  full_join(txeco.splash.60day) %>%
  full_join(txeco.splash.90day)

###############################################################################
## Determine 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 60, 90 day soil moisture
###############################################################################
d1 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 1  & date <= sampling.date) %>%
  summarize(wn1 = soil.moisture)
d2 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 2  & date <= sampling.date) %>%
  summarize(wn2 = mean(soil.moisture, na.rm = TRUE))
d3 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 3  & date <= sampling.date) %>%
  summarize(wn3 = mean(soil.moisture, na.rm = TRUE))
d4 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 4  & date <= sampling.date) %>%
  summarize(wn4 = mean(soil.moisture, na.rm = TRUE))
d5 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 5  & date <= sampling.date) %>%
  summarize(wn5 = mean(soil.moisture, na.rm = TRUE))
d6 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 6  & date <= sampling.date) %>%
  summarize(wn6 = mean(soil.moisture, na.rm = TRUE))
d7 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 7  & date <= sampling.date) %>%
  summarize(wn7 = mean(soil.moisture, na.rm = TRUE))
d8 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 8  & date <= sampling.date) %>%
  summarize(wn8 = mean(soil.moisture, na.rm = TRUE))
d9 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 9  & date <= sampling.date) %>%
  summarize(wn9 = mean(soil.moisture, na.rm = TRUE))
d10 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 10  & date <= sampling.date) %>%
  summarize(wn10 = mean(soil.moisture, na.rm = TRUE))
d15 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 15  & date <= sampling.date) %>%
  summarize(wn15 = mean(soil.moisture, na.rm = TRUE))
d20 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 20  & date <= sampling.date) %>%
  summarize(wn20 = mean(soil.moisture, na.rm = TRUE))
d30 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 30  & date <= sampling.date) %>%
  summarize(wn30 = mean(soil.moisture, na.rm = TRUE))
d60 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 60  & date <= sampling.date) %>%
  summarize(wn60 = mean(soil.moisture, na.rm = TRUE))
d90 <- concat.clim %>% group_by(site, sampling.year, visit.type) %>% 
  filter(date > sampling.date - 90  & date <= sampling.date) %>%
  summarize(wn90 = mean(soil.moisture, na.rm = TRUE))

sm.timescales <- d1 %>% full_join(d2) %>% full_join(d3) %>% full_join(d4) %>% full_join(d5) %>%
  full_join(d6) %>% full_join(d7) %>% full_join(d8) %>% full_join(d9) %>%
  full_join(d10) %>% full_join(d15) %>% full_join(d20) %>% full_join(d30) %>% 
  full_join(d60) %>% full_join(d90) 

###############################################################################
## Calculate normal site Priestley-Taylor coefficient and aridity
## index (P/PET)
###############################################################################
txeco.splash.15yr.aridity <- concat.clim %>%
  filter(year >= 2006 & year <= 2020) %>%
  dplyr::group_by(site, year) %>%
  dplyr::summarize(ho = mean(ho, na.rm = TRUE),
                   hn = mean(hn, na.rm = TRUE),
                   ppfd = mean(ppfd, na.rm = TRUE),
                   cond = sum(cond, na.rm = TRUE),
                   eet = sum(eet, na.rm = TRUE),
                   pet = sum(pet, na.rm = TRUE),
                   aet = sum(aet, na.rm = TRUE),
                   wn = mean(soil.moisture, na.rm = TRUE),
                   ai = sum(pn, na.rm = TRUE) / sum(pet, na.rm = TRUE),
                   pt = sum(aet, na.rm = TRUE) / sum(eet, na.rm = TRUE)) %>%
  dplyr::ungroup(year) %>%
  dplyr::summarize(ho.15yr = mean(ho, na.rm = TRUE),
                   hn.15yr = mean(hn, na.rm = TRUE),
                   ppfd.15yr = mean(ppfd, na.rm = TRUE),
                   cond.15yr = mean(cond, na.rm = TRUE),
                   eet.15yr = mean(eet, na.rm = TRUE),
                   pet.15yr = mean(pet, na.rm = TRUE),
                   aet.15yr = mean(aet, na.rm = TRUE),
                   wn.15yr = mean(wn, na.rm = TRUE),
                   ai.15yr = mean(ai, na.rm = TRUE),
                   pt.15yr = mean(pt, na.rm = TRUE))

## Merge files
sites.aridity.indices <- txeco.splash.30day %>%
  full_join(txeco.splash.60day) %>%
  full_join(txeco.splash.90day) %>%
  full_join(txeco.splash.15yr.aridity, by = "site") %>%
  full_join(sm.timescales)

## Write 30, 60, 90 day and 15/30 year SPLASH results
write.csv(sites.aridity.indices, "../climate_data/TXeco_siteAridity_SPLASH.csv",
          row.names = FALSE)


