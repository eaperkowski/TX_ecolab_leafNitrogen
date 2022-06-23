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
                                  y = 1991,
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
splash.readin.1992$Brazos_2020_18.1992splash$num_lines <- 365

# Harris 2020-03
splash.readin.1992$Harris_2020_03.1992splash$sf <- splash.readin.1992$Harris_2020_03.1992splash$sf[seq_along(splash.readin.1992$Harris_2020_03.1992splash$sf) %% 2 > 0]
splash.readin.1992$Harris_2020_03.1992splash$tair <- splash.readin.1992$Harris_2020_03.1992splash$tair[seq_along(splash.readin.1992$Harris_2020_03.1992splash$tair) %% 2 > 0]
splash.readin.1992$Harris_2020_03.1992splash$pn <- splash.readin.1992$Harris_2020_03.1992splash$pn[seq_along(splash.readin.1992$Harris_2020_03.1992splash$pn) %% 2 > 0]
splash.readin.1992$Harris_2020_03.1992splash$num_lines <- 365

# Menard 2020-01
splash.readin.1992$Menard_2020_01.1992splash$sf <- splash.readin.1992$Menard_2020_01.1992splash$sf[seq_along(splash.readin.1992$Menard_2020_01.1992splash$sf) %% 2 > 0]
splash.readin.1992$Menard_2020_01.1992splash$tair <- splash.readin.1992$Menard_2020_01.1992splash$tair[seq_along(splash.readin.1992$Menard_2020_01.1992splash$tair) %% 2 > 0]
splash.readin.1992$Menard_2020_01.1992splash$pn <- splash.readin.1992$Menard_2020_01.1992splash$pn[seq_along(splash.readin.1992$Menard_2020_01.1992splash$pn) %% 2 > 0]
splash.readin.1992$Menard_2020_01.1992splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1992$Uvalde_2020_02.1992splash$sf <- splash.readin.1992$Uvalde_2020_02.1992splash$sf[seq_along(splash.readin.1992$Uvalde_2020_02.1992splash$sf) %% 2 > 0]
splash.readin.1992$Uvalde_2020_02.1992splash$tair <- splash.readin.1992$Uvalde_2020_02.1992splash$tair[seq_along(splash.readin.1992$Uvalde_2020_02.1992splash$tair) %% 2 > 0]
splash.readin.1992$Uvalde_2020_02.1992splash$pn <- splash.readin.1992$Uvalde_2020_02.1992splash$pn[seq_along(splash.readin.1992$Uvalde_2020_02.1992splash$pn) %% 2 > 0]
splash.readin.1992$Uvalde_2020_02.1992splash$num_lines <- 365


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
                                  y = 1992,
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
                                  y = 1993,
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
                                  y = 1994,
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
                                  y = 1995,
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
splash.readin.1996$Brazos_2020_18.1996splash$num_lines <- 365

# Harris 2020-03
splash.readin.1996$Harris_2020_03.1996splash$sf <- splash.readin.1996$Harris_2020_03.1996splash$sf[seq_along(splash.readin.1996$Harris_2020_03.1996splash$sf) %% 2 > 0]
splash.readin.1996$Harris_2020_03.1996splash$tair <- splash.readin.1996$Harris_2020_03.1996splash$tair[seq_along(splash.readin.1996$Harris_2020_03.1996splash$tair) %% 2 > 0]
splash.readin.1996$Harris_2020_03.1996splash$pn <- splash.readin.1996$Harris_2020_03.1996splash$pn[seq_along(splash.readin.1996$Harris_2020_03.1996splash$pn) %% 2 > 0]
splash.readin.1996$Harris_2020_03.1996splash$num_lines <- 365

# Menard 2020-01
splash.readin.1996$Menard_2020_01.1996splash$sf <- splash.readin.1996$Menard_2020_01.1996splash$sf[seq_along(splash.readin.1996$Menard_2020_01.1996splash$sf) %% 2 > 0]
splash.readin.1996$Menard_2020_01.1996splash$tair <- splash.readin.1996$Menard_2020_01.1996splash$tair[seq_along(splash.readin.1996$Menard_2020_01.1996splash$tair) %% 2 > 0]
splash.readin.1996$Menard_2020_01.1996splash$pn <- splash.readin.1996$Menard_2020_01.1996splash$pn[seq_along(splash.readin.1996$Menard_2020_01.1996splash$pn) %% 2 > 0]
splash.readin.1996$Menard_2020_01.1996splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.1996$Uvalde_2020_02.1996splash$sf <- splash.readin.1996$Uvalde_2020_02.1996splash$sf[seq_along(splash.readin.1996$Uvalde_2020_02.1996splash$sf) %% 2 > 0]
splash.readin.1996$Uvalde_2020_02.1996splash$tair <- splash.readin.1996$Uvalde_2020_02.1996splash$tair[seq_along(splash.readin.1996$Uvalde_2020_02.1996splash$tair) %% 2 > 0]
splash.readin.1996$Uvalde_2020_02.1996splash$pn <- splash.readin.1996$Uvalde_2020_02.1996splash$pn[seq_along(splash.readin.1996$Uvalde_2020_02.1996splash$pn) %% 2 > 0]
splash.readin.1996$Uvalde_2020_02.1996splash$num_lines <- 365


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
                                  y = 1996,
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
                                  y = 1997,
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
                                  y = 1998,
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
                                  y = 1999,
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
splash.readin.2000$Brazos_2020_18.2000splash$num_lines <- 365

# Harris 2020-03
splash.readin.2000$Harris_2020_03.2000splash$sf <- splash.readin.2000$Harris_2020_03.2000splash$sf[seq_along(splash.readin.2000$Harris_2020_03.2000splash$sf) %% 2 > 0]
splash.readin.2000$Harris_2020_03.2000splash$tair <- splash.readin.2000$Harris_2020_03.2000splash$tair[seq_along(splash.readin.2000$Harris_2020_03.2000splash$tair) %% 2 > 0]
splash.readin.2000$Harris_2020_03.2000splash$pn <- splash.readin.2000$Harris_2020_03.2000splash$pn[seq_along(splash.readin.2000$Harris_2020_03.2000splash$pn) %% 2 > 0]
splash.readin.2000$Harris_2020_03.2000splash$num_lines <- 365

# Menard 2020-01
splash.readin.2000$Menard_2020_01.2000splash$sf <- splash.readin.2000$Menard_2020_01.2000splash$sf[seq_along(splash.readin.2000$Menard_2020_01.2000splash$sf) %% 2 > 0]
splash.readin.2000$Menard_2020_01.2000splash$tair <- splash.readin.2000$Menard_2020_01.2000splash$tair[seq_along(splash.readin.2000$Menard_2020_01.2000splash$tair) %% 2 > 0]
splash.readin.2000$Menard_2020_01.2000splash$pn <- splash.readin.2000$Menard_2020_01.2000splash$pn[seq_along(splash.readin.2000$Menard_2020_01.2000splash$pn) %% 2 > 0]
splash.readin.2000$Menard_2020_01.2000splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2000$Uvalde_2020_02.2000splash$sf <- splash.readin.2000$Uvalde_2020_02.2000splash$sf[seq_along(splash.readin.2000$Uvalde_2020_02.2000splash$sf) %% 2 > 0]
splash.readin.2000$Uvalde_2020_02.2000splash$tair <- splash.readin.2000$Uvalde_2020_02.2000splash$tair[seq_along(splash.readin.2000$Uvalde_2020_02.2000splash$tair) %% 2 > 0]
splash.readin.2000$Uvalde_2020_02.2000splash$pn <- splash.readin.2000$Uvalde_2020_02.2000splash$pn[seq_along(splash.readin.2000$Uvalde_2020_02.2000splash$pn) %% 2 > 0]
splash.readin.2000$Uvalde_2020_02.2000splash$num_lines <- 365


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
                                  y = 2000,
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
                                  y = 2001,
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
                                  y = 2002,
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
                                  y = 2003,
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
splash.readin.2004$Brazos_2020_18.2004splash$num_lines <- 365

# Harris 2020-03
splash.readin.2004$Harris_2020_03.2004splash$sf <- splash.readin.2004$Harris_2020_03.2004splash$sf[seq_along(splash.readin.2004$Harris_2020_03.2004splash$sf) %% 2 > 0]
splash.readin.2004$Harris_2020_03.2004splash$tair <- splash.readin.2004$Harris_2020_03.2004splash$tair[seq_along(splash.readin.2004$Harris_2020_03.2004splash$tair) %% 2 > 0]
splash.readin.2004$Harris_2020_03.2004splash$pn <- splash.readin.2004$Harris_2020_03.2004splash$pn[seq_along(splash.readin.2004$Harris_2020_03.2004splash$pn) %% 2 > 0]
splash.readin.2004$Harris_2020_03.2004splash$num_lines <- 365

# Menard 2020-01
splash.readin.2004$Menard_2020_01.2004splash$sf <- splash.readin.2004$Menard_2020_01.2004splash$sf[seq_along(splash.readin.2004$Menard_2020_01.2004splash$sf) %% 2 > 0]
splash.readin.2004$Menard_2020_01.2004splash$tair <- splash.readin.2004$Menard_2020_01.2004splash$tair[seq_along(splash.readin.2004$Menard_2020_01.2004splash$tair) %% 2 > 0]
splash.readin.2004$Menard_2020_01.2004splash$pn <- splash.readin.2004$Menard_2020_01.2004splash$pn[seq_along(splash.readin.2004$Menard_2020_01.2004splash$pn) %% 2 > 0]
splash.readin.2004$Menard_2020_01.2004splash$num_lines <- 365

# Uvalde 2020-02
splash.readin.2004$Uvalde_2020_02.2004splash$sf <- splash.readin.2004$Uvalde_2020_02.2004splash$sf[seq_along(splash.readin.2004$Uvalde_2020_02.2004splash$sf) %% 2 > 0]
splash.readin.2004$Uvalde_2020_02.2004splash$tair <- splash.readin.2004$Uvalde_2020_02.2004splash$tair[seq_along(splash.readin.2004$Uvalde_2020_02.2004splash$tair) %% 2 > 0]
splash.readin.2004$Uvalde_2020_02.2004splash$pn <- splash.readin.2004$Uvalde_2020_02.2004splash$pn[seq_along(splash.readin.2004$Uvalde_2020_02.2004splash$pn) %% 2 > 0]
splash.readin.2004$Uvalde_2020_02.2004splash$num_lines <- 365


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
                                  y = 2004,
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
                                  y = 2005,
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

################# Stopped here



###############################################################################
## Prepare and run SPLASH v1.0 model for 2019 data
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


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2019 <- lapply(file.list.2019, read.csv)

for (i in seq_along(splash.month.2019)) {
  splash.month.2019[[i]] <- splash.month.2019[[i]] %>%
    dplyr::select(m, i)
}

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
                                  y = 2019,
                                  wn = splash.water.2019$soil.moisture,
                                  sf = splash.water.2019$sf,
                                  tc = splash.water.2019$tair,
                                  pn = splash.water.2019$pn,
                                  kWm = 500)

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
## Prepare and run SPLASH v1.0 model for 2020 data
###############################################################################

## List files
file.list.2020 <- list.files("../climate_data/splash_prep_files/splash_2020",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2020 <- setNames(file.list.2020, 
                           str_extract(basename(file.list.2020), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2020 <- lapply(file.list.2020, read_csv)


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2020 <- lapply(file.list.2020, read.csv)

for (i in seq_along(splash.month.2020)) {
  splash.month.2020[[i]] <- splash.month.2020[[i]] %>%
    dplyr::select(m, i)
}

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
                                  kWm = 500)
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
## Prepare and run SPLASH v1.0 model for 2021 data
###############################################################################

## List files
file.list.2021 <- list.files("../climate_data/splash_2021",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)

file.list.2021 <- setNames(file.list.2021, 
                           str_extract(basename(file.list.2021), 
                                       '.*(?=\\.csv)'))

## Read files based on 'splash' read_csv function
splash.readin.2021 <- lapply(file.list.2021, read_csv)


## Load ancillary details needed to run 'spin_up" and 'run_one_day'
splash.month.2021 <- lapply(file.list.2021, read.csv)

for (i in seq_along(splash.month.2021)) {
  splash.month.2021[[i]] <- splash.month.2021[[i]] %>%
    dplyr::select(m, i)
}

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
                                  kWm = 500)
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
daily.clim <- sites.daily.2019 %>%
  full_join(sites.daily.2020) %>%
  full_join(sites.daily.2021) %>%
  full_join(sites) %>%
  select(site, date, year, month, day, latitude, longitude, elv_m,
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
         sampling.date = initial.2020,
         visit.type = "initial") %>%
  group_by(site, date) %>%
  distinct() %>%
  select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, date, sf:aet) %>%
  ungroup(date) %>%
  filter(date > sampling.date - 90 & date <= sampling.date)

# 2020 primary site visits
primary.2020eco <- daily.clim %>%
  dplyr::filter(site == "Bexar_2019_13" | site == "Harris_2020_03" |
                  site == "Menard_2020_01" | site == "Uvalde_2020_02" |
                  site == "Williamson_2019_10") %>%
  dplyr::filter(sampling.year == 2020) %>%
  mutate(date = ymd(date),
         sampling.date = primary.2020,
         visit.type = "primary") %>%
  group_by(site, date) %>%
  distinct() %>%  ## Note duplicate values for Harris, two rows of single dates
  select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, date, sf:aet) %>%
  ungroup(date) %>%
  filter(date > sampling.date - 90 & date <= sampling.date)


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
         sampling.date = initial.2021,
         visit.type = "initial") %>%
  group_by(site, date) %>%
  distinct() %>%  ## Note duplicate values for Harris, two rows of single dates
  select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, date, sf:aet) %>%
  ungroup(date) %>%
  filter(date > sampling.date - 90 & date <= sampling.date)

# 2021 primary site visits
primary.2021eco <- daily.clim %>%
  dplyr::filter(site == "Bandera_2020_03" | site == "Bell_2021_08" | 
                  site == "Burnet_2020_12" | site == "Harris_2020_03" | 
                  site == "Uvalde_2020_02") %>%
  dplyr::filter(sampling.year == 2021) %>%
  mutate(date = ymd(date),
         sampling.date = primary.2021,
         visit.type = "primary") %>%
  group_by(site, date) %>%
  distinct() %>%  ## Note duplicate values for Harris, two rows of single dates
  select(site, latitude:elv_m, sampling.date, sampling.year, visit.type, date, sf:aet) %>%
  ungroup(date) %>%
  filter(date > sampling.date - 90 & date <= sampling.date)

###############################################################################
## Merge initial and primary site 90-day SPLASH runs into single dataframe
###############################################################################
concat.clim <- initial.2020eco %>%
  full_join(initial.2021eco) %>%
  full_join(primary.2020eco) %>%
  full_join(primary.2021eco)

###############################################################################
## Load sampling date logs, append to initial.2020eco object, subset data further
## by 90 days leading up to site visit
###############################################################################
txeco.splash.90day <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(pt.90 = mean(aet/eet, na.rm = TRUE),
            ai.90 = sum(pn)/sum(pet))

txeco.splash.60day <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  filter(date > sampling.date - 60) %>%
  summarize(pt.60 = mean(aet/eet, na.rm = TRUE),
            ai.60 = sum(pn)/sum(pet))

txeco.splash.30day <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  filter(date > sampling.date - 30) %>%
  summarize(pt.30 = mean(aet/eet, na.rm = TRUE),
            ai.30 = sum(pn)/sum(pet))

## Merge files
sites.aridity.indices <- txeco.splash.30day %>%
  full_join(txeco.splash.60day) %>%
  full_join(txeco.splash.90day)


## Write 30, 60, 90 day aridity index mean file
write.csv(sites.aridity.indices, "../climate_data/TXeco_gsSiteAridity_SPLASH.csv",
          row.names = FALSE)







