## Load libraries
library(splash)
library(dplyr)
library(jsonlite)
library(purrr)
library(data.table)
library(stringr)
library(ggplot2)

###############################################################################
## Prepare and run SPLASH v1.0 model for 2019 data
###############################################################################

## List files
file.list.2019 <- list.files("../climate_data/splash_2019",
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
                                 "[0-9]{4}eco_[A-Za-z0-9]*")

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
                                  pn = splash.water.2019$pn)
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
file.list.2020 <- list.files("../climate_data/splash_2020",
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
                                      "[0-9]{4}eco_[A-Za-z0-9]*")

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
                                      "[0-9]{4}eco_[A-Za-z0-9]*")

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
                                  pn = splash.water.2021$pn)
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
## Merge SPLASHv1.0 2019, 2020, and 2021 data into central data.frame,
## calculate monthly alpha and aridity values
###############################################################################
daily.clim <- sites.daily.2019 %>%
  full_join(sites.daily.2020) %>%
  full_join(sites.daily.2021)

splash.monthly <- daily.clim %>%
  tidyr::unite("month.year", month:year, sep = "/", remove = FALSE) %>%
  mutate(month.year = lubridate::my(month.year)) %>%
  group_by(site, year, month) %>%
  summarize(month.prcp = sum(pn, na.rm = TRUE),
            month.aet = sum(aet, na.rm = TRUE),
            month.pet = sum(pet, na.rm = TRUE),
            month.eet = sum(eet, na.rm = TRUE)) %>%
  mutate(alpha = month.aet / month.eet,
         alpha.stand = (alpha - min(alpha)) / (max(alpha) - min(alpha)),
         aridity = month.prcp / month.pet) %>%
  right_join(daily.clim, by = c("site", "year", "month")) %>%
  select(site, lat_deg:date, month, day, year, sf:aet, month.prcp:aridity)



