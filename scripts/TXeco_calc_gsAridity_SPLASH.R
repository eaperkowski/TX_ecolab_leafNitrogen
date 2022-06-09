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
  select(-site) %>%
  select(site = property, everything())

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







