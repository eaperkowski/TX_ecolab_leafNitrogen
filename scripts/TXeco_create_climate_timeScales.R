###############################################################################
## Libraries
###############################################################################
library(dplyr)
library(lubridate)

###############################################################################
## Load daily PRISM data for 2020 and 2021 sites (initial and primary). Will
## later be merged into single data.frame to calculate Tavg and precip
###############################################################################
# 2020 initial site visits
initial.2020eco <- read.csv("../climate_data/TXeco_PRISM_daily.csv") %>%
  dplyr::filter(site == "Bell_2020_05" | site == "Bexar_2019_13" | 
                  site == "Blanco_2019_16" | site == "Brazos_2020_18" | 
                  site == "Comal_2020_21" | site == "Edwards_2019_17" |
                  site == "Fayette_2019_04" | site == "Harris_2020_03" | 
                  site == "Menard_2020_01" | site == "Russel_2020_01" | 
                  site == "Sansaba_2020_01" | site == "Uvalde_2020_02" |
                  site == "Williamson_2019_09" | site == "Williamson_2019_10") %>%
  mutate(date = ymd(date)) %>%
  group_by(site, date) %>%
  distinct()

# 2020 primary site visits
primary.2020eco <- read.csv("../climate_data/TXeco_PRISM_daily.csv") %>%
  dplyr::filter(site == "Bexar_2019_13" | site == "Harris_2020_03" |
                  site == "Menard_2020_01" | site == "Uvalde_2020_02" |
                  site == "Williamson_2019_10") %>%
  mutate(date = ymd(date)) %>%
  group_by(site, date) %>%
  distinct() ## Note duplicate values for Harris, two rows of single dates

# 2021 initial site visits
initial.2021eco <- read.csv("../climate_data/TXeco_PRISM_daily.csv") %>%
  dplyr::filter(site == "Austin_2020_03" | site == "Bandera_2020_03" |
                  site == "Bell_2021_08" | site == "Brazos_2020_16" |
                  site == "Brazos_2020_18" | site == "Burnet_2020_12" |
                  site == "Burnet_2020_14" | site == "Comal_2020_19" |
                  site == "Fayette_2020_09" | site == "Fayette_2021_12" |
                  site == "Harris_2020_03" | site == "Hays_2021_54" |
                  site == "Kerr_2020_03" | site == "Uvalde_2020_02" |
                  site == "Washington_2020_08") %>%
  mutate(date = ymd(date)) %>%
  group_by(site, date) %>%
  distinct()

# 2021 primary site visits
primary.2021eco <- read.csv("../climate_data/TXeco_PRISM_daily.csv") %>%
  dplyr::filter(site == "Bandera_2020_03" | site == "Bell_2021_08" | 
                  site == "Burnet_2020_12" | site == "Harris_2020_03" | 
                  site == "Uvalde_2020_02") %>%
  mutate(date = ymd(date)) %>%
  group_by(site, date) %>%
  distinct()

###############################################################################
## Load sampling date logs, append to initial.2020eco object, subset data further
## by 90 days leading up to site visit
###############################################################################
initial.2020eco <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  dplyr::filter(sampling.year == 2020) %>%
  dplyr::select(site = property, sampling.date = initial.2020) %>%
  mutate(sampling.date = ymd(sampling.date),
         sampling.year = 2020,
         visit.type = "initial") %>%
  full_join(initial.2020eco) %>%
  group_by(site)

primary.2020eco <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  dplyr::filter(sampling.year == 2020 & !is.na(primary.2020)) %>%
  dplyr::select(site = property, sampling.date = primary.2020) %>%
  mutate(sampling.date = ymd(sampling.date),
         sampling.year = 2020,
         visit.type = "primary") %>%
  full_join(primary.2020eco) %>%
  group_by(site)

initial.2021eco <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  dplyr::filter(sampling.year == 2021) %>%
  dplyr::select(site = property, sampling.date = initial.2021) %>%
  mutate(sampling.date = ymd(sampling.date),
         sampling.year = 2021,
         visit.type = "initial") %>%
  full_join(initial.2021eco) %>%
  group_by(site)

primary.2021eco <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  dplyr::filter(sampling.year == 2021 & !is.na(primary.2021)) %>%
  dplyr::select(site = property, sampling.date = primary.2021) %>%
  mutate(sampling.date = ymd(sampling.date),
         sampling.year = 2021,
         visit.type = "primary") %>%
  full_join(primary.2021eco) %>%
  group_by(site)

###############################################################################
## Merge initial and primary site 90-day climate date into single datafile
###############################################################################
concat.clim <- initial.2020eco %>%
  full_join(initial.2021eco) %>%
  full_join(primary.2020eco) %>%
  full_join(primary.2021eco) %>%
  mutate(daily.vpdmean = (daily.vpdmax + daily.vpdmin)/2) %>%
  dplyr::select(-m, -i)

###############################################################################
## Calculate 15-year climate normals. Will be later merged into
## timescale dataframe
###############################################################################
norm.15yr <- concat.clim %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(site, sampling.year, visit.type, year) %>%
  summarize(map = sum(daily.prcp, na.rm = TRUE),
            mat = mean(daily.tmean, na.rm = TRUE),
            mav = mean(daily.vpdmean, na.rm = TRUE),
            matmin = mean(daily.tmin, na.rm = TRUE),
            matmax = mean(daily.tmax, na.rm = TRUE)) %>%
  ungroup(year) %>%
  summarize(map.15yr = mean(map),
            mat.15yr = mean(mat),
            mav.15yr = mean(mav))

###############################################################################
## Iteratively calculate mean temperature and precipitation totals from 1 day
## to 90 days leading up to measurement period
###############################################################################
d365 <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  filter(date > sampling.date - 365 & date < sampling.date) %>%
  summarize(tavg365 = mean(daily.tmean, na.rm = TRUE),
            prcp365 = sum(daily.prcp, na.rm = TRUE),
            vpd365 = mean(daily.vpdmean, na.rm = TRUE),
            sf365 = mean(sf)) %>%
  na.omit()

d90 <- concat.clim %>%
  filter(date > sampling.date - 90 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg90 = mean(daily.tmean, na.rm = TRUE),
            prcp90 = sum(daily.prcp, na.rm = TRUE),
            vpd90 = mean(daily.vpdmean, na.rm = TRUE),
            sf90 = mean(sf)) %>%
  na.omit()

d60 <- concat.clim %>% filter(date > sampling.date - 60 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg60 = mean(daily.tmean, na.rm = TRUE),
            prcp60 = sum(daily.prcp, na.rm = TRUE),
            vpd60 = mean(daily.vpdmean, na.rm = TRUE),
            sf60 = mean(sf))

d30 <- concat.clim %>% filter(date > sampling.date - 30 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg30 = mean(daily.tmean, na.rm = TRUE),
            prcp30 = sum(daily.prcp, na.rm = TRUE),
            vpd30 = mean(daily.vpdmean, na.rm = TRUE),
            sf30 = mean(sf))

d20 <- concat.clim %>% filter(date > sampling.date - 20 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg20 = mean(daily.tmean, na.rm = TRUE),
            prcp20 = sum(daily.prcp, na.rm = TRUE),
            vpd20 = mean(daily.vpdmean, na.rm = TRUE),
            sf20 = mean(sf))

d15 <- concat.clim %>% filter(date > sampling.date - 15 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg15 = mean(daily.tmean, na.rm = TRUE),
            prcp15 = sum(daily.prcp, na.rm = TRUE),
            vpd15 = mean(daily.vpdmean, na.rm = TRUE),
            sf15 = mean(sf))

d10 <- concat.clim %>% filter(date > sampling.date - 10 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg10 = mean(daily.tmean, na.rm = TRUE),
            prcp10 = sum(daily.prcp, na.rm = TRUE),
            vpd10 = mean(daily.vpdmean, na.rm = TRUE),
            sf10 = mean(sf))

d9 <- concat.clim %>% filter(date > sampling.date - 9 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg9 = mean(daily.tmean, na.rm = TRUE),
            prcp9 = sum(daily.prcp, na.rm = TRUE),
            vpd9 = mean(daily.vpdmean, na.rm = TRUE),
            sf9 = mean(sf))

d8 <- concat.clim %>% filter(date > sampling.date - 8 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg8 = mean(daily.tmean, na.rm = TRUE),
            prcp8 = sum(daily.prcp, na.rm = TRUE),
            vpd8 = mean(daily.vpdmean, na.rm = TRUE),
            sf8 = mean(sf))

d7 <- concat.clim %>% filter(date > sampling.date - 7 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg7 = mean(daily.tmean, na.rm = TRUE),
            prcp7 = sum(daily.prcp, na.rm = TRUE),
            vpd7 = mean(daily.vpdmean, na.rm = TRUE),
            sf7 = mean(sf))

d6 <- concat.clim %>% filter(date > sampling.date - 6 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg6 = mean(daily.tmean, na.rm = TRUE),
            prcp6 = sum(daily.prcp, na.rm = TRUE),
            vpd6 = mean(daily.vpdmean, na.rm = TRUE),
            sf6 = mean(sf))

d5 <- concat.clim %>% filter(date > sampling.date - 5 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg5 = mean(daily.tmean, na.rm = TRUE),
            prcp5 = sum(daily.prcp, na.rm = TRUE),
            vpd5 = mean(daily.vpdmean, na.rm = TRUE),
            sf5 = mean(sf))

d4 <- concat.clim %>% filter(date < sampling.date - 4 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg4 = mean(daily.tmean, na.rm = TRUE),
            prcp4 = sum(daily.prcp, na.rm = TRUE),
            vpd4 = mean(daily.vpdmean, na.rm = TRUE),
            sf4 = mean(sf))

d3 <- concat.clim %>% filter(date > sampling.date - 3 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg3 = mean(daily.tmean, na.rm = TRUE),
            prcp3 = sum(daily.prcp, na.rm = TRUE),
            vpd3 = mean(daily.vpdmean, na.rm = TRUE),
            sf3 = mean(sf))

d2 <- concat.clim %>% filter(date > sampling.date - 2 & date < sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg2 = mean(daily.tmean, na.rm = TRUE),
            prcp2 = sum(daily.prcp, na.rm = TRUE),
            vpd2 = mean(daily.vpdmean, na.rm = TRUE),
            sf2 = mean(sf))

d1 <- concat.clim %>% filter(date > sampling.date - 1 & date <= sampling.date) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg1 = daily.tmean,
            prcp1 = daily.prcp,
            vpd1 = daily.vpdmean,
            sf1 = sf)

## Merge all iterative climate means with normals data frame
## Also merge aridity index values for single climate data file
d <- norm.15yr %>% full_join(d365) %>% full_join(d90) %>% full_join(d60) %>%
  full_join(d30) %>% full_join(d20) %>% full_join(d15) %>% full_join(d10) %>%
  full_join(d9) %>% full_join(d8) %>% full_join(d7) %>% 
  full_join(d6) %>% full_join(d5) %>% full_join(d4) %>% full_join(d3) %>% 
  full_join(d2) %>% full_join(d1) 

## Read in climate aridity index values
splash.df <- read.csv("../climate_data/TXeco_siteAridity_SPLASH.csv")

d <- d %>% full_join(splash.df) %>% 
  dplyr::select(site:mav.15yr, 
                ai.30:ai.15yr,
                everything())

## Write csv
write.csv(d, "../climate_data/TXeco_climate_data.csv",
          row.names = FALSE)
