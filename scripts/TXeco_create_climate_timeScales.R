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
## Calculate 15-year and 30-year climate normals. Will be later merged into
## timescale dataframe
###############################################################################
norm.30yr <- concat.clim %>%
  filter(year != 2021) %>%
  group_by(site, sampling.year, visit.type, year) %>%
  summarize(map = sum(daily.prcp, na.rm = TRUE),
            mat = mean(daily.tmean, na.rm = TRUE),
            mav = mean(daily.vpdmean, na.rm = TRUE),
            matmin = mean(daily.tmin, na.rm = TRUE)) %>%
  ungroup(year) %>%
  summarize(map.30yr = mean(map),
            mat.30yr = mean(mat),
            mav.30yr = mean(mav))

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

normals <- norm.15yr %>%
  full_join(norm.30yr)
  

###############################################################################
## Iteratively calculate mean temperature and precipitation totals from 1 day
## to 90 days leading up to measurement period
###############################################################################
d90 <- concat.clim %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg90 = mean(daily.tmean),
            prcp90 = sum(daily.prcp),
            vpd90 = mean(daily.vpdmean, na.rm = TRUE),
            sf90 = mean(sf)) %>%
  na.omit()
  

d89 <- concat.clim %>%
  filter(date > sampling.date - 89) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg89 = mean(daily.tmean),
            prcp89 = sum(daily.prcp),
            vpd89 = mean(daily.vpdmean),
            sf89 = mean(sf))

d88 <- concat.clim %>%
  filter(date > sampling.date - 88) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg88 = mean(daily.tmean),
            prcp88 = sum(daily.prcp),
            vpd88 = mean(daily.vpdmean),
            sf88 = mean(sf))

d87 <- concat.clim %>%
  filter(date > sampling.date - 87) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg87 = mean(daily.tmean),
            prcp87 = sum(daily.prcp),
            vpd87 = mean(daily.vpdmean),
            sf87 = mean(sf))

d86 <- concat.clim %>% filter(date > sampling.date - 86) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg86 = mean(daily.tmean),
            prcp86 = sum(daily.prcp),
            vpd86 = mean(daily.vpdmean),
            sf86 = mean(sf))

d85 <- concat.clim %>% filter(date > sampling.date - 85) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg85 = mean(daily.tmean),
            prcp85 = sum(daily.prcp),
            vpd85 = mean(daily.vpdmean),
            sf85 = mean(sf))

d84 <- concat.clim %>% filter(date > sampling.date - 84) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg84 = mean(daily.tmean),
            prcp84 = sum(daily.prcp),
            vpd84 = mean(daily.vpdmean),
            sf84 = mean(sf))

d83 <- concat.clim %>% filter(date > sampling.date - 83) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg83 = mean(daily.tmean),
            prcp83 = sum(daily.prcp),
            vpd83 = mean(daily.vpdmean),
            sf83 = mean(sf))

d82 <- concat.clim %>% filter(date > sampling.date - 82) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg82 = mean(daily.tmean),
            prcp82 = sum(daily.prcp),
            vpd82 = mean(daily.vpdmean),
            sf82 = mean(sf))

d81 <- concat.clim %>% filter(date > sampling.date - 81) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg81 = mean(daily.tmean),
            prcp81 = sum(daily.prcp),
            vpd81 = mean(daily.vpdmean),
            sf81 = mean(sf))

d80 <- concat.clim %>% filter(date > sampling.date - 80) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg80 = mean(daily.tmean),
            prcp80 = sum(daily.prcp),
            vpd80 = mean(daily.vpdmean),
            sf80 = mean(sf))

d79 <- concat.clim %>% filter(date > sampling.date - 79) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg79 = mean(daily.tmean),
            prcp79 = sum(daily.prcp),
            vpd79 = mean(daily.vpdmean),
            sf79 = mean(sf))

d78 <- concat.clim %>% filter(date > sampling.date - 78) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg78 = mean(daily.tmean),
            prcp78 = sum(daily.prcp),
            vpd78 = mean(daily.vpdmean),
            sf78 = mean(sf))

d77 <- concat.clim %>% filter(date > sampling.date - 77) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg77 = mean(daily.tmean),
            prcp77 = sum(daily.prcp),
            vpd77 = mean(daily.vpdmean),
            sf77 = mean(sf))

d76 <- concat.clim %>% filter(date > sampling.date - 76) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg76 = mean(daily.tmean),
            prcp76 = sum(daily.prcp),
            vpd76 = mean(daily.vpdmean),
            sf76 = mean(sf))

d75 <- concat.clim %>% filter(date > sampling.date - 75) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg75 = mean(daily.tmean),
            prcp75 = sum(daily.prcp),
            vpd75 = mean(daily.vpdmean),
            sf75 = mean(sf))

d74 <- concat.clim %>% filter(date > sampling.date - 74) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg74 = mean(daily.tmean),
            prcp74 = sum(daily.prcp),
            vpd74 = mean(daily.vpdmean),
            sf74 = mean(sf))

d73 <- concat.clim %>% filter(date > sampling.date - 73) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg73 = mean(daily.tmean),
            prcp73 = sum(daily.prcp),
            vpd73 = mean(daily.vpdmean),
            sf73 = mean(sf))

d72 <- concat.clim %>% filter(date > sampling.date - 72) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg72 = mean(daily.tmean),
            prcp72 = sum(daily.prcp),
            vpd72 = mean(daily.vpdmean),
            sf72 = mean(sf))

d71 <- concat.clim %>% filter(date > sampling.date - 71) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg71 = mean(daily.tmean),
            prcp71 = sum(daily.prcp),
            vpd71 = mean(daily.vpdmean),
            sf71 = mean(sf))

d70 <- concat.clim %>% filter(date > sampling.date - 70) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg70 = mean(daily.tmean),
            prcp70 = sum(daily.prcp),
            vpd70 = mean(daily.vpdmean),
            sf70 = mean(sf))

d69 <- concat.clim %>% filter(date > sampling.date - 69) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg69 = mean(daily.tmean),
            prcp69 = sum(daily.prcp),
            vpd69 = mean(daily.vpdmean),
            sf69 = mean(sf))

d68 <- concat.clim %>% filter(date > sampling.date - 68) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg68 = mean(daily.tmean),
            prcp68 = sum(daily.prcp),
            vpd68 = mean(daily.vpdmean),
            sf68 = mean(sf))

d67 <- concat.clim %>% filter(date > sampling.date - 67) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg67 = mean(daily.tmean),
            prcp67 = sum(daily.prcp),
            vpd67 = mean(daily.vpdmean),
            sf67 = mean(sf))

d66 <- concat.clim %>% filter(date > sampling.date - 66) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg66 = mean(daily.tmean),
            prcp66 = sum(daily.prcp),
            vpd66 = mean(daily.vpdmean),
            sf66 = mean(sf))

d65 <- concat.clim %>% filter(date > sampling.date - 65) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg65 = mean(daily.tmean),
            prcp65 = sum(daily.prcp),
            vpd65 = mean(daily.vpdmean),
            sf65 = mean(sf))

d64 <- concat.clim %>% filter(date > sampling.date - 64) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg64 = mean(daily.tmean),
            prcp64 = sum(daily.prcp),
            vpd64 = mean(daily.vpdmean),
            sf64 = mean(sf))

d63 <- concat.clim %>% filter(date > sampling.date - 63) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg63 = mean(daily.tmean),
            prcp63 = sum(daily.prcp),
            vpd63 = mean(daily.vpdmean),
            sf63 = mean(sf))

d62 <- concat.clim %>% filter(date > sampling.date - 62) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg62 = mean(daily.tmean),
            prcp62 = sum(daily.prcp),
            vpd62 = mean(daily.vpdmean),
            sf62 = mean(sf))

d61 <- concat.clim %>% filter(date > sampling.date - 61) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg61 = mean(daily.tmean),
            prcp61 = sum(daily.prcp),
            vpd61 = mean(daily.vpdmean),
            sf61 = mean(sf))

d60 <- concat.clim %>% filter(date > sampling.date - 60) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg60 = mean(daily.tmean),
            prcp60 = sum(daily.prcp),
            vpd60 = mean(daily.vpdmean),
            sf60 = mean(sf))

d59 <- concat.clim %>% filter(date > sampling.date - 59) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg59 = mean(daily.tmean),
            prcp59 = sum(daily.prcp),
            vpd59 = mean(daily.vpdmean),
            sf59 = mean(sf))

d58 <- concat.clim %>% filter(date > sampling.date - 58) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg58 = mean(daily.tmean),
            prcp58 = sum(daily.prcp),
            vpd58 = mean(daily.vpdmean),
            sf58 = mean(sf))

d57 <- concat.clim %>% filter(date > sampling.date - 57) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg57 = mean(daily.tmean),
            prcp57 = sum(daily.prcp),
            vpd57 = mean(daily.vpdmean),
            sf57 = mean(sf))

d56 <- concat.clim %>% filter(date > sampling.date - 56) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg56 = mean(daily.tmean),
            prcp56 = sum(daily.prcp),
            vpd56 = mean(daily.vpdmean),
            sf56 = mean(sf))

d55 <- concat.clim %>% filter(date > sampling.date - 55) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg55 = mean(daily.tmean),
            prcp55 = sum(daily.prcp),
            vpd55 = mean(daily.vpdmean),
            sf55 = mean(sf))

d54 <- concat.clim %>% filter(date > sampling.date - 54) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg54 = mean(daily.tmean),
            prcp54 = sum(daily.prcp),
            vpd54 = mean(daily.vpdmean),
            sf54 = mean(sf))

d53 <- concat.clim %>% filter(date > sampling.date - 53) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg53 = mean(daily.tmean),
            prcp53 = sum(daily.prcp),
            vpd53 = mean(daily.vpdmean),
            sf53 = mean(sf))

d52 <- concat.clim %>% filter(date > sampling.date - 52) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg52 = mean(daily.tmean),
            prcp52 = sum(daily.prcp),
            vpd52 = mean(daily.vpdmean),
            sf52 = mean(sf))

d51 <- concat.clim %>% filter(date > sampling.date - 51) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg51 = mean(daily.tmean),
            prcp51 = sum(daily.prcp),
            vpd51 = mean(daily.vpdmean),
            sf51 = mean(sf))

d50 <- concat.clim %>% filter(date > sampling.date - 50) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg50 = mean(daily.tmean),
            prcp50 = sum(daily.prcp),
            vpd50 = mean(daily.vpdmean),
            sf50 = mean(sf))

d49 <- concat.clim %>% filter(date > sampling.date - 49) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg49 = mean(daily.tmean),
            prcp49 = sum(daily.prcp),
            vpd49 = mean(daily.vpdmean),
            sf49 = mean(sf))

d48 <- concat.clim %>% filter(date > sampling.date - 48) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg48 = mean(daily.tmean),
            prcp48 = sum(daily.prcp),
            vpd48 = mean(daily.vpdmean),
            sf48 = mean(sf))

d47 <- concat.clim %>% filter(date > sampling.date - 47) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg47 = mean(daily.tmean),
            prcp47 = sum(daily.prcp),
            vpd47 = mean(daily.vpdmean),
            sf47 = mean(sf))

d46 <- concat.clim %>% filter(date > sampling.date - 46) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg46 = mean(daily.tmean),
            prcp46 = sum(daily.prcp),
            vpd46 = mean(daily.vpdmean),
            sf46 = mean(sf))

d45 <- concat.clim %>% filter(date > sampling.date - 45) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg45 = mean(daily.tmean),
            prcp45 = sum(daily.prcp),
            vpd45 = mean(daily.vpdmean),
            sf45 = mean(sf))

d44 <- concat.clim %>% filter(date > sampling.date - 44) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg44 = mean(daily.tmean),
            prcp44 = sum(daily.prcp),
            vpd44 = mean(daily.vpdmean),
            sf44 = mean(sf))

d43 <- concat.clim %>% filter(date > sampling.date - 43) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg43 = mean(daily.tmean),
            prcp43 = sum(daily.prcp),
            vpd43 = mean(daily.vpdmean),
            sf43 = mean(sf))

d42 <- concat.clim %>% filter(date > sampling.date - 42) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg42 = mean(daily.tmean),
            prcp42 = sum(daily.prcp),
            vpd42 = mean(daily.vpdmean),
            sf42 = mean(sf))

d41 <- concat.clim %>% filter(date > sampling.date - 41) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg41 = mean(daily.tmean),
            prcp41 = sum(daily.prcp),
            vpd41 = mean(daily.vpdmean),
            sf41 = mean(sf))

d40 <- concat.clim %>% filter(date > sampling.date - 40) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg40 = mean(daily.tmean),
            prcp40 = sum(daily.prcp),
            vpd40 = mean(daily.vpdmean),
            sf40 = mean(sf))

d39 <- concat.clim %>% filter(date > sampling.date - 39) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg39 = mean(daily.tmean),
            prcp39 = sum(daily.prcp),
            vpd39 = mean(daily.vpdmean),
            sf39 = mean(sf))

d38 <- concat.clim %>% filter(date > sampling.date - 38) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg38 = mean(daily.tmean),
            prcp38 = sum(daily.prcp),
            vpd38 = mean(daily.vpdmean),
            sf38 = mean(sf))

d37 <- concat.clim %>% filter(date > sampling.date - 37) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg37 = mean(daily.tmean),
            prcp37 = sum(daily.prcp),
            vpd37 = mean(daily.vpdmean),
            sf37 = mean(sf))

d36 <- concat.clim %>% filter(date > sampling.date - 36) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg36 = mean(daily.tmean),
            prcp36 = sum(daily.prcp),
            vpd36 = mean(daily.vpdmean),
            sf36 = mean(sf))

d35 <- concat.clim %>% filter(date > sampling.date - 35) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg35 = mean(daily.tmean),
            prcp35 = sum(daily.prcp),
            vpd35 = mean(daily.vpdmean),
            sf35 = mean(sf))

d34 <- concat.clim %>% filter(date > sampling.date - 34) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg34 = mean(daily.tmean),
            prcp34 = sum(daily.prcp),
            vpd34 = mean(daily.vpdmean),
            sf34 = mean(sf))

d33 <- concat.clim %>% filter(date > sampling.date - 33) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg33 = mean(daily.tmean),
            prcp33 = sum(daily.prcp),
            vpd33 = mean(daily.vpdmean),
            sf33 = mean(sf))

d32 <- concat.clim %>% filter(date > sampling.date - 32) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg32 = mean(daily.tmean),
            prcp32 = sum(daily.prcp),
            vpd32 = mean(daily.vpdmean),
            sf32 = mean(sf))

d31 <- concat.clim %>% filter(date > sampling.date - 31) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg31 = mean(daily.tmean),
            prcp31 = sum(daily.prcp),
            vpd31 = mean(daily.vpdmean),
            sf31 = mean(sf))

d30 <- concat.clim %>% filter(date > sampling.date - 30) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg30 = mean(daily.tmean),
            prcp30 = sum(daily.prcp),
            vpd30 = mean(daily.vpdmean),
            sf30 = mean(sf))

d29 <- concat.clim %>% filter(date > sampling.date - 29) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg29 = mean(daily.tmean),
            prcp29 = sum(daily.prcp),
            vpd29 = mean(daily.vpdmean),
            sf29 = mean(sf))

d28 <- concat.clim %>% filter(date > sampling.date - 28) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg28 = mean(daily.tmean),
            prcp28 = sum(daily.prcp),
            vpd28 = mean(daily.vpdmean),
            sf28 = mean(sf))

d27 <- concat.clim %>% filter(date > sampling.date - 27) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg27 = mean(daily.tmean),
            prcp27 = sum(daily.prcp),
            vpd27 = mean(daily.vpdmean),
            sf27 = mean(sf))

d26 <- concat.clim %>% filter(date > sampling.date - 26) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg26 = mean(daily.tmean),
            prcp26 = sum(daily.prcp),
            vpd26 = mean(daily.vpdmean),
            sf26 = mean(sf))

d25 <- concat.clim %>% filter(date > sampling.date - 25) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg25 = mean(daily.tmean),
            prcp25 = sum(daily.prcp),
            vpd25 = mean(daily.vpdmean),
            sf25 = mean(sf))

d24 <- concat.clim %>% filter(date > sampling.date - 24) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg24 = mean(daily.tmean),
            prcp24 = sum(daily.prcp),
            vpd24 = mean(daily.vpdmean),
            sf24 = mean(sf))

d23 <- concat.clim %>% filter(date > sampling.date - 23) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg23 = mean(daily.tmean),
            prcp23 = sum(daily.prcp),
            vpd23 = mean(daily.vpdmean),
            sf23 = mean(sf))

d22 <- concat.clim %>% filter(date > sampling.date - 22) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg22 = mean(daily.tmean),
            prcp22 = sum(daily.prcp),
            vpd22 = mean(daily.vpdmean),
            sf22 = mean(sf))

d21 <- concat.clim %>% filter(date > sampling.date - 21) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg21 = mean(daily.tmean),
            prcp21 = sum(daily.prcp),
            vpd21 = mean(daily.vpdmean),
            sf21 = mean(sf))

d20 <- concat.clim %>% filter(date > sampling.date - 20) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg20 = mean(daily.tmean),
            prcp20 = sum(daily.prcp),
            vpd20 = mean(daily.vpdmean),
            sf20 = mean(sf))

d19 <- concat.clim %>% filter(date > sampling.date - 19) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg19 = mean(daily.tmean),
            prcp19 = sum(daily.prcp),
            vpd19 = mean(daily.vpdmean),
            sf19 = mean(sf))

d18 <- concat.clim %>% filter(date > sampling.date - 18) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg18 = mean(daily.tmean),
            prcp18 = sum(daily.prcp),
            vpd18 = mean(daily.vpdmean),
            sf18 = mean(sf))

d17 <- concat.clim %>% filter(date > sampling.date - 17) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg17 = mean(daily.tmean),
            prcp17 = sum(daily.prcp),
            vpd17 = mean(daily.vpdmean),
            sf17 = mean(sf))

d16 <- concat.clim %>% filter(date > sampling.date - 16) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg16 = mean(daily.tmean),
            prcp16 = sum(daily.prcp),
            vpd16 = mean(daily.vpdmean),
            sf16 = mean(sf))

d15 <- concat.clim %>% filter(date > sampling.date - 15) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg15 = mean(daily.tmean),
            prcp15 = sum(daily.prcp),
            vpd15 = mean(daily.vpdmean),
            sf15 = mean(sf))

d14 <- concat.clim %>% filter(date > sampling.date - 14) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg14 = mean(daily.tmean),
            prcp14 = sum(daily.prcp),
            vpd14 = mean(daily.vpdmean),
            sf14 = mean(sf))

d13 <- concat.clim %>% filter(date > sampling.date - 13) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg13 = mean(daily.tmean),
            prcp13 = sum(daily.prcp),
            vpd13 = mean(daily.vpdmean),
            sf13 = mean(sf))

d12 <- concat.clim %>% filter(date > sampling.date - 12) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg12 = mean(daily.tmean),
            prcp12 = sum(daily.prcp),
            vpd12 = mean(daily.vpdmean),
            sf12 = mean(sf))

d11 <- concat.clim %>% filter(date > sampling.date - 11) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg11 = mean(daily.tmean),
            prcp11 = sum(daily.prcp),
            vpd11 = mean(daily.vpdmean),
            sf11 = mean(sf))

d10 <- concat.clim %>% filter(date > sampling.date - 10) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg10 = mean(daily.tmean),
            prcp10 = sum(daily.prcp),
            vpd10 = mean(daily.vpdmean),
            sf10 = mean(sf))

d9 <- concat.clim %>% filter(date > sampling.date - 9) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg9 = mean(daily.tmean),
            prcp9 = sum(daily.prcp),
            vpd9 = mean(daily.vpdmean),
            sf9 = mean(sf))

d8 <- concat.clim %>% filter(date > sampling.date - 8) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg8 = mean(daily.tmean),
            prcp8 = sum(daily.prcp),
            vpd8 = mean(daily.vpdmean),
            sf8 = mean(sf))

d7 <- concat.clim %>% filter(date > sampling.date - 7) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg7 = mean(daily.tmean),
            prcp7 = sum(daily.prcp),
            vpd7 = mean(daily.vpdmean),
            sf7 = mean(sf))

d6 <- concat.clim %>% filter(date > sampling.date - 6) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg6 = mean(daily.tmean),
            prcp6 = sum(daily.prcp),
            vpd6 = mean(daily.vpdmean),
            sf6 = mean(sf))

d5 <- concat.clim %>% filter(date > sampling.date - 5) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg5 = mean(daily.tmean),
            prcp5 = sum(daily.prcp),
            vpd5 = mean(daily.vpdmean),
            sf5 = mean(sf))

d4 <- concat.clim %>% filter(date > sampling.date - 4) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg4 = mean(daily.tmean),
            prcp4 = sum(daily.prcp),
            vpd4 = mean(daily.vpdmean),
            sf4 = mean(sf))

d3 <- concat.clim %>% filter(date > sampling.date - 3) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg3 = mean(daily.tmean),
            prcp3 = sum(daily.prcp),
            vpd3 = mean(daily.vpdmean),
            sf3 = mean(sf))

d2 <- concat.clim %>% filter(date > sampling.date - 2) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg2 = mean(daily.tmean),
            prcp2 = sum(daily.prcp),
            vpd2 = mean(daily.vpdmean),
            sf2 = mean(sf))

d1 <- concat.clim %>% filter(date > sampling.date - 1) %>%
  group_by(site, sampling.year, visit.type) %>%
  summarize(tavg1 = mean(daily.tmean),
            prcp1 = sum(daily.prcp),
            vpd1 = mean(daily.vpdmean),
            sf1 = mean(sf))




## Merge all iterative climate means with normals data frame
## Also merge aridity index values for single climate data file
d <- normals %>% full_join(d30) %>% full_join(d29) %>% full_join(d28) %>% full_join(d27) %>% 
  full_join(d26) %>% full_join(d25) %>% full_join(d24) %>% full_join(d23) %>% 
  full_join(d22) %>% full_join(d21) %>% full_join(d20) %>% full_join(d19) %>% 
  full_join(d18) %>% full_join(d17) %>% full_join(d16) %>% full_join(d15) %>% 
  full_join(d14) %>% full_join(d13) %>% full_join(d12) %>% full_join(d11) %>% 
  full_join(d10) %>% full_join(d9) %>% full_join(d8) %>% full_join(d7) %>% 
  full_join(d6) %>% full_join(d5) %>% full_join(d4) %>% full_join(d3) %>% 
  full_join(d2) %>% full_join(d1) 

## Read in climate aridity index values
splash.df <- read.csv("../climate_data/TXeco_siteAridity_SPLASH.csv")

d <- d %>% full_join(splash.df) %>% 
  dplyr::select(site:mav.30yr, 
                ai.30:ai.30yr,
                everything())

## Write csv
write.csv(d, "../climate_data/TXeco_climate_data.csv",
          row.names = FALSE)
