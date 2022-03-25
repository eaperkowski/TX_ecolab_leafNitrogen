## Script: clean climate normal data

###############################################################################
## Load libraries, Ecolab site coordinates, set PRISM directory, add pubtheme
###############################################################################
# Load libraries
library(dplyr)
library(raster)
library(prism)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(terra)
library(plotbiomes)
library(viridis)
library(stringr)

# Load Ecolab site coords
ecosites <- read.csv("data_sheets/TXeco_sitecoords.csv") # add path to site coords file here
ecosites <- distinct(ecosites, property, .keep_all = TRUE) # remove duplicate sites
eco.coords <- dplyr::select(ecosites, x = longitude, y = latitude) # select only lat/long data, code as xy

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("climate_data/prism/prism_monthly/")


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
df.sites.precip <- as.data.frame(terra::extract(pd.precip,
                                                SpatialPoints(eco.coords),
                                                sp = F))

# Add precipitation site, latitude, longitude data
df.sites.precip$latitude = eco.coords$y
df.sites.precip$longitude = eco.coords$x
df.sites.precip$site = ecosites$property

df.sites.precip <- df.sites.precip %>%
  dplyr::select(site, latitude, longitude, everything())

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
names(df.sites.precip) <- c("site", "latitude", "longitude",
                            gsub("^","precip_",str_extract(names(df.sites.precip[4:363]),
                                                         "[0-9]{6}")))
names(df.sites.precip) <- c("site", "latitude", "longitude",
                            str_c("precip", str_extract(names(df.sites.precip[4:363]),
                                              "[0-9]{2}$"),
                                  str_extract(names(df.sites.precip[4:363]),
                                              "[0-9]{4}"), sep = "_"))


# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
df.sites.precip <- df.sites.precip %>%
  tidyr::pivot_longer(cols = precip_01_1991:precip_12_2020,
               names_to = "date", values_to = "monthly.prcp") %>%
  mutate(date = str_replace(date, "precip_", "")) %>%
  tidyr::separate(date, into = c("month", "year"), "_") %>%
  group_by(site, month) %>%
  summarize(monthly.precip = mean(monthly.prcp, na.rm = TRUE))

annual.prcp <- df.sites.precip %>%
  group_by(site) %>%
  summarize(map = sum(monthly.precip, na.rm = TRUE))


################
# Tmean
################
pd.tmean <- pd_stack(prism_archive_subset("tmean", "monthly", 
                                          years = 1991:2020, mon = 1:12))
df.sites.tmean <- as.data.frame(terra::extract(pd.tmean,
                                               SpatialPoints(eco.coords),
                                               sp = F))

# Add  site, latitude, longitude data
df.sites.tmean$latitude = eco.coords$y
df.sites.tmean$longitude = eco.coords$x
df.sites.tmean$site = ecosites$property


# Rename columns to have climate data indicator as prefix with month/year
# as suffix
df.sites.tmean <- df.sites.tmean %>%
  dplyr::select(site, latitude, longitude, everything())

names(df.sites.tmean) <- c("site", "latitude", "longitude",
                           gsub("^",
                                str_extract(names(df.sites.tmean[4:363]),
                                            "[0-9]{6}")))
names(df.sites.tmean) <- c("site", "latitude", "longitude",
                           str_c("tmean", 
                                 str_extract(names(df.sites.tmean[4:363]),
                                             "[0-9]{2}$"),
                                 str_extract(names(df.sites.tmean[4:363]),
                                             "[0-9]{4}"), sep = "_"))


# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
df.sites.tmean <- df.sites.tmean %>%
  tidyr::pivot_longer(cols = tmean_01_1991:tmean_12_2020,
               names_to = "date", values_to = "monthly.tmean") %>%
  mutate(date = str_replace(date, "tmean_", "")) %>%
  tidyr::separate(date, into = c("month", "year"), "_") %>%
  group_by(site, month) %>%
  summarize(monthly.tmean = mean(monthly.tmean, na.rm = TRUE))

annual.tmean <- df.sites.tmean %>%
  group_by(site) %>%
  summarize(mat = mean(monthly.tmean, na.rm = TRUE))

################
# Tmax
################
pd.tmax <- pd_stack(prism_archive_subset("tmax", "monthly", 
                                         years = 1991:2020, mon = 1:12))
df.sites.tmax <- as.data.frame(terra::extract(pd.tmax,
                                              SpatialPoints(eco.coords),
                                              sp = F))

# Add  site, latitude, longitude data
df.sites.tmax$latitude = eco.coords$y
df.sites.tmax$longitude = eco.coords$x
df.sites.tmax$site = ecosites$property


# Rename columns to have climate data indicator as prefix with month/year
# as suffix
df.sites.tmax <- df.sites.tmax %>%
  dplyr::select(site, latitude, longitude, everything())

names(df.sites.tmax) <- c("site", "latitude", "longitude",
                           gsub("^","",
                                str_extract(names(df.sites.tmax[4:363]),
                                            "[0-9]{6}")))
names(df.sites.tmax) <- c("site", "latitude", "longitude",
                           str_c("tmax", 
                                 str_extract(names(df.sites.tmax[4:363]),
                                             "[0-9]{2}$"),
                                 str_extract(names(df.sites.tmax[4:363]),
                                             "[0-9]{4}"), sep = "_"))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
df.sites.tmax <- df.sites.tmax %>%
  tidyr::pivot_longer(cols = tmax_01_1991:tmax_12_2020,
               names_to = "date", values_to = "monthly.tmax") %>%
  mutate(date = str_replace(date, "tmax_", "")) %>%
  tidyr::separate(date, into = c("month", "year"), "_") %>%
  group_by(site, month) %>%
  summarize(monthly.tmax = mean(monthly.tmax, na.rm = TRUE))

annual.tmax <- df.sites.tmax %>%
  group_by(site) %>%
  summarize(tmax = mean(monthly.tmax, na.rm = TRUE))

################
# Tmin
################
pd.tmin <- pd_stack(prism_archive_subset("tmin", "monthly", 
                                         years = 1991:2020, mon = 1:12))
df.sites.tmin <- as.data.frame(terra::extract(pd.tmin,
                                              SpatialPoints(eco.coords),
                                              sp = F))

# Add  site, latitude, longitude data
df.sites.tmin$latitude = eco.coords$y
df.sites.tmin$longitude = eco.coords$x
df.sites.tmin$site = ecosites$property

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
df.sites.tmin <- df.sites.tmin %>%
  dplyr::select(site, latitude, longitude, everything())

names(df.sites.tmin) <- c("site", "latitude", "longitude",
                          gsub("^","",
                               str_extract(names(df.sites.tmin[4:363]),
                                           "[0-9]{6}")))
names(df.sites.tmin) <- c("site", "latitude", "longitude",
                          str_c("tmin", 
                                str_extract(names(df.sites.tmin[4:363]),
                                            "[0-9]{2}$"),
                                str_extract(names(df.sites.tmin[4:363]),
                                            "[0-9]{4}"), sep = "_"))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
df.sites.tmin <- df.sites.tmin %>%
  tidyr::pivot_longer(cols = tmin_01_1991:tmin_12_2020,
                      names_to = "date", values_to = "monthly.tmin") %>%
  mutate(date = str_replace(date, "tmin_", "")) %>%
  tidyr::separate(date, into = c("month", "year"), "_") %>%
  group_by(site, month) %>%
  summarize(monthly.tmin = mean(monthly.tmin, na.rm = TRUE))

annual.tmin <- df.sites.tmin %>%
  group_by(site) %>%
  summarize(tmin = mean(monthly.tmin, na.rm = TRUE))



################
# VPDmax
################
pd.vpdmax <- pd_stack(prism_archive_subset("vpdmax", "monthly", 
                                           years = 1991:2020, mon = 1:12))
df.sites.vpdmax<- as.data.frame(terra::extract(pd.vpdmax,
                                               SpatialPoints(eco.coords),
                                               sp = F))

# Add  site, latitude, longitude data
df.sites.vpdmax$latitude = eco.coords$y
df.sites.vpdmax$longitude = eco.coords$x
df.sites.vpdmax$site = ecosites$property

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
df.sites.vpdmax <- df.sites.vpdmax %>%
  dplyr::select(site, latitude, longitude, everything())

names(df.sites.vpdmax) <- c("site", "latitude", "longitude",
                          gsub("^","",
                               str_extract(names(df.sites.vpdmax[4:363]),
                                           "[0-9]{6}")))
names(df.sites.vpdmax) <- c("site", "latitude", "longitude",
                          str_c("vpdmax", 
                                str_extract(names(df.sites.vpdmax[4:363]),
                                            "[0-9]{2}$"),
                                str_extract(names(df.sites.vpdmax[4:363]),
                                            "[0-9]{4}"), sep = "_"))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
df.sites.vpdmax <- df.sites.vpdmax %>%
  tidyr::pivot_longer(cols = vpdmax_01_1991:vpdmax_12_2020,
                      names_to = "date", values_to = "monthly.vpdmax") %>%
  mutate(date = str_replace(date, "vpdmax_", "")) %>%
  tidyr::separate(date, into = c("month", "year"), "_") %>%
  group_by(site, month) %>%
  summarize(monthly.vpdmax = mean(monthly.vpdmax, na.rm = TRUE))

annual.vpdmax <- df.sites.vpdmax %>%
  group_by(site) %>%
  summarize(vpdmax = mean(monthly.vpdmax, na.rm = TRUE))

################
# VPDmin
################
pd.vpdmin <- pd_stack(prism_archive_subset("vpdmin", "monthly", 
                                           years = 1991:2020, mon = 1:12))
df.sites.vpdmin <- as.data.frame(terra::extract(pd.vpdmin,
                                                SpatialPoints(eco.coords),
                                                sp = F))

# Add  site, latitude, longitude data
df.sites.vpdmin$latitude = eco.coords$y
df.sites.vpdmin$longitude = eco.coords$x
df.sites.vpdmin$site = ecosites$property

# Rename columns to have climate data indicator as prefix with month/year
# as suffix
df.sites.vpdmin <- df.sites.vpdmin %>%
  dplyr::select(site, latitude, longitude, everything())

names(df.sites.vpdmin) <- c("site", "latitude", "longitude",
                            gsub("^","",
                                 str_extract(names(df.sites.vpdmin[4:363]),
                                             "[0-9]{6}")))
names(df.sites.vpdmin) <- c("site", "latitude", "longitude",
                            str_c("vpdmin", 
                                  str_extract(names(df.sites.vpdmin[4:363]),
                                              "[0-9]{2}$"),
                                  str_extract(names(df.sites.vpdmin[4:363]),
                                              "[0-9]{4}"), sep = "_"))

# Pivot to long data format and summarize precipitation by month (1991-2020)
# to simulate 1991-2020 climate normals
df.sites.vpdmin <- df.sites.vpdmin %>%
  tidyr::pivot_longer(cols = vpdmin_01_1991:vpdmin_12_2020,
                      names_to = "date", values_to = "monthly.vpdmin") %>%
  mutate(date = str_replace(date, "vpdmin_", "")) %>%
  tidyr::separate(date, into = c("month", "year"), "_") %>%
  group_by(site, month) %>%
  summarize(monthly.vpdmin = mean(monthly.vpdmin, na.rm = TRUE))

annual.vpdmin <- df.sites.vpdmin %>%
  group_by(site) %>%
  summarize(vpdmin = mean(monthly.vpdmin, na.rm = TRUE))

## Join normals data into single dataset
normals <- annual.prcp %>%
  full_join(annual.tmean) %>%
  full_join(annual.tmax) %>%
  full_join(annual.tmin) %>%
  full_join(annual.vpdmax) %>%
  full_join(annual.vpdmin)

## Write climate normals .csv
write.csv(normals, "climate_data/TXeco_PRISM_19912020_sitenorms.csv")

## Plot map/mat and whittaker biomes together
ggplot(data = normals, aes(x = mat, y = map / 10)) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, fill = factor(biome)),
               color = "gray98", size = 0.5) +
  geom_point(size = 3, shape = 21, color = "black", alpha = 0.75) +
  scale_fill_brewer(palette = "Spectral") +
  #scale_y_continuous(limits = c(50, 150), breaks = seq(50, 150, 25)) +
  #scale_x_continuous(limits = c(18, 22), breaks = seq(18, 22, 1)) +
  labs(x = expression(bold("Mean annual temperature ("~degree~"C)")),
       y = "Mean annual precipitation (cm)",
       fill = "Biome type") +
  pubtheme +
  theme(legend.text = element_text(size = 10))

