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
library(ggsn)
library(maps)

# Load Ecolab site coords
ecosites <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  filter(site != "2020eco_Bell" & site != "2020eco_Russel") # add path to site coords file here
ecosites <- distinct(ecosites, property, .keep_all = TRUE) # remove duplicate sites

## Modify sampling year col for repeat site visits
ecosites$sampling.year[ecosites$property == "Brazos_2020_18" |
                         ecosites$property == "Harris_2020_03" |
                         ecosites$property == "Uvalde_2020_02"] <- "2020_2021"
ecosites.short <- dplyr::select(ecosites, 
                                site = property, 
                                latitude,
                                longitude,
                                elevation.m, 
                                sampling.year)

eco.coords <- dplyr::select(ecosites, x = longitude, y = latitude) # select only lat/long data, code as xy

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("../climate_data/prism/prism_monthly/")

## Iridescent palette: 
iridescent <- c("#FEFBE9", "#FCF7D5", "#F5F3C1", "#EAF0B5",
                "#DDECBF", "#D0E7CA", "#C2E3D2", "#B5DDD8",
                "#A8D8DC", "#9BD2E1", "#8DCBE4", "#81C4E7",
                "#7BBCE7", "#7EB2E4", "#88A5DD", "#9398D2",
                "#9B8AC4", "#9D7DB2", "#9A709E", "#906388",
                "#805770", "#684957", "#46353A", "#000000")
iridescent.reverse <- c("#000000", "#46353A", "#684957", "#805770",
                        "#906388", "#9A709E", "#9D7DB2", "#9B8AC4",
                        "#9398D2", "#88A5DD", "#7EB2E4", "#7BBCE7",
                        "#81C4E7", "#8DCBE4", "#9BD2E1", "#A8D8DC", 
                        "#B5DDD8", "#C2E3D2", "#D0E7CA", "#DDECBF", 
                        "#EAF0B5", "#F5F3C1", "#FCF7D5", "#FEFBE9")

###############################################################################
## Download temp/precip normal data from PRISM
###############################################################################
# Get temperature and precipitation normal data (only need to do this once-
# imports data files into central file)

## Workaround to not having PRISM 1991-2020 climate norms (download monthly
## and then calculate mean across years)
#get_prism_monthlys(type = "tmean", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "ppt", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "tmin", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "tmax", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "tdmean", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "vpdmin", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "vpdmax", years = 1991:2020, mon = 1:12, keepZip = FALSE)
# 
# get_prism_normals(type = "tmean", "4km", annual = TRUE, keepZip = FALSE)
# get_prism_normals(type = "ppt", "4km", annual = TRUE, keepZip = FALSE)
# get_prism_normals(type = "tmin", "4km", annual = TRUE, keepZip = FALSE)
# get_prism_normals(type = "tmax", "4km", annual = TRUE, keepZip = FALSE)
# get_prism_normals(type = "tdmean", "4km", annual = TRUE, keepZip = FALSE)
# get_prism_normals(type = "vpdmin", "4km", annual = TRUE, keepZip = FALSE)
# get_prism_normals(type = "vpdmax", "4km", annual = TRUE, keepZip = FALSE)

###############################################################################
## Create US shapefile to mask rasters to only Texas
###############################################################################
# Note: no need to do this if you're wanting a full US map. Or, change
# "texas" to desired state if you want "subset" climate data from a single
# state
us <- getData("GADM", country="USA", level = 1, path = "../climate_data/")
texas <- us[match(toupper("texas"), toupper(us$NAME_1)), ]

###############################################################################
## Stack raster files, mask to include TX and convert to df
###############################################################################
# Precipitation (pd_stack stacks all monthly climate normal data into single 
# RasterStack)

## Stack Rasterbricks, crop and mask values to only include TX
pd.precip <- pd_stack(prism_archive_subset("ppt", "monthly"))
precip.masked <- mask(crop(pd.precip, extent(texas)), texas) # turns to rasterBrick file
precip.masked.df <- as.data.frame(rasterToPoints(precip.masked))

## Pivot raster columns to long format, remove prefix and group by month
## precip. Then, in a step, calculate 2006-2020 mean annual precipitation 
## of all grid cells 
precip.masked.df_cleaned <- precip.masked.df %>%
  tidyr::pivot_longer(cols = PRISM_ppt_stable_4kmM3_199101_bil:PRISM_ppt_stable_4kmM3_202012_bil,
                      names_prefix = "PRISM_ppt_stable_4kmM3_",
                      values_to = "monthly.prcp",
                      names_to = "month") %>%
  mutate(month = gsub("*_bil", "", month),
         date = lubridate::ym(month),
         year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(x, y, year) %>%
  summarize(annual.prcp = sum(monthly.prcp, na.rm = TRUE)) %>%
  ungroup(year) %>%
  summarize(map = mean(annual.prcp, na.rm = TRUE))

min(precip.masked.df_cleaned$map)


## Extract monthly precipitation data from grid cell containing each site.
## Note that grid cell is on 4km resolution
df.sites.precip <- as.data.frame(terra::extract(pd.precip,
                                                SpatialPoints(eco.coords),
                                                sp = F))
df.sites.precip$latitude = eco.coords$y
df.sites.precip$longitude = eco.coords$x
df.sites.precip$site = ecosites$property

## Pivot raster columns to long format, remove prefix and group by month
## precip. Then, in a step, calculate 2006-2020 mean annual precipitation 
## of all grid cells 
df.sites.precip_cleaned <- df.sites.precip %>%
  tidyr::pivot_longer(cols = PRISM_ppt_stable_4kmM3_199101_bil:PRISM_ppt_stable_4kmM3_202012_bil,
               names_prefix = "PRISM_ppt_stable_4kmM3_",
               values_to = "monthly.prcp",
               names_to = "month") %>%
  mutate(month = gsub("*_bil", "", month),
         date = lubridate::ym(month),
         year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(site, latitude, longitude, year) %>%
  summarize(annual.prcp = sum(monthly.prcp)) %>%
  ungroup(year) %>%
  summarize(map = mean(annual.prcp, na.rm = TRUE))


# Temperature
pd.temp <- pd_stack(prism_archive_subset("tmean", "monthly"))
temp.masked <- mask(crop(pd.temp, extent(texas)), texas)
temp.masked.df <- as.data.frame(rasterToPoints(temp.masked))

## Pivot raster columns to long format, remove prefix and group by month
## precip. Then, in a step, calculate 2006-2020 mean annual precipitation 
## of all grid cells 
temp.masked.df_cleaned <- temp.masked.df %>%
  tidyr::pivot_longer(cols = PRISM_tmean_stable_4kmM3_199101_bil:PRISM_tmean_stable_4kmM3_202012_bil,
                      names_prefix = "PRISM_tmean_stable_4kmM3_",
                      values_to = "monthly.mean.temp",
                      names_to = "month") %>%
  mutate(month = gsub("*_bil", "", month),
         date = lubridate::ym(month),
         year = lubridate::year(date),
         month = lubridate::month(date))

temp.masked_20062020_mat <- temp.masked.df_cleaned %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(x, y, year) %>%
  summarize(annual.mean.temp = mean(monthly.mean.temp, na.rm = TRUE)) %>%
  ungroup(year) %>%
  summarize(mat = mean(annual.mean.temp, na.rm = TRUE))

max(temp.masked_20062020_mat$mat)
min(temp.masked_20062020_mat$mat)

## Extract monthly precipitation data from grid cell containing each site.
## Note that grid cell is on 4km resolution
df.sites.temp <- as.data.frame(terra::extract(pd.temp,
                                                SpatialPoints(eco.coords),
                                                sp = F))
df.sites.temp$latitude = eco.coords$y
df.sites.temp$longitude = eco.coords$x
df.sites.temp$site = ecosites$property

## Pivot raster columns to long format, remove prefix and group by month
## precip. Then, in a step, calculate 2006-2020 mean annual precipitation 
## of all grid cells 
df.sites.temp_cleaned <- df.sites.temp %>%
  tidyr::pivot_longer(cols = PRISM_tmean_stable_4kmM3_199101_bil:PRISM_tmean_stable_4kmM3_202012_bil,
                      names_prefix = "PRISM_tmean_stable_4kmM3_",
                      values_to = "monthly.temp",
                      names_to = "month") %>%
  mutate(month = gsub("*_bil", "", month),
         date = lubridate::ym(month),
         year = lubridate::year(date),
         month = lubridate::month(date))

df.sites.temp_20062020_mat <- df.sites.temp_cleaned %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(site, latitude, longitude, year) %>%
  summarize(annual.temp = mean(monthly.temp, na.rm = TRUE)) %>%
  ungroup(year) %>%
  summarize(mat = mean(annual.temp, na.rm = TRUE))

# VPD
pd.vpdmax <- pd_stack(prism_archive_subset("vpdmax", "monthly"))
pd.vpdmin <- pd_stack(prism_archive_subset("vpdmin", "monthly"))

## Mask vpdmax and vpdmin, then, in a step, calculate vpdmean
## following same procedure for tmean
vpdmax.masked <- mask(crop(pd.vpdmax, extent(texas)), texas)
vpdmin.masked <- mask(crop(pd.vpdmin, extent(texas)), texas)
vpdmean.masked <- (vpdmax.masked + vpdmin.masked) / 2

## Convert RasterBrick object to data frame
vpdmean.masked.df <- as.data.frame(rasterToPoints(vpdmean.masked))

## Pivot raster columns to long format, remove prefix and group by month
## precip. Then, in a step, calculate 2006-2020 mean annual VPDmean 
## of all grid cells. NOTE: dividing by 10 to convert VPD from
## hPa to kPa
vpdmean.masked.df_cleaned <- vpdmean.masked.df %>%
  tidyr::pivot_longer(cols = layer.1:layer.360,
                      values_to = "monthly.mean.vpd",
                      names_to = "month") %>%
  cbind(temp.masked.df_cleaned) %>%
  dplyr::select(1:2, 7, 10, 9, 4) %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(x, y, year) %>%
  summarize(annual.mean.vpd = mean(monthly.mean.vpd, na.rm = TRUE) / 10) %>%
  ungroup(year) %>%
  summarize(mav = mean(annual.mean.vpd, na.rm = TRUE))

min(vpdmean.masked.df_cleaned$mav)
max(vpdmean.masked.df_cleaned$mav)

## Extract monthly precipitation data from grid cell containing each site.
## Note that grid cell is on 4km resolution
df.sites.vpd <- as.data.frame(terra::extract(vpdmean.masked,
                                              SpatialPoints(eco.coords),
                                              sp = F))
df.sites.temp$latitude = eco.coords$y
df.sites.temp$longitude = eco.coords$x
df.sites.temp$site = ecosites$property

## Pivot raster columns to long format, remove prefix and group by month
## precip. Then, in a step, calculate 2006-2020 mean annual precipitation 
## of all grid cells 
df.sites.vpd_cleaned <- df.sites.vpd %>%
  tidyr::pivot_longer(cols = layer.1:layer.360,
                      values_to = "monthly.mean.vpd",
                      names_to = "month") %>%
  cbind(df.sites.temp_cleaned) %>%
  dplyr::select(5, 3:4, 6, 9, 8, 2) %>%
  filter(year >= 2006 & year <= 2020) %>%
  group_by(site, latitude, longitude, year) %>%
  summarize(annual.mean.vpd = mean(monthly.mean.vpd, na.rm = TRUE) / 10) %>%
  ungroup(year) %>%
  summarize(mav = mean(annual.mean.vpd, na.rm = TRUE))

###############################################################################
## Merge precipitation, temperature, VPD normals into single
## file. Two separate files: one with values extracted from
## grid cell that contains each site, and one with all TX
## grid cells
###############################################################################
df.normals <- precip.masked.df_cleaned %>%
  full_join(temp.masked_20062020_mat) %>%
  full_join(vpdmean.masked.df_cleaned) %>%
  dplyr::select(latitude = y, longitude = x, map, mat, mav)

df.sites <- df.sites.precip_cleaned %>%
  full_join(df.sites.temp_20062020_mat) %>%
  full_join(df.sites.vpd_cleaned) %>%
  full_join(ecosites.short) %>%
  dplyr::select(site, latitude, longitude, elevation.m, 
                sampling.year, map, mat, mav)

write.csv(df.sites, "../data_sheets/TXeco_climate_normals.csv", row.names = FALSE)

###############################################################################
## MAP and MAT plots (general across TX, not site specific)
###############################################################################
map.plot <- ggplot() +
  geom_raster(data = df.normals, 
              aes(x = longitude, y = latitude, fill = map)) +
  geom_point(data = ecosites.short, 
             aes(x = longitude, y = latitude, 
                 shape = factor(sampling.year, levels = c("2020", "2021", "2020_2021"))), 
             size = 3) +
  borders(database = "state", region = "texas", 
          xlim = c(-107, -92), ylim = c(25, 37), colour = "black") +
  ggsn::scalebar(x.min = -107, x.max = -92, y.min = 25.5, y.max = 37,
                 location = "bottomleft",
                 transform = TRUE, model = "WGS84",
                 dist_unit = "km", dist = 150, height = 0.05, 
                 st.dist = 0.025, st.size = 5, border.size = 0.25) +
  coord_equal() +
  scale_fill_gradientn(colours = c("#A50026", "#DD3D2D",
                                   "#F67E4B", "#FDB366",
                                   "#FEDA8B", "#EAECCC",
                                   "#C2E4EF", "#98E4EF",
                                   "#98CAE1", "#6EA6CD",
                                   "#364B9A", "#364B9A"),
                       limits = c(0, 1850), breaks = seq(0, 1800, 600),
                       space = "Lab", 
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  scale_shape_manual(values = c(21, 24, 16),
                     labels = c("2020", "2021", "2020/2021")) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = expression(bold("Longitude ("*degree*")")), 
       y = expression(bold("Latitude ("*degree*")")),
       fill = "MAP (mm)",
       shape = "Sampling year") +
  theme_classic(base_size = 22) +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank()) +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               ticks.linewidth = 2,
                               frame.linewidth = 1,
                               frame.colour = "black",
                               hjust = 0))

map.plot

## MAT normals
mat.plot <- ggplot() +
  geom_raster(data = df.normals, 
              aes(x = longitude, y = latitude, fill = mat)) +
  geom_point(data = ecosites, 
             aes(x = longitude, y = latitude, 
                 shape = factor(sampling.year, levels = c("2020", "2021", "2020_2021"))), 
             size = 3) +
  borders(database = "state", region = "texas", 
          xlim = c(-107, -92), ylim = c(25, 37), colour = "black") +
  # ggsn::scalebar(x.min = -107, x.max = -92, y.min = 25.5, y.max = 37,
  #                location = "bottomleft",
  #                transform = TRUE, model = "WGS84",
  #                dist_unit = "km", dist = 150, height = 0.025, 
  #                st.dist = 0.025, st.size = 3, border.size = 0.25) +
  coord_equal() +
  scale_fill_gradientn(colours = c("#FFFFFF",
                                   "#FFFFE5", "#FFF7BC", 
                                   "#FEE391", "#FEC44F", 
                                   "#FB9A29", "#EC7014", 
                                   "#CC4C02", "#993404", 
                                   "#662506"),
                       limits = c(11, 25), breaks = seq(12, 24, 4),
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  scale_shape_manual(values = c(21, 24, 16),
                     labels = c("2020", "2021", "2020/2021")) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  #scale_fill_continuous(type = "viridis",
  #                      limits = c(10, 25), breaks = seq(10, 25, 5)) +
  labs(x = expression(bold("Longitude ("*degree*")")), 
       y = expression(bold("Latitude ("*degree*")")),
       fill = expression(bold("MAT ("*degree*"C)")),
       shape = "Sampling year") +
  theme_classic(base_size = 22) +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank()) +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               ticks.linewidth = 2,
                               frame.linewidth = 1,
                               frame.colour = "black"))

mat.plot

png("../working_drafts/TXeco_siteMaps.png", 
    width = 20, height = 8, units = 'in', res = 600)
ggarrange(map.plot, mat.plot, ncol = 2, nrow = 1, 
          legend = "right", align = "hv", labels = "AUTO",
          font.label = list(size = 25))
dev.off()

###############################################################################
## Create MAP/MAT plot per site
###############################################################################
biome_mat_map <- ggplot(df.normals, aes(x = mat, y = map)) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm * 10, fill = factor(biome)),
               color = "gray98", size = 0.5) +
  geom_point(size = 0.5, alpha = 0.01, color = "grey", shape = 1) +
  geom_point(data = df.sites, aes(x = mat, y = map)) +
  scale_fill_brewer(palette = "Spectral") +
  #scale_y_continuous(limits = c(50, 150), breaks = seq(50, 150, 25)) +
  #scale_x_continuous(limits = c(18, 22), breaks = seq(18, 22, 1)) +
  labs(x = expression(bold("Mean annual temperature ("*degree*"C)")),
       y = "Mean annual precipitation (mm)",
       fill = "Biome type") +
  theme_classic(base_size = 22) +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())

ggsave(plot = biome_mat_map, 
       filename = "../working_drafts/TXeco_biome_plot.png",
       dpi = 600, height = 5, width = 12)



