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

# Load Ecolab site coords
ecosites <- read.csv("data_sheets/TXeco_sitecoords.csv") # add path to site coords file here
ecosites <- distinct(ecosites, property, .keep_all = TRUE) # remove duplicate sites
eco.coords <- dplyr::select(ecosites, x = longitude, y = latitude) # select only lat/long data, code as xy

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("climate_data/prism/prism_monthly/")

# Add pubtheme for figuremaking
pubtheme <- theme_bw() +
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),
        panel.border = element_rect(size = 3, fill = NA),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.background=element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14, face = "bold"),
        axis.ticks.length = unit(0.25, "cm"),
        panel.grid.minor.y = element_blank())

###############################################################################
## Download temp/precip normal data from PRISM
###############################################################################
# Get temperature and precipitation normal data (only need to do this once-
# imports data files into central file)

## Workaround to not having PRISM 1991-2020 climate norms (download monthly
## and then calculate mean across years)
get_prism_monthlys(type = "tmean", years = 1991:2020, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "ppt", years = 1991:2020, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tmin", years = 1991:2020, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tmax", years = 1991:2020, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tdmean", years = 1991:2020, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "vpdmin", years = 1991:2020, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "vpdmax", years = 1991:2020, mon = 1:12, keepZip = FALSE)

###############################################################################
## Create US shapefile to mask rasters to only Texas
###############################################################################
# Note: no need to do this if you're wanting a full US map. Or, change
# "texas" to desired state if you want "subset" climate data from a single
# state
us <- getData("GADM", country="USA", level = 1)
texas <- us[match(toupper("texas"), toupper(us$NAME_1)), ]

###############################################################################
## Stack raster files, mask to include TX and convert to df
###############################################################################
# Precipitation (pd_stack stacks all monthly climate normal data into single 
# RasterStack)
pd.precip <- pd_stack(prism_archive_subset("ppt", "monthly normals", resolution = "800m"))
precip.masked <- mask(pd.precip, texas) # turns to rasterBrick file
precip.masked.df <- as.data.frame(rasterToPoints(precip.masked))

# Temperature
pd.temp <- pd_stack(prism_archive_subset("tmean", "monthly normals", resolution = "800m"))
temp.masked <- mask(pd.temp, texas)
temp.masked.df <- as.data.frame(rasterToPoints(temp.masked))

###############################################################################
## Extract grids for prcp/temp within each field site, join to central df
###############################################################################
# Note: eco.coords is a file containing xy coordinate data for sites. Data frame
# should only contain xy data (x = longitude, y = latitude) and must have longitude
# as the first column

# Precipitation
df.sites.precip <- as.data.frame(terra::extract(precip.masked,
                                                SpatialPoints(eco.coords),
                                                sp = F))
df.sites.precip$latitude = eco.coords$y
df.sites.precip$longitude = eco.coords$x
df.sites.precip$site = ecosites$property

names(df.sites.precip[4:363]) <- gsub("PRISM_ppt_stable_4kmM3_", "prcp_", 
                                      x = names(df.sites.precip[4:363]))





# Temperature
df.sites.temp <- as.data.frame(terra::extract(temp.masked,
                                              SpatialPoints(eco.coords),
                                              sp = F))
df.sites.temp$latitude = eco.coords$y
df.sites.temp$longitude = eco.coords$x
df.sites.temp$site = ecosites$property

# Join extracted grids for each field site
df.sites <- df.sites.precip %>%
  full_join(df.sites.temp) %>%
  dplyr::select(site, latitude, longitude, everything())

# Rename column names
names(df.sites) <- c("site","latitude", "longitude", "jan.prcp.norm01", 
                     "prcp.norm02", "prcp.norm03", "prcp.norm04", 
                     "prcp.norm05", "prcp.norm06", "prcp.norm07", 
                     "prcp.norm08", "prcp.norm09", "prcp.norm10", 
                     "prcp.norm11", "prcp.norm12", "tmean.norm01", 
                     "tmean.norm02", "tmean.norm03", "tmean.norm04", 
                     "tmean.norm05", "tmean.norm06", "tmean.norm07", 
                     "tmean.norm08", "tmean.norm09", "tmean.norm10", 
                     "tmean.norm11", "tmean.norm12")

# Add mean annual temp and mean annual precip column
df.sites <- df.sites %>%
  dplyr::mutate(map = jan.precip.norm + feb.precip.norm + mar.precip.norm + apr.precip.norm +
           may.precip.norm + jun.precip.norm + jul.precip.norm + aug.precip.norm + 
           sep.precip.norm + oct.precip.norm + nov.precip.norm + dec.precip.norm,
         mat = (jan.temp.norm + feb.temp.norm + mar.temp.norm + apr.temp.norm +
                  may.temp.norm + jun.temp.norm + jul.temp.norm + aug.temp.norm + 
                  sep.temp.norm + oct.temp.norm + nov.temp.norm + dec.temp.norm) / 12)

# Write climate normal file to .csv
write.csv(df.sites, "climate_data/TXeco_PRISM_sitenorms.csv", row.names = FALSE)


###############################################################################
## Merge temperature and precipitation data to single data frame. This is for 
## a map of Texas MAP and MAT, not site specific
###############################################################################
df <- precip.masked.df %>%
  full_join(temp.masked.df)

# Rename column names
names(df) <- c("longitude", "latitude", "jan.precip.norm", "feb.precip.norm",
               "mar.precip.norm", "apr.precip.norm", "may.precip.norm",
               "jun.precip.norm", "jul.precip.norm", "aug.precip.norm",
               "sep.precip.norm", "oct.precip.norm", "nov.precip.norm",
               "dec.precip.norm", "jan.temp.norm", "feb.temp.norm",
               "mar.temp.norm", "apr.temp.norm", "may.temp.norm",
               "jun.temp.norm", "jul.temp.norm", "aug.temp.norm",
               "sep.temp.norm", "oct.temp.norm", "nov.temp.norm",
               "dec.temp.norm")

# Add mean annual temp and mean annual precip column
df <- df %>%
  dplyr::mutate(map = jan.precip.norm + feb.precip.norm + mar.precip.norm + apr.precip.norm +
                  may.precip.norm + jun.precip.norm + jul.precip.norm + aug.precip.norm + 
                  sep.precip.norm + oct.precip.norm + nov.precip.norm + dec.precip.norm,
                mat = (jan.temp.norm + feb.temp.norm + mar.temp.norm + apr.temp.norm +
                         may.temp.norm + jun.temp.norm + jul.temp.norm + aug.temp.norm + 
                         sep.temp.norm + oct.temp.norm + nov.temp.norm + dec.temp.norm) / 12)

###############################################################################
## Monthly precipitation plots (general across TX, not site specific)
###############################################################################
# January precip normals
jan.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = jan.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "January") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

# February precip normals
feb.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = feb.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "February") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank())

# March precip normals
mar.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = mar.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "March") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank())

# April precip normals
apr.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = apr.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "April") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank())

# May precip normals
may.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = may.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "May") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

# June precip normals
jun.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = jun.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "June") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank())

# July precip normals
jul.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = jul.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "July") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank())

# August precip normals
aug.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = aug.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "August") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank())

# September precip normals
sep.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = sep.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "September") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

# October precip normals
oct.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = oct.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "October") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())

# November precip normals
nov.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = nov.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "November") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())

# December precip normals
dec.prcp <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = dec.precip.norm)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(0, 200), breaks = seq(0, 200, 50)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = NULL, y = NULL,
       fill = "Precipitation (mm)",
       title = "December") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())

###############################################################################
## Merge monthly precip plots into single fig
###############################################################################
ggarrange(jan.prcp, feb.prcp, mar.prcp, apr.prcp, may.prcp, jun.prcp, jul.prcp,
          aug.prcp, sep.prcp, oct.prcp, nov.prcp, dec.prcp, ncol = 4, nrow = 3,
          common.legend = TRUE, legend = "right") %>%
  annotate_figure(bottom = text_grob(
    expression(bold("Longitude (dd)")),
    size = 15)) %>%
  annotate_figure(left = text_grob(
    expression(bold("Latitude (dd)")),
    size = 15, rot = 90)) %>%
  ggexport(filename = "monthly_normal.precip.png",
           width = 12000, height = 9000, res = 600)

###############################################################################
## MAP and MAT plots (general across TX, not site specific)
###############################################################################
map.plot <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = map)) +
  geom_point(data = ecosites, aes(x = longitude, y = latitude)) +
  coord_equal() +
  scale_fill_continuous(type = "viridis",
                        limits = c(-100, 1900), breaks = seq(0, 1800, 600)) +
  #scale_fill_distiller(palette = "Greens", direction = 1,
  #                     limits = c(100, 1700), breaks = seq(100, 1700, 500)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = "Longitude (dd)", y = "Latitude (dd)",
       fill = "Precipitation (mm)",
       title = "Mean annual precipitation (1981-2010)") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

## MAT normals
mat.plot <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = mat)) +
  geom_point(data = ecosites, aes(x = longitude, y = latitude)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = c(9, 26), breaks = seq(10, 25, 5)) +
  coord_equal() +
  #scale_fill_continuous(type = "viridis",
  #                      limits = c(10, 25), breaks = seq(10, 25, 5)) +
  labs(x = "Longitude (dd)", y = "Latitude (dd)",
       fill = expression(bold("Temperature ("~degree~"C)")),
       title = "Mean annual temperature (1991-2020)") +
  pubtheme +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

###############################################################################
## Merge MAP/MAT plots
###############################################################################
normals.plot <- map.plot + mat.plot + plot_layout(guides = "collect")
ggsave(plot = normals.plot, 
       filename = "climate_normals.png",
       dpi = 600, height = 5, width = 12)

ggarrange(map.plot, mat.plot, biome_mat_map, ncol = 3, nrow = 1, 
          common.legend = TRUE, legend = "right", align = "hv")


###############################################################################
## Create MAP/MAT plot per site
###############################################################################
biome_mat_map <- ggplot(data = df.sites, aes(x = mat, y = map / 10)) +
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

ggsave(plot = normals.plot, 
       filename = "climate_normals.png",
       dpi = 600, height = 5, width = 12)






