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

# Load Ecolab site coords
ecosites <- read.csv("../data_sheets/TXeco_sitecoords.csv") %>%
  filter(site != "2020eco_Bell" & site != "2020eco_Russel") # add path to site coords file here
ecosites <- distinct(ecosites, property, .keep_all = TRUE) # remove duplicate sites
eco.coords <- dplyr::select(ecosites, x = longitude, y = latitude) # select only lat/long data, code as xy

# Set PRISM directory (put folder where prism data will be stored here)
prism_set_dl_dir("../climate_data/prism/prism_normal/")

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
us <- getData("GADM", country="USA", level = 1, path = "../climate_data/")
texas <- us[match(toupper("texas"), toupper(us$NAME_1)), ]

###############################################################################
## Stack raster files, mask to include TX and convert to df
###############################################################################
# Precipitation (pd_stack stacks all monthly climate normal data into single 
# RasterStack)

pd.precip <- pd_stack(prism_archive_subset("ppt", "annual normals", resolution = "4km"))
precip.masked <- mask(pd.precip, texas) # turns to rasterBrick file
precip.masked.df <- as.data.frame(rasterToPoints(precip.masked))

df.sites.precip <- as.data.frame(terra::extract(pd.precip,
                                                SpatialPoints(eco.coords),
                                                sp = F))
df.sites.precip$latitude = eco.coords$y
df.sites.precip$longitude = eco.coords$x
df.sites.precip$site = ecosites$property

# Temperature
pd.temp <- pd_stack(prism_archive_subset("tmean", "annual normals", resolution = "4km"))
temp.masked <- mask(pd.temp, texas)
temp.masked.df <- as.data.frame(rasterToPoints(temp.masked))

df.sites.temp <- as.data.frame(terra::extract(pd.temp,
                                                SpatialPoints(eco.coords),
                                                sp = F))
df.sites.temp$latitude = eco.coords$y
df.sites.temp$longitude = eco.coords$x
df.sites.temp$site = ecosites$property

df.normals <- precip.masked.df %>%
  full_join(temp.masked.df) %>%
  dplyr::select(latitude = y, longitude = x, 
                map = PRISM_ppt_30yr_normal_4kmM3_annual_bil,
                mat = PRISM_tmean_30yr_normal_4kmM3_annual_bil)

df.sites <- df.sites.temp %>%
  full_join(df.sites.precip) %>%
  select(site, latitude, longitude, 
         mat = PRISM_tmean_30yr_normal_4kmM3_annual_bil,
         map = PRISM_ppt_30yr_normal_4kmM3_annual_bil)


ecosites$visit.freq <- 1
ecosites$sampling.year[ecosites$property == "Brazos_2020_18" |
                      ecosites$property == "Harris_2020_03" |
                      ecosites$property == "Uvalde_2020_02"] <- "2020_2021"


###############################################################################
## MAP and MAT plots (general across TX, not site specific)
###############################################################################
map.plot <- ggplot() +
  geom_raster(data = df.normals, aes(x = longitude, y = latitude, fill = map)) +
  geom_point(data = ecosites, 
             aes(x = longitude, y = latitude, 
                 shape = factor(sampling.year, levels = c("2020", "2021", "2020_2021"))), 
             size = 3) +
  ggsn::scalebar(data = df.normals, location = "bottomleft",
                 transform = TRUE, model = "WGS84",
                 dist_unit = "km", dist = 100, st.dist = 0.025, st.size = 2.5, border.size = 0.25) +
  coord_equal() +
  scale_fill_gradientn(colours = iridescent.reverse,
                       limits = c(0, 1700), breaks = seq(0, 1500, 500),
                       space = "Lab", 
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  scale_shape_manual(values = c(21, 24, 16),
                     labels = c("2020", "2021", "2020/2021")) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = "Longitude (dd)", y = "Latitude (dd)",
       fill = "MAP (mm)",
       shape = "Sampling year") +
  theme_bw(base_size = 18) +
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
  geom_raster(data = df.normals, aes(x = longitude, y = latitude, fill = mat)) +
  geom_point(data = ecosites, 
             aes(x = longitude, y = latitude, 
                 shape = factor(sampling.year, levels = c("2020", "2021", "2020_2021"))), 
             size = 3) +
  ggsn::scalebar(data = df.normals, location = "bottomleft",
                 transform = TRUE, model = "WGS84",
                 dist_unit = "km", dist = 100, st.dist = 0.025, st.size = 2.5, border.size = 0.25) +
  coord_equal() +
  scale_fill_gradientn(colours = c("#CEFFFF", "#C6F7D6", "#A2F49B",
                                   "#BBE453", "#D5CE04", "#E7B503",
                                   "#F19903", "#F6790B", "#F94902",
                                   "#E40515", "#A80003"),
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
  labs(x = "Longitude (dd)", y = "Latitude (dd)",
       fill = expression(bold("MAT ("~degree~"C)")),
       shape = "Sampling year") +
  theme_bw(base_size = 18) +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank()) +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               ticks.linewidth = 2,
                               frame.linewidth = 1,
                               frame.colour = "black"))

mat.plot

png("../working_drafts/TXeco_siteMaps.png", 
    width = 16, height = 7, units = 'in', res = 600)
ggarrange(map.plot, mat.plot, ncol = 2, nrow = 1, legend = "right", align = "hv")
dev.off()


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
  theme_bw(base_size = 18) +
  theme(title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())

ggsave(plot = normals.plot, 
       filename = "climate_normals.png",
       dpi = 600, height = 5, width = 12)