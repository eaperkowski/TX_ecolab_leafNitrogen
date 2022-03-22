## Load libraries
library(dplyr) #data wrangling
library(raster) ##working with raster data
library(prism)
library(ggplot2)
library(ggpubr)
library(patchwork)

## Load Ecolab site coords
ecosites <- read.csv("/Users/eaperkowski/git/TX_ecolab_leafNitrogen/data_sheets/TXeco_sitecoords.csv")

## Set prism directory
prism_set_dl_dir("/Users/eaperkowski/git/TX_ecolab_leafNitrogen/climate_data/prism/prism_normal/")

## Get temperature and precipitation normal data (only do this once)
get_prism_normals(type = "tmean", resolution = "800m", mon = 1:12, annual = TRUE, keepZip = FALSE)
get_prism_normals(type = "ppt", resolution = "800m", mon = 1:12, annual = TRUE, keepZip = FALSE)

## Create US shapefile to eventually mask raster stacks/bricks to only include
## Texas
us <- getData("GADM", country="USA", level=1)
texas <- us[match(toupper("texas"),toupper(us$NAME_1)),]

## Stack precipitation raster files, convert to data frame
pd.precip <- pd_stack(prism_archive_subset("ppt", "monthly normals", resolution = "800m"))
precip.masked <- mask(pd.precip, texas)
precip.masked <- as.data.frame(rasterToPoints(precip.masked))

## Stack mean temperature raster files and convert to data frame
pd.temp <- pd_stack(prism_archive_subset("tmean", "monthly normals", resolution = "800m"))
temp.masked <- mask(pd.temp, texas)
temp.masked <- as.data.frame(rasterToPoints(temp.masked))

## Merge temperature and precipitation data to single data frame
df <- precip.masked %>%
  full_join(temp.masked)

## Rename column names
names(df) <- c("longitude", "latitude",
               "jan.precip.norm",
               "feb.precip.norm",
               "mar.precip.norm",
               "apr.precip.norm",
               "may.precip.norm",
               "jun.precip.norm",
               "jul.precip.norm",
               "aug.precip.norm",
               "sep.precip.norm",
               "oct.precip.norm",
               "nov.precip.norm",
               "dec.precip.norm",
               "jan.temp.norm",
               "feb.temp.norm",
               "mar.temp.norm",
               "apr.temp.norm",
               "may.temp.norm",
               "jun.temp.norm",
               "jul.temp.norm",
               "aug.temp.norm",
               "sep.temp.norm",
               "oct.temp.norm",
               "nov.temp.norm",
               "dec.temp.norm")

## Add mean annual temp and mean annual precip column
df <- df %>%
  #group_by(longitude) %>%
  mutate(map = jan.precip.norm + feb.precip.norm + mar.precip.norm + apr.precip.norm +
           may.precip.norm + jun.precip.norm + jul.precip.norm + aug.precip.norm + 
           sep.precip.norm + oct.precip.norm + nov.precip.norm + dec.precip.norm,
         mat = (jan.temp.norm + feb.temp.norm + mar.temp.norm + apr.temp.norm +
           may.temp.norm + jun.temp.norm + jul.temp.norm + aug.temp.norm + 
           sep.temp.norm + oct.temp.norm + nov.temp.norm + dec.temp.norm)/ 12)

## Get Texas map
texas.map <- map_data(map = "county", region = "texas")

## January precip normals
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

## February precip normals
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

## March precip normals
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

## April precip normals
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

## May precip normals
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

## June precip normals
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

## July precip normals
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

## August precip normals
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

## September precip normals
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

## October precip normals
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

## November precip normals
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

## December precip normals
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

## MAP normals
map.plot <- ggplot() +
  geom_raster(data = df, aes(x = longitude, y = latitude, fill = map)) +
  geom_point(data = ecosites, aes(x = longitude, y = latitude)) +
  scale_fill_continuous(type = "viridis",
                        limits = c(-100, 1900), breaks = seq(0, 1800, 600)) +
  #scale_fill_distiller(palette = "Greens", direction = 1,
  #                     limits = c(100, 1700), breaks = seq(100, 1700, 500)) +
  scale_x_continuous(limits = c(-107, -92), breaks = seq(-107, -92, 3)) +
  scale_y_continuous(limits = c(25, 37), breaks = seq(25, 37, 3)) +
  labs(x = "Longitude (dd)", y = "Latitude (dd)",
       fill = "Precipitation (mm)",
       title = "Mean annual precipitation (1991-2020)") +
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

## Merge monthly precip plots into single fig
ggarrange(jan.prcp, feb.prcp, mar.prcp, apr.prcp, may.prcp, jun.prcp, jul.prcp,
          aug.prcp, sep.prcp, oct.prcp, nov.prcp, dec.prcp, ncol = 4, nrow = 3,
          common.legend = TRUE, legend = "right") %>%
  annotate_figure(bottom = text_grob(
    expression(bold("Longitude (dd)")),
    size = 15)) %>%
  annotate_figure(left = text_grob(
    expression(bold("Latitude (dd)")),
    size = 15, rot = 90)) %>%
  ggexport(filename = "/Users/eaperkowski/git/TX_ecolab_leafNitrogen/monthly_normal.precip.png",
           width = 12000, height = 9000, res = 600)
  
## Merge MAT and MAP plots
normals.plot <- map.plot + mat.plot + plot_layout(guides = "collect")
ggsave(plot = normals.plot, 
       filename = "/Users/eaperkowski/git/TX_ecolab_leafNitrogen/climate_normals.png",
       dpi = 600, height = 5, width = 12)

