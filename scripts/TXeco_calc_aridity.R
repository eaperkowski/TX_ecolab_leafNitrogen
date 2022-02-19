################################################################
# Load libraries
################################################################
## Load libraries
library(SPEI)
library(tidyverse)
library(lubridate)
library(reshape)
library(ggpubr)

## Central figure theme
pubtheme <- theme_bw() +
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16, face = "bold"),
        panel.border = element_rect(size = 3, fill = NA),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.background=element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14, face = "bold"),
        axis.ticks.length = unit(0.25, "cm"),
        panel.grid.minor = element_blank())

################################################################
# Import daily weather means, 2006-2020 Normals csv files,
# and site coordinates
################################################################
monthly.means <- read.csv("../data_sheets/TXeco_climate_monthlymesowest.csv",
                        na.strings = "NA", stringsAsFactors = FALSE)
normals <- read.csv("../data_sheets/TXeco_climate_normals.csv",
                    na.strings = "NA", stringsAsFactors = FALSE)
site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv",
                        na.strings = "NA", stringsAsFactors = FALSE)

################################################################
# Join site coords with monthly means
################################################################
site.coords <- site.coords %>%
  select(property, latitude, longitude)

# Remove 2021 ecolab sites (until data becomes available)
site.coords <- site.coords[-c(15:30), ]


monthly.means <- monthly.means %>%
  full_join(site.coords) %>%
  unite("site.long", c(property, visit.type), sep = " ")

################################################################
# Separate properties into list of data frames. This is necessary
# because SPEI requires a time series notation that can only
# be accomplished through a series of unique lists
################################################################
monthly.means <- setNames(split(x = monthly.means,
                              f = monthly.means$site.long),
                         paste0(unique(monthly.means$site.long)))

# Clean missing data points
monthly.means[["Uvalde_2020_02 p"]] <- monthly.means[["Uvalde_2020_02 p"]][-8,]
monthly.means[["Uvalde_2020_02 i"]] <- monthly.means[["Uvalde_2020_02 i"]][-9,]

################################################################
# Use Hargreaves equation to estimate 30-day reference 
# evapotranspiration, merge back to central data frame
################################################################
for(i in seq_along(monthly.means)) {
  monthly.means[[i]]$et0 <- as.numeric(hargreaves(Tmin = monthly.means[[i]]$month.tmin,
                                                  Tmax = monthly.means[[i]]$month.tmax,
                                                  lat = unique(monthly.means[[i]]$latitude)))
  
  monthly.means[[i]]$water.balance <- monthly.means[[i]]$month.precip - monthly.means[[i]]$et0
  monthly.means[[i]]$spei <- spei(data = as.ts(monthly.means[[i]]$water.balance), scale = 1)$fitted
  monthly.means[[i]]$aridity <- monthly.means[[i]]$month.precip / monthly.means[[i]]$et0
}


## Merge list of data.frames into single data.frame
monthly.mean <- merge_all(monthly.means)

## Separate concatenated "site.long" back to three separate columns
monthly.mean <- separate(monthly.mean, site.long, 
                        into = c("site", "visit.type"),
                        sep = " ")

## Convert month/year to date column
monthly.mean <- monthly.mean %>%
  unite("month.year", c(month, year), sep = "/", remove = FALSE) %>%
  mutate(month.year1 = my(month.year))


################################################################
# Visualize SPEI and AI over time
################################################################
ggplot(data = monthly.mean, aes(x = month.year1,
                                y = spei, color = site,
                                linetype = visit.type)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
  facet_wrap(~reorder(site, longitude)) +
  guides(color = "none") +
  labs(x = "Date", y = "Standardized Precipitation-Evapotranspiration Index") +
  theme_bw()

summer.mean <- monthly.mean %>%
  filter(month == "6"  & year == "2020" & visit.type == "i") %>%
  group_by(site, visit.type, longitude) %>%
  summarize(mean.spei = mean(spei, na.rm = TRUE),
            sd.spei = sd(spei, na.rm = TRUE),
            mean.ai = mean(aridity, na.rm = TRUE),
            sd.ai = sd(aridity, na.rm = TRUE))

ggplot(data = summer.mean, 
       aes(x = reorder(site, longitude),
           y = mean.spei, fill = site)) +
  geom_bar(stat = "identity") +
  facet_grid(~visit.type) +
  guides(color = "none") +
  labs(x = "Site", y = "SPEI") +
  scale_y_continuous(limits = c(-2,2), breaks = seq(-2, 2, 0.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



################################################################
# Visualize 30-day aridity index
################################################################
# Initial site visits for 2020 and 2021
init.2021 <- ggplot(data = subset(monthly.mean, visit.type == "i" & 
                                    sampling.year == "2021"), 
                    aes(x = reorder(site, longitude), y = test.spei)) +
  geom_boxplot()
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  labs(x = "Site", y = NULL) +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))


init.2020 <- ggplot(data = subset(monthly.mean, visit.type == "i" & 
                                   sampling.year == "2020"), 
                   aes(x = reorder(site, longitude), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "30-day SPEI") +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

ggarrange(init.2020, init.2021, ncol = 2, align = "hv")

# Primary site visits for 2020 and 2021
prim.2021 <- ggplot(data = subset(monthly.mean, visit.type == "p" & 
                       sampling.year == "2021"), 
       aes(x = reorder(site, spei), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "SPEI") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

prim.2020 <- ggplot(data = subset(monthly.mean, visit.type == "p" & 
                                    sampling.year == "2020"), 
                    aes(x = reorder(site, longitude), y = spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "SPEI") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) 


ggarrange(init.2021, prim.2021, 
          ncol = 2, nrow = 1, common.legend = TRUE,
          align = "hv", legend = "right", labels = "AUTO",
          font.label = list(size = 18, face = "bold")) %>%
  ggexport(filename = "/Users/eaperkowski/Desktop/2021eco_spei.png", 
           width = 7000, height = 4000, res = 600)

################################################################
# Clean normals data sheet and put in list of data frames by
# site to pass Hargreaves function through
################################################################
normals <- normals %>%
  select(property, sampling.year, visit.type, month, 
         norm.precip, norm.tmax, norm.tmin) %>%
  full_join(site.coords) %>%
  group_by(property, sampling.year, visit.type, month, latitude, longitude) %>%
  summarize(norm.precip = mean(norm.precip, na.rm = TRUE),
            norm.tmax = mean(norm.tmax, na.rm = TRUE),
            norm.tmin = mean(norm.tmin, na.rm = TRUE)) %>%
  unite("site.long", c(property, sampling.year, visit.type), sep = " ")

normals <- setNames(split(x = normals,
                               f = normals$site.long),
                         paste0(unique(normals$site.long)))

################################################################
# Use Hargreaves equation to estimate 30-day reference 
# evapotranspiration, merge back to central data frame
################################################################
for(i in seq_along(normals)) {
  normals[[i]]$et0 <- as.numeric(hargreaves(Tmin = as.numeric(normals[[i]]$norm.tmin),
                                            Tmax = as.numeric(normals[[i]]$norm.tmax),
                                            lat = unique(normals[[i]]$latitude)))
  normals[[i]]$water.balance <- as.numeric(normals[[i]]$norm.precip - normals[[i]]$et0)
  normals[[i]]$spei <- as.vector(spei(data = as.ts(normals[[i]]$water.balance), scale = 1)$fitted)
  normals[[i]]$aridity <- normals[[i]]$norm.precip / normals[[i]]$et0
}

## Merge list of data.frames into single data.frame
normals <- merge_all(normals)

ggplot(data = normals, aes(x = month, y = aridity, color = site.long)) +
  geom_line() +
  facet_grid


## Separate concatenated "site.long" back to three separate columns
normals <- separate(normals, site.long, 
                    into = c("site", "sampling.year", "visit.type"),
                    sep = " ")

################################################################
# Calculate 15-yr normal SPEI and AI for all sites
################################################################
normals.gs <- normals %>%
  group_by(site, visit.type, sampling.year, longitude) %>%
  summarize(mean.annual.aridity = mean(aridity, na.rm = TRUE),
            mean.annual.spei = mean(spei, na.rm = TRUE))

## Initial field site visits
norm.2021 <- ggplot(data = subset(normals.gs, sampling.year == "2021" & visit.type == "i"), 
       aes(x = reorder(site, longitude), y = mean.spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "15-year normal SPEI") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
  
norm.2020 <- ggplot(data = subset(normals.gs, sampling.year == "2020" & visit.type == "i"), 
                    aes(x = reorder(site, longitude), y = mean.spei)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "15-year normal SPEI") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))



ggarrange(norm.2021) %>%
  ggexport(filename = "/Users/eaperkowski/Desktop/2021eco_norm_spei.png", 
           width = 4500, height = 4000, res = 600)

ggplot(data = subset(normals.gs, sampling.year == "2021" & visit.type == "p"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## 2020 field sites
ggplot(data = subset(normals.gs, sampling.year == "2020" & visit.type == "i"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
ggplot(data = subset(normals.gs, sampling.year == "2020" & visit.type == "p"), 
       aes(x = reorder(site, longitude), y = mean.aridity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Aridity index (P/PET)") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


normal.summary <- normals %>%
  filter(sampling.year == "2021") %>%
  group_by(site, visit.type) %>%
  summarize(spei = mean(spei, na.rm = TRUE),
            aridity = mean(aridity, na.rm = TRUE))
