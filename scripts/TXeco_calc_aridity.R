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
monthly.mean <- read.csv("../climate_data/TXeco_climate_monthlymesowest.csv",
                        na.strings = "NA", stringsAsFactors = FALSE)
normals <- read.csv("../climate_data/TXeco_climate_normals.csv",
                    na.strings = "NA", stringsAsFactors = FALSE)
site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv",
                        na.strings = "NA", stringsAsFactors = FALSE)

################################################################
# Join site coords with monthly means
################################################################
site.coords <- site.coords %>%
  select(property, sampling.year, latitude, longitude)

monthly.mean <- monthly.mean %>%
  full_join(site.coords, by = c("property", "sampling.year")) %>%
  unite("site.long", c(property, sampling.year), sep = " ") %>%
  slice_head(n = 377)

################################################################
# Separate properties into list of data frames. This is necessary
# because SPEI requires a time series notation that can only
# be accomplished through a series of unique lists
################################################################
monthly.mean <- setNames(split(x = monthly.mean,
                              f = monthly.mean$site.long),
                         paste0(unique(monthly.mean$site.long)))

# Clean missing data points
monthly.mean[["Uvalde_2020_02 2020"]] <- monthly.mean[["Uvalde_2020_02 2020"]][-9,]
monthly.mean[["Burnet_2020_12 2021"]] <- monthly.mean[["Burnet_2020_12 2021"]][-5,]
monthly.mean[["Brazos_2020_16 2021"]] <- monthly.mean[["Brazos_2020_16 2021"]][-c(1:2),]


################################################################
# Use Hargreaves equation to estimate 30-day reference 
# evapotranspiration, merge back to central data frame
################################################################
for(i in seq_along(monthly.mean)) {
  monthly.mean[[i]]$et0 <- as.numeric(hargreaves(Tmin = monthly.mean[[i]]$month.tmin,
                                                  Tmax = monthly.mean[[i]]$month.tmax,
                                                  lat = unique(monthly.mean[[i]]$latitude)))
  
  monthly.mean[[i]]$water.balance <- monthly.mean[[i]]$month.precip - monthly.mean[[i]]$et0
  monthly.mean[[i]]$spei <- spei(data = as.ts(monthly.mean[[i]]$water.balance), scale = 1)$fitted
  monthly.mean[[i]]$aridity <- monthly.mean[[i]]$month.precip / monthly.mean[[i]]$et0
}


## Merge list of data.frames into single data.frame
monthly.mean <- merge_all(monthly.mean)

## Separate concatenated "site.long" back to three separate columns
monthly.mean <- separate(monthly.mean, site.long, 
                        into = c("site", "sampling.year"),
                        sep = " ")

## Convert month/year to date column
monthly.mean <- monthly.mean %>%
  unite("month.year", c(month, year), sep = "/", remove = FALSE) %>%
  mutate(month.year = my(month.year))

## Create data file for statistical models
spei.initial <- monthly.mean %>%
  filter(month == 5) %>%
  mutate(visit.type = "i")

spei.primary.2021 <- monthly.mean %>%
  filter(site == "Bell_2021_08" | site == "Harris_2020_03" |
           site == "Uvalde_2020_02" | site == "Bandera_2020_03" |
           site == "Burnet_2020_12") %>%
  filter(sampling.year == 2021 & month == 6 & year == 2021) %>%
  mutate(visit.type = "p")

spei.primary.2020 <- monthly.mean %>%
  filter(site == "Harris_2020_03" | site == "Menard_2020_01" |
           site == "Bexar_2019_13" | site == "Williamson_2019_10" |
           site == "Uvalde_2020_02") %>%
  filter(sampling.year == 2020 & month == 7 & year == 2020) %>%
  mutate(visit.type = "p")

gs.spei <- spei.initial %>%
  full_join(spei.primary.2020) %>%
  full_join(spei.primary.2021) %>%
  select(site, sampling.year, visit.type, month.year, month.precip, 
         et0, water.balance, spei, aridity)

################################################################
# Visualize SPEI over time
################################################################
## 2020 site spei over time
ggplot(data = subset(monthly.mean, sampling.year == "2020"), 
       aes(x = month.year, y = spei, color = site)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
  facet_wrap(~reorder(site, longitude)) +
  guides(color = "none") +
  labs(x = "Date", y = "Standardized Precipitation-Evapotranspiration Index") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(size = 11))

## 2020 site spei during month of initial sampling
initial.2020 <- ggplot(data = subset(monthly.mean, 
                                     sampling.year == "2020" & month == "6" & year == "2020"), 
                       aes(x = reorder(site, spei), y = spei)) +
  geom_bar(data = subset(normals, month == 6 & sampling.year == 2020), 
    stat = "identity", fill = "yellow", alpha = 0.75) +
  geom_bar(stat = "identity", fill = "black") +
  scale_y_continuous(limits = c(-2, 2.5), breaks = seq(-2, 2, 1)) +
  labs(x = NULL, 
       y = "SPEI") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1))


## 2020 site spei during month of primary sampling
primary.2020 <- ggplot(data = subset(monthly.mean, 
                                     sampling.year == "2020" & month == "7" & year == "2020"), 
                       aes(x = reorder(site, spei), y = spei)) +
  geom_bar(data = subset(normals, month == 7 & sampling.year == 2020 & ( 
    site == "Harris_2020_03" | site == "Williamson_2019_10" |
      site == "Menard_2020_01" | site == "Bexar_2019_13" |
      site == "Uvalde_2020_02")), 
    stat = "identity", fill = "yellow", alpha = 0.75) +
  geom_bar(stat = "identity", fill = "black") +
  scale_y_continuous(limits = c(-2, 2.5), breaks = seq(-2, 2, 1)) +
  labs(x = NULL, 
       y = NULL) +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1))

## 2021 site spei over time
ggplot(data = subset(monthly.mean, sampling.year == "2021"), 
       aes(x = month.year, y = spei, color = site)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
  facet_wrap(~reorder(site, longitude)) +
  guides(color = "none") +
  labs(x = "Date", y = "SPEI") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(size = 11))

## 2021 site spei during month of initial sampling
initial.2021 <- ggplot(data = subset(monthly.mean, 
                                     sampling.year == "2021" & month == "5" & year == "2021"), 
                       aes(x = reorder(site, spei), y = spei)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_bar(data = subset(normals, month == 5 & sampling.year == 2021), 
    stat = "identity", fill = "yellow", alpha = 0.75) +
  scale_y_continuous(limits = c(-2, 2.5), breaks = seq(-2, 2, 1)) +
  labs(x = "Site", 
       y = "SPEI") +
  pubtheme +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1))

## 2021 site spei during month of primary sampling
primary.2021 <- ggplot(data = subset(monthly.mean, 
                                     sampling.year == "2021" & month == "6" & year == "2021"), 
                       aes(x = reorder(site, spei), y = spei)) +
  geom_bar(data = subset(normals, month == 6 & sampling.year == 2021 & ( 
                           site == "Harris_2020_03" | site == "Bandera_2020_03" |
                           site == "Bell_2021_08" | site == "Burnet_2020_12" |
                           site == "Uvalde_2020_02")), 
                         stat = "identity", fill = "yellow", alpha = 0.75) +
  geom_bar(stat = "identity", fill = "black") +
  scale_y_continuous(limits = c(-2, 2.5), breaks = seq(-2, 2, 1)) +
  labs(x = "Site", 
       y = NULL) +
  pubtheme +
  theme(axis.text.x = element_text(angle = 50, size = 10, hjust = 1))

## 2020 and 2021 30-day SPEI
ggarrange(initial.2020, primary.2020, initial.2021, primary.2021,
          ncol = 2, nrow = 2, common.legend = TRUE)


################################################################
# Clean normals data sheet and put in list of data frames by
# site to pass Hargreaves function through
################################################################
normals <- normals %>%
  select(property = site, sampling.year, month, 
         norm.precip, norm.tmax, norm.tmin) %>%
  left_join(site.coords) %>%
  select(property, latitude, sampling.year, month, norm.tmax, norm.tmin, norm.precip) %>%
  unite("site.long", c(property, sampling.year), sep = " ")

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
normals <- separate(normals, site.long, 
                    into = c("site", "sampling.year"),
                    sep = " ")
normals <- normals %>%
  unite("month.year", c(month, sampling.year), sep = "/", remove = FALSE) %>%
  mutate(month.year = my(month.year))

################################################################
# Visualize some patterns
################################################################
ggplot(data = subset(normals, sampling.year == 2020), 
       aes(x = month.year, y = spei, color = site)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  facet_wrap(~site) +
  guides(color = "none") +
  labs(x = "Date", y = "Standardized Precipitation-Evapotranspiration Index") +
  pubtheme +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        strip.text = element_text(size = 12))

ggplot(data = subset(normals, sampling.year == 2021), 
       aes(x = month.year, y = spei, color = site)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  facet_wrap(~site) +
  guides(color = "none") +
  labs(x = "Date", y = "Standardized Precipitation-Evapotranspiration Index") +
  pubtheme +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        strip.text = element_text(size = 12))


################################################################
# Create normals data frame and merge with gs.spei data.frame
################################################################
spei.norm.init <- normals %>%
  filter(ifelse(sampling.year == 2020, month == 6, month == 5)) %>%
  mutate(visit.type = "i")

spei.norm.prim <- normals %>%
  filter(site == "Bell_2021_08" | site == "Harris_2020_03" |
           site == "Uvalde_2020_02" | site == "Bandera_2020_03" |
           site == "Burnet_2020_12" | site == "Menard_2020_01" |
           site == "Bexar_2019_13" | site == "Williamson_2019_10") %>%
  filter(ifelse(sampling.year == 2020, month == 7, month == 6)) %>%
  mutate(visit.type = "p")

spei.clim <- spei.norm.init %>%
  full_join(spei.norm.prim) %>% 
  select(site, sampling.year, visit.type, month.year, norm.precip, norm.et0 = et0, 
         norm.water.balance = water.balance, norm.spei = spei, norm.aridity = aridity) %>%
  full_join(gs.spei, by = c("site", "sampling.year", "visit.type"))


write.csv(spei.clim, "../data_sheets/TXeco_spei_data.csv", row.names = FALSE)
