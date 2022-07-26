##########################################################################
## Load libraries, import data, and load calc_chi
##########################################################################
# Load libraries
library(dplyr)
library(tidyr)
library(LeafArea)
library(readxl)
library(reshape)

##########################################################################
## Import raw leaf trait data files
##########################################################################
biomass <- read.csv("../data_sheets/TXeco_drybiomass.csv", na.strings = "NA")

##########################################################################
## Import soil data, spei data, site coord data, and species id data
##########################################################################
clim <- read.csv("../climate_data/TXeco_climate_data.csv", 
                 stringsAsFactors = FALSE, na.strings = "NA") %>%
  dplyr::select(site:pt.15yr, tavg30:sf1)

site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv")

spp.info <- read.csv("../data_sheets/TXeco_species_id.csv", 
                     na.strings = c("NA", "<NA>", ""))

soil <- read.csv("../data_sheets/TXeco_soil_characteristics.csv")

##########################################################################
## Load calc_chi function
##########################################################################
source("/Users/eaperkowski/git/r_functions/calc_chi.R")
source("/Users/eaperkowski/git/r_functions/calc_beta.R")
source("/Users/eaperkowski/git/r_functions/calc_optchi.R")

##########################################################################
## Import raw costech files
##########################################################################
file.list <- list.files(path = "../cn_results",
                        recursive = TRUE,
                        pattern = "\\.csv$",
                        full.names = TRUE)

file.list <- setNames(file.list, file.list)
cn.plates <- lapply(file.list, read.csv, na.strings = "NA")

##########################################################################
## Merge raw costech files, remove QC and standards, and include only
## %N and %C
##########################################################################
cn.plates <- cn.plates %>%
  merge_all() %>%
  filter(sample.type == "unknown" & sample.id != "QC") %>%
  dplyr::select(id = sample.id, n.leaf = nitrogen.weight.percent, 
         c.leaf = carbon.weight.percent)

##########################################################################
## Quantify leaf area using leaf area images
##########################################################################
ij.path <- "/Applications/ImageJ.app"
imagepath <- "/Users/eaperkowski/git/TX_ecolab_leafNitrogen/leaf_area/"

leaf.area <- run.ij(path.imagej = ij.path,
                    set.directory = imagepath,
                    distance.pixel = 117.9034,
                    known.distance = 1,
                    set.memory = 300, low.size = 0.1)
names(leaf.area)[1] <- "id"

##########################################################################
## Append leaf areas to biomass and cn plate data, calculate sla and narea
##########################################################################
leaf <- biomass %>%
  full_join(leaf.area) %>%
  full_join(cn.plates) %>%
  mutate(sla = (total.leaf.area / dry.wgt),
         marea = 1/sla*10000,
         narea = (n.leaf/100) /sla * 10000,
         leaf.cn = c.leaf / n.leaf)

# Separate site in leaf data.frame, merge with site.coords to get property name
full.df <- leaf %>%
  full_join(spp.info) %>%
  separate(id, c("year", "site", "visit.type", "rep")) %>%
  unite("site", year:site) %>%
  full_join(site.coords) %>%
  unite("site", site:visit.type) %>%
  dplyr::select(-(X:X.4)) %>%
  full_join(soil) %>%
  separate(col = site, into = c("sampling.year", "county", "visit.type"), sep = "_") %>%
  dplyr::mutate(sampling.year = ifelse(sampling.year == "2020eco", 
                                as.numeric("2020"), as.numeric("2021"))) %>%
  unite("id", county:rep, remove = FALSE) %>%
  dplyr::mutate(visit.type = ifelse(visit.type == "i",
                                    "initial", "primary")) %>%
  dplyr::select(site = property, id, sampling.year, visit.type, elevation.m,
                soil.pH:soil.potassium, total.leaf.area:pft) %>%
  merge(clim, by = c("site", "sampling.year", "visit.type")) %>%
  group_by(site, sampling.year, id, visit.type, pft, photo, duration, n.fixer, 
           NCRS.code) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  filter(pft != "c3_tree") %>%
  dplyr::mutate(chi = ifelse(pft == "c4_graminoid", 
                             calc_chi_c4(d13C),
                             calc_chi_c3(d13C)))

## Add chi column
full.df$chi[full.df$chi < 0.2 | full.df$chi > 0.95] <- NA
full.df$beta <- calc_beta(chi = full.df$chi, temp = full.df$tavg7, 
                          vpd = full.df$vpd7 * 10, z = full.df$elevation.m)

## Write csv
write.csv(full.df, "../data_sheets/TXeco_compiled_datasheet.csv")
