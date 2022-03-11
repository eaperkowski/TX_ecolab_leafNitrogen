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
spei <- read.csv("../data_sheets/TXeco_spei_data.csv", 
                 stringsAsFactors = FALSE, na.strings = "NA")

spei <- dplyr::select(spei, site:visit.type, norm.precip, norm.spei, 
                      norm.aridity, month.precip, spei, aridity)

site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv")

spp.info <- read.csv("../data_sheets/TXeco_species_id.csv", 
                     na.strings = c("NA", "<NA>", ""))

soil <- read.csv("../data_sheets/TXeco_soil_characteristics.csv")

##########################################################################
## Load calc_chi function
##########################################################################
source("/Users/eaperkowski/git/r_functions/calc_chi.R")

##########################################################################
## Import raw costech files
##########################################################################
file.list <- list.files(path = "/Users/eaperkowski/OneDrive - Texas Tech University/TXeco_leafnitrogen/cn_results/",
                        recursive = TRUE,
                        pattern = "\\.xlsx$",
                        full.names = TRUE)

file.list <- setNames(file.list, file.list)
cn.plates <- lapply(file.list, read_xlsx)

##########################################################################
## Merge raw costech files, remove QC and standards, and include only
## %N and %C
##########################################################################
cn.plates <- cn.plates %>%
  merge_all() %>%
  filter(sample.type == "unknown" & sample.id != "QC") %>%
  select(id = sample.id, n.leaf = nitrogen.weight.percent, 
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
         narea = (n.leaf / sla) * 10000)

## Stopped here 03/10/2022 ##



# Separate site in leaf data.frame, merge with site.coords to get property name
test <- leaf %>%
  full_join(spp.info) %>%
  separate(id, c("year", "site", "visit.type", "rep")) %>%
  unite("site", year:site) %>%
  full_join(site.coords) %>%
  unite("site", site:visit.type) %>%
  full_join(soil) %>%
  separate(site, c("sampling.year", "county", "visit.type")) %>%
  dplyr::mutate(sampling.year = ifelse(sampling.year == "2020eco", 
                                as.numeric("2020"), as.numeric("2021"))) %>%
  unite("id", county:rep, remove = FALSE) %>%
  dplyr::select(site = property, id, sampling.year, visit.type, 
                soil.pH:soil.potassium, total.leaf.area:pft) %>%
  merge(spei, by = c("site", "sampling.year", "visit.type")) %>%
  group_by(site, sampling.year, id, visit.type, pft, photo, duration, n.fixer, 
           NCRS.code) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  filter(pft != "c3_tree")

## Add chi column
test$chi <- calc_chi(test$d13C, type = test$photo)

test
write.csv(test, "../data_sheets/TXeco_compiled_datasheet.csv")
