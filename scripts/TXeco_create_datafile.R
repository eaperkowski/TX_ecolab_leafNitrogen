##########################################################################
## Load libraries, import data, and load calc_chi
##########################################################################
# Load libraries
library(dplyr)
library(tidyr)

# Import datasheets
spei <- read.csv("../data_sheets/TXeco_spei_data.csv",
                 stringsAsFactors = FALSE,
                 na.strings = "NA")
spei <- dplyr::select(spei, site:visit.type, norm.precip, norm.spei, norm.aridity,
               month.precip, spei, aridity)
leaf <- read.csv("../data_sheets/TXeco_leaftraits.csv")
site.coords <- read.csv("../data_sheets/TXeco_sitecoords.csv")
spp.info <- read.csv("../data_sheets/TXeco_species_id.csv", 
                     na.strings = c("NA", "<NA>", ""))
soil <- read.csv("../data_sheets/TXeco_soil_characteristics.csv")

# Load calc_chi
source("/Users/eaperkowski/git/r_functions/calc_chi.R")

# Separate site in leaf data.frame, merge with site.coords to get property name
test <- leaf %>%
  left_join(spp.info) %>%
  separate(site, c("year", "site", "visit.type", "rep")) %>%
  unite("site", year:site) %>%
  full_join(site.coords) %>%
  unite("site", site:visit.type) %>%
  left_join(soil) %>%
  separate(site, c("sampling.year", "county", "visit.type")) %>%
  dplyr::mutate(sampling.year = ifelse(sampling.year == "2020eco", 
                                as.numeric("2020"), as.numeric("2021"))) %>%
  unite("id", county:rep, remove = FALSE) %>%
  dplyr::select(site = property, id, sampling.year, visit.type, soil.pH:soil.potassium,
                total.leaf.area:pft) %>%
  merge(spei, by = c("site", "sampling.year", "visit.type")) %>%
  group_by(site, sampling.year, id, visit.type, photo, pft, NCRS.code) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE)

test$chi <- calc_chi(test$d13C, type = test$photo)

write.csv(test, "../data_sheets/TXeco_compiled_datasheet.csv")
