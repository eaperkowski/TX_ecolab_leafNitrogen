## Load libraries
library(smapr)
library(sp)
library(raster)
library(lubridate)

## Set SMAP credentials
set_smap_credentials(username = "eaperkowski", password = "TXecolab_leafN1920",
                     overwrite = TRUE)

###############################################################################
# Download smap data
###############################################################################
## Add date sequence covering all relevant timescales for TXecolab property
## visits. Starts 90-days before June 1, 2020; ends July 31, 2021
ecolab_date_seq <- seq.Date(from = as.Date("2021-05-09"),
                            to = as.Date("2021-06-30"),
                            by = 1)

## Find available SMAP datasets
ecolab_available_smap <- find_smap(id = 'SPL4SMAU',
                                   dates = ecolab_date_seq,
                                   version = 6)

## Download SMAP datasets
local_files <- download_smap(directory = "/Users/eaperkowski/git/TX_ecolab_leafNitrogen/smap/",
                             overwrite = T, verbose = F)

local_files$name[1:2]
list_smap(local_files[1, ])
list_smap(local_files, all = TRUE)

sm_raster <- extract_smap(local_files, name = "/Analysis_Data/sm_profile_analysis")

plot(sm_raster)
