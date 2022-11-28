## Libraries
library(soilDB)
library(dplyr)
library(dplyr)

## Load site data 
sites <- read.csv("../data_sheets/TXeco_sitecoords.csv",
                 stringsAsFactors = FALSE) %>%
  mutate(lat = as.numeric(latitude), long = as.numeric(longitude)) %>%
  distinct(property, .keep_all = TRUE) %>%
  dplyr::select(id = property, lat, long)

## Modify Harris Co. property to nearest grid
sites[sites$id == "Harris_2020_03", 2:3] <- c(29.8683, -95.3049)

## Extract Soil Grid data from GPS coordinates
site.soilgrid <- fetchSoilGrids(x = sites,
                                loc.names = c("id", "lat", "long"),
                                verbose = TRUE,
                                progress = TRUE)

## Clean SoilGrids output to include mean 0-30cm soil data
site.soilgrid.df <- site.soilgrid@horizons %>%
  dplyr::select(id, horizon = label, 
                bulkdens = bdodmean, bulkdens.uc = bdoduncertainty,
                cec = cecmean, cec.uncertainty= cecuncertainty, 
                perc.gravel = cfvomean, perc.gravel.uc = cfvouncertainty,
                perc.clay = claymean, perc.clay.uc = clayuncertainty, 
                n.mean = nitrogenmean, n.uc = nitrogenuncertainty,
                ph.mean = phh2omean, ph.uc = phh2ouncertainty, 
                perc.sand = sandmean, perc.sand.uc = sanduncertainty,
                perc.silt = siltmean, perc.silt.uc = siltuncertainty, 
                soc = socmean, soc.uc = socuncertainty) %>%
  filter(horizon == "0-5" | horizon == "5-15") %>%
  group_by(id) %>%
  summarize(across(bulkdens:soc.uc, mean)) %>%
  mutate(across(bulkdens:soc.uc, round, 1),
         om = soc * 1.724) # van Bemmelen factor for converting SOC -> SOM

## Load bedrock GEOTiff into 
bedrock.rast <- rast("../climate_data/soil_grids/TXeco_soilGrid_250m_bedrock.tif")

## Check to make sure raster plots
plot(bedrock.rast)

## Extract values from grid cells containing sites
eco.coords <- dplyr::select(sites, x = long, y = lat) # select only lat/long data, code as xy

site.soilgrid.full <- terra::extract(x = bedrock.rast,
                                     y = eco.coords, xy = TRUE) %>%
  select(bedrock = TXeco_soilGrid_250m_bedrock) %>%
  cbind(sites) %>%
  select(id, lat, long, bedrock) %>%
  full_join(site.soilgrid.df)

write.csv(site.soilgrid.full, "../data_sheets/TXeco_soilgrid_data.csv",
          row.names = FALSE)
