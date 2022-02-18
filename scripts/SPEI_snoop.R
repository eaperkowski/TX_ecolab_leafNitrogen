df.bell <- read.csv("/Users/eaperkowski/git/TX_ecolab_leafNitrogen/climate_data/mesowest/2020_Bell_i.csv")


test.hourly <- df.bell %>%
  mutate(date.time.round = floor_date(as.POSIXct(strptime(date.time,
                                                          format = "%m/%d/%Y %H:%M UTC")),
                                      unit = "hour")) %>%
  separate(date.time.round, into = c("date", "time.round"), 
           sep = " ", remove = FALSE) %>%
  group_by(date, time.round) %>%
  summarize(air.temp = mean(air.temp, na.rm = TRUE),
            relative.humidity = mean(relative.humidity, na.rm = TRUE),
            additive.precip = max(additive.precip, na.rm = TRUE),
            sea.level.pressure = mean(sea.level.pressure, na.rm = TRUE),
            atm.pressure = mean(atm.pressure, na.rm = TRUE)) %>%
  mutate(hourly.precip = additive.precip - lag(additive.precip),
         hourly.precip = ifelse(hourly.precip == "Inf" | hourly.precip < 0, 
                                0, hourly.precip),
         site = unique(df.bell$site))
  

test.daily <- test.hourly %>%
  group_by(date, site) %>%
  summarize(max.temp = max(air.temp, na.rm = TRUE),
            min.temp = min(air.temp, na.rm = TRUE),
            daily.precip = sum(hourly.precip, na.rm = TRUE)) %>%
  mutate(latitude = unique(df.bell$latitude),
         longitude = unique(df.bell$longitude))



## Test monthly
test.monthly <- test.daily %>%
  mutate(month = month(date)) %>%
  group_by(site, month, latitude, longitude) %>%
  summarize(tmax.mean = mean(max.temp, na.rm = TRUE),
            tmin.mean = min(min.temp, na.rm = TRUE),
            monthly.precip = sum(daily.precip))  %>%
  filter(is.na(month) == FALSE) %>%
  data.frame()



## SPEI
test.monthly <- setNames(split(x = test.monthly,
                               f = test.monthly$site),
                         paste0(unique(test.monthly$site)))


for(i in seq_along(test.monthly)) {
  test.monthly[[i]]$et0 <- as.numeric(hargreaves(Tmin = test.monthly[[i]]$tmin.mean,
                                                 Tmax = test.monthly[[i]]$tmax.mean,
                                                 lat = unique(test.monthly[[i]]$latitude)))
  
  test.monthly[[i]]$water.balance <- test.monthly[[i]]$monthly.precip - test.monthly[[i]]$et0
  test.monthly[[i]]$spei <- as.vector(spei(data = as.ts(test.monthly[[i]]$water.balance), scale = 1)$fitted)
  test.monthly[[i]]$aridity <- test.monthly[[i]]$monthly.precip / test.monthly[[i]]$et0
}

merge_all(test.monthly)




