## Load libraries
library(splash)
library(dplyr)

## Set seed
set.seed(5)

## Create test data frame with random sunlight fraction (sf),
## mean air temp (tair), and daily precip (pn) values
test.df <- data.frame(y = rep(2021, times = 150),
                      m = rep(seq(1,5,1), each = 30),
                      i = rep(seq(1,30,1), times = 5),
                      sf = rnorm(150, mean = 0.75, sd = 0.1),
                      tair = rnorm(150, mean = 25, sd = 2),
                      pn = runif(150, min = 0, max = 2))

## Save data frame
write.csv(test.df, "/Users/eaperkowski/Desktop/test.df.csv", 
          row.names = FALSE)

## Load test data frame created above. Read_csv is a function from
## "splash" model that formats data to easily spin up model
test <- read_csv(fname = "/Users/eaperkowski/Desktop/test.df.csv",
         y = 2021)

## add ancillary site details
test$lat_deg <- 29.776805
test$elv_m <- 66
test$m <- rep(seq(1,5,1), each = 30)
test$i <- rep(seq(1,30,1), times = 5)
test$pn <- runif(150, min = 0, max = 2)


## Create dummy daily_totals value for daily spin up of soil moisture
daily_totals <- matrix(data = rep(0, 150), nrow = 150, ncol = 1)
daily_totals <- as.data.frame(daily_totals)
names(daily_totals) <- c("wn")

## Spin up model to get soil moisture estimates
daily_totals <- 
  spin_up(test, daily_totals)

spin_up

## Merge soil moisture estimates with test data frame
test <- as.data.frame(test)
full.test <- test %>%
  coalesce(daily_totals) %>%
  select(-file_name)

## Run merged data frame with soil moisture estimates to estimate
## eet, pet, aet, and ppfd

?run_one_day
all.clim <- run_one_day(lat = full.test$lat_deg,
                        elv = full.test$elv_m,
                        n = full.test$i,
                        y = full.test$year,
                        wn = full.test$wn,
                        sf = full.test$sf,
                        tc = full.test$tair,
                        pn = full.test$pn) %>%
  as.data.frame %>%
  coalesce(full.test) %>%
  select(month = m, day = i, year, lat_deg, elv_m, sf:pn, wn, everything(), -num_lines)


library(ggplot2)
ggplot(data = all.clim, aes(x = aet, y = eet)) +
  geom_point() +
  geom_smooth(method = "lm")

month.plant.moisture <- all.clim %>%
  group_by(month) %>%
  summarize(mean.aet = mean(aet, na.rm = TRUE),
            mean.pet = mean(pet, na.rm = TRUE),
            mean.eet = mean(eet, na.rm = TRUE)) %>%
  mutate(plantavail.surf.moist = mean.aet / mean.eet)

day.plant.moisture <- all.clim %>%
  group_by(month, day) %>%
  summarize(plantavail.surf.moist = aet / eet)

