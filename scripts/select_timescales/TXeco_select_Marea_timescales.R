###############################################################################
# Libraries
###############################################################################
library(dplyr)
library(car)
library(lme4)
library(MuMIn)
library(nlme)
library(sjstats)
library(ggplot2)

###############################################################################
# Load compiled data file
###############################################################################
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv")

###############################################################################
# Iterative models for growing season aridity and for log(narea)
###############################################################################
df$marea[c(20, 21, 244)] <- NA

# 30-day aridity
ai.30 <- lmer(log(marea) ~ ai.30 + (1 | sampling.year) + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.30))
hist(residuals(ai.30))
outlierTest(ai.30)
Anova(ai.30)

# 60-day aridity
ai.60 <- lmer(log(marea) ~ ai.60 + (1 | sampling.year) + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.60))
hist(residuals(ai.60))
outlierTest(ai.60)
Anova(ai.60)

# 90-day aridity
ai.90 <- lmer(log(marea) ~ ai.90 + (1 | sampling.year) + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.90))
hist(residuals(ai.90))
outlierTest(ai.90)
Anova(ai.90)

# 15-year aridity
ai.15yr <- lmer(log(marea) ~ ai.15yr + (1 | sampling.year) + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.15yr))
hist(residuals(ai.15yr))
outlierTest(ai.15yr)
Anova(ai.15yr)

# Model selection across timescales
ai.30_modelSelect <- data.frame(timescale = "30_day",
                                AICc = AICc(ai.30),
                                RMSE = rmse(ai.30), 
                                r.squaredGLMM(ai.30))
ai.60_modelSelect <- data.frame(timescale = "60_day",
                                AICc = AICc(ai.60),
                                RMSE = rmse(ai.60), 
                                r.squaredGLMM(ai.60))
ai.90_modelSelect <- data.frame(timescale = "90_day", 
                                AICc = AICc(ai.90),
                                RMSE = rmse(ai.90), 
                                r.squaredGLMM(ai.90))
ai.15yr_modelSelect <- data.frame(timescale = "15_year",
                                  AICc = AICc(ai.15yr),
                                  RMSE = rmse(ai.15yr), 
                                  r.squaredGLMM(ai.15yr))

ai.30_modelSelect %>% full_join(ai.60_modelSelect) %>% 
  full_join(ai.90_modelSelect) %>% full_join(ai.15yr_modelSelect) %>%
  arrange(AICc)
## 30-day aridity is the best model

###############################################################################
# Precipitation AICc for log(narea)
###############################################################################
prcp30 <- lmer(log(marea) ~ prcp30 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp29 <- lmer(log(marea) ~ prcp29 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp28 <- lmer(log(marea) ~ prcp28 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp27 <- lmer(log(marea) ~ prcp27 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp26 <- lmer(log(marea) ~ prcp26 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp25 <- lmer(log(marea) ~ prcp25 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp24 <- lmer(log(marea) ~ prcp24 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp23 <- lmer(log(marea) ~ prcp23 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp22 <- lmer(log(marea) ~ prcp22 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp21 <- lmer(log(marea) ~ prcp21 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp20 <- lmer(log(marea) ~ prcp20 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp19 <- lmer(log(marea) ~ prcp19 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp18 <- lmer(log(marea) ~ prcp18 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp17 <- lmer(log(marea) ~ prcp17 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp16 <- lmer(log(marea) ~ prcp16 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp15 <- lmer(log(marea) ~ prcp15 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp14 <- lmer(log(marea) ~ prcp14 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp13 <- lmer(log(marea) ~ prcp13 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp12 <- lmer(log(marea) ~ prcp12 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp11 <- lmer(log(marea) ~ prcp11 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp10 <- lmer(log(marea) ~ prcp10 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp9 <- lmer(log(marea) ~ prcp9 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp8 <- lmer(log(marea) ~ prcp8 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp7 <- lmer(log(marea) ~ prcp7 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp6 <- lmer(log(marea) ~ prcp6 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp5 <- lmer(log(marea) ~ prcp5 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp4 <- lmer(log(marea) ~ prcp4 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp3 <- lmer(log(marea) ~ prcp3 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp2 <- lmer(log(marea) ~ prcp2 + (1 | sampling.year) + (1 | NCRS.code), data = df)
prcp1 <- lmer(log(marea) ~ prcp1 + (1 | sampling.year) + (1 | NCRS.code), data = df)
map <- lmer(log(marea) ~ map.15yr + (1 | sampling.year) + (1 | NCRS.code), data = df)

# Model selection across timescales
prcp30.modelSelect <- data.frame(day = 30, var = "prcp", AICc = AICc(prcp30), 
                          RMSE = rmse(prcp30), r.squaredGLMM(prcp30))
prcp29.modelSelect <- data.frame(day = 29, var = "prcp", AICc = AICc(prcp29), 
                          RMSE = rmse(prcp29), r.squaredGLMM(prcp29))
prcp28.modelSelect <- data.frame(day = 28, var = "prcp", AICc = AICc(prcp28), 
                          RMSE = rmse(prcp28), r.squaredGLMM(prcp28))
prcp27.modelSelect <- data.frame(day = 27, var = "prcp", AICc = AICc(prcp27), 
                          RMSE = rmse(prcp27), r.squaredGLMM(prcp27))
prcp26.modelSelect <- data.frame(day = 26, var = "prcp", AICc = AICc(prcp26), 
                          RMSE = rmse(prcp26), r.squaredGLMM(prcp26))
prcp25.modelSelect <- data.frame(day = 25, var = "prcp", AICc = AICc(prcp25), 
                          RMSE = rmse(prcp25), r.squaredGLMM(prcp25))
prcp24.modelSelect <- data.frame(day = 24, var = "prcp", AICc = AICc(prcp24), 
                          RMSE = rmse(prcp24), r.squaredGLMM(prcp24))
prcp23.modelSelect <- data.frame(day = 23, var = "prcp", AICc = AICc(prcp23), 
                          RMSE = rmse(prcp23), r.squaredGLMM(prcp23))
prcp22.modelSelect <- data.frame(day = 22, var = "prcp", AICc = AICc(prcp22), 
                          RMSE = rmse(prcp22), r.squaredGLMM(prcp22))
prcp21.modelSelect <- data.frame(day = 21, var = "prcp", AICc = AICc(prcp21), 
                          RMSE = rmse(prcp21), r.squaredGLMM(prcp21))
prcp20.modelSelect <- data.frame(day = 20, var = "prcp", AICc = AICc(prcp20), 
                          RMSE = rmse(prcp20), r.squaredGLMM(prcp20))
prcp19.modelSelect <- data.frame(day = 19, var = "prcp", AICc = AICc(prcp19), 
                          RMSE = rmse(prcp19), r.squaredGLMM(prcp19))
prcp18.modelSelect <- data.frame(day = 18, var = "prcp", AICc = AICc(prcp18), 
                          RMSE = rmse(prcp18), r.squaredGLMM(prcp18))
prcp17.modelSelect <- data.frame(day = 17, var = "prcp", AICc = AICc(prcp17), 
                          RMSE = rmse(prcp17), r.squaredGLMM(prcp17))
prcp16.modelSelect <- data.frame(day = 16, var = "prcp", AICc = AICc(prcp16), 
                          RMSE = rmse(prcp16), r.squaredGLMM(prcp16))
prcp15.modelSelect <- data.frame(day = 15, var = "prcp", AICc = AICc(prcp15), 
                          RMSE = rmse(prcp15), r.squaredGLMM(prcp15))
prcp14.modelSelect <- data.frame(day = 14, var = "prcp", AICc = AICc(prcp14), 
                          RMSE = rmse(prcp14), r.squaredGLMM(prcp14))
prcp13.modelSelect <- data.frame(day = 13, var = "prcp", AICc = AICc(prcp13), 
                          RMSE = rmse(prcp13), r.squaredGLMM(prcp13))
prcp12.modelSelect <- data.frame(day = 12, var = "prcp", AICc = AICc(prcp12), 
                          RMSE = rmse(prcp12), r.squaredGLMM(prcp12))
prcp11.modelSelect <- data.frame(day = 11, var = "prcp", AICc = AICc(prcp11), 
                          RMSE = rmse(prcp11), r.squaredGLMM(prcp11))
prcp10.modelSelect <- data.frame(day = 10, var = "prcp", AICc = AICc(prcp10), 
                          RMSE = rmse(prcp10), r.squaredGLMM(prcp10))
prcp9.modelSelect <- data.frame(day = 9, var = "prcp", AICc = AICc(prcp9), 
                         RMSE = rmse(prcp9), r.squaredGLMM(prcp9))
prcp8.modelSelect <- data.frame(day = 8, var = "prcp", AICc = AICc(prcp8), 
                         RMSE = rmse(prcp8), r.squaredGLMM(prcp8))
prcp7.modelSelect <- data.frame(day = 7, var = "prcp", AICc = AICc(prcp7), 
                         RMSE = rmse(prcp7), r.squaredGLMM(prcp7))
prcp6.modelSelect <- data.frame(day = 6, var = "prcp", AICc = AICc(prcp6), 
                         RMSE = rmse(prcp6), r.squaredGLMM(prcp6))
prcp5.modelSelect <- data.frame(day = 5, var = "prcp", AICc = AICc(prcp5), 
                         RMSE = rmse(prcp5), r.squaredGLMM(prcp5))
prcp4.modelSelect <- data.frame(day = 4, var = "prcp", AICc = AICc(prcp4), 
                         RMSE = rmse(prcp4), r.squaredGLMM(prcp4))
prcp3.modelSelect <- data.frame(day = 3, var = "prcp", AICc = AICc(prcp3), 
                         RMSE = rmse(prcp3), r.squaredGLMM(prcp3))
prcp2.modelSelect <- data.frame(day = 2, var = "prcp", AICc = AICc(prcp2), 
                         RMSE = rmse(prcp2), r.squaredGLMM(prcp2))
prcp1.modelSelect <- data.frame(day = 1, var = "prcp", AICc = AICc(prcp1), 
                         RMSE = rmse(prcp1), r.squaredGLMM(prcp1))
map.15yr.modelSelect <- data.frame(day = 5478, var = "temp", AICc = AICc(map), 
                                   RMSE = rmse(map), r.squaredGLMM(map))

prcp30.modelSelect %>% 
  full_join(map.15yr.modelSelect) %>% full_join(prcp29.modelSelect) %>% 
  full_join(prcp28.modelSelect) %>% full_join(prcp27.modelSelect) %>% 
  full_join(prcp26.modelSelect) %>% full_join(prcp25.modelSelect) %>%
  full_join(prcp24.modelSelect) %>% full_join(prcp23.modelSelect) %>% 
  full_join(prcp22.modelSelect) %>% full_join(prcp21.modelSelect) %>% 
  full_join(prcp20.modelSelect) %>% full_join(prcp19.modelSelect) %>%
  full_join(prcp18.modelSelect) %>% full_join(prcp17.modelSelect) %>% 
  full_join(prcp16.modelSelect) %>% full_join(prcp15.modelSelect) %>% 
  full_join(prcp14.modelSelect) %>% full_join(prcp13.modelSelect) %>%
  full_join(prcp12.modelSelect) %>% full_join(prcp11.modelSelect) %>% 
  full_join(prcp10.modelSelect) %>% full_join(prcp9.modelSelect) %>% 
  full_join(prcp8.modelSelect) %>% full_join(prcp7.modelSelect) %>%
  full_join(prcp6.modelSelect) %>% full_join(prcp5.modelSelect) %>% 
  full_join(prcp4.modelSelect) %>% full_join(prcp3.modelSelect) %>% 
  full_join(prcp2.modelSelect) %>% full_join(prcp1.modelSelect) %>%
  mutate(concat.select = AICc + RMSE) %>%
  arrange(AICc)
## 27-day precipitation is best model

###############################################################################
# Iterative models for Tmean and log(narea)
###############################################################################
temp30 <- lmer(log(marea) ~ tavg30 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp29 <- lmer(log(marea) ~ tavg29 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp28 <- lmer(log(marea) ~ tavg28 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp27 <- lmer(log(marea) ~ tavg27 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp26 <- lmer(log(marea) ~ tavg26 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp25 <- lmer(log(marea) ~ tavg25 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp24 <- lmer(log(marea) ~ tavg24 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp23 <- lmer(log(marea) ~ tavg23 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp22 <- lmer(log(marea) ~ tavg22 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp21 <- lmer(log(marea) ~ tavg21 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp20 <- lmer(log(marea) ~ tavg20 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp19 <- lmer(log(marea) ~ tavg19 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp18 <- lmer(log(marea) ~ tavg18 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp17 <- lmer(log(marea) ~ tavg17 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp16 <- lmer(log(marea) ~ tavg16 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp15 <- lmer(log(marea) ~ tavg15 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp14 <- lmer(log(marea) ~ tavg14 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp13 <- lmer(log(marea) ~ tavg13 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp12 <- lmer(log(marea) ~ tavg12 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp11 <- lmer(log(marea) ~ tavg11 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp10 <- lmer(log(marea) ~ tavg10 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp9 <- lmer(log(marea) ~ tavg9 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp8 <- lmer(log(marea) ~ tavg8 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp7 <- lmer(log(marea) ~ tavg7 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp6 <- lmer(log(marea) ~ tavg6 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp5 <- lmer(log(marea) ~ tavg5 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp4 <- lmer(log(marea) ~ tavg4 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp3 <- lmer(log(marea) ~ tavg3 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp2 <- lmer(log(marea) ~ tavg2 + (1 | sampling.year) + (1 | NCRS.code), data = df)
temp1 <- lmer(log(marea) ~ tavg1 + (1 | sampling.year) + (1 | NCRS.code), data = df)
mat <- lmer(log(marea) ~ mat.15yr + (1 | sampling.year) + (1 | NCRS.code), data = df)

# Model selection across timescales
temp30.modelSelect <- data.frame(day = 30, var = "temp", AICc = AICc(temp30), 
                                 RMSE = rmse(temp30), r.squaredGLMM(temp30))
temp29.modelSelect <- data.frame(day = 29, var = "temp", AICc = AICc(temp29), 
                                 RMSE = rmse(temp29), r.squaredGLMM(temp29))
temp28.modelSelect <- data.frame(day = 28, var = "temp", AICc = AICc(temp28), 
                                 RMSE = rmse(temp28), r.squaredGLMM(temp28))
temp27.modelSelect <- data.frame(day = 27, var = "temp", AICc = AICc(temp27), 
                                 RMSE = rmse(temp27), r.squaredGLMM(temp27))
temp26.modelSelect <- data.frame(day = 26, var = "temp", AICc = AICc(temp26), 
                                 RMSE = rmse(temp26), r.squaredGLMM(temp26))
temp25.modelSelect <- data.frame(day = 25, var = "temp", AICc = AICc(temp25), 
                                 RMSE = rmse(temp25), r.squaredGLMM(temp25))
temp24.modelSelect <- data.frame(day = 24, var = "temp", AICc = AICc(temp24), 
                                 RMSE = rmse(temp24), r.squaredGLMM(temp24))
temp23.modelSelect <- data.frame(day = 23, var = "temp", AICc = AICc(temp23), 
                                 RMSE = rmse(temp23), r.squaredGLMM(temp23))
temp22.modelSelect <- data.frame(day = 22, var = "temp", AICc = AICc(temp22), 
                                 RMSE = rmse(temp22), r.squaredGLMM(temp22))
temp21.modelSelect <- data.frame(day = 21, var = "temp", AICc = AICc(temp21), 
                                 RMSE = rmse(temp21), r.squaredGLMM(temp21))
temp20.modelSelect <- data.frame(day = 20, var = "temp", AICc = AICc(temp20), 
                                 RMSE = rmse(temp20), r.squaredGLMM(temp20))
temp19.modelSelect <- data.frame(day = 19, var = "temp", AICc = AICc(temp19), 
                                 RMSE = rmse(temp19), r.squaredGLMM(temp19))
temp18.modelSelect <- data.frame(day = 18, var = "temp", AICc = AICc(temp18), 
                                 RMSE = rmse(temp18), r.squaredGLMM(temp18))
temp17.modelSelect <- data.frame(day = 17, var = "temp", AICc = AICc(temp17), 
                                 RMSE = rmse(temp17), r.squaredGLMM(temp17))
temp16.modelSelect <- data.frame(day = 16, var = "temp", AICc = AICc(temp16), 
                                 RMSE = rmse(temp16), r.squaredGLMM(temp16))
temp15.modelSelect <- data.frame(day = 15, var = "temp", AICc = AICc(temp15), 
                                 RMSE = rmse(temp15), r.squaredGLMM(temp15))
temp14.modelSelect <- data.frame(day = 14, var = "temp", AICc = AICc(temp14), 
                                 RMSE = rmse(temp14), r.squaredGLMM(temp14))
temp13.modelSelect <- data.frame(day = 13, var = "temp", AICc = AICc(temp13), 
                                 RMSE = rmse(temp13), r.squaredGLMM(temp13))
temp12.modelSelect <- data.frame(day = 12, var = "temp", AICc = AICc(temp12), 
                                 RMSE = rmse(temp12), r.squaredGLMM(temp12))
temp11.modelSelect <- data.frame(day = 11, var = "temp", AICc = AICc(temp11), 
                                 RMSE = rmse(temp11), r.squaredGLMM(temp11))
temp10.modelSelect <- data.frame(day = 10, var = "temp", AICc = AICc(temp10), 
                                 RMSE = rmse(temp10), r.squaredGLMM(temp10))
temp9.modelSelect <- data.frame(day = 9, var = "temp", AICc = AICc(temp9), 
                                RMSE = rmse(temp9), r.squaredGLMM(temp9))
temp8.modelSelect <- data.frame(day = 8, var = "temp", AICc = AICc(temp8), 
                                RMSE = rmse(temp8), r.squaredGLMM(temp8))
temp7.modelSelect <- data.frame(day = 7, var = "temp", AICc = AICc(temp7), 
                                RMSE = rmse(temp7), r.squaredGLMM(temp7))
temp6.modelSelect <- data.frame(day = 6, var = "temp", AICc = AICc(temp6), 
                                RMSE = rmse(temp6), r.squaredGLMM(temp6))
temp5.modelSelect <- data.frame(day = 5, var = "temp", AICc = AICc(temp5), 
                                RMSE = rmse(temp5), r.squaredGLMM(temp5))
temp4.modelSelect <- data.frame(day = 4, var = "temp", AICc = AICc(temp4), 
                                RMSE = rmse(temp4), r.squaredGLMM(temp4))
temp3.modelSelect <- data.frame(day = 3, var = "temp", AICc = AICc(temp3), 
                                RMSE = rmse(temp3), r.squaredGLMM(temp3))
temp2.modelSelect <- data.frame(day = 2, var = "temp", AICc = AICc(temp2), 
                                RMSE = rmse(temp2), r.squaredGLMM(temp2))
temp1.modelSelect <- data.frame(day = 1, var = "temp", AICc = AICc(temp1), 
                                RMSE = rmse(temp1), r.squaredGLMM(temp1))
mat.15yr.modelSelect <- data.frame(day = 5478, var = "temp", AICc = AICc(mat), 
                                   RMSE = rmse(mat), r.squaredGLMM(mat))

temp30.modelSelect %>% 
  full_join(mat.15yr.modelSelect) %>% full_join(temp29.modelSelect) %>% 
  full_join(temp28.modelSelect) %>% full_join(temp27.modelSelect) %>% 
  full_join(temp26.modelSelect) %>% full_join(temp25.modelSelect) %>%
  full_join(temp24.modelSelect) %>% full_join(temp23.modelSelect) %>% 
  full_join(temp22.modelSelect) %>% full_join(temp21.modelSelect) %>% 
  full_join(temp20.modelSelect) %>% full_join(temp19.modelSelect) %>%
  full_join(temp18.modelSelect) %>% full_join(temp17.modelSelect) %>% 
  full_join(temp16.modelSelect) %>% full_join(temp15.modelSelect) %>% 
  full_join(temp14.modelSelect) %>% full_join(temp13.modelSelect) %>%
  full_join(temp12.modelSelect) %>% full_join(temp11.modelSelect) %>% 
  full_join(temp10.modelSelect) %>% full_join(temp9.modelSelect) %>% 
  full_join(temp8.modelSelect) %>% full_join(temp7.modelSelect) %>%
  full_join(temp6.modelSelect) %>% full_join(temp5.modelSelect) %>% 
  full_join(temp4.modelSelect) %>% full_join(temp3.modelSelect) %>% 
  full_join(temp2.modelSelect) %>% full_join(temp1.modelSelect) %>%
  arrange(AICc)
## 20-day temperature is best model