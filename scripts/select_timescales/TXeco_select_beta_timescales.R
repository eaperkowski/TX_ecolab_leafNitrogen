###############################################################################
# Libraries
###############################################################################
library(dplyr)
library(car)
library(lme4)
library(MuMIn)
library(modelr)
library(merTools)

###############################################################################
# Load compiled data file
###############################################################################
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv") %>%
  filter(pft != "c3_shrub" & pft!= "c3_graminoid" & site != "Bell_2020_05" & 
           site != "Russel_2020_01")

###############################################################################
# Precipitation AICc for log(beta)
###############################################################################
prcp365 <- lmer(log(beta) ~ prcp365 + (1 | NCRS.code), data = df)
prcp90 <- lmer(log(beta) ~ prcp90 + (1 | NCRS.code), data = df)
prcp60 <- lmer(log(beta) ~ prcp60 + (1 | NCRS.code), data = df)
prcp30 <- lmer(log(beta) ~ prcp30 + (1 | NCRS.code), data = df)
prcp20 <- lmer(log(beta) ~ prcp20 + (1 | NCRS.code), data = df)
prcp15 <- lmer(log(beta) ~ prcp15 + (1 | NCRS.code), data = df)
prcp10 <- lmer(log(beta) ~ prcp10 + (1 | NCRS.code), data = df)
prcp9 <- lmer(log(beta) ~ prcp9 + (1 | NCRS.code), data = df)
prcp8 <- lmer(log(beta) ~ prcp8 + (1 | NCRS.code), data = df)
prcp7 <- lmer(log(beta) ~ prcp7 + (1 | NCRS.code), data = df)
prcp6 <- lmer(log(beta) ~ prcp6 + (1 | NCRS.code), data = df)
prcp5 <- lmer(log(beta) ~ prcp5 + (1 | NCRS.code), data = df)
prcp4 <- lmer(log(beta) ~ prcp4 + (1 | NCRS.code), data = df)
prcp3 <- lmer(log(beta) ~ prcp3 + (1 | NCRS.code), data = df)
prcp2 <- lmer(log(beta) ~ prcp2 + (1 | NCRS.code), data = df)
prcp1 <- lmer(log(beta) ~ prcp1 + (1 | NCRS.code), data = df)
map <- lmer(log(beta) ~ map.15yr + (1 | NCRS.code), data = df)

# Model selection across timescales
prcp90.modelSelect <- data.frame(day = 90, var = "prcp", AICc = AICc(prcp90), 
                                 RMSE = RMSE.merMod(prcp90), r.squaredGLMM(prcp90))
prcp60.modelSelect <- data.frame(day = 60, var = "prcp", AICc = AICc(prcp60), 
                                 RMSE = RMSE.merMod(prcp60), r.squaredGLMM(prcp60))
prcp30.modelSelect <- data.frame(day = 30, var = "prcp", AICc = AICc(prcp30), 
                          RMSE = RMSE.merMod(prcp30), r.squaredGLMM(prcp30))
prcp20.modelSelect <- data.frame(day = 20, var = "prcp", AICc = AICc(prcp20), 
                          RMSE = RMSE.merMod(prcp20), r.squaredGLMM(prcp20))
prcp15.modelSelect <- data.frame(day = 15, var = "prcp", AICc = AICc(prcp15), 
                          RMSE = RMSE.merMod(prcp15), r.squaredGLMM(prcp15))
prcp10.modelSelect <- data.frame(day = 10, var = "prcp", AICc = AICc(prcp10), 
                          RMSE = RMSE.merMod(prcp10), r.squaredGLMM(prcp10))
prcp9.modelSelect <- data.frame(day = 9, var = "prcp", AICc = AICc(prcp9), 
                         RMSE = RMSE.merMod(prcp9), r.squaredGLMM(prcp9))
prcp8.modelSelect <- data.frame(day = 8, var = "prcp", AICc = AICc(prcp8), 
                         RMSE = RMSE.merMod(prcp8), r.squaredGLMM(prcp8))
prcp7.modelSelect <- data.frame(day = 7, var = "prcp", AICc = AICc(prcp7), 
                         RMSE = RMSE.merMod(prcp7), r.squaredGLMM(prcp7))
prcp6.modelSelect <- data.frame(day = 6, var = "prcp", AICc = AICc(prcp6), 
                         RMSE = RMSE.merMod(prcp6), r.squaredGLMM(prcp6))
prcp5.modelSelect <- data.frame(day = 5, var = "prcp", AICc = AICc(prcp5), 
                         RMSE = RMSE.merMod(prcp5), r.squaredGLMM(prcp5))
prcp4.modelSelect <- data.frame(day = 4, var = "prcp", AICc = AICc(prcp4), 
                         RMSE = RMSE.merMod(prcp4), r.squaredGLMM(prcp4))
prcp3.modelSelect <- data.frame(day = 3, var = "prcp", AICc = AICc(prcp3), 
                         RMSE = RMSE.merMod(prcp3), r.squaredGLMM(prcp3))
prcp2.modelSelect <- data.frame(day = 2, var = "prcp", AICc = AICc(prcp2), 
                         RMSE = RMSE.merMod(prcp2), r.squaredGLMM(prcp2))
prcp1.modelSelect <- data.frame(day = 1, var = "prcp", AICc = AICc(prcp1), 
                         RMSE = RMSE.merMod(prcp1), r.squaredGLMM(prcp1))
map.15yr.modelSelect <- data.frame(day = 5478, var = "temp", AICc = AICc(map), 
                                   RMSE = RMSE.merMod(map), r.squaredGLMM(map))

prcp30.modelSelect %>% full_join(map.15yr.modelSelect) %>% 
  full_join(prcp60.modelSelect) %>% full_join(prcp90.modelSelect) %>%
  full_join(prcp20.modelSelect) %>% full_join(prcp15.modelSelect) %>% 
  full_join(prcp10.modelSelect) %>% full_join(prcp9.modelSelect) %>% 
  full_join(prcp8.modelSelect) %>% full_join(prcp7.modelSelect) %>%
  full_join(prcp6.modelSelect) %>% full_join(prcp5.modelSelect) %>% 
  full_join(prcp4.modelSelect) %>% full_join(prcp3.modelSelect) %>% 
  full_join(prcp2.modelSelect) %>% full_join(prcp1.modelSelect) %>%
  mutate(concat.select = AICc + RMSE) %>%
  arrange(AICc)
## 9-day precipitation is best model

