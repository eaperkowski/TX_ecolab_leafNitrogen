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
df <- read.csv("../../data_sheets/TXeco_compiled_datasheet.csv") %>%
  filter(pft != "c3_shrub" & pft!= "c3_graminoid" & site != "Bell_2020_05" & 
           site != "Russel_2020_01")

###############################################################################
# Precipitation AICc for log(beta)
###############################################################################
wn90 <- lmer(log(beta) ~ wn90 + (1 | NCRS.code), data = df)
wn60 <- lmer(log(beta) ~ wn60 + (1 | NCRS.code), data = df)
wn30 <- lmer(log(beta) ~ wn30 + (1 | NCRS.code), data = df)
wn20 <- lmer(log(beta) ~ wn20 + (1 | NCRS.code), data = df)
wn15 <- lmer(log(beta) ~ wn15 + (1 | NCRS.code), data = df)
wn10 <- lmer(log(beta) ~ wn10 + (1 | NCRS.code), data = df)
wn9 <- lmer(log(beta) ~ wn9 + (1 | NCRS.code), data = df)
wn8 <- lmer(log(beta) ~ wn8 + (1 | NCRS.code), data = df)
wn7 <- lmer(log(beta) ~ wn7 + (1 | NCRS.code), data = df)
wn6 <- lmer(log(beta) ~ wn6 + (1 | NCRS.code), data = df)
wn5 <- lmer(log(beta) ~ wn5 + (1 | NCRS.code), data = df)
wn4 <- lmer(log(beta) ~ wn4 + (1 | NCRS.code), data = df)
wn3 <- lmer(log(beta) ~ wn3 + (1 | NCRS.code), data = df)
wn2 <- lmer(log(beta) ~ wn2 + (1 | NCRS.code), data = df)
wn1 <- lmer(log(beta) ~ wn1 + (1 | NCRS.code), data = df)
map <- lmer(log(beta) ~ map.15yr + (1 | NCRS.code), data = df)

# Model selection across timescales
wn90.modelSelect <- data.frame(day = 90, var = "wn", AICc = AICc(wn90), 
                                 RMSE = RMSE.merMod(wn90), r.squaredGLMM(wn90))
wn60.modelSelect <- data.frame(day = 60, var = "wn", AICc = AICc(wn60), 
                                 RMSE = RMSE.merMod(wn60), r.squaredGLMM(wn60))
wn30.modelSelect <- data.frame(day = 30, var = "wn", AICc = AICc(wn30), 
                          RMSE = RMSE.merMod(wn30), r.squaredGLMM(wn30))
wn20.modelSelect <- data.frame(day = 20, var = "wn", AICc = AICc(wn20), 
                          RMSE = RMSE.merMod(wn20), r.squaredGLMM(wn20))
wn15.modelSelect <- data.frame(day = 15, var = "wn", AICc = AICc(wn15), 
                          RMSE = RMSE.merMod(wn15), r.squaredGLMM(wn15))
wn10.modelSelect <- data.frame(day = 10, var = "wn", AICc = AICc(wn10), 
                          RMSE = RMSE.merMod(wn10), r.squaredGLMM(wn10))
wn9.modelSelect <- data.frame(day = 9, var = "wn", AICc = AICc(wn9), 
                         RMSE = RMSE.merMod(wn9), r.squaredGLMM(wn9))
wn8.modelSelect <- data.frame(day = 8, var = "wn", AICc = AICc(wn8), 
                         RMSE = RMSE.merMod(wn8), r.squaredGLMM(wn8))
wn7.modelSelect <- data.frame(day = 7, var = "wn", AICc = AICc(wn7), 
                         RMSE = RMSE.merMod(wn7), r.squaredGLMM(wn7))
wn6.modelSelect <- data.frame(day = 6, var = "wn", AICc = AICc(wn6), 
                         RMSE = RMSE.merMod(wn6), r.squaredGLMM(wn6))
wn5.modelSelect <- data.frame(day = 5, var = "wn", AICc = AICc(wn5), 
                         RMSE = RMSE.merMod(wn5), r.squaredGLMM(wn5))
wn4.modelSelect <- data.frame(day = 4, var = "wn", AICc = AICc(wn4), 
                         RMSE = RMSE.merMod(wn4), r.squaredGLMM(wn4))
wn3.modelSelect <- data.frame(day = 3, var = "wn", AICc = AICc(wn3), 
                         RMSE = RMSE.merMod(wn3), r.squaredGLMM(wn3))
wn2.modelSelect <- data.frame(day = 2, var = "wn", AICc = AICc(wn2), 
                         RMSE = RMSE.merMod(wn2), r.squaredGLMM(wn2))
wn1.modelSelect <- data.frame(day = 1, var = "wn", AICc = AICc(wn1), 
                         RMSE = RMSE.merMod(wn1), r.squaredGLMM(wn1))

aicc.results <- wn30.modelSelect %>% 
  full_join(wn60.modelSelect) %>% full_join(wn90.modelSelect) %>%
  full_join(wn20.modelSelect) %>% full_join(wn15.modelSelect) %>% 
  full_join(wn10.modelSelect) %>% full_join(wn9.modelSelect) %>% 
  full_join(wn8.modelSelect) %>% full_join(wn7.modelSelect) %>%
  full_join(wn6.modelSelect) %>% full_join(wn5.modelSelect) %>% 
  full_join(wn4.modelSelect) %>% full_join(wn3.modelSelect) %>% 
  full_join(wn2.modelSelect) %>% full_join(wn1.modelSelect) %>%
  mutate(concat.select = AICc + RMSE) %>%
  arrange(AICc)
## 3-day precipitation is best model

## A few plots to visualize timescales
ggplot(data = aicc.results, aes(x = day, y = AICc)) +
  geom_point() +
  geom_line()

ggplot(data = aicc.results, aes(x = day, y = RMSE)) +
  geom_point() +
  geom_line()

ggplot(data = aicc.results, aes(x = day, y = R2c)) +
  geom_point() +
  geom_line()

ggplot(data = aicc.results, aes(x = day, y = R2m)) +
  geom_point() +
  geom_line()

