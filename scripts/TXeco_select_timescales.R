###############################################################################
# Libraries
###############################################################################
library(dplyr)
library(car)
library(lme4)
library(MuMIn)
library(ggpubr)
library(merTools)

###############################################################################
# Load compiled data file
###############################################################################
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv")

###############################################################################
# Iterative models for soil moisture and beta
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

aicc.results.wn <- wn30.modelSelect %>% 
  full_join(wn60.modelSelect) %>% full_join(wn90.modelSelect) %>%
  full_join(wn20.modelSelect) %>% full_join(wn15.modelSelect) %>% 
  full_join(wn10.modelSelect) %>% full_join(wn9.modelSelect) %>% 
  full_join(wn8.modelSelect) %>% full_join(wn7.modelSelect) %>%
  full_join(wn6.modelSelect) %>% full_join(wn5.modelSelect) %>% 
  full_join(wn4.modelSelect) %>% full_join(wn3.modelSelect) %>% 
  full_join(wn2.modelSelect) %>% full_join(wn1.modelSelect) %>%
  mutate(concat.select = AICc + RMSE) %>%
  arrange(day) %>%
  dplyr::select(day, aicc.wn = AICc, rmse.wn = RMSE)
# 3-day soil moisture is best model

###############################################################################
# Iterative models for mean air temperature and chi
###############################################################################
temp90 <- lmer(chi ~ tavg90 + (1 | NCRS.code), data = df)
temp60 <- lmer(chi ~ tavg60 + (1 | NCRS.code), data = df)
temp30 <- lmer(chi ~ tavg30 + (1 | NCRS.code), data = df)
temp20 <- lmer(chi ~ tavg20 + (1 | NCRS.code), data = df)
temp15 <- lmer(chi ~ tavg15 + (1 | NCRS.code), data = df)
temp10 <- lmer(chi ~ tavg10 + (1 | NCRS.code), data = df)
temp9 <- lmer(chi ~ tavg9 + (1 | NCRS.code), data = df)
temp8 <- lmer(chi ~ tavg8 + (1 | NCRS.code), data = df)
temp7 <- lmer(chi ~ tavg7 + (1 | NCRS.code), data = df)
temp6 <- lmer(chi ~ tavg6 + (1 | NCRS.code), data = df)
temp5 <- lmer(chi ~ tavg5 + (1 | NCRS.code), data = df)
temp4 <- lmer(chi ~ tavg4 + (1 | NCRS.code), data = df)
temp3 <- lmer(chi ~ tavg3 + (1 | NCRS.code), data = df)
temp2 <- lmer(chi ~ tavg2 + (1 | NCRS.code), data = df)
temp1 <- lmer(chi ~ tavg1 + (1 | NCRS.code), data = df)

# Model selection across timescales
temp90.modelSelect <- data.frame(day = 90, var = "temp", AICc = AICc(temp90), 
                                 RMSE = RMSE.merMod(temp90), r.squaredGLMM(temp90))
temp60.modelSelect <- data.frame(day = 60, var = "temp", AICc = AICc(temp60), 
                                 RMSE = RMSE.merMod(temp60), r.squaredGLMM(temp60))
temp30.modelSelect <- data.frame(day = 30, var = "temp", AICc = AICc(temp30), 
                                 RMSE = RMSE.merMod(temp30), r.squaredGLMM(temp30))
temp20.modelSelect <- data.frame(day = 20, var = "temp", AICc = AICc(temp20), 
                                 RMSE = RMSE.merMod(temp20), r.squaredGLMM(temp20))
temp15.modelSelect <- data.frame(day = 15, var = "temp", AICc = AICc(temp15), 
                                 RMSE = RMSE.merMod(temp15), r.squaredGLMM(temp15))
temp10.modelSelect <- data.frame(day = 10, var = "temp", AICc = AICc(temp10), 
                                 RMSE = RMSE.merMod(temp10), r.squaredGLMM(temp10))
temp9.modelSelect <- data.frame(day = 9, var = "temp", AICc = AICc(temp9), 
                                RMSE = RMSE.merMod(temp9), r.squaredGLMM(temp9))
temp8.modelSelect <- data.frame(day = 8, var = "temp", AICc = AICc(temp8), 
                                RMSE = RMSE.merMod(temp8), r.squaredGLMM(temp8))
temp7.modelSelect <- data.frame(day = 7, var = "temp", AICc = AICc(temp7), 
                                RMSE = RMSE.merMod(temp7), r.squaredGLMM(temp7))
temp6.modelSelect <- data.frame(day = 6, var = "temp", AICc = AICc(temp6), 
                                RMSE = RMSE.merMod(temp6), r.squaredGLMM(temp6))
temp5.modelSelect <- data.frame(day = 5, var = "temp", AICc = AICc(temp5), 
                                RMSE = RMSE.merMod(temp5), r.squaredGLMM(temp5))
temp4.modelSelect <- data.frame(day = 4, var = "temp", AICc = AICc(temp4), 
                                RMSE = RMSE.merMod(temp4), r.squaredGLMM(temp4))
temp3.modelSelect <- data.frame(day = 3, var = "temp", AICc = AICc(temp3), 
                                RMSE = RMSE.merMod(temp3), r.squaredGLMM(temp3))
temp2.modelSelect <- data.frame(day = 2, var = "temp", AICc = AICc(temp2), 
                                RMSE = RMSE.merMod(temp2), r.squaredGLMM(temp2))
temp1.modelSelect <- data.frame(day = 1, var = "temp", AICc = AICc(temp1), 
                                RMSE = RMSE.merMod(temp1), r.squaredGLMM(temp1))

aicc.results.temp <- temp30.modelSelect %>%
  full_join(temp90.modelSelect) %>% full_join(temp60.modelSelect) %>%
  full_join(temp20.modelSelect) %>% full_join(temp15.modelSelect) %>% 
  full_join(temp10.modelSelect) %>% full_join(temp9.modelSelect) %>% 
  full_join(temp8.modelSelect) %>% full_join(temp7.modelSelect) %>%
  full_join(temp6.modelSelect) %>% full_join(temp5.modelSelect) %>% 
  full_join(temp4.modelSelect) %>% full_join(temp3.modelSelect) %>% 
  full_join(temp2.modelSelect) %>% full_join(temp1.modelSelect) %>%
  arrange(day) %>%
  dplyr::select(day, aicc.temp = AICc, rmse.temp = RMSE)
## 4-day temperature is best model

###############################################################################
# Iterative models for mean VPD and chi
###############################################################################
vpd90 <- lmer(chi ~ vpd90 + (1 | NCRS.code), data = df)
vpd60 <- lmer(chi ~ vpd60 + (1 | NCRS.code), data = df)
vpd30 <- lmer(chi ~ vpd30 + (1 | NCRS.code), data = df)
vpd20 <- lmer(chi ~ vpd20 + (1 | NCRS.code), data = df)
vpd15 <- lmer(chi ~ vpd15 + (1 | NCRS.code), data = df)
vpd10 <- lmer(chi ~ vpd10 + (1 | NCRS.code), data = df)
vpd9 <- lmer(chi ~ vpd9 + (1 | NCRS.code), data = df)
vpd8 <- lmer(chi ~ vpd8 + (1 | NCRS.code), data = df)
vpd7 <- lmer(chi ~ vpd7 + (1 | NCRS.code), data = df)
vpd6 <- lmer(chi ~ vpd6 + (1 | NCRS.code), data = df)
vpd5 <- lmer(chi ~ vpd5 + (1 | NCRS.code), data = df)
vpd4 <- lmer(chi ~ vpd4 + (1 | NCRS.code), data = df)
vpd3 <- lmer(chi ~ vpd3 + (1 | NCRS.code), data = df)
vpd2 <- lmer(chi ~ vpd2 + (1 | NCRS.code), data = df)
vpd1 <- lmer(chi ~ vpd1 + (1 | NCRS.code), data = df)

# Model selection across timescales
vpd90.modelSelect <- data.frame(day = 90, var = "vpd", AICc = AICc(vpd90), 
                                RMSE = RMSE.merMod(vpd90), r.squaredGLMM(vpd90))
vpd60.modelSelect <- data.frame(day = 60, var = "vpd", AICc = AICc(vpd60), 
                                RMSE = RMSE.merMod(vpd60), r.squaredGLMM(vpd60))
vpd30.modelSelect <- data.frame(day = 30, var = "vpd", AICc = AICc(vpd30), 
                                RMSE = RMSE.merMod(vpd30), r.squaredGLMM(vpd30))
vpd20.modelSelect <- data.frame(day = 20, var = "vpd", AICc = AICc(vpd20), 
                                RMSE = RMSE.merMod(vpd20), r.squaredGLMM(vpd20))
vpd15.modelSelect <- data.frame(day = 15, var = "vpd", AICc = AICc(vpd15), 
                                RMSE = RMSE.merMod(vpd15), r.squaredGLMM(vpd15))
vpd10.modelSelect <- data.frame(day = 10, var = "vpd", AICc = AICc(vpd10), 
                                RMSE = RMSE.merMod(vpd10), r.squaredGLMM(vpd10))
vpd9.modelSelect <- data.frame(day = 9, var = "vpd", AICc = AICc(vpd9), 
                               RMSE = RMSE.merMod(vpd9), r.squaredGLMM(vpd9))
vpd8.modelSelect <- data.frame(day = 8, var = "vpd", AICc = AICc(vpd8), 
                               RMSE = RMSE.merMod(vpd8), r.squaredGLMM(vpd8))
vpd7.modelSelect <- data.frame(day = 7, var = "vpd", AICc = AICc(vpd7), 
                               RMSE = RMSE.merMod(vpd7), r.squaredGLMM(vpd7))
vpd6.modelSelect <- data.frame(day = 6, var = "vpd", AICc = AICc(vpd6), 
                               RMSE = RMSE.merMod(vpd6), r.squaredGLMM(vpd6))
vpd5.modelSelect <- data.frame(day = 5, var = "vpd", AICc = AICc(vpd5), 
                               RMSE = RMSE.merMod(vpd5), r.squaredGLMM(vpd5))
vpd4.modelSelect <- data.frame(day = 4, var = "vpd", AICc = AICc(vpd4), 
                               RMSE = RMSE.merMod(vpd4), r.squaredGLMM(vpd4))
vpd3.modelSelect <- data.frame(day = 3, var = "vpd", AICc = AICc(vpd3), 
                               RMSE = RMSE.merMod(vpd3), r.squaredGLMM(vpd3))
vpd2.modelSelect <- data.frame(day = 2, var = "vpd", AICc = AICc(vpd2), 
                               RMSE = RMSE.merMod(vpd2), r.squaredGLMM(vpd2))
vpd1.modelSelect <- data.frame(day = 1, var = "vpd", AICc = AICc(vpd1), 
                               RMSE = RMSE.merMod(vpd1), r.squaredGLMM(vpd1))

aicc.results.vpd <- vpd30.modelSelect %>% 
  full_join(vpd90.modelSelect) %>% full_join(vpd60.modelSelect) %>% 
  full_join(vpd20.modelSelect) %>%  full_join(vpd15.modelSelect) %>% 
  full_join(vpd10.modelSelect) %>% full_join(vpd9.modelSelect) %>% 
  full_join(vpd8.modelSelect) %>% full_join(vpd7.modelSelect) %>%
  full_join(vpd6.modelSelect) %>% full_join(vpd5.modelSelect) %>% 
  full_join(vpd4.modelSelect) %>% full_join(vpd3.modelSelect) %>% 
  full_join(vpd2.modelSelect) %>% full_join(vpd1.modelSelect) %>%
  mutate(concat.select = AICc + RMSE) %>%
  arrange(day) %>%
  dplyr::select(day, aicc.vpd = AICc, rmse.vpd = RMSE)
## 4-day VPD is best model

###############################################################################
# Merge model selection results
###############################################################################
aicc.results <- aicc.results.wn %>%
  full_join(aicc.results.temp) %>%
  full_join(aicc.results.vpd) %>%
  mutate(aicc.wn = round(aicc.wn, digits = 2),
         rmse.wn = round(rmse.wn, digits = 4),
         aicc.temp = round(aicc.temp, digits = 2),
         rmse.temp = round(rmse.temp, digits = 4),
         aicc.vpd = round(aicc.vpd, digits = 2),
         rmse.vpd = round(rmse.vpd, digits = 4))

write.csv(aicc.results,
          "../data_sheets/TXeco_modelselection_results.csv",
          row.names = FALSE)

###############################################################################
# Create plots for AICc values across timescales
###############################################################################
wn.beta <- ggplot(data = aicc.results, aes(x = day, y = aicc.wn)) +
  geom_point() +
  geom_point(data = subset(aicc.results, day == 3), 
             fill = "red", size = 2, shape = 21) +
  geom_line() +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
  scale_y_continuous(limits = c(1300, 1330), breaks = seq(1300, 1330, 10)) +
  labs(x = NULL, y = expression(bold("AIC"["c"])),
       title = expression("Soil moisture (mm)")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

temp.chi <- ggplot(data = aicc.results, aes(x = day, y = aicc.temp)) +
  geom_point() +
  geom_point(data = subset(aicc.results, day == 4), 
             fill = "red", size = 2, shape = 21) +
  geom_line() +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
  scale_y_continuous(limits = c(-922, -910), breaks = seq(-922, -910, 4)) +
  labs(x = expression(bold("Days before site visit")), y = NULL,
       title = expression("Air temperature ("*degree*"C)")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

vpd.chi <- ggplot(data = aicc.results, aes(x = day, y = aicc.vpd)) +
  geom_point() +
  geom_point(data = subset(aicc.results, day == 4), 
             fill = "red", size = 2, shape = 21) +
  geom_line() +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
  scale_y_continuous(limits = c(-925, -909), breaks = seq(-925, -910, 5)) +
  labs(x = NULL, y = NULL,
       title = "VPD (kPa)") +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

png(filename = "../working_drafts/figs/TXeco_figS2_aicc_results.png",
    width = 12, height = 4, units = 'in', res = 600)
ggarrange(wn.beta, temp.chi, vpd.chi, ncol = 3, align = "hv")
dev.off()
