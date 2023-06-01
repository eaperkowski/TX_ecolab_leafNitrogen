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
df <- read.csv("../../TXeco/TXeco_data.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(pft != "c3_shrub") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb",
                             "c3_nonlegume", 
                             ifelse(pft == "c3_legume", 
                                    "c3_legume", 
                                    NA))),
         beta = ifelse(beta > 2000, NA, beta),
         beta = ifelse(pft == "c4_nonlegume" & beta > 400, NA, beta),
         beta = ifelse(pft == "c3_legume" & beta > 1000, NA, beta),
         marea = ifelse(marea > 1000, NA, marea))

###############################################################################
# Iterative models for soil moisture and beta
###############################################################################
wn90 <- lmer(sqrt(beta) ~ wn90_perc + (1 | NCRS.code), data = df)
wn60 <- lmer(sqrt(beta) ~ wn60_perc + (1 | NCRS.code), data = df)
wn30 <- lmer(sqrt(beta) ~ wn30_perc + (1 | NCRS.code), data = df)
wn20 <- lmer(sqrt(beta) ~ wn20_perc + (1 | NCRS.code), data = df)
wn15 <- lmer(sqrt(beta) ~ wn15_perc + (1 | NCRS.code), data = df)
wn10 <- lmer(sqrt(beta) ~ wn10_perc + (1 | NCRS.code), data = df)
wn9 <- lmer(sqrt(beta) ~ wn09_perc + (1 | NCRS.code), data = df)
wn8 <- lmer(sqrt(beta) ~ wn08_perc + (1 | NCRS.code), data = df)
wn7 <- lmer(sqrt(beta) ~ wn07_perc + (1 | NCRS.code), data = df)
wn6 <- lmer(sqrt(beta) ~ wn06_perc + (1 | NCRS.code), data = df)
wn5 <- lmer(sqrt(beta) ~ wn05_perc + (1 | NCRS.code), data = df)
wn4 <- lmer(sqrt(beta) ~ wn04_perc + (1 | NCRS.code), data = df)
wn3 <- lmer(sqrt(beta) ~ wn03_perc + (1 | NCRS.code), data = df)
wn2 <- lmer(sqrt(beta) ~ wn02_perc + (1 | NCRS.code), data = df)
wn1 <- lmer(sqrt(beta) ~ wn01_perc + (1 | NCRS.code), data = df)

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
  arrange(day) %>%
  dplyr::select(day, aicc.wn = AICc, rmse.wn = RMSE)
# 90-day soil moisture is best model

plot(aicc.results.wn$day, aicc.results.wn$aicc.wn)


###############################################################################
# Iterative models for mean VPD and chi
###############################################################################
vpd90 <- lmer(chi ~ vpd90 + (1 | NCRS.code), data = df)
vpd60 <- lmer(chi ~ vpd60 + (1 | NCRS.code), data = df)
vpd30 <- lmer(chi ~ vpd30 + (1 | NCRS.code), data = df)
vpd20 <- lmer(chi ~ vpd20 + (1 | NCRS.code), data = df)
vpd15 <- lmer(chi ~ vpd15 + (1 | NCRS.code), data = df)
vpd10 <- lmer(chi ~ vpd10 + (1 | NCRS.code), data = df)
vpd9 <- lmer(chi ~ vpd09 + (1 | NCRS.code), data = df)
vpd8 <- lmer(chi ~ vpd08 + (1 | NCRS.code), data = df)
vpd7 <- lmer(chi ~ vpd07 + (1 | NCRS.code), data = df)
vpd6 <- lmer(chi ~ vpd06 + (1 | NCRS.code), data = df)
vpd5 <- lmer(chi ~ vpd05 + (1 | NCRS.code), data = df)
vpd4 <- lmer(chi ~ vpd04 + (1 | NCRS.code), data = df)
vpd3 <- lmer(chi ~ vpd03 + (1 | NCRS.code), data = df)
vpd2 <- lmer(chi ~ vpd02 + (1 | NCRS.code), data = df)
vpd1 <- lmer(chi ~ vpd01 + (1 | NCRS.code), data = df)

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
## 90-day VPD is best model

plot(aicc.results.vpd$day, aicc.results.vpd$aicc.vpd)

###############################################################################
# Merge model selection results
###############################################################################
aicc.results <- aicc.results.wn %>%
  full_join(aicc.results.vpd) %>%
  mutate(aicc.wn = round(aicc.wn, digits = 2),
         rmse.wn = round(rmse.wn, digits = 4),
         aicc.vpd = round(aicc.vpd, digits = 2),
         rmse.vpd = round(rmse.vpd, digits = 4))

write.csv(aicc.results,
          "../working_drafts/tables/TXeco_TableS2_modelselection.csv",
          row.names = FALSE)

###############################################################################
# Create plots for AICc values across timescales
###############################################################################
wn.beta <- ggplot(data = aicc.results, aes(x = day, y = aicc.wn)) +
  geom_point() +
  geom_point(data = subset(aicc.results, day == 90), 
             fill = "red", shape = 21) +
  geom_line() +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
  scale_y_continuous(limits = c(2984, 2993), breaks = seq(2984, 2993, 3)) +
  labs(x = "Days prior to measurement", y = expression(bold("AIC"["c"])),
       title = expression(bold("Soil moisture (% WHC)"))) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(linewidth = 1.25))

vpd.chi <- ggplot(data = aicc.results, aes(x = day, y = aicc.vpd)) +
  geom_point() +
  geom_point(data = subset(aicc.results, day == 90), 
             fill = "red", size = 2, shape = 21) +
  geom_line() +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
  scale_y_continuous(limits = c(-884, -860), breaks = seq(-884, -860, 8)) +
  labs(x = "Days prior to measurement", y = NULL,
       title = "VPD (kPa)") +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(size = 1.25))

png(filename = "../working_drafts/figs/TXeco_figS2_aicc_results.png",
    width = 12, height = 4.5, units = 'in', res = 600)
ggarrange(wn.beta, vpd.chi, ncol = 2, align = "hv")
dev.off()
