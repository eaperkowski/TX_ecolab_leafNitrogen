##########################################################################
## Load libraries and import data
##########################################################################
# Libraries
library(lme4)
library(emmeans)
library(car)
library(tidyverse)
library(MuMIn)
library(multcomp)
library(multcompView)
library(ggpubr)
library(sjPlot)
library(lavaan)

# Turn off digit rounding in emmean args
emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(site != "Bell_2020_05" & 
           site != "Russel_2020_01") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb" | pft == "c3_shrub",
                             "c3_nonlegume", 
                             ifelse(pft == "legume", 
                                    "c3_legume", 
                                    NA))),
         chi = ifelse(chi > 0.95 | chi < 0.20, NA, chi))

## Add colorblind friendly palette
cbbPalette <- c("#0077BB", "#33BBEE", "#009988", "#EE7733", "#CC3311")
cbbPalette2 <- c("#CC3311", "#EE7733", "#009988", "#33BBEE", "#0077BB")
cbbPalette3 <- c("#DDAA33", "#BB5566", "#004488")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_legume"])
length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c4_nonlegume"])

## Convert VPD from hPa (PRISM units) to kPa (standard)
df$vpd1 <- df$vpd1 / 10

##########################################################################
## Beta
##########################################################################
df$pft <- factor(df$pft, levels = c("c3_legume", "c4_nonlegume", "c3_nonlegume"))
df$beta[c(84)] <- NA

beta <- lmer(log(beta) ~ wn3 * soil.no3n * pft + 
               (1 | NCRS.code), data = df)

# Check model assumptions
plot(beta)
qqnorm(residuals(beta))
qqline(residuals(beta))
densityPlot(residuals(beta))
shapiro.test(residuals(beta))
outlierTest(beta)

# Model output
summary(beta)
Anova(beta)
r.squaredGLMM(beta)


# Two-way interaction between wn3 and pft
test(emtrends(beta, ~pft, "wn3"))

# Individual effect of wn3 
test(emtrends(beta, ~1, "wn3"))

exp(-0.00689)

# Individual effect of soil NO3-N
test(emtrends(beta, ~1, "soil.no3n"))

# PFT-only effect
emmeans(beta, pairwise~pft)

beta.coefs <- round(summary(beta)$coefficients, digits = 4)

table1 <- data.frame(Anova(beta)) %>%

  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, 
                          "<0.001", 
                          round(Pr..Chisq., 3))) %>%
  dplyr::select(treatment, df = Df, χ = Chisq, P_value)

table1$treatment <- c("Soil moisture (SM)",
                      "Soil NO3-N (N)",
                      "PFT",
                      "SM * N",
                      "SM * PFT",
                      "N * PFT",
                      "SM * N * PFT")

write.csv(table1, "../working_drafts/tables/TXeco_table1_beta.csv", row.names = FALSE)

##########################################################################
## Beta plots
##########################################################################
beta.no3n.pred <- data.frame(get_model_data(beta, 
                                            type = "pred", 
                                            terms = "soil.no3n"))
beta.no3n.int <- data.frame(get_model_data(beta, 
                                           type = "int", 
                                           terms = c("soil.no3n", "pft")))
beta.h2o.pred <- data.frame(get_model_data(beta, type = "pred", 
                                           terms = c("wn3")))
beta.h2o.int <- data.frame(get_model_data(beta, type = "int", 
                                          terms = c("wn3", "pft")))

set.seed(5)
beta.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                    aes(x = soil.no3n, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = beta.no3n.pred, 
              aes(x = x, y = predicted, ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = beta.no3n.pred, size = 1.5,
            aes(x = x, y = log(predicted))) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.no3n.ind

beta.no3n.int <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(beta.no3n.int, group == "c3_legume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[1]) +
  geom_line(data = subset(beta.no3n.int, group == "c3_legume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[1]) +
  geom_ribbon(data = subset(beta.no3n.int, group == "c4_nonlegume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[2]) +
  geom_line(data = subset(beta.no3n.int, group == "c4_nonlegume"),
            aes(x = x, y = log(predicted)), 
            color = cbbPalette3[2], size = 1, lty = 2) +
  geom_ribbon(data = subset(beta.no3n.int, group == "c3_nonlegume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_line(data = subset(beta.no3n.int, group == "c3_nonlegume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[3]) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.no3n.int 

beta.h2o.ind <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn3, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 2, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = beta.h2o.pred, 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = beta.h2o.pred, size = 1.5,
            aes(x = x, y = log(predicted)), lty = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Precipitation"[365]*" (mm)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 22) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.ind

beta.h2o.int <- ggplot(data = subset(df, !is.na(pft)), 
                          aes(x = wn3, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(beta.h2o.int, group == "c3_legume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[1]) +
  geom_line(data = subset(beta.h2o.int, group == "c3_legume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[1], lty = 2) +
  geom_ribbon(data = subset(beta.h2o.int, group == "c4_nonlegume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[2]) +
  geom_line(data = subset(beta.h2o.int, group == "c4_nonlegume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[2]) +
  geom_ribbon(data = subset(beta.h2o.int, group == "c3_nonlegume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_line(data = subset(beta.h2o.int, group == "c3_nonlegume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[3], lty = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil moisture (mm)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.int

png("../working_drafts/figs/TXeco_fig2_beta.png",
    width = 12, height = 4, units = 'in', res = 600)
ggarrange(beta.h2o.int, beta.no3n.ind, ncol = 2, common.legend = TRUE,
          legend = "right", align = "hv", labels = "AUTO",
          font.label = list(size = 18))
dev.off()

##########################################################################
## Chi
##########################################################################
df$chi[c(62, 117, 315, 317, 481)] <- NA
df$chi[c(456, 483)] <- NA
df$chi[c(284, 292, 484)] <- NA

chi <- lmer(chi ~ (vpd4 + tavg4 + (wn3 * soil.no3n)) * pft + 
              (1 | NCRS.code), data = df)

# Check model assumptions
plot(chi)
qqnorm(residuals(chi))
qqline(residuals(chi))
densityPlot(residuals(chi))
shapiro.test(residuals(chi))
outlierTest(chi)

# Model output
summary(chi)
Anova(chi)
r.squaredGLMM(chi)

## Single factor effect of 3-day soil moisture on chi
test(emtrends(chi, ~pft, "wn3"))

## Single factor effect of 4 day temperature on chi
test(emtrends(chi, ~pft, "tavg4"))

## Single factor effect of 4 day VPD on chi
test(emtrends(chi, ~pft, "vpd4"))

## Single factor effect of soil.no3n on chi
test(emtrends(chi, ~pft, "soil.no3n"))

## Single factor effect of beta on chi
test(emtrends(chi, ~1, "beta"))

## Single factor effect of soil.no3n on chi
cld(emmeans(chi, pairwise~pft))

table2 <- data.frame(Anova(chi)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, 
                          "<0.001", 
                          round(Pr..Chisq., 3))) %>%
  dplyr::select(treatment, df = Df, χ = Chisq, P_value)

table2$treatment <- c("VPD",
                      "Temperature (T)",
                      "Soil moisture (SM)",
                      "Soil NO3-N (N)",
                      "PFT",
                      "SM * N",
                      "VPD * PFT",
                      "T * PFT",
                      "SM * PFT",
                      "N * PFT",
                      "SM * N * PFT")

write.csv(table2, "../working_drafts/tables/TXeco_table2_chi.csv", row.names = FALSE)

##########################################################################
## Chi plots
##########################################################################
chi.no3n.pred <- data.frame(get_model_data(chi, 
                                           type = "pred", 
                                           terms = "soil.no3n"))
chi.no3n.inter <- data.frame(get_model_data(chi, 
                                            type = "pred", 
                                            terms = c("soil.no3n", "pft")))
chi.h2o.pred <- data.frame(get_model_data(chi, type = "pred", 
                                          terms = c("wn.60")))
chi.h2o.inter <- data.frame(get_model_data(chi, type = "pred", 
                                           terms = c("wn.60", "pft")))

set.seed(5)
chi.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                      aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = pft),width = 0.7, size = 3, 
              alpha = 0.7, shape = 21) +
  geom_ribbon(data = chi.no3n.pred, 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25) +
  geom_line(data = chi.no3n.pred, size = 1.5,
            aes(x = x, y = predicted)) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid = element_blank())
chi.no3n.ind

chi.no3n.int <- ggplot(data = subset(df, !is.na(pft)), 
                      aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.7, size = 3, alpha = 0.7, shape = 21) +
  
  geom_ribbon(data = subset(chi.no3n.inter, group == "c3_legume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[1]) +
  geom_line(data = subset(chi.no3n.inter, group == "c3_legume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[1]) +
  geom_ribbon(data = subset(chi.no3n.inter, group == "c4_nonlegume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[2]) +
  geom_line(data = subset(chi.no3n.inter, group == "c4_nonlegume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[2]) +
  geom_ribbon(data = subset(chi.no3n.inter, group == "c3_nonlegume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_line(data = subset(chi.no3n.inter, group == "c3_nonlegume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[3]) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid = element_blank())
chi.no3n.int  

chi.h2o.ind <- ggplot(data = subset(df, !is.na(pft)), 
                      aes(x = wn.60, y = chi)) +
  geom_jitter(aes(fill = pft),width = 0.7, size = 3, 
              alpha = 0.7, shape = 21) +
  geom_ribbon(data = chi.h2o.pred, 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25) +
  geom_line(data = chi.h2o.pred, size = 1.5,
            aes(x = x, y = predicted)) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(20, 120), breaks = seq(20, 120, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("60-day mean daily soil moisture (mm)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid = element_blank())
chi.h2o.ind

chi.h2o.int <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn.60, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.7, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(chi.h2o.inter, group == "c3_legume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[1]) +
  geom_line(data = subset(chi.h2o.inter, group == "c3_legume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[1]) +
  geom_ribbon(data = subset(chi.h2o.inter, group == "c4_nonlegume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[2]) +
  geom_line(data = subset(chi.h2o.inter, group == "c4_nonlegume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[2]) +
  geom_ribbon(data = subset(chi.h2o.inter, group == "c3_nonlegume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_line(data = subset(chi.h2o.inter, group == "c3_nonlegume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[3]) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(20, 120), breaks = seq(20, 120, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("60-day mean daily soil moisture (mm)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid = element_blank())
chi.h2o.int

png("../working_drafts/TXeco_chi_h2o_int.png",
    width = 8, height = 5, units = 'in', res = 600)
chi.h2o.int
dev.off()

png("../working_drafts/TXeco_chi_h2o_ind.png",
    width = 8, height = 5, units = 'in', res = 600)
chi.h2o.ind
dev.off()

png("../working_drafts/TXeco_chi_no3n_int.png",
    width = 8, height = 5, units = 'in', res = 600)
chi.no3n.int
dev.off()

png("../working_drafts/TXeco_chi_no3n_ind.png",
    width = 8, height = 5, units = 'in', res = 600)
chi.no3n.ind
dev.off()

##########################################################################
## Chi w/ beta
##########################################################################
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(site != "Bell_2020_05" & 
           site != "Russel_2020_01") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb" | pft == "c3_shrub",
                             "c3_nonlegume", 
                             ifelse(pft == "legume", 
                                    "c3_legume", 
                                    NA))),
         chi = ifelse(chi > 0.95 | chi < 0.20, NA, chi))
df$pft <- factor(df$pft, levels = c("c3_legume", "c4_nonlegume", "c3_nonlegume"))

df$chi[c(62, 84, 86, 308, 315, 317)] <- NA
df$chi[c(304, 456, 483)] <- NA
df$chi[c(402, 481, 484)] <- NA

chi.beta <- lmer(chi ~ (vpd4 + tavg4 + beta) * pft + 
              (1 | NCRS.code), data = df)

# Check model assumptions
plot(chi.beta)
qqnorm(residuals(chi.beta))
qqline(residuals(chi.beta))
densityPlot(residuals(chi.beta))
shapiro.test(residuals(chi.beta))
outlierTest(chi.beta)

# Model output
summary(chi.beta)
Anova(chi.beta)
r.squaredGLMM(chi.beta)

# Two way interaction between vpd and pft
test(emtrends(chi.beta, ~pft, "vpd4"))

# Two way interaction between vpd and pft
test(emtrends(chi.beta, ~pft, "tavg4"))

# Two way interaction between vpd and pft
test(emtrends(chi.beta, pairwise~pft, "beta"))


table3 <- data.frame(Anova(chi.beta)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, 
                          "<0.001", 
                          round(Pr..Chisq., 3))) %>%
  dplyr::select(treatment, df = Df, Chisq, P_value)

table3$treatment <- c("VPD",
                      "Temperature (T)",
                      "beta",
                      "PFT",
                      "VPD * PFT",
                      "T * PFT",
                      "Beta * PFT")

write.csv(table3, "../working_drafts/tables/TXeco_table3b_chibeta.csv", row.names = FALSE)

##########################################################################
## Narea - direct effects
##########################################################################
df$narea[df$narea > 10] <- NA
df$narea[c(76, 80, 156, 273, 382)] <- NA
df$narea[509] <- NA

narea <- lmer(log(narea) ~ (beta + chi + soil.no3n) * pft + (1 | NCRS.code),
              data = df)

# Check model assumptions
plot(narea)
qqnorm(residuals(narea))
qqline(residuals(narea))
hist(residuals(narea))
densityPlot(residuals(narea))
shapiro.test(residuals(narea))
outlierTest(narea)

# Model output
summary(narea)
Anova(narea)
r.squaredGLMM(narea)

## Individual beta effect
test(emtrends(narea, ~1, "beta"))
emmeans(narea, ~1, at = list(beta = 0))

## Individual chi effect
test(emtrends(narea, ~1, "chi"))
emmeans(narea, ~1, at = list(beta = 0))

## Individual beta effect
test(emtrends(narea, ~pft, "beta"))
emmeans(narea, ~1, at = list(beta = 0))

## Individual soil N effect
test(emtrends(narea, ~1, "soil.no3n"))

table4 <- data.frame(Anova(narea)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, 
                          "<0.001", 
                          round(Pr..Chisq., 3))) %>%
  dplyr::select(treatment, df = Df, Chisq, P_value)

table4$treatment <- c("Unit cost ratio (beta)",
                      "chi",
                      "Soil NO3-N (N)",
                      "PFT",
                      "beta * PFT",
                      "chi * PFT",
                      "N * PFT")

write.csv(table4, "../working_drafts/tables/TXeco_table4_leafN.csv", 
          row.names = FALSE)

##########################################################################
## Narea plots
##########################################################################
narea.beta.pred <- as.data.frame(get_model_data(narea, type = "int", 
                                                terms = c("beta", "pft")))
narea.chi.pred <- data.frame(get_model_data(narea, type = "pred", 
                                            terms = "chi"))
narea.soiln.pred <- data.frame(get_model_data(narea, type = "pred", 
                                              terms = "soil.no3n"))

narea.beta.plot <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = beta, y = log(narea))) +
  geom_jitter(aes(fill = pft), width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = narea.beta.pred,
              aes(x = x, y = log(predicted), 
                  ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = narea.beta.pred, size = 1,
            aes(x = x, y = log(predicted))) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(-1.5, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold(beta)),
       y = expression(bold(ln)~"N"[area]),
       fill = "Funtional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)
narea.beta.plot

narea.chi.plot <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = narea)) +
  geom_jitter(aes(fill = pft), width = 0.01, size = 3, 
              alpha = 0.7, shape = 21) +
  geom_ribbon(data = narea.chi.pred,
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = narea.chi.pred, size = 1,
            aes(x = x, y = log(predicted)), lty = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-1.5, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold(chi)),
       y = expression(bold(ln)~"N"[area]),
       fill = "Functional type") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.grid = element_blank())
narea.chi.plot

narea.soilno3n.plot <- ggplot(data = subset(df, !is.na(pft)), 
                                  aes(x = soil.no3n, y = log(narea))) +
  geom_jitter(aes(fill = pft), width = 0.01, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = narea.soiln.pred,
              aes(x = x, y = log(predicted), ymin = log(conf.low),
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = narea.soiln.pred, 
            size = 1, aes(x = x, y = log(predicted)), lty = 2) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(ln)~"N"[area]),
       fill = "Functional type") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.grid = element_blank())

png("../working_drafts/TXeco_narea_beta.png",
    width = 8, height = 5, units = 'in', res = 600)
narea.beta.plot
dev.off()

png("../working_drafts/TXeco_narea_chi.png",
    width = 8, height = 5, units = 'in', res = 600)
narea.chi.plot
dev.off()

png("../working_drafts/TXeco_narea_soiln.png",
    width = 8, height = 5, units = 'in', res = 600)
narea.soilno3n.plot
dev.off()

png("../working_drafts/TXeco_narea_soiln_int.png",
    width = 8, height = 5, units = 'in', res = 600)
narea.soilno3n.int.plot
dev.off()

##########################################################################
## Structural equation model
##########################################################################
library(tidySEM)

df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(site != "Bell_2020_05" & 
           site != "Russel_2020_01") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb" | pft == "c3_shrub",
                             "c3_nonlegume", 
                             ifelse(pft == "legume", 
                                    "c3_legume", 
                                    NA))),
         chi = ifelse(chi > 0.95 | chi < 0.20, NA, chi))

df$narea[df$narea > 10] <- NA
df$narea[c(76, 80, 156, 273, 382, 509)] <- NA
df$chi[c(62, 84, 86, 308, 315, 317, 304, 456, 483,
         402, 481, 484)] <- NA
df$beta[c(84)] <- NA

## Fxn to standardize things
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return( z)
  }


models <- ' # regressions
            narea ~ (beta + chi + soil.no3n) * pft
            beta ~ (wn3 + soil.no3n) * pft
            chi ~ (vpd4 + tavg4) * pft
            chi ~~ beta
            vpd4 ~~ tavg4
            wn3 ~ pft
            soil.no3n ~ pft'

test_fit <- sem(models, data = df)
lavaan::summary(test_fit, fit.measures = TRUE)

summary.coefs <- summary(test_fit)$pe[c(1:14),]
summary.coefs$linesize <- abs(summary.coefs$z)
summary.coefs$linesize_std <- scale(summary.coefs$linesize) * 2 + 4

summary.coefs[,5:10] <- round(summary.coefs[,5:10], digits = 3)

write.csv(summary.coefs, "../working_drafts/tables/TXeco_SEM_results.csv", row.names = FALSE)
