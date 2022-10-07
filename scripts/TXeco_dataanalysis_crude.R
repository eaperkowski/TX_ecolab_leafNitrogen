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

## Calculate relative soil moisture metrics (based on 150 mm bucket)
df$wn.30.rel <- df$wn.30 / 150
df$wn.60.rel <- df$wn.60 / 150
df$wn.90.rel <- df$wn.90 / 150
df$wn.15yr.rel <- df$wn.15yr / 150
df$wn.30yr.rel <- df$wn.30yr / 150

## Convert VPD from hPa (PRISM units) to kPa (standard)
df$vpd1 <- df$vpd1 / 10

plot(df$soil.cec, df$beta)
cor.test(df$soil.no3n, df$soil.)

ggplot(data = df, aes(x = soil.cec, y = soil.no3n)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 22)

ggplot(data = df, aes(x = soil.cec, y = soil.phos)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 22)

ggplot(data = df, aes(x = soil.cec, y = soil.potassium)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 22)

##########################################################################
## Beta
##########################################################################
df$pft <- factor(df$pft, levels = c("c3_legume", "c4_nonlegume", "c3_nonlegume"))
df$beta[c(62, 275, 315)] <- NA

beta <- lmer(log(beta) ~ wn.60 * soil.pH * pft + 
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

# Two-way interaction between map.15yr and pft
test(emtrends(beta, ~pft, "wn.60"))

# Individual effect of soil NO3-N
test(emtrends(beta, ~1, "soil.cec"))

# PFT-only effect
emmeans(beta, pairwise~pft)

# Three-way interaction between NO3-N, wn.60, and pft
test(emtrends(beta, ~wn.60*pft, "soil.no3n", at = list(wn.60 = c(50, 70, 100))))

##########################################################################
## Beta plots
##########################################################################
beta.no3n.pred <- data.frame(get_model_data(beta, 
                                            type = "pred", 
                                            terms = "soil.no3n"))
beta.no3n.int <- data.frame(get_model_data(beta, 
                                           type = "pred", 
                                           terms = c("soil.no3n", "pft")))
beta.h2o.pred <- data.frame(get_model_data(beta, type = "pred", 
                                           terms = c("wn.60")))
beta.h2o.int <- data.frame(get_model_data(beta, type = "pred", 
                                          terms = c("wn.60", "pft")))

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

png("../working_drafts/TXeco_beta_no3n_ind.png",
    width = 8, height = 5, units = 'in', res = 600)
beta.no3n.ind
dev.off()

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
                       aes(x = prcp365, y = log(beta))) +
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
  scale_x_continuous(limits = c(375, 1650), breaks = seq(400, 1600, 400)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Precipitation"[365]*" (mm)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.ind

beta.h2o.int <- ggplot(data = subset(df, !is.na(pft)), 
                          aes(x = prcp365, y = log(beta))) +
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
  scale_x_continuous(limits = c(375, 1650), breaks = seq(400, 1600, 400)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("365-day precipitation (mm)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.int

png("../working_drafts/TXeco_beta_h2o_ind.png",
    width = 8, height = 5, units = 'in', res = 600)
beta.h2o.ind
dev.off()

png("../working_drafts/TXeco_beta_h2o_int.png",
    width = 8, height = 5, units = 'in', res = 600)
beta.h2o.int
dev.off()

png("../working_drafts/TXeco_fig1_beta.png",
    width = 14, height = 6, units = 'in', res = 600)
ggarrange(beta.no3n.int, beta.h2o.int, ncol = 2, common.legend = TRUE,
          legend = "right", align = "hv")
dev.off()

##########################################################################
## Chi
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

df$chi[c(62, 114, 119, 315, 480)] <- NA
df$chi[c(117, 456)] <- NA
df$chi[c(292, 500)] <- NA

chi <- lmer(chi ~ (vpd7 + tavg7 + (wn.60 * soil.pH)) * pft + 
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

## Single factor effect of prcp365 on chi
test(emtrends(chi, ~1, "vpd7"))

## Single factor effect of prcp365 on chi
test(emtrends(chi, ~1, "tavg7"))



## Single factor effect of wn.60 on chi
test(emtrends(chi, ~1, "prcp7"))

## Effect of wn.60 on chi across species
test(emtrends(chi, ~pft, "wn.60"))
emmeans(chi, ~pft, "wn.60", at = list(wn.60=0))

## Single factor effect of soil.no3n on chi
test(emtrends(chi, ~1, "soil.no3n"))

## Single factor effect of beta on chi
test(emtrends(chi, ~1, "beta"))

## Single factor effect of soil.no3n on chi
cld(emmeans(chi, pairwise~pft))

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
## Narea
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

df$narea[df$narea > 5] <- NA
df$narea[c(509)] <- NA

narea <- lmer(log(narea) ~ prcp365 * soil.no3n * pft + chi + beta + (1 | NCRS.code),
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
test(emtrends(narea, ~pft, "soil.no3n"))

##########################################################################
## Narea plots
##########################################################################
narea.beta.pred <- as.data.frame(get_model_data(narea, type = "pred", 
                                                terms = "beta"))
narea.chi.pred <- data.frame(get_model_data(narea, type = "pred", 
                                            terms = "chi"))
narea.soiln.pred <- data.frame(get_model_data(narea, type = "pred", 
                                              terms = "soil.no3n"))

narea.beta.plot <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = log(beta), y = log(narea))) +
  geom_jitter(aes(fill = pft), width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = narea.beta.pred,
              aes(x = log(x), y = log(predicted), 
                  ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = narea.beta.pred, size = 1,
            aes(x = log(x), y = log(predicted))) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold(ln~beta)),
       y = expression(bold(ln)~"N"[area]),
       fill = "Funtional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.grid = element_blank())
narea.beta.plot

narea.chi.plot <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(narea))) +
  geom_jitter(aes(fill = pft), width = 0.01, size = 3, alpha = 0.7, shape = 21) +
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
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
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
## Nmass
##########################################################################
df$n.leaf[c(509)] <- NA

nmass <- lmer(log(n.leaf) ~ prcp365 * soil.no3n * pft + beta + chi + 
                (1 | NCRS.code), data = df)

# Check model assumptions
plot(nmass)
qqnorm(residuals(nmass))
qqline(residuals(nmass))
hist(residuals(nmass))
shapiro.test(residuals(nmass))
outlierTest(nmass)

# Model output
summary(nmass)
Anova(nmass)
r.squaredGLMM(nmass)

test(emtrends(nmass, ~1, "prcp365"))
test(emtrends(nmass, ~1, "soil.no3n"))

test(emtrends(nmass, ~wn.60, "soil.no3n", at = list(wn.60 = c(0, 40, 80, 120))))

##########################################################################
## Marea
##########################################################################
df$marea[c(20, 21)] <- NA

marea <- lmer(log(marea) ~ prcp365 * soil.no3n * pft + beta + chi + 
                (1 | NCRS.code), data = df)

# Check model assumptions
plot(marea)
qqnorm(residuals(marea))
qqline(residuals(marea))
hist(residuals(marea))
shapiro.test(residuals(marea))
outlierTest(marea)

# Model output
summary(marea)
Anova(marea)
r.squaredGLMM(marea)

test(emtrends(marea, ~1, "prcp365"))
test(emtrends(marea, ~1, "soil.no3n"))
test(emtrends(marea, ~1, "beta"))

test(emtrends(marea, ~wn.60, "soil.no3n", at = list(wn.60 = c(0, 40, 80, 120))))
