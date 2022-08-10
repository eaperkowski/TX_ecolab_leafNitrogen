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
           site != "Russel_2020_01")

## Add colorblind friendly palette
cbbPalette <- c("#0077BB", "#33BBEE", "#009988", "#EE7733", "#CC3311")
cbbPalette2 <- c("#CC3311", "#EE7733", "#009988", "#33BBEE", "#0077BB")
cbbPalette3 <- c("#DDAA33", "#BB5566", "#004488")

## Rename pfts by c3_legume, c3_nonlegume, c4_nonlegume
df$pft[df$pft == "c4_graminoid"] <- "c4_nonlegume"
df$pft[df$pft == "c3_graminoid" | df$pft == "c3_forb" | df$pft == "c3_shrub"] <- "c3_nonlegume"
df$pft[df$pft == "legume"] <- "c3_legume"

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

##########################################################################
## Beta
##########################################################################
df$beta[c(62, 275, 315)] <- NA
df$pft <- factor(df$pft, levels = c("c3_legume", "c4_nonlegume", "c3_nonlegume"))

## Check soil moisture timescale
beta.30 <- lmer(log(beta) ~ wn.30 * soil.no3n * pft + 
                  (1 | NCRS.code), data = df)
beta.60 <- lmer(log(beta) ~ wn.60 * soil.no3n * pft + 
                  (1 | NCRS.code), data = df)
beta.90 <- lmer(log(beta) ~ wn.90 * soil.no3n  * pft + 
                  (1 | NCRS.code), data = df)
beta.15yr <- lmer(log(beta) ~ wn.15yr * soil.no3n  * pft + 
                    (1 | NCRS.code), data = df)

## Model selection for relevant soil moisture timescale
AICc(beta.30, beta.60, beta.90, beta.15yr) %>% arrange(AICc)
## 60-day soil moisture estimates are the best model

# Check model assumptions
plot(beta.60)
qqnorm(residuals(beta.60))
qqline(residuals(beta.60))
densityPlot(residuals(beta.60))
shapiro.test(residuals(beta.60))
outlierTest(beta.60)

# Model output
summary(beta.60)
Anova(beta.60)
r.squaredGLMM(beta.60)

# Two-way interaction between wn.60 and pft
test(emtrends(beta.60, ~pft, "wn.60"))
emmeans(beta.60, ~pft, "wn.60", at = list(wn.60 = 0))

# Individual effect of soil NO3-N
test(emtrends(beta.60, ~1, "soil.no3n"))
emmeans(beta.60, ~1, "soil.no3n", at = list(soil.no3n = 0))

# Three-way interaction between wn.60, soil.no3n, and pft
test(emtrends(beta.60, ~pft*soil.no3n, "wn.60", 
              at = list(soil.no3n = c(0, 20, 40, 80))))
emmeans(beta.60, ~pft, "wn.60", at = list(wn.60 = 0))
## Negative effect of wn.60 on beta increases with soil NO3-N availability 
## (makes sense because soil NO3-N generally decreases beta)

test(emtrends(beta.60, ~wn.60*pft, "soil.no3n", 
              at = list(wn.60 = c(30, 60, 90))))
emmeans(beta.60, ~pft, "wn.60", at = list(wn.60 = 0))


# Individual effect of soil NO3-N
test(emtrends(beta.60, ~1, "soil.no3n"))
emmeans(beta.60, ~1, "soil.no3n", at = list(soil.no3n = 0))

test(emtrends(beta.60, ~pft*soil.no3n, "wn.60", at = list(soil.no3n = c(0, 20, 40, 80))))
emmeans(beta.60, ~pft, "wn.60", at = list(wn.60 = 0))

##########################################################################
## Beta plot
##########################################################################
beta.no3n.pred <- data.frame(get_model_data(beta.60, 
                                            type = "pred", 
                                            terms = "soil.no3n"))
beta.no3n.int <- data.frame(get_model_data(beta.60, 
                                           type = "pred", 
                                           terms = c("soil.no3n", "pft")))

beta.no3n.plot <- ggplot(data = subset(df, !is.na(pft)), 
                    aes(x = soil.no3n, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_hline(yintercept = 5, lty = 2, lwd = 1) +
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
beta.no3n.plot

png("../working_drafts/TXeco_beta_no3n.png",
    width = 8, height = 6, units = 'in', res = 600)
beta.no3n.plot
dev.off()

beta.h2o.ind <- data.frame(get_model_data(beta.60, type = "pred", 
                                          terms = c("wn.60")))
beta.h2o.int <- data.frame(get_model_data(beta.60, type = "pred", 
                                           terms = c("wn.60", "pft")))

beta.h2o.plot <- ggplot(data = subset(df, !is.na(photo)), 
                          aes(x = wn.60, y = log(beta))) +
  #geom_hline(yintercept = 5, lty = 2, lwd = 1) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = beta.h2o.ind, 
  #             aes(x = x, y = log(predicted), ymin = log(conf.low), 
  #                 ymax = log(conf.high)), alpha = 0.1) +
  # geom_line(data = subset(beta.h2o.ind), size = 1.5,
  #           aes(x = x, y = log(predicted)), lty = 2) +
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
  scale_x_continuous(limits = c(20, 120), breaks = seq(20, 120, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("60-day mean daily soil moisture (mm)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.plot

ggarrange(beta.h2o.plot, beta.no3n.plot, 
          ncol = 2, align = "hv", legend = "right", common.legend = TRUE)

png("../working_drafts/TXeco_beta.png",
    width = 13, height = 5, units = 'in', res = 600)
ggarrange(beta.h2o.plot, beta.no3n.plot, ncol = 2, align = "hv",
          legend = "right", common.legend = TRUE, 
          labels = "AUTO", font.label = list(size = 18))
dev.off()

##########################################################################
## Chi
##########################################################################
df$chi[c(62, 114, 119, 315, 317, 483, 492)] <- NA
df$chi[c(11, 247, 456, 481, 484)] <- NA
df$chi[c(84, 276, 293, 304, 308, 402, 482, 491, 37, 292, 502, 503, 284)] <- NA

chi <- lmer(chi ~ vpd1 + tavg13 + ((wn.30 + soil.no3n) * (n.fixer + photo)) + 
              beta + (1 | NCRS.code), data = df)

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

## Individual effect of vpd1 and tavg 13
test(emtrends(chi, ~1, "vpd1"))
emmeans(chi, ~1, "vpd1", at = list(vpd1=0))

test(emtrends(chi, ~1, "tavg13"))
emmeans(chi, ~1, "tavg13", at = list(tavg13=0))

test(emtrends(chi, ~n.fixer, "soil.no3n"))
emmeans(chi, ~n.fixer, "soil.no3n", at = list(soil.no3n=0))

## Two-way interaction between 30-day relative soil moisture and photopath
test(emtrends(chi, ~photo, "wn.30"))
emmeans(chi, ~photo, "wn.30", at = list(wn.30 = 0))

## Two-way interaction between soil N availability and photopath
test(emtrends(chi, ~photo, "soil.no3n"))

## Two-way interaction between soil N availability and n.fix pathway
test(emtrends(chi, ~n.fixer, "soil.no3n"))

##########################################################################
## Chi plots
##########################################################################
chi.water.plot <- ggplot(data = subset(df, !is.na(photo)), 
                                           aes(x = wn.30, y = chi)) +
  geom_jitter(aes(fill = photo),width = 0.5, size = 3, alpha = 0.3, shape = 21) +
  stat_function(fun = function(x) 0.00045*x + 0.761, lwd = 2, lty = 2,
                xlim = c(25, 125), color = cbbPalette2[2]) + #c3
  stat_function(fun = function(x) -0.0021*x + 0.747, lwd = 2,
                xlim = c(25, 125), color = cbbPalette2[5]) + #c4
  scale_fill_manual(values = c(cbbPalette2[c(2,5)]), labels = c(expression("C"[3]),
                                                                expression("C"[4]))) +
  scale_x_continuous(limits = c(25, 130), breaks = seq(25, 130, 35)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("30-day soil moisture (mm day"^-1~")")),
       y = expression(bold(chi)),
       fill = "Photo. pathway") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)

chi.soiln.plot <- ggplot(data = subset(df, !is.na(n.fixer)), 
                         aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = n.fixer), width = 0.5, size = 3, alpha = 0.3, shape = 21) +
  stat_function(fun = function(x) -0.00125*x + 0.720, lwd = 2,
                xlim = c(0, 80), color = cbbPalette2[2]) + # Nfixer
  stat_function(fun = function(x) -0.00069*x + 0.713, lwd = 2,
                xlim = c(0, 80), color = cbbPalette2[5]) + # Non-fixer
  scale_fill_manual(values = c(cbbPalette2[c(2,5)]), 
                    labels = c(expression("N"[fixer]),
                               expression("Non-N"[fixer]))) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(chi)),
       fill = "N-fixing capability") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)

png("../working_drafts/TXeco_chi.png",
    width = 12, height = 5, units = 'in', res = 600)
ggarrange(chi.water.plot, chi.soiln.plot, ncol = 2, align = "hv",
          legend = "bottom", labels = "AUTO", font.label = list(size = 18))
dev.off()


##########################################################################
## Nmass
##########################################################################
df$narea[c(20, 21, 227, 228, 400, 509)] <- NA

narea <- lmer(log(narea) ~ ((wn.30 * soil.no3n) * (n.fixer + photo)) + chi + 
                log(beta) + marea + n.leaf + (1 | NCRS.code),
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

# Marginal interaction between wn.30 and photo
test(emtrends(narea, ~1, "beta"))
emmeans(narea, ~1, "beta", at = list(beta=0))





##########################################################################
## Narea plots
##########################################################################
chi.water.plot <- ggplot(data = subset(df, !is.na(photo)), 
                         aes(x = log(beta), y = log(narea))) +
  geom_jitter(aes(fill = photo),width = 0.5, size = 3, alpha = 0.3, shape = 21) +
  stat_function(fun = function(x) 0.0076*x + 0.364, lwd = 2,
                xlim = c(25, 125), color = cbbPalette2[2]) + #c3
  stat_function(fun = function(x) 0.00235*x + 0.198, lwd = 2,
                xlim = c(25, 125), color = cbbPalette2[5], lty = 2) + #c4
  scale_fill_manual(values = c(cbbPalette2[c(2,5)]), labels = c(expression("C"[3]),
                                                                expression("C"[4]))) +
  scale_x_continuous(limits = c(25, 130), breaks = seq(25, 130, 35)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("30-day soil moisture (mm day"^-1~")")),
       y = expression(bold(ln~"(N"[mass]~")")),
       fill = "Photo. pathway") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)

chi.soilN.plot <- ggplot(data = subset(df, !is.na(photo)), 
                         aes(x = wn.30, y = log(n.leaf))) +
  geom_jitter(aes(fill = photo),width = 0.5, size = 3, alpha = 0.3, shape = 21) +
  stat_function(fun = function(x) 0.0076*x + 0.364, lwd = 2,
                xlim = c(25, 125), color = cbbPalette2[2]) + #c3
  stat_function(fun = function(x) 0.00235*x + 0.198, lwd = 2,
                xlim = c(25, 125), color = cbbPalette2[5], lty = 2) + #c4
  scale_fill_manual(values = c(cbbPalette2[c(2,5)]), labels = c(expression("C"[3]),
                                                                expression("C"[4]))) +
  scale_x_continuous(limits = c(25, 130), breaks = seq(25, 130, 35)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("30-day soil moisture (mm day"^-1~")")),
       y = expression(bold(ln~"(N"[mass]~")")),
       fill = "Photo. pathway") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)








narea.plot <- ggplot(data = df, aes(x = beta, y = narea)) +
  geom_vline(xintercept = 146, lty = 2, lwd = 1) +
  geom_jitter(aes(fill = pft),
              width = 0.02, size = 3, alpha = 0.3, shape = 21) +
  stat_function(fun = function(x) exp(-0.00158*x + 0.798), lwd = 2,
                xlim = c(0, 600), color = cbbPalette2[1]) +
  stat_function(fun = function(x) exp(-0.00140*x + 0.093), lwd = 2, lty = 2,
                xlim = c(0, 600), color = cbbPalette2[2]) +
  stat_function(fun = function(x) exp(-0.00124*x + 1.295), lwd = 2, lty = 2,
                xlim = c(0, 600), color = cbbPalette2[3]) +
  scale_fill_manual(values = cbbPalette2, labels = c(expression("C"[3]~"forb"),
                                                     expression("C"[4]~"graminoid"),
                                                     expression("C"[3]~"legume"))) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7.5, 2.5)) +
  labs(x = expression(beta),
       y = expression("Leaf N"[area]~"(gN m"^-2~")"),
       fill = "Plant functional type") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)


png("../working_drafts/TXeco_chi_b.png",
    width = 8, height = 4, units = 'in', res = 600)
chi.plot
dev.off()

##########################################################################
## Narea
##########################################################################
df$n.leaf[c(344, 462)] <- NA


nmass <- lmer(log(n.leaf) ~ ai.60 * log(beta) * soil.no3n * pft + 
                (1 | sampling.year) + (1 | NCRS.code),
              data = df)

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

