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

# Turn off digit rounding in emmean args
emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(pft != "c3_shrub" & site != "Bell_2020_05" & 
           site != "Russel_2020_01")

## Add colorblind friendly palette
cbbPalette <- c("#0077BB", "#33BBEE", "#009988", "#EE7733", "#CC3311")
cbbPalette2 <- c("#CC3311", "#EE7733", "#009988", "#33BBEE", "#0077BB")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_graminoid"])
length(df$pft[df$pft == "c4_graminoid"])
length(df$pft[df$pft == "legume"])
length(df$pft[df$pft == "c3_forb"])

## Calculate relative soil moisture metrics (based on 150 mm bucket)
df$wn.30.rel <- df$wn.30 / 150
df$wn.60.rel <- df$wn.60 / 150
df$wn.90.rel <- df$wn.90 / 150
df$wn.15yr.rel <- df$wn.15yr / 150
df$wn.30yr.rel <- df$wn.30yr / 150

##########################################################################
## Beta
##########################################################################
df$beta[260] <- NA

## Check soil moisture timescale
beta.30 <- lmer(log(beta) ~ (wn.30.rel + soil.no3n) * (n.fixer + photo) + 
                  (1 | NCRS.code), data = df)
beta.60 <- lmer(log(beta) ~ (wn.60.rel + soil.no3n) * (n.fixer + photo) + 
                  (1 | NCRS.code), data = df)
beta.90 <- lmer(log(beta) ~ (wn.90.rel + soil.no3n) * (n.fixer + photo) + 
                  (1 | NCRS.code), data = df)
beta.15yr <- lmer(log(beta) ~ (wn.15yr.rel + soil.no3n) * (n.fixer + photo) + 
                    (1 | NCRS.code), data = df)

## Model selection for relevant soil moisture timescale
AICc(beta.30, beta.60, beta.90, beta.15yr) %>% arrange(AICc)
## 30-day soil moisture estimates are the best model

# Check model assumptions
plot(beta.30)
qqnorm(residuals(beta.30))
qqline(residuals(beta.30))
densityPlot(residuals(beta.30))
shapiro.test(residuals(beta.30))
outlierTest(beta.30)

# Model output
summary(beta.30)
Anova(beta.30)
r.squaredGLMM(beta.30)

# Two-way interaction between wn.30.rel and photo
test(emtrends(beta.30, ~photo, "wn.30"))
emmeans(beta.30, ~photo, "wn.30.rel", at = list(wn.30.rel = 0))

# Two-way interaction between soil.no3n and photo
test(emtrends(beta.30, ~photo, "soil.no3n"))

# Two-way interaction between soil.no3n and n.fixer
test(emtrends(beta.30, ~n.fixer, "soil.no3n"))

# Individual effect of soil.no3n
test(emtrends(beta.30, ~1, "soil.no3n"))
emmeans(beta.30, ~1, "soil.no3n", at = list(soil.no3n = 0))

# Individual effect of soil.no3n
test(emtrends(beta.30, ~1, "soil.no3n"))

# Individual effect of wn.30.rel
test(emtrends(beta.30, ~1, "wn.30.rel"))
emmeans(beta.30, ~1, at = list(wn.30.rel = 0))

# Individual effect of photo
emmeans(beta.30, pairwise~photo)

# Marginal interaction between soil.no3n and n.fixer
test(emtrends(beta.30, pairwise~n.fixer, "soil.no3n"))

# Marginal interaction between soil.no3n and n.fixer
test(emtrends(beta.30, pairwise~photo, "soil.no3n"))

## Notes:
## Interaction between wn.30.rel and photo pathway indicate no effect of
## soil moisture on beta in c3 species, but strong negative effect of
## increasing soil moisture on beta in c4 species. Supports strong individual
## effect of photo pathway on beta, where c3 species generally had higher
## beta than c4 species
##
## Individual effect of wn.30.rel and soil N indicate strong negative effect
## of increasing soil N and wn.30.rel on beta
##
## Marginal interaction between soil.no3n and Nfixer, and soil.no3n and photo
## reveal stronger negative effect of soil N on beta in N-fixers; stronger negative
## effect of soil N on beta in c4 species

##########################################################################
## Beta plot
##########################################################################
min(df$wn.30.rel)
max(df$wn.30.rel)

beta.sm.plot <- ggplot(data = subset(df, !is.na(photo)), 
                                  aes(x = wn.30.rel, y = log(beta))) +
  geom_hline(yintercept = 5, lty = 2, lwd = 1) +
  geom_jitter(aes(fill = photo),
              width = 0.02, size = 3, alpha = 0.3, shape = 21) +
  stat_function(fun = function(x) 0.131*x + 3.749, lwd = 2, lty = 2, # c3
                xlim = c(0.1, 0.9), color = cbbPalette[1]) +
  stat_function(fun = function(x) -4.450*x + 3.418, lwd = 2, # c4
                xlim = c(0.1, 0.9), color = cbbPalette[4]) +
  stat_function(fun = function(x) -2.159*x + 3.584, lwd = 2, # overall
                xlim = c(0.1, 0.9), color = "black") + 
  scale_fill_manual(values = c(cbbPalette[1], cbbPalette[4]), 
                    labels = c(expression("C"[3]),
                               expression("C"[4]))) +
  scale_x_continuous(limits = c(0.1, 0.9), breaks = seq(0.1, 0.9, 0.2)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = "30-day relative soil moisture",
       y = expression(ln~beta),
       fill = "Photosynthetic pathway") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)

beta.soiln.plot <- ggplot(data = subset(df, !is.na(photo) & !is.na(n.fixer)), 
                       aes(x = soil.no3n, y = log(beta))) +
  geom_hline(yintercept = 5, lty = 2, lwd = 1) +
  geom_jitter(aes(fill = photo, shape = n.fixer),
              width = 0.02, size = 3, alpha = 0.3) +
  # stat_function(fun = function(x) *x + , lwd = 2, lty = 2, # c3
  #               xlim = c(0.1, 0.9), color = cbbPalette[1]) +
  # stat_function(fun = function(x) *x + , lwd = 2, # c4
  #               xlim = c(0.1, 0.9), color = cbbPalette[4]) +
  stat_function(fun = function(x) -0.0134*x + 2.841, lwd = 2, # overall
                xlim = c(0, 80), color = "black") + 
  scale_shape_manual(values = c(21, 22), labels = c("No", "Yes")) +
  scale_fill_manual(values = c(cbbPalette[1], cbbPalette[4]), 
                    labels = c(expression("C"[3]),
                               expression("C"[4]))) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression("Soil NO"[3]~"- N availability (ppm)"),
       y = expression(ln~beta),
       fill = "Photosynthetic pathway",
       shape = "N-fixing association") +
  theme_bw(base_size = 18) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme(legend.text.align = 0)


png("../working_drafts/TXeco_beta_b.png",
    width = 8, height = 4, units = 'in', res = 600)
beta
dev.off()

##########################################################################
## Chi
##########################################################################
df$chi[c(62, 111, 116, 300)] <- NA
df$chi[c(465)] <- NA
df$chi[c(260, 467, 468)] <- NA

chi <- lmer(chi ~ vpd1 + tavg13 + ((wn.30 + soil.no3n) * (n.fixer + photo)) + 
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

## Individual effect of vpd1
test(emtrends(chi, ~1, "vpd1"))

## Two-way interaction between 30-day relative soil moisture and photopath
test(emtrends(chi, ~photo, "wn.30.rel"))

## Two-way interaction between soil N availability and photopath
test(emtrends(chi, ~photo, "soil.no3n"))

## Two-way interaction between soil N availability and n.fix pathway
test(emtrends(chi, ~n.fixer, "soil.no3n"))

##########################################################################
## Chi plots
##########################################################################
min(df$ai.15yr)
max(df$ai.15yr)

chi.plot <- ggplot(data = df, aes(x = ai.15yr, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.02, size = 3, alpha = 0.7, shape = 21) +
  stat_function(fun = function(x) 0.145*x + 0.726, lwd = 2,
                xlim = c(0.3, 1), color = cbbPalette2[1]) +
  stat_function(fun = function(x) -0.370*x + 0.759, lwd = 2,
                xlim = c(0.35, 1), color = cbbPalette2[2]) +
  stat_function(fun = function(x) 0.208*x + 0.660, lwd = 2, lty = 2,
                xlim = c(0.3, 1), color = cbbPalette2[3]) +
  scale_fill_manual(values = cbbPalette2, labels = c(expression("C"[3]~"forb"),
                                                     expression("C"[4]~"graminoid"),
                                                     expression("C"[3]~"legume"))) +
  scale_x_continuous(limits = c(0.3, 1), breaks = seq(0.3, 1, 0.1)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = "Mean annual aridity (2006-2020)",
       y = expression(chi),
       fill = "Plant functional type") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)


png("../working_drafts/TXeco_chi_b.png",
    width = 8, height = 4, units = 'in', res = 600)
chi.plot
dev.off()


##########################################################################
## Nmass
##########################################################################
df$n.leaf[c(493)] <- NA

nmass <- lmer(log(n.leaf) ~ vpd15 + tavg21 + 
                ((wn.30.rel + soil.no3n) * (n.fixer + photo)) + (1 | NCRS.code),
              data = df)

# Check model assumptions
plot(nmass)
qqnorm(residuals(nmass))
qqline(residuals(nmass))
hist(residuals(nmass))
densityPlot(residuals(nmass))
shapiro.test(residuals(nmass))
outlierTest(nmass)

# Model output
summary(nmass)
Anova(nmass)
r.squaredGLMM(nmass)

## Three-way interaction between ai.15yr, log(beta), and pft
test(emtrends(narea, ~soil.no3n*log(beta), "ai.15yr", 
              at = list(beta = c(1,2,4,6),
                        soil.no3n = c(0, 20, 40, 80))))
emmeans(narea, ~log(beta)*pft, "ai.15yr", 
        at = list(beta = c(1,2,4,6),
                  ai.15yr = 0))

## Two-way interaction between ai.15yr and pft
test(emtrends(narea, ~pft, "ai.15yr"))
emmeans(narea, ~pft, "ai.15yr", at = list(ai.15yr = 0))


## Two-way interaction between log(beta) and pft
test(emtrends(narea, ~pft, "log(beta)"))
emmeans(narea, ~pft, "beta", at = list(beta = 0))


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

