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
library(relaimpo)

# Turn off digit rounding in emmean args
emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(pft != "c3_shrub" & site != "Bell_2020_05" & 
           site != "Russel_2020_01")
df$narea.chi <- df$narea / df$chi

## Add colorblind friendly palette
cbbPalette <- c("#DDAA33", "#BB5566", "#004488", "#555555", "#FFFFFF")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_graminoid"])
length(df$pft[df$pft == "c4_graminoid"])
length(df$pft[df$pft == "legume"])
length(df$pft[df$pft == "c3_forb"])

##########################################################################
## Climatic effects on Narea
##########################################################################
df$narea[c(223, 275)] <- NA

narea.clim <- lmer(log(narea) ~ (ai.90 + ai.15yr) * mat.15yr * map.15yr * 
                     (1 | sampling.year) + (1 | NCRS.code),
                   data = df)

# Check model assumptions
plot(narea.clim)
qqnorm(residuals(narea.clim))
qqline(residuals(narea.clim))
hist(residuals(narea.clim))
shapiro.test(residuals(narea.clim))
outlierTest(narea.clim)

# Model output
summary(narea.clim)
Anova(narea.clim)
r.squaredGLMM(narea.clim)

##########################################################################
## Coarse lmer for Narea
##########################################################################
df$narea[c(83, 89, 165, 244, 299)] <- NA
df$narea[c(183)] <- NA

narea <- lmer(log(narea) ~ (ai.90 + ai.15yr) * soil.no3n * pft + 
                (1 | sampling.year) + (1 | NCRS.code),
              data = subset(df, pft != "c3_shrub" & 
                              site != "Bell_2020_05" & 
                              site != "Russel_2020_01"))

# Check model assumptions
plot(narea)
qqnorm(residuals(narea))
qqline(residuals(narea))
hist(residuals(narea))
shapiro.test(residuals(narea))
outlierTest(narea)

# Model output
summary(narea)
Anova(narea)
r.squaredGLMM(narea)

data.frame(Anova(narea))
## find relative importance for each factor from model
narea.relimp.results <- calc.relip.mm(narea)$lmg
relimp.narea <- data.frame(factor = c("ai.90", 
                                      "ai.15yr", 
                                      "soil.no3n",
                                      "pft", 
                                      "ai.90 * soil.no3n",
                                      "ai.15yr * soil.no3n",
                                      "ai.90 * pft",
                                      "ai.15yr * pft",
                                      "soil.no3n * pft",
                                      "ai.90 * soil.no3n * pft",
                                      "ai.15yr * soil.no3n * pft",
                                      "unexplained"),
                           relative.importance = as.numeric(
                             as.character(c(narea.relimp.results[1:3], 
                                            sum(narea.relimp.results[4:6]),
                                            narea.relimp.results[7:8],
                                            sum(narea.relimp.results[9:11]),
                                            sum(narea.relimp.results[12:14]),
                                            sum(narea.relimp.results[15:17]),
                                            sum(narea.relimp.results[18:20]),
                                            sum(narea.relimp.results[21:23]),
                                            1 - sum(narea.relimp.results)))))


relimp_leafnarea <- NULL
relimp_leafnarea$Factor <- c("90-day AI", 
                             "15-year AI", 
                             "Soil NO3-N",
                             "PFT", 
                             "90-day AI * Soil NO3-N",
                             "15-year AI * Soil NO3-N",
                             "90-day AI * PFT",
                             "15-year AI * PFT",
                             "Soil NO3-N * PFT",
                             "90-day AI * Soil NO3-N * PFT",
                             "15-year AI * Soil NO3-N * PFT",
                             "Unexplained")
                             

relimp_leafnarea$Importance <- as.numeric(as.character(c(narea.relimp.results[1:3], 
                                                         sum(narea.relimp.results[4:6]),
                                                         narea.relimp.results[7:8],
                                                         sum(narea.relimp.results[9:11]),
                                                         sum(narea.relimp.results[12:14]),
                                                         sum(narea.relimp.results[15:17]),
                                                         sum(narea.relimp.results[18:20]),
                                                         sum(narea.relimp.results[21:23]),
                                                         1 - sum(narea.relimp.results))))
relimp_leafnarea_df <- as.data.frame(relimp_leafnarea)


# Pairwise comparisons
## Test ai.90 trend within each pft
test(emtrends(narea, ~soil.no3n*pft, "ai.15yr", at = list(soil.no3n = c(0, 10, 20, 40, 80))))
emmeans(narea, ~pft, "ai.90", at = list(ai.90 = 0))



## Test ai.90 trend within each pft
test(emtrends(narea, ~pft, "ai.90"))
emmeans(narea, ~pft, "ai.90", at = list(ai.90 = 0))

## Test ai.90 trend averaged across pfts
test(emtrends(narea, ~1, "ai.90"))
emmeans(narea, ~1, "ai.90", at = list(ai.90 = 0))

## Test ai.15yr trend within each pft
test(emtrends(narea, ~pft, "ai.15yr"))
emmeans(narea, ~pft, "ai.15yr", at = list(ai.15yr = 0))

## Test ai.15yr trend averaged across pfts
test(emtrends(narea, ~1, "ai.15yr"))
emmeans(narea, ~1, "ai.15yr", at = list(ai.15yr = 0))

## Test soil.no3n trend within each pft
test(emtrends(narea, ~pft, "soil.no3n"))
emmeans(narea, ~pft, "soil.no3n", at = list(soil.no3n = 0))

## Test soil.no3n trend averaged across pfts
test(emtrends(narea, ~1, "soil.no3n"))
emmeans(narea, ~1, "soil.no3n", at = list(soil.no3n = 0))


# Plots
narea.ai90 <- ggplot(data = subset(df, pft != "c3_shrub"),
                           aes(x = ai.90, y = log(narea))) +
  geom_jitter(aes(shape = pft, fill = pft), width = 0.02, size = 3, alpha = 0.7) +
  stat_function(fun = function(x) -0.295*x + 0.803, lwd = 2,
                xlim = c(0.3, 1.5), lty = 2) +  
  stat_function(aes(color = "c3_forb"), 
                fun = function(x) 0.136*x + 0.580, lwd = 2, alpha = 0.7,
                xlim = c(0.3, 1.5), lty = 2) +
  stat_function(aes(color = "c3_graminoid"), 
                fun = function(x) -0.430*x + 0.751, lwd = 2, alpha = 0.7,
                xlim = c(0.3, 1.5), lty = 2) +
  stat_function(aes(color = "c4_graminoid"), 
                fun = function(x) 0.666*x - 0.320, lwd = 2, alpha = 0.7,
                xlim = c(0.3, 1.5), lty = 2) +
  stat_function(aes(color = "legume"), 
                fun = function(x) -1.551*x + 2.200, lwd = 2, alpha = 0.7,
                xlim = c(0.3, 1.5)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  scale_x_continuous(limits = c(0.3, 1.5), breaks = seq(0.3, 1.5, 0.3)) +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c(expression("C"[3]~"forb"),
                                expression("C"[3]~"graminoid"),
                                expression("C"[4]~"graminoid"),
                                expression("C"[3]~"legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("C"[3]~"legume"))) +
  labs(x = expression("AI"["90"]),
       y = expression(ln~"(N"[area]~")"),
       fill = "Plant functional type",
       shape = "Plant functional type") +
  theme_bw(base_size = 24) +
  theme(legend.text.align = 0) +
  guides(color = "none")

## Normal aridity index
narea.ai15yr<- ggplot(data = subset(df, pft != "c3_shrub"),
                      aes(x = ai.15yr, y = log(narea))) +
  geom_jitter(aes(shape = pft, fill = pft), width = 0.02, size = 3, alpha = 0.7) +
  stat_function(fun = function(x) -0.268*x + 0.754, lwd = 2,
                xlim = c(0.36, 1), lty = 2) +  
  stat_function(aes(color = "c3_forb"), 
                fun = function(x) -0.663*x + 1.073, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1)) +
  stat_function(aes(color = "c3_graminoid"), 
                fun = function(x) -2.007*x + 1.646, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1), lty = 2) +
  stat_function(aes(color = "c4_graminoid"), 
                fun = function(x) -1.401*x + 0.988, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1)) +
  stat_function(aes(color = "legume"), 
                fun = function(x) 3.000*x - 0.687, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  scale_x_continuous(limits = c(0.36, 1), breaks = seq(0.36, 1, 0.16)) +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c(expression("C"[3]~"forb"),
                                expression("C"[3]~"graminoid"),
                                expression("C"[4]~"graminoid"),
                                expression("C"[3]~"legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("C"[3]~"legume"))) +
  labs(x = expression("AI"["2006_2020"]),
       y = expression(ln~"(N"[area]~")"),
       fill = "Plant functional type",
       shape = "Plant functional type") +
  theme_bw(base_size = 24) +
  theme(legend.text.align = 0) +
  guides(color = "none")

# Soil N
narea.soiln <- ggplot(data = subset(df, pft != "c3_shrub"),
                      aes(x = soil.no3n, y = log(narea))) +
  geom_jitter(aes(shape = pft, fill = pft), width = 0.005, size = 3, alpha = 0.7) +
  stat_function(fun = function(x) 0.005*x + 0.513, lwd = 2,
                xlim = c(0, 80)) +  
  stat_function(aes(color = "c3_forb"), 
                fun = function(x) 0.001*x + 0.655, lwd = 2, alpha = 0.7,
                lty = 2, xlim = c(0, 80)) +
  stat_function(aes(color = "c3_graminoid"), 
                fun = function(x) 0.007*x + 0.315, lwd = 2, alpha = 0.7,
                lty = 2, xlim = c(0, 80)) +
  stat_function(aes(color = "c4_graminoid"), 
                fun = function(x) 0.002*x + 0.122, lwd = 2, alpha = 0.7,
                lty = 2, xlim = c(0, 80)) +
  stat_function(aes(color = "legume"), 
                fun = function(x) 0.008*x + 0.961, lwd = 2,
                xlim = c(0, 80)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c(expression("C"[3]~"forb"),
                                expression("C"[3]~"graminoid"),
                                expression("C"[4]~"graminoid"),
                                expression("C"[3]~"legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("C"[3]~"legume"))) +
  labs(x = expression("Soil N (ppm NO"[3]~"- N)"),
       y = expression(ln~"(N"[area]~")"),
       fill = "Plant functional type",
       shape = "Plant functional type") +
  theme_bw(base_size = 24) +
  theme(legend.text.align = 0) +
  guides(color = "none")


png("../working_drafts/TXeco_Narea.png",
    width = 20, height = 5, units = 'in', res = 600)
ggarrange(narea.ai90, narea.ai15yr, narea.soiln,
          nrow = 1, ncol = 3, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", 
          font.label = list(size = 24, face = "bold"))
dev.off()

##########################################################################
## Coarse lmer for Nleaf
##########################################################################
df$n.leaf[c(19, 384, 533)] <- NA

nmass <- lmer(log(n.leaf) ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
              data = subset(df, pft != "c3_shrub"))

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

# Pairwise comparisons
# Individual effects
test(emtrends(nmass, ~1, "ai.15yr"))
emmeans(nmass, ~1, "ai.15yr", at = list(ai.15yr = 0))

test(emtrends(nmass, ~1, "ai.90"))
emmeans(nmass, ~1, "ai.90", at = list(ai.90 = 0))

test(emtrends(nmass, ~1, "soil.no3n"))
test(emtrends(nmass, ~pft, "ai.15yr"))

# Two-way interaction between ai.90 and soil.no3n
test(emtrends(nmass, ~soil.no3n, "ai.90", 
              at = list(soil.no3n = c(0, 10, 20, 40, 80))))
emmeans(nmass, ~soil.no3n*pft, "ai.90", 
        at = list(soil.no3n = c(0, 10, 20, 40, 80), ai.90 = 0))

## Test ai.15yr trend within eaft pft, averaged across soil N levels
test(emtrends(nmass, ~pft, "ai.15yr"))
emmeans(nmass, ~pft, "ai.15yr", at = list(ai.15yr = 0))

# Plots
nmass.ai90 <- ggplot(data = subset(df, pft != "c3_shrub"),
                     aes(x = ai.90, y = log(n.leaf))) +
  geom_jitter(aes(shape = pft, fill = pft), width = 0.02, size = 3, alpha = 0.7) +
   stat_function(fun = function(x) 0.308*x + 0.425, lwd = 2,
                 xlim = c(0.3, 1.5)) +  
  # stat_function(aes(color = "c3_forb"), 
  #               fun = function(x) *x +, lwd = 2, alpha = 0.7,
  #               xlim = c(0.3, 1.5), lty = 2) +
  # stat_function(aes(color = "c3_graminoid"), 
  #               fun = function(x) *x +, lwd = 2, alpha = 0.7,
  #               xlim = c(0.3, 1.5), lty = 2) +
  # stat_function(aes(color = "c4_graminoid"), 
  #               fun = function(x) *x -, lwd = 2, alpha = 0.7,
  #               xlim = c(0.3, 1.5), lty = 2) +
  # stat_function(aes(color = "legume"), 
  #               fun = function(x) *x +, lwd = 2, alpha = 0.7,
  #               xlim = c(0.3, 1.5)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  scale_x_continuous(limits = c(0.3, 1.5), breaks = seq(0.3, 1.5, 0.3)) +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c(expression("C"[3]~"forb"),
                                expression("C"[3]~"graminoid"),
                                expression("C"[4]~"graminoid"),
                                expression("legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("legume"))) +
  labs(x = expression("AI"["90"]),
       y = expression(ln~"(N"[mass]~")"),
       fill = "Plant functional type",
       shape = "Plant functional type") +
  theme_bw(base_size = 24) +
  theme(legend.text.align = 0) +
  guides(color = "none")

## Normal aridity index
nmass.ai15yr<- ggplot(data = subset(df, pft != "c3_shrub"),
                      aes(x = ai.15yr, y = log(n.leaf))) +
  geom_jitter(aes(shape = pft, fill = pft), width = 0.02, size = 3, alpha = 0.7) +
  stat_function(fun = function(x) -1.261*x + 1.398, lwd = 2,
                xlim = c(0.36, 1)) +  
  stat_function(aes(color = "c3_forb"), 
                fun = function(x) -1.190*x + 1.480, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1)) +
  stat_function(aes(color = "c3_graminoid"), 
                fun = function(x) -2.688*x + 2.026, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1), lty = 2) +
  stat_function(aes(color = "c4_graminoid"), 
                fun = function(x) -1.364*x + 1.137, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1)) +
  stat_function(aes(color = "legume"), 
                fun = function(x) 0.197*x + 0.948, lwd = 2, alpha = 0.7,
                xlim = c(0.36, 1), lty = 2) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  scale_x_continuous(limits = c(0.36, 1), breaks = seq(0.36, 1, 0.16)) +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c(expression("C"[3]~"forb"),
                                expression("C"[3]~"graminoid"),
                                expression("C"[4]~"graminoid"),
                                expression("legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("legume"))) +
  labs(x = expression("AI"["2006_2020"]),
       y = expression(ln~"(N"[mass]~")"),
       fill = "Plant functional type",
       shape = "Plant functional type") +
  theme_bw(base_size = 24) +
  theme(legend.text.align = 0) +
  guides(color = "none")

# Soil N
nmass.soiln <- ggplot(data = subset(df, pft != "c3_shrub"),
                      aes(x = soil.no3n, y = log(nmass))) +
  geom_jitter(aes(shape = pft, fill = pft), width = 0.005, size = 3, alpha = 0.7) +
  stat_function(fun = function(x) 0.005*x + 0.513, lwd = 2,
                xlim = c(0, 80)) +  
  stat_function(aes(color = "c3_forb"), 
                fun = function(x) 0.001*x + 0.655, lwd = 2, alpha = 0.7,
                lty = 2, xlim = c(0, 80)) +
  stat_function(aes(color = "c3_graminoid"), 
                fun = function(x) 0.007*x + 0.315, lwd = 2, alpha = 0.7,
                lty = 2, xlim = c(0, 80)) +
  stat_function(aes(color = "c4_graminoid"), 
                fun = function(x) 0.002*x + 0.122, lwd = 2, alpha = 0.7,
                lty = 2, xlim = c(0, 80)) +
  stat_function(aes(color = "legume"), 
                fun = function(x) 0.008*x + 0.961, lwd = 2,
                xlim = c(0, 80)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c(expression("C"[3]~"forb"),
                                expression("C"[3]~"graminoid"),
                                expression("C"[4]~"graminoid"),
                                expression("legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("legume"))) +
  labs(x = expression("Soil N (ppm NO"[3]~"- N)"),
       y = expression(ln~"(N"[mass]~")"),
       fill = "Plant functional type",
       shape = "Plant functional type") +
  theme_bw(base_size = 24) +
  theme(legend.text.align = 0) +
  guides(color = "none")

png("../working_drafts/TXeco_Nmass.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(nmass.leg, nmass.forb, nmass.c3gram, nmass.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

##########################################################################
## Coarse lmer for sla
##########################################################################
df$marea[c(20, 21, 244, 299)] <- NA

marea <- lmer(log(marea) ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
              data = subset(df, pft != "c3_shrub"))

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

# Pairwise comparisons
test(emtrends(marea, ~1, "ai.90"))

## Two way interaction between ai.90 and soil.no3n
test(emtrends(marea, ~soil.no3n, "ai.90", at = list(soil.no3n = c(0, 10, 20, 40, 80))))
emmeans(marea, ~soil.no3n, "ai.90", at = list(soil.no3n = c(0, 10, 20, 40, 80),
                                              ai.90 = 0))

## Two way interaction between ai.90 and pft
test(emtrends(marea, ~pft, "ai.90"))
emmeans(marea, ~soil.no3n, "ai.90", at = list(soil.no3n = c(0, 10, 20, 40, 80),
                                              ai.90 = 0))

## Two way interaction between ai.15yr and pft
test(emtrends(marea, ~pft, "ai.15yr"))
emmeans(marea, ~soil.no3n, "ai.15yr", at = list(soil.no3n = c(0, 10, 20, 40, 80),
                                              ai.90 = 0))

## Two way interaction between soil.no3n and pft
test(emtrends(marea, ~pft, "soil.no3n"))
emmeans(marea, ~soil.no3n, "ai.15yr", at = list(soil.no3n = c(0, 10, 20, 40, 80),
                                                ai.90 = 0))




png("../working_drafts/TXeco_Marea.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

##########################################################################
## Coarse lmer for chi
##########################################################################
df$chi[c(71, 93, 293, 301, 324, 426, 505, 506, 507, 508, 527, 534)] <- NA

chi <- lmer(chi ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
              data = subset(df, pft != "c3_shrub"))

# Check model assumptions
plot(chi)
qqnorm(residuals(chi))
qqline(residuals(chi))
hist(residuals(chi))
shapiro.test(residuals(chi))
outlierTest(chi)

# Model output
summary(chi)
Anova(chi)
r.squaredGLMM(chi)

test(emtrends(chi, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 10, 20, 40, 80))))


# Pairwise comparisons
test(emtrends(chi, ~soil.no3n*pft, "ai.90", 
              at = list(soil.no3n = c(0, 20, 40, 60))))

emmeans(chi, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60),
                                                  ai.90 = 0))


png("../working_drafts/TXeco_chi.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(chi.leg, chi.forb, chi.c3gram, chi.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

##########################################################################
## Coarse lmer for narea:chi
##########################################################################
chi <- lmer(log(narea.chi) ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
            data = subset(df, pft != "c3_shrub"))

# Check model assumptions
plot(chi)
qqnorm(residuals(chi))
qqline(residuals(chi))
hist(residuals(chi))
shapiro.test(residuals(chi))
outlierTest(chi)

# Model output
summary(chi)
Anova(chi) ## 3-way interaction between ai.90, soil.no3n, and pft
r.squaredGLMM(chi)

# Pairwise comparisons
test(emtrends(chi, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 10, 20, 30))))
emmeans(chi, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 10, 20, 30),
                                                ai.90 = 0))



png("../working_drafts/TXeco_Narea_chi.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(narea.chi.leg, narea.chi.forb, narea.chi.c3gram, narea.chi.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

?ggarrange

##########################################################################
## Coarse lmer for leaf nitrogen per unit leaf area (short-term climate)
##########################################################################
df$n.area[c(98, 121)] <- NA

narea.gs <- lmer(sqrt(n.area) ~ spei * soil.no3n * pft + (1 | NCRS.code), 
                 data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub"))

# Check model assumptions
plot(narea.gs)
qqnorm(residuals(narea.gs))
qqline(residuals(narea.gs))
hist(residuals(narea.gs))
shapiro.test(residuals(narea.gs))
outlierTest(narea.gs)

# Model output
summary(narea.gs)
Anova(narea.gs) # no effect of spei, soil.no3n, or pft on Narea
r.squaredGLMM(narea.gs)

##########################################################################
## Coarse lmer for leaf nitrogen per unit leaf area (long-term climate)
##########################################################################
df$n.area[237] <- NA
narea.norm <- lmer(sqrt(n.area) ~ norm.spei * soil.no3n * pft + (1 | NCRS.code), 
                 data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub"))

# Check model assumptions
plot(narea.norm)
qqnorm(residuals(narea.norm))
qqline(residuals(narea.norm))
hist(residuals(narea.norm))
shapiro.test(residuals(narea.norm))
outlierTest(narea.norm)

# Model output
summary(narea.norm)
Anova(narea.norm) # no effect of norm.spei, soil.no3n, or pft on Narea
r.squaredGLMM(narea.norm)

##########################################################################
## Coarse lmer for leaf nitrogen content (short-term climate)
##########################################################################
df$n.leaf[c(89)] <- NA

nleaf.gs <- lmer(log(n.leaf) ~ spei * soil.no3n * pft + (1 | NCRS.code), 
                 data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub"))

# Check model assumptions
plot(nleaf.gs)
qqnorm(residuals(nleaf.gs))
qqline(residuals(nleaf.gs))
hist(residuals(nleaf.gs))
shapiro.test(residuals(nleaf.gs))
outlierTest(nleaf.gs)

# Model output
summary(nleaf.gs)
Anova(nleaf.gs) # no effect of spei, soil.no3n, or pft on Narea
r.squaredGLMM(nleaf.gs)

# Post-hoc 
test(emtrends(nleaf.gs, ~pft, var = "spei")) # Chi decreases with aridity in graminoids
test(emtrends(nleaf.gs, ~soil.no3n, var = "spei", 
              at = list(soil.no3n = c(0, 6, 12))))
test(emtrends(nleaf.gs, ~soil.no3n*pft, var = "spei", 
              at = list(soil.no3n = c(0, 6, 12))))


##########################################################################
## Coarse lmer for leaf nitrogen content
##########################################################################
df$n.area[237] <- NA
nleaf.norm <- lmer(log(n.leaf) ~ norm.spei * soil.no3n * pft + (1 | NCRS.code), 
                   data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub"))

# Check model assumptions
plot(nleaf.norm)
qqnorm(residuals(nleaf.norm))
qqline(residuals(nleaf.norm))
hist(residuals(nleaf.norm))
shapiro.test(residuals(nleaf.norm))
outlierTest(nleaf.norm)

# Model output
summary(nleaf.norm)
Anova(nleaf.norm) # no effect of norm.spei, soil.no3n, or pft on Narea
r.squaredGLMM(nleaf.norm)

test(emtrends(nleaf.norm, ~soil.no3n*pft, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))


##########################################################################
## Coarse lmer for sla
##########################################################################
df$sla[c(121, 124, 237, 248)] <- NA

sla.gs <- lmer(sqrt(sla) ~ spei  * soil.no3n * pft + (1 | NCRS.code), 
                 data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub"))

# Check model assumptions
plot(sla.gs)
qqnorm(residuals(sla.gs))
qqline(residuals(sla.gs))
hist(residuals(sla.gs))
shapiro.test(residuals(sla.gs))
outlierTest(sla.gs)

# Model output
summary(sla.gs)
Anova(sla.gs) # no effect of spei, soil.no3n, or pft on Narea
r.squaredGLMM(sla.gs)

# Post-hoc 
test(emtrends(sla.gs, ~pft, var = "spei")) # Chi decreases with aridity in graminoids
test(emtrends(sla.gs, ~soil.no3n, var = "spei", 
              at = list(soil.no3n = c(0, 6, 12))))
test(emtrends(sla.gs, ~soil.no3n*pft, var = "spei", 
              at = list(soil.no3n = c(0, 6, 12))))

ggplot(subset(df, sampling.year == 2020 & visit.type == "i"),
       aes(x = n.fixer, y = n.area)) +
  geom_boxplot() +
  facet_grid()


##########################################################################
## Coarse lmer for sla
##########################################################################
df$n.area[237] <- NA
nleaf.norm <- lmer(log(n.leaf) ~ norm.spei * soil.no3n * pft + (1 | NCRS.code), 
                   data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub"))

# Check model assumptions
plot(nleaf.norm)
qqnorm(residuals(nleaf.norm))
qqline(residuals(nleaf.norm))
hist(residuals(nleaf.norm))
shapiro.test(residuals(nleaf.norm))
outlierTest(nleaf.norm)

# Model output
summary(narea.norm)
Anova(narea.norm) # no effect of norm.spei, soil.no3n, or pft on Narea
r.squaredGLMM(narea.norm)

test(emtrends(nleaf.norm, ~soil.no3n*pft, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))
##########################################################################
## Coarse lmer for chi growing season
##########################################################################
df$chi[51] <- NA

chi <- lmer(chi ~ spei * soil.no3n * pft + (1 | NCRS.code),
               data = subset(df, sampling.year == 2020 & visit.type == "i" 
                             & pft != "c3_shrub" & chi > 0))

# Check model assumptions
plot(chi)
qqnorm(residuals(chi))
qqline(residuals(chi))
hist(residuals(chi))
shapiro.test(residuals(chi))
outlierTest(chi)

# Model output
summary(chi)
Anova(chi)
r.squaredGLMM(chi)

# Pairwise comparisons
emmeans(chi.gs, pairwise~pft) # C4 has lower chi than C3 (not surprising)
test(emtrends(chi.gs, ~pft, var = "spei")) # Chi decreases with aridity in graminoids
test(emtrends(chi.gs, ~soil.no3n, var = "spei", 
              at = list(soil.no3n = c(0, 6, 12))))
test(emtrends(chi.gs, ~soil.no3n*pft, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))

##########################################################################
## Coarse lmer for chi climate 
##########################################################################
df$chi[247] <- NA

chi.norm <- lmer(sqrt(chi) ~ norm.spei * soil.no3n * pft + (1 | NCRS.code),
               data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub" & chi > 0))

# Check model assumptions
plot(chi.norm)
qqnorm(residuals(chi.norm))
qqline(residuals(chi.norm))
hist(residuals(chi.norm))
shapiro.test(residuals(chi.norm))
outlierTest(chi.norm)

# Model output
summary(chi.norm)
Anova(chi.norm)
r.squaredGLMM(chi.norm)

# Pairwise comparisons
emmeans(chi.norm, pairwise~pft) # C4 has lower chi than C3 (not surprising)
test(emtrends(chi.norm, ~pft, var = "norm.spei")) # Chi decreases with aridity in graminoids
test(emtrends(chi.norm, ~soil.no3n, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))
test(emtrends(chi.norm, ~soil.no3n*pft, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))

##########################################################################
## Coarse lmer for narea:chi for normal climate
##########################################################################
df$narea.chi[c(93, 121, 124, 128, 145, 237, 248, 292)] <- NA

narea.chi <- lmer(log(narea.chi) ~ norm.spei * soil.no3n * pft + (1 | NCRS.code),
                 data = subset(df, sampling.year == 2020 & visit.type == "i" & pft != "c3_shrub" & chi > 0))

# Check model assumptions
plot(chi.norm)
qqnorm(residuals(narea.chi))
qqline(residuals(narea.chi))
hist(residuals(narea.chi))
shapiro.test(residuals(narea.chi))
outlierTest(narea.chi)

# Model output
summary(narea.chi)
Anova(narea.chi)
r.squaredGLMM(narea.chi)

# Pairwise comparisons
test(emtrends(chi.norm, ~1, var = "norm.spei"))



emmeans(chi.norm, pairwise~pft) # C4 has lower chi than C3 (not surprising)
test(emtrends(chi.norm, ~pft, var = "norm.spei")) # Chi decreases with aridity in graminoids
test(emtrends(chi.norm, ~soil.no3n, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))
test(emtrends(chi.norm, ~soil.no3n*pft, var = "norm.spei", 
              at = list(soil.no3n = c(0, 6, 12))))
