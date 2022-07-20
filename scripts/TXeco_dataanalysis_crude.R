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
               na.strings = c("NA", "NaN"))
df$narea.chi <- df$narea / df$chi

## Add colorblind friendly palette
cbbPalette <- c("#DDAA33", "#BB5566", "#004488", "#555555", "#FFFFFF")


##########################################################################
## Coarse lmer for Narea
##########################################################################
df$narea[c(83, 89, 165, 244, 299)] <- NA

narea <- lmer(log(narea) ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
              data = subset(df, pft != "c3_shrub"))

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

# Pairwise comparisons
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
                                expression("legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("legume"))) +
  labs(x = expression("AI"["90"]),
       y = expression(log~"(N"[area]~")"),
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
                                expression("legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("legume"))) +
  labs(x = expression("AI"["2006_2020"]),
       y = expression(log~"(N"[area]~")"),
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
                                expression("legume"))) +
  scale_fill_manual(values = cbbPalette, 
                    labels = c(expression("C"[3]~"forb"),
                               expression("C"[3]~"graminoid"),
                               expression("C"[4]~"graminoid"),
                               expression("legume"))) +
  labs(x = expression("Soil N (ppm NO"[3]~"- N)"),
       y = expression(log~"(N"[area]~")"),
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
test(emtrends(nmass, ~1, "ai.90"))
test(emtrends(nmass, ~1, "soil.no3n"))


test(emtrends(nmass, ~pft, "ai.15yr"))

# Two-way interaction between ai.90 and soil.no3n
test(emtrends(nmass, ~soil.no3n, "ai.90", 
              at = list(soil.no3n = c(0, 10, 20, 40, 80))))
emmeans(nmass, ~soil.no3n*pft, "ai.90", 
        at = list(soil.no3n = c(0, 10, 20, 40, 80), ai.90 = 0))

## Test ai.90 trend within eaft pft, averaged across soil N levels
test(emtrends(nmass, ~pft, "ai.90"))
emmeans(nmass, ~pft, "ai.90", at = list(ai.90 = 0))


## Narea plot for legume
nmass.leg <- ggplot(data = subset(df, pft == "legume"),
                    aes(x = ai.90, y = log(n.leaf))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  # stat_function(fun = function(x) 0.070*x + 1.016,
  #               lwd = 2, lty = 2) +
  # stat_function(aes(color = "0"), fun = function(x) -0.050*x + 1.040,
  #               lwd = 2, lty = 2, alpha = 0.5) +
  # stat_function(aes(color = "10"), fun = function(x) 0.012*x + 1.028,
  #               lwd = 2, lty = 2, alpha = 0.5) + 
  # stat_function(aes(color = "20"), fun = function(x) 0.073*x + 1.015,
  #               lwd = 2, lty = 2, alpha = 0.5) +
  # stat_function(aes(color = "40"), fun = function(x) 0.197*x + 0.991,
  #               lwd = 2, lty = 2, alpha = 0.5) +
  # stat_function(aes(color = "80"), fun = function(x) 0.444*x + 0.943,
  #               lwd = 2, lty = 2, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_manual(values = cbbPalette) +
  labs(title = expression(bold("Legume")),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[mass]~")"),
       color = expression("Soil N (ppm NO"[3]~"-N)")) +
  theme_bw(base_size = 18) +
  theme(title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.1)))

nmass.forb <- ggplot(data = subset(df, pft == "c3_forb"),
                     aes(x = ai.90, y = log(narea))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(fun = function(x) 0.539*x + 0.397,
                lwd = 2) +
  stat_function(aes(color = "0"), fun = function(x) 1.133*x - 0.141,
                lwd = 2, alpha = 0.5) +
  stat_function(aes(color = "10"), fun = function(x) 0.827*x + 0.136,
                lwd = 2, alpha = 0.5) + 
  stat_function(aes(color = "20"), fun = function(x) 0.522*x + 0.413,
                lwd = 2, alpha = 0.5) +
  stat_function(aes(color = "40"), fun = function(x) -0.089*x + 0.967,
                lwd = 2, lty = 2, alpha = 0.5) +
  stat_function(aes(color = "80"), fun = function(x) -1.312*x + 2.075,
                lwd = 2, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_manual(values = cbbPalette) +
  labs(title = expression(bold("C"[3]~"forb")),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[mass]~")"),
       color = expression("Soil N (ppm NO"[3]~"-N)")) +
  theme_bw(base_size = 18) +
  theme(title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.1)))

nmass.c3gram <- ggplot(data = subset(df, pft == "c3_graminoid"),
                       aes(x = ai.90, y = log(narea))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  # stat_function(fun = function(x) 0.100*x + 0.360,
  #               lwd = 2, lty = 2) +
  # stat_function(aes(color = "0"), fun = function(x) 1.451*x - 0.770,
  #               lwd = 2, alpha = 0.5) +
  # stat_function(aes(color = "10"), fun = function(x) 0.756*x - 0.188,
  #               lwd = 2, alpha = 0.5) + 
  # stat_function(aes(color = "20"), fun = function(x) 0.060*x + 0.394,
  #               lwd = 2, alpha = 0.5) +
  # stat_function(aes(color = "40"), fun = function(x) -1.332*x + 1.578,
  #               lwd = 2, lty = 2, alpha = 0.5) +
  # stat_function(aes(color = "80"), fun = function(x) -4.115*x + 3.886,
  #               lwd = 2, lty = 2, alpha = 0.5) +
scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_manual(values = cbbPalette) +
  labs(title = expression(bold("C"[3]~"graminoid")),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[mass]~")"),
       color = expression("Soil N (ppm NO"[3]~"-N)")) +
  theme_bw(base_size = 18) +
  theme(title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.1)))

nmass.c4gram <- ggplot(data = subset(df, pft == "c4_graminoid"),
                       aes(x = ai.90, y = log(narea))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(fun = function(x) 0.687*x - 0.164,
                lwd = 2) +
  stat_function(aes(color = "0"), fun = function(x) 0.569*x - 0.199,
                lwd = 2, alpha = 0.5) +
  stat_function(aes(color = "10"), fun = function(x) 0.630*x - 0.181,
                lwd = 2, alpha = 0.5) + 
  stat_function(aes(color = "20"), fun = function(x) 0.691*x - 0.163,
                lwd = 2, alpha = 0.5) +
  stat_function(aes(color = "40"), fun = function(x) 0.813*x - 0.126,
                lwd = 2, lty = 2, alpha = 0.5) +
  stat_function(aes(color = "80"), fun = function(x) 1.056*x - 0.053,
                lwd = 2, lty = 2, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_manual(values = cbbPalette) +
  labs(title = expression(bold("C"[4]~"graminoid")),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[area]~")"),
       color = expression("Soil N (ppm NO"[3]~"-N)")) +
  theme_bw(base_size = 18) +
  theme(title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.1)))

png("../working_drafts/TXeco_Nmass.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(nmass.leg, nmass.forb, nmass.c3gram, nmass.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

##########################################################################
## Coarse lmer for sla
##########################################################################
df$sla[c(20, 21, 196, 235)] <- NA

marea <- lmer(log(lma * 10000) ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
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
test(emtrends(marea, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60))))

emmeans(marea, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60),
                                                  ai.90 = 0))


## Narea plot for legume
marea.leg <- ggplot(data = subset(df, pft == "legume"),
                    aes(x = ai.90, y = lma * 10000)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(-1.818*x + 5.799),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(-1.662*x + 5.790),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(-1.506*x + 5.780),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(-1.350*x + 5.771),
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, 70)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Legume",
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

marea.forb <- ggplot(data = subset(df, pft == "c3_forb"),
                     aes(x = ai.90, y = lma * 10000)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(-0.784*x + 5.198),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(-0.292*x + 4.730),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(0.201*x + 4.261),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(0.694*x + 3.792),
                lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, 70)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"forb"),
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

marea.c3gram <- ggplot(data = subset(df, pft == "c3_graminoid"),
                       aes(x = ai.90, y = lma * 10000)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(-0.226*x + 4.904),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(0.905*x + 4.142),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(2.035*x + 3.380),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(3.165*x + 2.618),
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 60)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

marea.c4gram <- ggplot(data = subset(df, pft == "c4_graminoid"),
                       aes(x = ai.90, y = lma * 10000)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(-0.843*x + 5.098),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(0.077*x + 4.385),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(0.998*x + 3.671),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(1.919*x + 2.958),
                lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 60)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[4]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

png("../working_drafts/TXeco_Marea.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(marea.leg, marea.forb, marea.c3gram, marea.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

##########################################################################
## Coarse lmer for chi
##########################################################################
df$chi[c(71, 233, 240, 249, 396, 402)] <- NA
df$chi[df$pft == "c3_graminoid" & df$chi < 0.5] <- NA

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


# Pairwise comparisons
test(emtrends(chi, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60))))

emmeans(chi, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60),
                                                  ai.90 = 0))


## Chi plot for legume
chi.leg <- ggplot(data = subset(df, pft == "legume"),
                    aes(x = ai.90, y = chi)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) 0.269*x + 0.600,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) 0.161*x + 0.670,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) 0.053*x + 0.741,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) -0.055*x + 0.811,
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, 0.1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Legume",
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

chi.forb <- ggplot(data = subset(df, pft == "c3_forb"),
                     aes(x = ai.90, y = chi)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) 0.042*x + 0.779,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) 0.058*x + 0.768,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) 0.074*x + 0.757,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) 0.090*x + 0.746,
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, 0.1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"forb"),
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

chi.c3gram <- ggplot(data = subset(df, pft == "c3_graminoid"),
                       aes(x = ai.90, y = chi)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) -0.968*x + 1.289,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) 0.395*x + 0.382,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) 1.759*x - 0.524,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) 3.123*x - 1.431,
                lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

chi.c4gram <- ggplot(data = subset(df, pft == "c4_graminoid"),
                       aes(x = ai.90, y = chi)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) -0.095*x + 0.105,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) 0.132*x - 0.059,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) 0.359*x - 0.223,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) 0.586*x - 0.387,
                lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[4]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("M"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

png("../working_drafts/TXeco_chi.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(chi.leg, chi.forb, chi.c3gram, chi.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()








##########################################################################
## Coarse lmer for narea:chi
##########################################################################
df$narea.chi[c(18, 52, 54, 235, 271, 308, 321, 325, 403, 464)] <- NA

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

## Narea:chi plot for legume
narea.chi.leg <- ggplot(data = subset(df, pft == "legume"),
                        aes(x = ai.90, y = log(narea.chi))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"),
                fun = function(x) -2.078*x + 2.700, lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "10"),
                fun = function(x) -1.824*x + 2.598, lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"),
                fun = function(x) -1.570*x + 2.495, lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "30"),
                fun = function(x) -1.316*x + 2.393, lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.6)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Legume",
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[area]~":"~chi~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

narea.chi.forb <- ggplot(data = subset(df, pft == "c3_forb"),
                         aes(x = ai.90, y = log(narea.chi))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"),
                fun = function(x) 0.279*x + 0.703, lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "10"),
                fun = function(x) 0.151*x + 0.803, lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"),
                fun = function(x) 0.022*x + 0.902, lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "30"),
                fun = function(x) -0.106*x + 1.002, lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.6)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"forb"),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[area]~":"~chi~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

narea.chi.c3gram <- ggplot(data = subset(df, pft == "c3_graminoid"),
                         aes(x = ai.90, y = log(narea.chi))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"),
                fun = function(x) 4.727*x - 1.654, lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "10"),
                fun = function(x) 2.343*x - 0.066, lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"),
                fun = function(x) -0.042*x + 1.522, lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "30"),
                fun = function(x) -2.427*x + 3.110, lwd = 2, lty = 2, alpha = 0.7) +
    scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.6)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[area]~":"~chi~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

narea.chi.c4gram <- ggplot(data = subset(df, pft == "c4_graminoid"),
                           aes(x = ai.90, y = log(narea.chi))) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"),
                fun = function(x) 0.603*x + 2.975, lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "10"),
                fun = function(x) 1.861*x + 2.139, lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"),
                fun = function(x) 3.119*x + 1.302, lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "30"),
                fun = function(x) 4.377*x + 0.466, lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[4]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression(log~"(N"[area]~":"~chi~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)


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
