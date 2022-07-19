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
library(sjPlot)
library(ggpubr)

emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(site != "Bell_2020_05" & site != "Russel_2020_01")
df$narea.chi <- df$narea / df$chi


##########################################################################
## Coarse lmer for Narea
##########################################################################
df$narea[c(235, 287, 290, 509)] <- NA

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
test(emtrends(narea, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60))))

emmeans(narea, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60),
                                                ai.90 = 0))


## Narea plot for legume
narea.leg <- ggplot(data = subset(df, pft == "legume"),
                        aes(x = ai.90, y = narea)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(-1.812*x + 2.248),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(-1.447*x + 2.143),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(-1.082*x + 2.039),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(-0.717*x + 1.934),
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Legume",
       x = expression("AI"["90_day"]),
       y = expression("N"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

narea.forb <- ggplot(data = subset(df, pft == "c3_forb"),
                         aes(x = ai.90, y = narea)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(0.239*x + 0.522),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(0.052*x + 0.667),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(-0.135*x + 0.813),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(-0.322*x + 0.959),
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"forb"),
       x = expression("AI"["90_day"]),
       y = expression("N"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

narea.c3gram <- ggplot(data = subset(df, pft == "c3_graminoid"),
                           aes(x = ai.90, y = narea)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(1.946*x - 1.010),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(0.679*x + 0.080),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(-0.587*x + 1.169),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(-1.854*x + 2.258),
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("N"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

narea.c4gram <- ggplot(data = subset(df, pft == "c4_graminoid"),
                           aes(x = ai.90, y = narea)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) exp(0.244*x - 0.046),
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) exp(1.036*x - 0.571),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) exp(1.827*x - 1.096),
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) exp(2.619*x - 1.621),
                lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[4]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("N"[area]~"(g m"^-2~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)


png("../working_drafts/TXeco_Narea.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(narea.leg, narea.forb, narea.c3gram, narea.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

##########################################################################
## Coarse lmer for Nleaf
##########################################################################
df$n.leaf[c(19, 375, 509)] <- NA

nmass <- lmer(sqrt(n.leaf) ~ (ai.90 + ai.15yr) * soil.no3n * pft + (1 | NCRS.code),
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
test(emtrends(nmass, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60))))

emmeans(nmass, ~soil.no3n*pft, "ai.90", at = list(soil.no3n = c(0, 20, 40, 60),
                                                  ai.90 = 0))


## Narea plot for legume
nleaf.leg <- ggplot(data = subset(df, pft == "legume"),
                    aes(x = ai.90, y = n.leaf)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) (-0.041*x + 1.689)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "10"), fun = function(x) (0.055*x + 1.677)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) (0.151*x + 1.665)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "30"), fun = function(x) (0.247*x + 1.654)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 2)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Legume",
       x = expression("AI"["90_day"]),
       y = expression("N"[mass]~"(g g"^-1~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

nleaf.forb <- ggplot(data = subset(df, pft == "c3_forb"),
                     aes(x = ai.90, y = n.leaf)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) (0.801*x + 0.854)^2,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) (0.376*x + 1.242)^2,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) (-0.048*x + 1.630)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) (-0.473*x + 2.019)^2,
                lwd = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 2)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"forb"),
       x = expression("AI"["90_day"]),
       y = expression("N"[mass]~"(g g"^-1~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

nleaf.c3gram <- ggplot(data = subset(df, pft == "c3_graminoid"),
                       aes(x = ai.90, y = n.leaf)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) (0.690*x + 0.664)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) (0.249*x + 1.091)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "40"), fun = function(x) (-0.192*x + 1.518)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "60"), fun = function(x) (-0.633*x + 1.945)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[3]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("N"[mass]~"(g g"^-1~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

nleaf.c4gram <- ggplot(data = subset(df, pft == "c4_graminoid"),
                       aes(x = ai.90, y = n.leaf)) +
  geom_point(size = 3, shape = 21, fill = "grey", alpha = 0.7) +
  stat_function(aes(color = "0"), fun = function(x) (0.271*x + 0.920)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "10"), fun = function(x) (0.445*x + 0.874)^2,
                lwd = 2, alpha = 0.7) +
  stat_function(aes(color = "20"), fun = function(x) (0.619*x + 0.828)^2,
                lwd = 2, lty = 2, alpha = 0.7) +
  stat_function(aes(color = "30"), fun = function(x) (0.793*x + 0.781)^2,
                lwd = 2, lty = 2, lty = 2, alpha = 0.7) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(limits = c(0.25, 1.5), breaks = seq(0.25, 1.5, 0.25)) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = expression("C"[4]~"graminoid"),
       x = expression("AI"["90_day"]),
       y = expression("N"[mass]~"(g g"^-1~")"),
       color = "Soil N (ppm NO3-N)") +
  theme_bw(base_size = 18)

png("../working_drafts/TXeco_Nmass.png",
    width = 10, height = 8, units = 'in', res = 600)
ggarrange(nleaf.leg, nleaf.forb, nleaf.c3gram, nleaf.c4gram,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO")
dev.off()

unique(df$soil.no3n)

##########################################################################
## Coarse lmer for chi
##########################################################################
df$narea[c(235, 290, 509)] <- NA

chi <- lmer(chi ~ ai.90 * ai.15yr * soil.no3n * pft + (1 | NCRS.code),
              data = df)

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
