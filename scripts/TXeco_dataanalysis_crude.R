##########################################################################
## Load libraries and import data
##########################################################################
# Libraries
library(lme4)
library(emmeans)
library(car)
library(tidyverse)
library(dplyr)
library(MuMIn)
library(multcomp)
library(multcompView)
library(sjPlot)

emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN"))
df$narea.chi <- df$n.area / df$chi

unique(df$pft)

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
