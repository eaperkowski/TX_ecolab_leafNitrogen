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
  filter(pft != "c3_shrub" & pft!= "c3_graminoid" & site != "Bell_2020_05" & 
           site != "Russel_2020_01")
df$narea.chi <- df$narea / df$chi

## Add colorblind friendly palette
cbbPalette <- c("#0077BB", "#33BBEE", "#009988", "#EE7733", "#CC3311")
cbbPalette2 <- c("#CC3311", "#EE7733", "#009988", "#33BBEE", "#0077BB")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_graminoid"])
length(df$pft[df$pft == "c4_graminoid"])
length(df$pft[df$pft == "legume"])
length(df$pft[df$pft == "c3_forb"])

##########################################################################
## Beta
##########################################################################
df$beta[c(62, 293)] <- NA


beta <- lmer(log(beta) ~ ai.60 * soil.no3n * pft + (1 | sampling.year) +
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


# Two-way interaction between ai.60 and pft
test(emtrends(beta, ~pft, "ai.60"))
emmeans(beta, ~pft, at = list(ai.60 = 0))

# Individual pft effect
emmeans(beta, pairwise~pft)


##########################################################################
## Beta plot
##########################################################################
beta.plot <- ggplot(data = df, aes(x = ai.60, y = beta)) +
  geom_hline(yintercept = 146, lty = 2, lwd = 1) +
  geom_jitter(aes(fill = pft),
              width = 0.02, size = 3, alpha = 0.7, shape = 21) +
  stat_function(fun = function(x) exp(0.524*x + 3.653), lwd = 2,
                xlim = c(0.3, 1.8), color = cbbPalette2[1]) +
  stat_function(fun = function(x) exp(-4.282*x + 4.824), lwd = 2,
                xlim = c(0.3, 1.8), color = cbbPalette2[2]) +
  stat_function(fun = function(x) exp(0.602*x + 3.221), lwd = 2, lty = 2,
                xlim = c(0.3, 1.8), color = cbbPalette2[3]) +
  scale_fill_manual(values = cbbPalette2, labels = c(expression("C"[3]~"forb"),
                                                     expression("C"[4]~"graminoid"),
                                                     expression("C"[3]~"legume"))) +
  scale_x_continuous(limits = c(0.3, 1.8), breaks = seq(0.3, 1.8, 0.3)) +
  scale_y_continuous(limits = c(-50, 600), breaks = seq(0, 600, 200)) +
  labs(x = "60-day P/PET",
       y = expression(ln~beta),
       fill = "Plant functional type") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0)


png("../working_drafts/TXeco_beta_b.png",
    width = 8, height = 4, units = 'in', res = 600)
beta
dev.off()

##########################################################################
## Chi
##########################################################################
df$chi[c(62, 83, 113, 132, 253, 254, 255, 271, 293, 295, 365,
         433, 436, 434, 437, 453, 455)] <- NA

chi <- lmer(chi ~ ai.15yr * soil.no3n * pft + (1 | sampling.year) +
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

# Two-way interaction between ai.15yr and pft
test(emtrends(chi, ~pft, "ai.15yr"))
emmeans(chi, ~pft, at = list(ai.15yr = 0))

# Individual effect of ai.15yr on chi
emmeans(chi, pairwise~pft)


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
## Narea
##########################################################################
df$narea[c(462)] <- NA

cor.test(df$chi, df$beta, method = "pearson")
cor.test(df$ai.15yr, df$beta)
## Note: beta and ai.15yr are not correlated, so it might be worth adding 
## both predictors to lmer

narea <- lmer(log(narea) ~ ai.15yr * log(beta) * soil.no3n * pft + 
                     (1 | sampling.year) + (1 | NCRS.code),
                   data = df)

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

