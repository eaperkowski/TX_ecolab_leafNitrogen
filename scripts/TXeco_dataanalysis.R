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
df$vpd4 <- df$vpd4 / 10

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
emmeans(beta, ~1, "wn3", at = list(wn = 0))

# Individual effect of soil NO3-N
test(emtrends(beta, ~1, "soil.no3n"))

# PFT-only effect
emmeans(beta, pairwise~pft)

##########################################################################
## Chi w/o beta
##########################################################################
df.chi.nobeta <- df

df.chi.nobeta$chi[c(62, 117, 315, 317, 481)] <- NA
df.chi.nobeta$chi[c(456, 483)] <- NA
df.chi.nobeta$chi[c(284, 292, 484)] <- NA

chi <- lmer(chi ~ (vpd4 + tavg4 + (wn3 * soil.no3n)) * pft + 
              (1 | NCRS.code), data = df.chi.nobeta)

# Check model assumptions
plot(chi)
qqnorm(residuals(chi))
qqline(residuals(chi))
densityPlot(residuals(chi))
shapiro.test(residuals(chi))
outlierTest(chi)

# Model output
round(summary(chi)$coefficients, digits = 3)
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
df.chi.beta <- df

df.chi.beta$chi[c(62, 84, 86, 308, 315, 317)] <- NA
df.chi.beta$chi[c(304, 456, 483)] <- NA
df.chi.beta$chi[c(402, 481, 484)] <- NA

chi.beta <- lmer(chi ~ (vpd4 + tavg4 + beta) * pft + 
              (1 | NCRS.code), data = df.chi.beta)

# Check model assumptions
plot(chi.beta)
qqnorm(residuals(chi.beta))
qqline(residuals(chi.beta))
densityPlot(residuals(chi.beta))
shapiro.test(residuals(chi.beta))
outlierTest(chi.beta)

# Model output
round(summary(chi.beta)$coefficients, digits = 3)
Anova(chi.beta)
r.squaredGLMM(chi.beta)

# Two way interaction between vpd and pft
test(emtrends(chi.beta, ~pft, "vpd4"))

# Two way interaction between vpd and pft
test(emtrends(chi.beta, ~pft, "tavg4"))

# Two way interaction between vpd and pft
test(emtrends(chi.beta, pairwise~pft, "beta"))

##########################################################################
## Narea - direct effects
##########################################################################
# Remove outliers (Bonferroni p<0.05 condition)
df$narea[df$narea > 10] <- NA
df$narea[c(76, 80, 156, 273, 382)] <- NA
df$narea[509] <- NA

# Fit model
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
round(summary(narea)$coefficients, digits = 3)
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
library(lavaan)

df.sem <- df
df.sem$narea[df$narea > 10] <- NA
df.sem$narea[c(76, 80, 156, 273, 382, 509)] <- NA
df.sem$chi[c(62, 84, 86, 308, 315, 317, 304, 456, 483,
         402, 481, 484)] <- NA
df.sem$beta[c(84)] <- NA

models <- ' # regressions
            narea ~ beta + chi + soil.no3n + pft
            beta ~ wn3 + soil.no3n + pft
            chi ~ vpd4 + beta + tavg4 + pft
            vpd4 ~ tavg4
            soil.no3n ~ wn3'

test_fit <- sem(models, data = df.sem)
lavaan::summary(test_fit, fit.measures = TRUE)

summary.coefs <- summary(test_fit)$pe[c(1:13),]
summary.coefs$linesize <- abs(summary.coefs$z)
summary.coefs$linesize_std <- scale(summary.coefs$linesize) * 2 + 4

summary.coefs[,5:10] <- round(summary.coefs[,5:10], digits = 3)
write.csv(summary.coefs,
          "../working_drafts/tables/TXeco_SEM_results.csv",
          row.names = FALSE)

##########################################################################
## Tables
##########################################################################
## Table 2 (Coefficients + model results summary)
beta.coefs <- data.frame(summary(beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef = round(Estimate, digits = 3),
         se = round(Std..Error, digits = 3)) %>%
  dplyr::select(treatment, coef, se, t.value) %>%
  filter(treatment == "(Intercept)" | treatment == "wn3" | 
           treatment == "soil.no3n" | treatment == "wn3:soil.no3n") %>%
  mutate(coef = ifelse(coef <0.001 & coef >= 0, "<0.001", coef)) %>%
  print(., row.names = FALSE)

table2 <- data.frame(Anova(beta)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(beta.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "wn3",
                                                  "soil.no3n",
                                                  "pft",
                                                  "wn3:soil.no3n",
                                                  "wn3:pft",
                                                  "soil.no3n:pft",
                                                  "wn3:soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef, Chisq, P_value) %>%
  arrange(treatment) %>%
  mutate(df = replace(df, is.na(df), "-"),
         coef = replace(coef, is.na(coef), "-"),
         Chisq = replace(Chisq, is.na(Chisq), "-"),
         P_value = replace(P_value, is.na(P_value), "-"))

table2$treatment <- c("Intercept", 
                      "Soil moisture (SM)", 
                      "Soil N (N)", 
                      "PFT",
                      "SM * N",
                      "SM * PFT",
                      "N * PFT", 
                      "SM * N * PFT")

write.csv(table2, "../working_drafts/tables/TXeco_table2_beta.csv", 
          row.names = FALSE)

## Table 3 (Coefficients + model results summary for both chi models 
## (w/ and w/o beta))
chi.nobeta.coefs <- data.frame(summary(chi)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nobeta = round(Estimate, digits = 3),
         se.nobeta = round(Std..Error, digits = 3),
         t.value.nobeta = round(t.value, digits = 3)) %>%
  dplyr::select(treatment, coef.nobeta, se.nobeta, t.value.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "vpd4" | 
           treatment == "wn3" | treatment == "soil.no3n" | 
           treatment == "wn3:soil.no3n") %>%
  mutate(coef.nobeta = ifelse(coef.nobeta <0.001 & coef.nobeta >= 0, 
                              "<0.001", coef.nobeta)) %>%
  print(., row.names = FALSE)

chi.beta.coefs <- data.frame(summary(chi.beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.beta = round(Estimate, digits = 3),
         se.beta = round(Std..Error, digits = 3),
         t.value.beta = round(t.value, digits = 3)) %>%
  dplyr::select(treatment, coef.beta, se.beta, t.value.beta) %>%
  filter(treatment == "(Intercept)" | treatment == "vpd4" | 
           treatment == "tavg4" | treatment == "beta") %>%
  mutate(coef.beta = ifelse(coef.beta <0.001 & coef.beta >= 0, 
                       "<0.001", 
                       coef.beta)) %>%
  print(., row.names = FALSE)

chi.nobeta.table3 <- data.frame(Anova(chi)) %>% 
  mutate(treatment = row.names(.),
         Chisq.nobeta = round(Chisq, 3),
         P_value.nobeta = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(chi.nobeta.coefs) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("(Intercept)", "vpd4", "tavg4", "wn3",
                                       "soil.no3n", "pft", "wn3:soil.no3n",
                                       "vpd4:pft", "tavg4:pft", "wn3:pft",
                                       "soil.no3n:pft", "wn3:soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nobeta, Chisq.nobeta, P_value.nobeta) %>%
  arrange(treatment)

chi.beta.table3 <- data.frame(Anova(chi.beta)) %>% 
  mutate(treatment = row.names(.),
         Chisq.beta = round(Chisq, 3),
         P_value.beta = ifelse(Pr..Chisq. < 0.001, "<0.001",
                                 round(Pr..Chisq., 3))) %>%
  full_join(chi.beta.coefs) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("(Intercept)", "vpd4", "tavg4", "beta", 
                                       "pft", 
                                       "vpd4:pft", "tavg4:pft", "beta:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.beta, Chisq.beta, P_value.beta) %>%
  arrange(treatment)


table3 <- chi.beta.table3 %>% full_join(chi.nobeta.table3) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("(Intercept)", "vpd4", "tavg4", "beta",
                                       "wn3", "soil.no3n", "pft", "wn3:soil.no3n",
                                       "vpd4:pft", "tavg4:pft", "beta:pft", "wn3:pft",
                                       "soil.no3n:pft", "wn3:soil.no3n:pft"))) %>%
  arrange(treatment) %>%
  mutate(df = replace(df, is.na(df), "-"),
         coef.beta = replace(coef.beta, is.na(coef.beta), "-"),
         coef.nobeta = replace(coef.nobeta, is.na(coef.nobeta), "-"),
         Chisq.beta = replace(Chisq.beta, is.na(Chisq.beta), "-"),
         Chisq.nobeta = replace(Chisq.nobeta, is.na(Chisq.nobeta), "-"),
         P_value.beta = replace(P_value.beta, is.na(P_value.beta), "-"),
         P_value.nobeta = replace(P_value.nobeta, is.na(P_value.nobeta), "-"))

table3$treatment <- c("Intercept", "VPD", "Temperature (T)", 
                      "Unit cost ratio (beta)", "Soil moisture", "Soil N",
                      "PFT", "SM * N", "VPD * PFT", "T * PFT", "beta * PFT",
                      "SM * PFT", "N * PFT", "SM * N * PFT")
names(table3) <- c("Treatment", "df", "Coefficient", "chi-square", "P-value", 
                   "Coefficient", "chi-square", "P-value")


write.csv(table3, "../working_drafts/tables/TXeco_table3_chi.csv", 
          row.names = FALSE)


## Table 4 (Coefficients + model results summary)
narea.coefs <- data.frame(summary(narea)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef = round(Estimate, digits = 3),
         se = round(Std..Error, digits = 3)) %>%
  dplyr::select(treatment, coef, se, t.value) %>%
  filter(treatment == c("(Intercept)", "beta", "chi", "soil.no3n")) %>%
  print(., row.names = FALSE)

table4 <- data.frame(Anova(narea)) %>%
  mutate(treatment = factor(row.names(.), 
                            levels = c("(Intercept)", "beta", "chi",
                                       "soil.no3n", "pft", "beta:pft",
                                       "chi:pft", "soil.no3n:pft")),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(narea.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "beta",
                                                  "chi",
                                                  "soil.no3n",
                                                  "pft",
                                                  "beta:pft",
                                                  "chi:pft",
                                                  "soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef, Chisq, P_value) %>%
  arrange(treatment) %>%
  mutate(df = replace(df, is.na(df), "-"),
         coef = replace(coef, is.na(coef), "-"),
         Chisq = replace(Chisq, is.na(Chisq), "-"),
         P_value = replace(P_value, is.na(P_value), "-"))

table4$treatment <- c("Intercept", "Unit cost ratio (beta)", "chi",
                      "Soil N (N)", "PFT", "beta * PFT",
                      "chi * PFT", "N * PFT")

write.csv(table4, "../working_drafts/tables/TXeco_table4_leafN.csv", 
          row.names = FALSE)


