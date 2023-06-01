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
library(piecewiseSEM)
library(nlme)

# Turn off digit rounding in emmean args
emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../../TXeco/TXeco_data.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(pft != "c3_shrub") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb",
                             "c3_nonlegume", 
                             ifelse(pft == "c3_legume", 
                                    "c3_legume", 
                                    NA))),
         beta = ifelse(beta > 2000, NA, beta),
         beta = ifelse(pft == "c4_nonlegume" & beta > 400, NA, beta),
         beta = ifelse(pft == "c3_legume" & beta > 1000, NA, beta),
         marea = ifelse(marea > 1000, NA, marea))

## Add colorblind friendly palette
cbbPalette3 <- c("#DDAA33", "#BB5566", "#004488")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_legume"])
length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c4_nonlegume"])
df$pft <- factor(df$pft, levels = c("c3_legume", "c4_nonlegume", "c3_nonlegume"))

length(df$pft[df$pft == "c4_nonlegume" & !is.na(df$chi)])
length(unique(df$NCRS.code))

## How many annuals within c3 legume/c3 nonlegume?
length(df$pft[df$pft == "c3_nonlegume" & df$duration == "annual"])
length(df$pft[df$pft == "c3_legume" & df$duration == "annual"])

length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c3_nonlegume" | df$pft == "c3_legume"])
length(df$pft[df$pft == "c4_nonlegume"]) / 504

hist(df$beta)
hist(df$beta[df$pft == "c4_nonlegume"])

##########################################################################
## Beta
##########################################################################
df$beta[c(392, 411)] <- NA

beta <- lmer(sqrt(beta) ~ wn90_perc * soil.no3n * pft + (1 | NCRS.code), 
             data = df)

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

# Post-hoc comparisons
test(emtrends(beta, pairwise~pft, "wn90_perc"))
test(emtrends(beta, pairwise~pft, "soil.no3n"))

# Individual effects
test(emtrends(beta, ~1, "soil.no3n"))
test(emtrends(beta, ~1, "wn90_perc"))
emmeans(beta, pairwise~pft)

##########################################################################
## Chi
##########################################################################
df$chi[c(131, 371, 499)] <- NA
df$chi[c(401, 492)] <- NA
df$chi[c(175, 440, 443, 485)] <- NA
df$chi[c(221, 434)] <- NA

chi <- lmer(chi ~ (vpd90 + (wn90_perc * soil.no3n)) * pft + 
              (1 | NCRS.code), data = df)

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

## Post-hoc comparisons 
test(emtrends(chi, ~1, "vpd90"))

test(emtrends(chi, ~1, "wn90_perc"))
test(emtrends(chi, pairwise~pft, "wn90_perc"))

test(emtrends(chi, ~1, "soil.no3n"))
test(emtrends(chi, pairwise~pft, "soil.no3n"))

emmeans(chi, pairwise~pft)

##########################################################################
## Narea
##########################################################################
df$narea[df$narea > 10] <- NA
df$narea[c(274)] <- NA

# Fit model
narea <- lmer(log(narea) ~ (chi + (soil.no3n * wn90_perc)) * pft + (1 | NCRS.code),
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

## Post hoc comparisons
test(emtrends(narea, pairwise~pft, "chi"))
test(emtrends(narea, ~1, "soil.no3n"))
test(emtrends(narea, ~1, "wn90_perc"))

emmeans(narea, pairwise~pft)

##########################################################################
## Nmass
##########################################################################
df$n.leaf[493] <- NA

# Fit model
nmass <- lmer(log(n.leaf) ~ (chi + (soil.no3n * wn90_perc)) * pft + (1 | NCRS.code),
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

# Post hoc tests
test(emtrends(nmass, pairwise~pft, "chi"))
test(emtrends(nmass, ~wn90_perc, "soil.no3n", at = list(wn90_perc = seq(0.15,0.75,0.05))))
test(emtrends(nmass, ~1, "wn90_perc"))
test(emtrends(nmass, ~1, "soil.no3n"))
emmeans(nmass, pairwise~pft)

##########################################################################
## Marea
##########################################################################
df$marea[df$marea > 1000] <- NA
df$marea[c(20, 21, 274, 308)] <- NA

# Fit model
marea <- lmer(log(marea) ~ (chi + (soil.no3n * wn90_perc)) * pft + (1 | NCRS.code),
              data = df)

# Check model assumptions
plot(marea)
qqnorm(residuals(marea))
qqline(residuals(marea))
hist(residuals(marea))
densityPlot(residuals(marea))
shapiro.test(residuals(marea))
outlierTest(marea)

# Model output
round(summary(marea)$coefficients, digits = 3)
Anova(marea)
r.squaredGLMM(marea)

# Post-hoc comparisons
test(emtrends(marea, pairwise~pft, "chi"))


test(emtrends(marea, ~wn90_perc*pft, "soil.no3n",
              at = list(wn90_perc = seq(0.15, 0.75, 0.01))))
test(emtrends(marea, pairwise~pft, "chi"))
test(emtrends(marea, pairwise~pft, "soil.no3n"))

emmeans(marea, pairwise~pft)

##########################################################################
## Structural equation model
##########################################################################
df.psem <- df
df.psem$n.fixer <- ifelse(df.psem$n.fixer == "yes", 1, 0)
df.psem$photo <- ifelse(df.psem$photo == "c3", 1, 0)

df.psem$beta.trans <- sqrt(df$beta)
df.psem$narea.trans <- log(df$narea)
df.psem$nmass.trans <- log(df$n.leaf)
df.psem$marea.trans <- log(df$marea)

## Base Narea PSEM model (before optimization)
narea_psem_preopt <- psem(
  
  ## Narea model
  narea = lme(narea.trans ~ marea.trans + nmass.trans,
              random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Nmass model
  nmass = lme(nmass.trans ~ chi + soil.no3n + marea.trans + n.fixer,
              random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Marea model
  marea = lme(marea.trans ~ chi + soil.no3n,
              random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Chi model
  chi = lme(chi ~ beta.trans + vpd90, 
            random = ~ 1 | NCRS.code,
            data = df.psem, na.action = na.omit),
  
  ## Beta model
  beta = lme(beta.trans ~ soil.no3n + wn90_perc + photo + n.fixer,
             random = ~ 1 | NCRS.code, data = df.psem, 
             na.action = na.omit),
  
  ## Soil N model
  soiln = lme(soil.no3n ~ wn90_perc + perc.clay, random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Soil moisture
  soil.moisture = lme(wn90_perc ~ perc.clay, random = ~ 1 | NCRS.code, 
                      data = df.psem, na.action = na.omit))

summary(narea_psem_preopt)

## Optimized Narea PSEM model
narea_psem_opt <- psem(
  
  ## Narea model
  narea = lme(narea.trans ~ marea.trans + nmass.trans,
              random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Nmass model
  nmass = lme(nmass.trans ~ chi + beta.trans + soil.no3n + wn90_perc + 
                perc.clay + marea.trans + vpd90 + n.fixer + photo,
              random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Marea model
  marea = lme(marea.trans ~ chi + soil.no3n + photo,
              random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Chi model
  chi = lme(chi ~ beta.trans + wn90_perc + vpd90 + photo, 
            random = ~ 1 | NCRS.code,
            data = df.psem, na.action = na.omit),
  
  ## Beta model
  beta = lme(beta.trans ~ soil.no3n + wn90_perc + perc.clay + vpd90 + photo + n.fixer,
             random = ~ 1 | NCRS.code, data = df.psem, 
             na.action = na.omit),
  
  ## Soil N model
  soiln = lme(soil.no3n ~ wn90_perc + perc.clay + vpd90, random = ~ 1 | NCRS.code, 
              data = df.psem, na.action = na.omit),
  
  ## Soil moisture
  soil.moisture = lme(wn90_perc ~ perc.clay + vpd90, random = ~ 1 | NCRS.code, 
                      data = df.psem, na.action = na.omit))
summary(narea_psem_opt)
plot(narea_psem_opt)

line.thick <- data.frame(summary(narea_psem_opt)$coefficients,
           line.thickness = abs(summary(
             narea_psem_opt)$coefficients$Std.Estimate) * 16.67)

line.thick <- line.thick %>%
  mutate(line.thickness = round(line.thickness, digits = 2)) %>%
  dplyr::select(-Var.9)

ggplot(data=line.thick, aes(x = abs(Std.Estimate), 
                            y = line.thickness)) + 
  geom_smooth() +
  geom_point()
  #scale_y_continuous(limits = c(0,15.2), breaks = seq(0, 15, 5))

##########################################################################
## Tables
##########################################################################
## Table 2 (Coefficients + model results summary)
beta.coefs <- data.frame(summary(beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef) %>%
  filter(treatment == "(Intercept)" | treatment == "wn90_perc" | 
           treatment == "soil.no3n" | treatment == "wn90_perc:soil.no3n") %>%
  mutate(coef = ifelse(coef <0.001 & coef >= 0, "<0.001", coef)) %>%
  print(., row.names = FALSE)

table2 <- data.frame(Anova(beta)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(beta.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "wn90_perc",
                                                  "soil.no3n",
                                                  "pft",
                                                  "wn90_perc:soil.no3n",
                                                  "wn90_perc:pft",
                                                  "soil.no3n:pft",
                                                  "wn90_perc:soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef, Chisq, P_value) %>%
  arrange(treatment)  %>%
  replace(is.na(.), "-")

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

## Table 3 (Coefficients and model summary)
chi.coefs <- data.frame(summary(chi)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nobeta = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "vpd90" |
           treatment == "wn90_perc" | 
           treatment == "soil.no3n" | 
           treatment == "wn90_perc:soil.no3n") %>%
  mutate(coef.nobeta = ifelse(coef.nobeta <0.001 & coef.nobeta >= 0, 
                              "<0.001", coef.nobeta)) %>%
  print(., row.names = FALSE)

table3 <- data.frame(Anova(chi)) %>% 
  mutate(treatment = row.names(.),
         Chisq.nobeta = round(Chisq, 3),
         P_value.nobeta = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(chi.coefs) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("(Intercept)", "vpd90", "wn90_perc",
                                       "soil.no3n", "pft", "wn90_perc:soil.no3n",
                                       "vpd90:pft",  "wn90_perc:pft",
                                       "soil.no3n:pft", "wn90_perc:soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nobeta, Chisq.nobeta, P_value.nobeta) %>%
  arrange(treatment)  %>%
  replace(is.na(.), "-")

write.csv(table3, "../working_drafts/tables/TXeco_table3_chi.csv", 
          row.names = FALSE)


## Table 4 (Coefficients + model results summary)
narea.coefs <- data.frame(summary(narea)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.narea = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef.narea) %>%
  filter(treatment == "(Intercept)" | 
           treatment == "chi" | treatment == "soil.no3n" | 
           treatment == "wn90_perc" | 
           treatment == "soil.no3n:wn90_perc") %>%
  mutate(coef.narea = ifelse(coef.narea <0.001 & coef.narea >= 0,
                       "<0.001", coef.narea)) %>%
  print(., row.names = FALSE)

narea.table <- data.frame(Anova(narea)) %>%
  mutate(treatment = row.names(.),
         Chisq.narea = round(Chisq, 3),
         P_value.narea = ifelse(Pr..Chisq. < 0.001, "<0.001",
                                 round(Pr..Chisq., 3))) %>%
  full_join(narea.coefs) %>%
  mutate(treatment = factor(
    treatment, levels = c("(Intercept)", "chi", "soil.no3n",
                          "wn90_perc", "pft", "soil.no3n:wn90_perc",
                          "chi:pft", "soil.no3n:pft",
                          "wn90_perc:pft", "soil.no3n:wn90_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.narea, Chisq.narea, P_value.narea) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

nmass.coefs <- data.frame(summary(nmass)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nmass = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef.nmass) %>%
  filter(treatment == "(Intercept)" | treatment == "chi" | 
           treatment == "soil.no3n" | 
           treatment == "wn90_perc" | 
           treatment == "soil.no3n:wn90_perc") %>%
  mutate(coef.nmass = ifelse(coef.nmass <0.001 & coef.nmass >= 0,
                       "<0.001", coef.nmass)) %>%
  print(., row.names = FALSE)

nmass.table <- data.frame(Anova(nmass)) %>%
  mutate(treatment = row.names(.),
         Chisq.nmass = round(Chisq, 3),
         P_value.nmass = ifelse(Pr..Chisq. < 0.001, "<0.001",
                                round(Pr..Chisq., 3))) %>%
  full_join(nmass.coefs) %>%
  mutate(treatment = factor(
    treatment, levels = c("(Intercept)", "chi", "soil.no3n",
                          "wn90_perc", "pft", "soil.no3n:wn90_perc",
                          "chi:pft", "soil.no3n:pft",
                          "wn90_perc:pft", "soil.no3n:wn90_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nmass, Chisq.nmass, P_value.nmass) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

marea.coefs <- data.frame(summary(marea)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.marea = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef.marea) %>%
  filter(treatment == "(Intercept)" | 
           treatment == "chi" | treatment == "soil.no3n" | 
           treatment == "wn90_perc" | 
           treatment == "soil.no3n:wn90_perc") %>%
  mutate(coef.marea = ifelse(coef.marea <0.001 & coef.marea >= 0,
                             "<0.001", coef.marea)) %>%
  print(., row.names = FALSE)

marea.table <- data.frame(Anova(marea)) %>%
  mutate(treatment = row.names(.),
         Chisq.marea = round(Chisq, 3),
         P_value.marea = ifelse(Pr..Chisq. < 0.001, "<0.001",
                                round(Pr..Chisq., 3))) %>%
  full_join(marea.coefs) %>%
  mutate(treatment = factor(
    treatment, levels = c("(Intercept)", "chi", "soil.no3n",
                          "wn90_perc", "pft", "soil.no3n:wn90_perc",
                          "chi:pft", "soil.no3n:pft",
                          "wn90_perc:pft", "soil.no3n:wn90_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.marea, Chisq.marea, P_value.marea) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

table4 <- narea.table %>% full_join(nmass.table) %>% 
  full_join(marea.table) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

write.csv(table4, "../working_drafts/tables/TXeco_table4_leafN.csv", 
          row.names = FALSE)


## Table 5 (SEM results)
table5.coefs <- summary(narea_psem_opt)$coefficients[, c(1:8)] %>%
  as.data.frame() %>%
  mutate(Std.Error = ifelse(Std.Error == "-", NA, Std.Error),
         across(Estimate:Std.Estimate, as.numeric),
         across(Estimate:Std.Estimate, round, 3),
         p_val = ifelse(P.Value < 0.001, "<0.001", P.Value),
         Std.Estimate = round(Std.Estimate, digits = 3)) %>%
  dplyr::select(resp = Response, pred = Predictor, std_est = Std.Estimate, 
                p_val)

table5 <- summary(narea_psem_opt)$R2 %>%
  dplyr::select(resp = Response, r2_marg = Marginal,
                r2_cond = Conditional) %>%
  full_join(table5.coefs) %>%
  dplyr::select(resp, pred, r2_marg, r2_cond, std_est:p_val)


write.csv(table5, "../working_drafts/tables/TXeco_table5_SEMclean.csv", 
          row.names = FALSE)


## Mean and standard deviation of beta
min(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
max(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
mean(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
median(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
sd(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)

min(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
max(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
mean(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
median(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
sd(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
