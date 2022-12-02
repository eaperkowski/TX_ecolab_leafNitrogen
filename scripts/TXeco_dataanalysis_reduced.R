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
library(lavaan)
library(piecewiseSEM)
library(semEff)
library(nlme)

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
df.sem <- df

## Add colorblind friendly palette
cbbPalette <- c("#0077BB", "#33BBEE", "#009988", "#EE7733", "#CC3311")
cbbPalette2 <- c("#CC3311", "#EE7733", "#009988", "#33BBEE", "#0077BB")
cbbPalette3 <- c("#DDAA33", "#BB5566", "#004488")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_legume"])
length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c4_nonlegume"])
df$pft <- factor(df$pft, levels = c("c3_legume", "c4_nonlegume", "c3_nonlegume"))

## Convert VPD from hPa (PRISM units) to kPa (standard)
df$vpd4 <- df$vpd4 / 10

##########################################################################
## Beta
##########################################################################
df$beta[c(84, 117)] <- NA


beta <- lmer(log(beta) ~ wn3_perc * soil.no3n * pft + (1 | NCRS.code), data = df)

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
test(emtrends(beta, ~pft, "wn3_perc"))
test(emtrends(beta, pairwise~pft, "soil.no3n"))

# Individual effects
test(emtrends(beta, ~1, "wn3_perc"))
test(emtrends(beta, ~pft, "soil.no3n"))
emmeans(beta, pairwise~pft)

##########################################################################
## Chi
##########################################################################
df$chi[c(117, 322, 481)] <- NA
df$chi[c(62, 315)] <- NA
df$chi[c(456)] <- NA
df$chi[c(317, 483)] <- NA
df$chi[c(292, 484)] <- NA
df$chi[284] <- NA
df$chi[484] <- NA

chi <- lmer(chi ~ (vpd4 + tavg4 + (wn3_perc * soil.no3n)) * pft + 
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
test(emtrends(chi, pairwise~pft, "wn3_perc"))
test(emtrends(chi, pairwise~pft, "tavg4"))
test(emtrends(chi, pairwise~pft, "vpd4"))
emmeans(chi, pairwise~pft)

test(emtrends(chi, ~1, "vpd4"))
test(emtrends(chi, ~1, "wn3_perc"))

##########################################################################
## Narea
##########################################################################
df$narea[df$narea > 10] <- NA
df$narea[509] <- NA

# Fit model
narea <- lmer(log(narea) ~ (beta + chi + (soil.no3n * wn3_perc)) * pft + (1 | NCRS.code),
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

## Post hoc comparisons
test(emtrends(narea, ~1, "beta"))
test(emtrends(narea, pairwise~pft, "beta"))

test(emtrends(narea, 
              ~wn3_perc, "soil.no3n", 
              at = list(wn3_perc = seq(0, 1, 0.1))))

emmeans(narea, pairwise~pft)

##########################################################################
## Structural equation model
##########################################################################
df.sem$beta[c(84, 117)] <- NA
df.sem$chi[c(117, 322, 481)] <- NA
df.sem$chi[c(62, 315)] <- NA
df.sem$chi[c(456)] <- NA
df.sem$chi[c(317, 483)] <- NA
df.sem$chi[c(292, 484)] <- NA
df.sem$chi[284] <- NA
df.sem$chi[484] <- NA
df.sem$narea[df.sem$narea > 10] <- NA
df.sem$narea[509] <- NA

df.sem$n.fixer <- ifelse(df.sem$n.fixer == "yes", 1, 0)
df.sem$photo <- ifelse(df$photo == "c3", 1, 0)

## Run PSEM model
test_psem <- psem(
  
  ## Narea model
  narea = lme(narea ~ beta + chi + soil.no3n + wn3_perc + photo + n.fixer +
                tavg4 + vpd4,
              random = ~ 1 | NCRS.code, 
              data = df.sem, na.action = na.omit),
  
  ## Chi model
  chi = lme(chi ~ vpd4 + tavg4 + photo, random = ~ 1 | NCRS.code,
            data = df.sem, na.action = na.omit),
  
  ## Beta model
  beta = lme(beta ~ soil.no3n + wn3_perc + chi + n.fixer + vpd4 + tavg4,
             random = ~ 1 | NCRS.code, data = df.sem, 
             na.action = na.omit),
  
  ## Soil N model
  soiln = lme(soil.no3n ~ wn3_perc, random = ~ 1 | NCRS.code, 
              data = df.sem, na.action = na.omit),
  
  ## Temperature model
  vpd = lme(vpd4 ~ tavg4, random = ~ 1 | NCRS.code, data = df.sem, 
            na.action = na.omit),
  
  ## Correlated errors (i.e. relationship is not presumed to
  ## be causally linked)
  beta %~~% chi,
  beta %~~% vpd4)

summary(test_psem)

summary(semEff(psem_boot), c("narea"))
summary(semEff(psem_boot), "beta")
summary(semEff(psem_boot), "chi")


##########################################################################
## Tables
##########################################################################
## Table 2 (Coefficients + model results summary)
beta.coefs <- data.frame(summary(beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef = format(Estimate, scientific = TRUE, digits = 3),
         se = round(Std..Error, digits = 3)) %>%
  dplyr::select(treatment, coef, se, t.value) %>%
  filter(treatment == "(Intercept)" | treatment == "wn3_perc" | 
           treatment == "soil.no3n" | treatment == "wn3_perc:soil.no3n") %>%
  mutate(coef = ifelse(coef <0.001 & coef >= 0, "<0.001", coef)) %>%
  print(., row.names = FALSE)

table2 <- data.frame(Anova(beta)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(beta.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "wn3_perc",
                                                  "soil.no3n",
                                                  "pft",
                                                  "wn3_perc:soil.no3n",
                                                  "wn3_perc:pft",
                                                  "soil.no3n:pft",
                                                  "wn3_perc:soil.no3n:pft"))) %>%
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
         coef.nobeta = format(Estimate, scientific = TRUE, digits = 3),
         se.nobeta = round(Std..Error, digits = 3),
         t.value.nobeta = round(t.value, digits = 3)) %>%
  dplyr::select(treatment, coef.nobeta, se.nobeta, t.value.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "vpd4" |
           treatment == "tavg4" | treatment == "wn3_perc" | 
           treatment == "soil.no3n" | 
           treatment == "wn3_perc:soil.no3n") %>%
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
                            levels = c("(Intercept)", "vpd4", "tavg4", "wn3_perc",
                                       "soil.no3n", "pft", "wn3_perc:soil.no3n",
                                       "vpd4:pft", "tavg4:pft", "wn3_perc:pft",
                                       "soil.no3n:pft", "wn3_perc:soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nobeta, Chisq.nobeta, P_value.nobeta) %>%
  arrange(treatment)  %>%
  replace(is.na(.), "-")

write.csv(table3, "../working_drafts/tables/TXeco_table3_chi.csv", 
          row.names = FALSE)


## Table 4 (Coefficients + model results summary)
narea.coefs <- data.frame(summary(narea)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nobeta = format(Estimate, scientific = TRUE, digits = 3),
         se.nobeta = round(Std..Error, digits = 3),
         t.value.nobeta = round(t.value, digits = 3)) %>%
  dplyr::select(treatment, coef.nobeta, se.nobeta, t.value.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "beta" |
           treatment == "chi" | treatment == "soil.no3n" | 
           treatment == "wn3_perc" | 
           treatment == "soil.no3n:wn3_perc") %>%
  mutate(coef.nobeta = ifelse(coef.nobeta <0.001 & coef.nobeta >= 0, 
                              "<0.001", coef.nobeta)) %>%
  print(., row.names = FALSE)

table4 <- data.frame(Anova(narea)) %>%
  mutate(treatment = row.names(.),
         Chisq.nobeta = round(Chisq, 3),
         P_value.nobeta = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(narea.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "beta",
                                                  "chi",
                                                  "soil.no3n",
                                                  "wn3_perc",
                                                  "pft",
                                                  "soil.no3n:wn3_perc",
                                                  "beta:pft",
                                                  "chi:pft",
                                                  "soil.no3n:pft",
                                                  "wn3_perc:pft",
                                                  "soil.no3n:wn3_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nobeta, Chisq.nobeta, P_value.nobeta) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

write.csv(table4, "../working_drafts/tables/TXeco_table4_leafN.csv", 
          row.names = FALSE)


## Table 5 (SEM results)

coefs(test_psem, standardize = "scale")
dSep(test_psem)
rsquared(test_psem)

table5.coefs <- summary(test_psem)$coefficients[c(1:15), c(1:8)] %>%
  as.data.frame() %>%
  mutate(Std.Error = ifelse(Std.Error == "-", NA, Std.Error),
         across(Estimate:Std.Estimate, as.numeric),
         z_score = Estimate / Std.Error,
         p_val = 2*pnorm(q=z_score, lower.tail = FALSE),
         p_val = ifelse(p_val > 1, 2-p_val, p_val),
         across(Estimate:p_val, round, 3),
         p_val = ifelse(p_val < 0.001, "<0.001", p_val),
         linesize = abs(scale(z_score) + abs(scale(Std.Estimate))*log(60))) %>%
  dplyr::select(resp = Response, pred = Predictor, std_est = Std.Estimate, 
         z_score, p_val, linesize)

table5 <- summary(test_psem)$R2 %>%
  dplyr::select(resp = Response, r2_marg = Marginal,
                r2_cond = Conditional) %>%
  full_join(table5.coefs) %>%
  dplyr::select(resp, pred, r2_marg, r2_cond, std_est:p_val)


write.csv(table5, "../working_drafts/tables/TXeco_table5_SEMclean.csv", 
          row.names = FALSE)
