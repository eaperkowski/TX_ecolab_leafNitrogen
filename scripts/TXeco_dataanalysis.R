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
df$beta[c(84, 317, 500)] <- NA

beta <- lmer(log(beta) ~ wn90_perc * soil.no3n * pft + (1 | NCRS.code), data = df)

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
test(emtrends(beta, ~pft, "wn90_perc"))
test(emtrends(beta, pairwise~pft, "soil.no3n"))

# Individual effects
test(emtrends(beta, ~1, "wn90_perc", regrid = "response"))
test(emtrends(beta, ~pft, "soil.no3n"))
emmeans(beta, pairwise~pft)

##########################################################################
## Chi w/o beta
##########################################################################
df.chi.nobeta <- df

df.chi.nobeta$chi[c(62, 117, 275, 317, 481)] <- NA
df.chi.nobeta$chi[c(276, 304, 322, 500)] <- NA
df.chi.nobeta$chi[c(315, 456, 480, 502)] <- NA
df.chi.nobeta$chi[c(71, 292, 483)] <- NA
df.chi.nobeta$chi[484] <- NA
df.chi.nobeta$chi[402] <- NA

chi <- lmer(chi ~ (vpd4 + tavg4 + (wn90_perc * soil.no3n)) * pft + 
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

## Post-hoc comparisons 
test(emtrends(chi, ~pft, "wn90_perc"))
test(emtrends(chi, ~pft, "tavg4"))
test(emtrends(chi, ~pft, "vpd4"))
emmeans(chi, pairwise~pft)

test(emtrends(chi, ~soil.no3n*pft, "wn90_perc",
              at = list(soil.no3n = c(0, 40, 80))))

##########################################################################
## Chi w/ beta
##########################################################################
df.chi.beta <- df

df.chi.beta$chi[c(62, 84, 86, 304, 308, 456)] <- NA
df.chi.beta$chi[c(322, 483)] <- NA
df.chi.beta$chi[c(315, 484)] <- NA
df.chi.beta$chi[c(220, 402)] <- NA

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

# Post-hoc comparisons
test(emtrends(chi.beta, ~1, "vpd4"))
test(emtrends(chi.beta, ~pft, "vpd4"))
test(emtrends(chi.beta, pairwise~pft, "beta"))
test(emtrends(chi.beta, ~pft, "tavg4"))
emmeans(chi.beta, pairwise~pft)

##########################################################################
## Narea with beta
##########################################################################
df.narea.beta <- df

# Remove outliers (Bonferroni p<0.05 condition)
df.narea.beta$narea[df.narea.beta$narea > 10] <- NA
df.narea.beta$narea[509] <- NA

# Fit model
narea.beta <- lmer(log(narea) ~ (beta + chi + soil.no3n) * pft + (1 | NCRS.code),
              data = df.narea.beta)

# Check model assumptions
plot(narea.beta)
qqnorm(residuals(narea.beta))
qqline(residuals(narea.beta))
hist(residuals(narea.beta))
densityPlot(residuals(narea.beta))
shapiro.test(residuals(narea.beta))
outlierTest(narea.beta)

# Model output
round(summary(narea.beta)$coefficients, digits = 3)
Anova(narea.beta)
r.squaredGLMM(narea.beta)

## Post hoc comparisons
test(emtrends(narea.beta, ~pft, "beta"))
test(emtrends(narea.beta, ~pft, "chi"))
emmeans(narea.beta, pairwise~pft)

##########################################################################
## Narea without beta
##########################################################################
df.narea.nobeta <- df

# Remove outliers (Bonferroni p<0.05 condition)
df.narea.nobeta$narea[df.narea.nobeta$narea > 10] <- NA
df.narea.nobeta$narea[509] <- NA

# Fit model
narea.nobeta <- lmer(log(narea) ~ (chi + (soil.no3n * wn90_perc)) * 
                       pft + (1 | NCRS.code), data = df.narea.nobeta)

# Check model assumptions
plot(narea.nobeta)
qqnorm(residuals(narea.nobeta))
qqline(residuals(narea.nobeta))
hist(residuals(narea.nobeta))
densityPlot(residuals(narea.nobeta))
shapiro.test(residuals(narea.nobeta))
outlierTest(narea.nobeta)

# Model output
round(summary(narea.nobeta)$coefficients, digits = 3)
Anova(narea.nobeta)
r.squaredGLMM(narea.nobeta)

# Pairwise comparisons
test(emtrends(narea.nobeta, ~1, "chi"))
test(emtrends(narea.nobeta, ~1, "soil.no3n"))
test(emtrends(narea.nobeta, ~pft, "chi"))
emmeans(narea.nobeta, pairwise~pft)

##########################################################################
## Structural equation model
##########################################################################
df.sem <- df

df.sem$beta[c(84, 317, 500)] <- NA
df.sem$chi[c(62, 84, 86, 304, 308, 456)] <- NA
df.sem$chi[c(322, 483)] <- NA
df.sem$chi[c(315, 484)] <- NA
df.sem$chi[c(220, 402)] <- NA
df.sem$narea[df.sem$narea > 10] <- NA
df.sem$narea[509] <- NA


## Standardize and center vars
df.sem$beta.std <- scale(df.sem$beta)
df.sem$no3n.std <- scale(df.sem$soil.no3n)
df.sem$wn90.std <- scale(df.sem$wn90_perc)
df.sem$vpd4.std <- scale(df.sem$vpd4)
df.sem$tavg4.std <- scale(df.sem$tavg4)
df.sem$chi.std <- scale(df.sem$chi)
df.sem$narea.std <- scale(df.sem$narea)

## Add models to be tested in SEM
models <- ' # regressions
            narea.std ~ b*beta.std + chi.std + no3n.std + pft
            beta.std ~ c*wn90.std + a*no3n.std + pft
            chi.std ~ f*vpd4.std + tavg4.std + pft
            vpd4.std ~ e*tavg4.std
            no3n.std ~ d*wn90.std

            # covariates
            beta.std ~~ chi.std

            # indirect effect of soil N and soil moisture on leaf N
            soiln.indirect:=a*b
            moisture.indirect:=c*b
            moisture.indirect2:=d*a*b
            temp.chi.indirect:=e*f'

test_fit <- sem(models, data = df.sem)
summary(test_fit, standardized = TRUE,
        ci = TRUE, fit.measures = TRUE)
fitMeasures(test_fit, c("cfi", "rmsea", "srmr"))


summary.coefs <- data.frame(summary(test_fit, standardized = TRUE,
                                    ci = TRUE, fit.measures = TRUE)$pe[c(1:13, 25:28),])
summary.coefs$line <- abs(summary.coefs$est)
summary.coefs$line_std <- scale(summary.coefs$line) * 2 + 4

summary.coefs[,c(5:16)] <- round(summary.coefs[, c(5:16)], digits = 3)
summary.coefs <- summary.coefs %>% 
  mutate(se = ifelse(se < 0.001 & se >= 0, "<0.001", se),
         pvalue = ifelse(pvalue < 0.001 & pvalue >= 0, "<0.001", pvalue)) %>%
  dplyr::select(resp = lhs, pred = rhs, everything(),
                -op, -label, -exo, -std.nox)

write.csv(summary.coefs,
          "../working_drafts/tables/TXeco_tableS2_SEMresults.csv",
          row.names = FALSE)

##########################################################################
## Tables
##########################################################################
## Table 2 (Coefficients + model results summary)
beta.coefs <- data.frame(summary(beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef = format(Estimate, scientific = TRUE, digits = 3),
         se = round(Std..Error, digits = 3)) %>%
  dplyr::select(treatment, coef, se, t.value) %>%
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

## Table 3 (Coefficients + model results summary for both chi models 
## (w/ and w/o beta))
chi.nobeta.coefs <- data.frame(summary(chi)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nobeta = format(Estimate, scientific = TRUE, digits = 3),
         se.nobeta = round(Std..Error, digits = 3),
         t.value.nobeta = round(t.value, digits = 3)) %>%
  dplyr::select(treatment, coef.nobeta, se.nobeta, t.value.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "vpd4" | 
           treatment == "wn90_perc" | treatment == "soil.no3n" | 
           treatment == "wn90_perc:soil.no3n") %>%
  mutate(coef.nobeta = ifelse(coef.nobeta <0.001 & coef.nobeta >= 0, 
                              "<0.001", coef.nobeta)) %>%
  print(., row.names = FALSE)

chi.beta.coefs <- data.frame(summary(chi.beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.beta = format(Estimate, scientific = TRUE, digits = 3),
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
                            levels = c("(Intercept)", "vpd4", "tavg4", "wn90_perc",
                                       "soil.no3n", "pft", "wn90_perc:soil.no3n",
                                       "vpd4:pft", "tavg4:pft", "wn90_perc:pft",
                                       "soil.no3n:pft", "wn90_perc:soil.no3n:pft"))) %>%
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
                                       "wn90_perc", "soil.no3n", "pft", "wn90_perc:soil.no3n",
                                       "vpd4:pft", "tavg4:pft", "beta:pft", "wn90_perc:pft",
                                       "soil.no3n:pft", "wn90_perc:soil.no3n:pft"))) %>%
  arrange(treatment)  %>%
  replace(is.na(.), "-")

table3$treatment <- c("(Intercept)", "VPD", "Temperature (T)", 
                      "Unit cost ratio (beta)", "Soil moisture", "Soil N",
                      "PFT", "SM * N", "VPD * PFT", "T * PFT", "beta * PFT",
                      "SM * PFT", "N * PFT", "SM * N * PFT")
names(table3) <- c("Treatment", "df", "Coefficient", "chi-square", "P-value", 
                   "Coefficient", "chi-square", "P-value")

write.csv(table3, "../working_drafts/tables/TXeco_table3_chi.csv", 
          row.names = FALSE)


## Table 4 (Coefficients + model results summary)
narea.nobeta.coefs <- data.frame(summary(narea.nobeta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nobeta = format(Estimate, scientific = TRUE, digits = 3),
         se.nobeta = round(Std..Error, digits = 3),
         t.value.nobeta = round(t.value, digits = 3)) %>%
  dplyr::select(treatment, coef.nobeta, se.nobeta, t.value.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "chi" | 
           treatment == "soil.no3n" | treatment == "wn90_perc" | 
           treatment == "soil.no3n:wn90_perc") %>%
  mutate(coef.nobeta = ifelse(coef.nobeta <0.001 & coef.nobeta >= 0, 
                              "<0.001", coef.nobeta)) %>%
  print(., row.names = FALSE)

narea.nobeta.table4 <- data.frame(Anova(narea.nobeta)) %>%
  mutate(treatment = row.names(.),
         Chisq.nobeta = round(Chisq, 3),
         P_value.nobeta = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(narea.nobeta.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "chi",
                                                  "soil.no3n",
                                                  "wn90_perc",
                                                  "pft",
                                                  "soil.no3n:wn90_perc",
                                                  "chi:pft",
                                                  "soil.no3n:pft",
                                                  "wn90_perc:pft",
                                                  "soil.no3n:wn90_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nobeta, Chisq.nobeta, P_value.nobeta) %>%
  arrange(treatment)

narea.beta.coefs <- data.frame(summary(narea.beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.beta = format(Estimate, scientific = TRUE, digits = 3),
         se.beta = round(Std..Error, digits = 3)) %>%
  dplyr::select(treatment, coef.beta, se.beta, t.value.beta = t.value) %>%
  filter(treatment == c("(Intercept)", "beta", "chi", "soil.no3n")) %>%
  print(., row.names = FALSE)

narea.beta.table4 <- data.frame(Anova(narea.beta)) %>%
  mutate(treatment = row.names(.),
         Chisq.beta = round(Chisq, 3),
         P_value.beta = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(narea.beta.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "beta",
                                                  "chi",
                                                  "soil.no3n",
                                                  "pft",
                                                  "beta:pft",
                                                  "chi:pft",
                                                  "soil.no3n:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.beta, Chisq.beta, P_value.beta) %>%
  arrange(treatment)

table4 <- narea.beta.table4 %>% full_join(narea.nobeta.table4) %>%
  replace(is.na(.), "-") %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "beta", "chi", "soil.no3n",
                                                  "wn90_perc", "pft", "soil.no3n:wn90_perc",
                                                  "beta:pft", "chi:pft",
                                                  "soil.no3n:pft", "wn90_perc:pft",
                                                  "soil.no3n:wn90_perc:pft"))) %>%
  arrange(treatment)
table4

table4$treatment <- c("Intercept", "Unit cost ratio (beta)", "chi",
                      "Soil N (N)", "Soil moisture (SM)", "PFT", "SM * N",
                      "beta * PFT", "chi * PFT", "N * PFT", "SM * PFT",
                      "SM * N * PFT")

write.csv(table4, "../working_drafts/tables/TXeco_table4_leafN.csv", 
          row.names = FALSE)


table5 <- summary.coefs %>%
  mutate(slope_ci = str_c(est, " [", ci.lower, ", ", ci.upper, "]", sep = "")) %>%
  dplyr::select(resp, pred, slope_ci, z, pvalue)
table5[14, c(1,2)] <- c("narea.std", "no3n*beta")
table5[15, c(1,2)] <- c("narea.std", "wn3*beta")
table5[16, c(1,2)] <- c("narea.std", "wn3*no3n*beta")
table5[17, c(1,2)] <- c("chi.std", "tavg4*vpd4")

table5 <- table5 %>% 
  mutate(resp = ifelse(resp == "narea.std", 
                       "Narea",
                       ifelse(resp == "beta.std", 
                              "beta",
                              ifelse(resp == "chi.std", 
                                     "chi",
                                     ifelse(resp == "vpd4.std",
                                            "vpd",
                                            ifelse(resp == "no3n.std",
                                                   "no3n", 
                                                   NA))))),
         pred = ifelse(pred == "beta.std", "beta",
                       ifelse(pred == "chi.std",
                              "chi",
                              ifelse(pred == "no3n.std",
                                     "no3n",
                                     ifelse(pred == "pft",
                                            "pft",
                                            ifelse(pred == "wn3.std",
                                                   "wn3",
                                                   ifelse(pred == "vpd4.std",
                                                          "vpd",
                                                          ifelse(pred == "tavg4.std",
                                                                 "tavg",
                                                                 pred))))))), 
         resp = factor(resp, levels = c("Narea", "beta", "chi", "vpd", "no3n"))) %>%
  arrange(resp)

write.csv(table5, "../working_drafts/tables/TXeco_table5_SEMclean.csv", 
          row.names = FALSE)
