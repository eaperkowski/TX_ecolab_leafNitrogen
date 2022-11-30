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

## Standardize and center vars
df.sem$beta.std <- scale(df.sem$beta)
df.sem$no3n.std <- scale(df.sem$soil.no3n)
df.sem$wn3.std <- scale(df.sem$wn3_perc)
df.sem$vpd4.std <- scale(df.sem$vpd4)
df.sem$tavg4.std <- scale(df.sem$tavg4)
df.sem$chi.std <- scale(df.sem$chi)
df.sem$narea.std <- scale(df.sem$narea)

## Add models to be tested in SEM
models <- ' # regressions
            narea.std ~ b*beta.std + chi.std + no3n.std + wn3.std + pft
            beta.std ~ wn3.std + a*no3n.std + pft
            chi.std ~ d*vpd4.std + tavg4.std + pft
            vpd4.std ~ e*tavg4.std
            no3n.std ~ c*wn3.std

            # covariates
            beta.std ~~ f*chi.std
            beta.std ~~ vpd4.std

            # indirect effect of soil N and soil moisture on leaf N
            soiln.beta.ind:=a*b
            sm.beta.ind:=c*b
            sm.n.beta.ind:=c*a*b
            chi.beta.ind:=f*b
            temp.ind:= d*e'

test_fit <- sem(models, data = df.sem)
summary(test_fit, standardized = TRUE,
        ci = TRUE, fit.measures = TRUE)
fitMeasures(test_fit, c("cfi", "rmsea", "srmr"))


summary.coefs <- data.frame(summary(test_fit, standardized = TRUE,
                                    ci = TRUE, fit.measures = TRUE)$pe[c(1:15, 27:31),])
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

## Get R2 values of each fitted value
lavInspect(test_fit, "rsquare")

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

table5 <- summary.coefs %>%
  mutate(slope_ci = str_c(est, " [", ci.lower, ", ", ci.upper, "]", sep = "")) %>%
  dplyr::select(resp, pred, slope_ci, z, pvalue)
table5[16, c(1,2)] <- c("narea.std", "no3n*beta")
table5[17, c(1,2)] <- c("narea.std", "wn3*beta")
table5[18, c(1,2)] <- c("narea.std", "wn3*no3n*beta")
table5[19, c(1,2)] <- c("narea.std", "chi*beta")
table5[20, c(1,2)] <- c("chi.std", "tavg4*vpd4")

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
