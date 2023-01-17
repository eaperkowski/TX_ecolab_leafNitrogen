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
         chi = ifelse(chi > 0.95 | chi < 0.20, NA, chi),
         marea = ifelse(marea > 1000, NA, marea))

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

length(df$pft[df$pft == "c4_nonlegume" & !is.na(df$chi)])

##########################################################################
## Beta
##########################################################################
df$beta[c(16, 387, 436, 459, 515)] <- NA
df$beta[c(17, 456)] <- NA
df$beta[c(435)] <- NA

beta <- lmer(log(beta) ~ wn2_perc * soil.no3n * pft + (1 | NCRS.code), 
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
test(emtrends(beta, pairwise~pft, "soil.no3n"))

# Individual effects
test(emtrends(beta, ~1, "wn2_perc"))
test(emtrends(beta, ~pft, "soil.no3n"))
emmeans(beta, pairwise~pft)

##########################################################################
## Chi
##########################################################################
df$chi[c(16, 378, 417, 436, 515)] <- NA
df$chi[c(17, 387, 435)] <- NA
df$chi[c(416, 456, 508)] <- NA
df$chi[c(321, 325)] <- NA
df$chi[c(71, 402)] <- NA

chi <- lmer(chi ~ (vpd4 + (wn2_perc * soil.no3n)) * pft + 
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
test(emtrends(chi, pairwise~pft, "vpd4"))
test(emtrends(chi, pairwise~pft, "wn2_perc"))
test(emtrends(chi, pairwise~pft, "soil.no3n"))
emmeans(chi, pairwise~pft)

##########################################################################
## Nmass
##########################################################################
df$n.leaf[c(509)] <- NA

# Fit model
nmass <- lmer(log(n.leaf) ~ (chi + (soil.no3n * wn2_perc)) * pft + (1 | NCRS.code),
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
format(summary(nmass)$coefficients, scientific = TRUE, digits = 3)
Anova(nmass)
r.squaredGLMM(nmass)

# Post hoc tests
test(emtrends(nmass, ~pft, "chi"))
test(emtrends(nmass, ~wn2_perc, "soil.no3n",
              at = list(wn2_perc = seq(0,1,0.05))))


test(emtrends(nmass, ~1, "soil.no3n"))
test(emtrends(nmass, ~1, "wn2_perc"))

emmeans(nmass, pairwise~pft)

##########################################################################
## Marea
##########################################################################
df$marea[df$marea > 1000] <- NA
df$marea[c(20, 21)] <- NA

# Fit model
marea <- lmer(log(marea) ~ (chi + (soil.no3n * wn2_perc)) * pft + (1 | NCRS.code),
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
test(emtrends(marea, ~1, "chi"))
test(emtrends(marea, pairwise~pft, "chi"))

test(emtrends(marea, ~wn2_perc, "soil.no3n",
              at = list(wn2_perc = seq(0,1,0.05))))
test(emtrends(marea, ~1, "wn2_perc"))
test(emtrends(marea, pairwise~pft, "soil.no3n"))

emmeans(marea, pairwise~pft)

##########################################################################
## Narea
##########################################################################
df$narea[df$narea > 10] <- NA
df$narea[c(509)] <- NA

# Fit model
narea <- lmer(log(narea) ~ (chi + (soil.no3n * wn2_perc)) * pft + (1 | NCRS.code),
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
test(emtrends(narea, pairwise~pft, "chi"))
test(emtrends(narea, ~1, "soil.no3n"))

test(emtrends(narea, 
              ~wn2_perc, "soil.no3n", 
              at = list(wn2_perc = seq(0, 1, 0.1))))

emmeans(narea, pairwise~pft)

##########################################################################
## Structural equation model
##########################################################################
df$n.fixer <- ifelse(df$n.fixer == "yes", 1, 0)
df$photo <- ifelse(df$photo == "c3", 1, 0)

## Original Narea PSEM model
narea_psem <- psem(
  
  ## Narea model
  narea = lme(narea ~ chi + soil.no3n + wn2_perc + marea + n.leaf + n.fixer + photo,
              random = ~ 1 | NCRS.code, 
              data = df, na.action = na.omit),
  
  ## Marea model
  marea = lme(marea ~ chi + soil.no3n + wn2_perc + n.fixer + photo,
              random = ~ 1 | NCRS.code, 
              data = df, na.action = na.omit),
  
  ## Nmass model
  n.leaf = lme(n.leaf ~ chi + soil.no3n + wn2_perc + marea + n.fixer + photo,
               random = ~ 1 | NCRS.code, 
               data = df, na.action = na.omit),
  
  ## Chi model
  chi = lme(chi ~ beta + vpd4, 
            random = ~ 1 | NCRS.code,
            data = df, na.action = na.omit),
  
  ## Beta model
  beta = lme(beta ~ soil.no3n + wn2_perc + photo + n.fixer,
             random = ~ 1 | NCRS.code, data = df, 
             na.action = na.omit),
  
  ## Soil N model
  soiln = lme(soil.no3n ~ wn2_perc, random = ~ 1 | NCRS.code, 
                            data = df, na.action = na.omit),
  
  ## VPD soil moisture
  vpd4 = lme(vpd4 ~ wn2_perc, random = ~ 1 | NCRS.code, 
           data = df, na.action = na.omit))

summary(narea_psem)

## Minimal Narea PSEM model
narea_psem_reduced <- psem(
  
  ## Narea model
  narea = lme(narea ~ chi + marea + n.leaf,
              random = ~ 1 | NCRS.code, 
              data = df, na.action = na.omit),
  
  ## Marea model
  marea = lme(marea ~ chi,
              random = ~ 1 | NCRS.code, 
              data = df, na.action = na.omit),
  
  ## Nmass model
  n.leaf = lme(n.leaf ~ chi + marea,
               random = ~ 1 | NCRS.code, 
               data = df, na.action = na.omit),
  
  ## Chi model
  chi = lme(chi ~ beta + vpd4 + photo, 
            random = ~ 1 | NCRS.code,
            data = df, na.action = na.omit),
  
  ## Beta model
  beta = lme(beta ~ soil.no3n + wn2_perc + photo + n.fixer,
             random = ~ 1 | NCRS.code, data = df, 
             na.action = na.omit),
  
  ## Soil N model
  soiln = lme(soil.no3n ~ wn2_perc, random = ~ 1 | NCRS.code, 
              data = df, na.action = na.omit),
  
  ## VPD soil moisture
  vpd4 = lme(vpd4 ~ wn2_perc, random = ~ 1 | NCRS.code, 
             data = df, na.action = na.omit))

summary(narea_psem_reduced)

plot(narea_psem_reduced)

# ## Corrected PSEM with added relationships from tests of directed separation:
# narea_psem_corrected <- psem(
#   
#   ## Narea model
#   narea = lme(narea ~ chi + soil.no3n + wn2_perc + marea + n.leaf + photo,
#               random = ~ 1 | NCRS.code, 
#               data = df, na.action = na.omit),
#   
#   ## Marea model
#   marea = lme(marea ~ chi + soil.no3n + wn2_perc + photo + n.fixer,
#               random = ~ 1 | NCRS.code, 
#               data = df, na.action = na.omit),
#   
#   ## Nmass model
#   n.leaf = lme(n.leaf ~ chi + soil.no3n + wn2_perc + marea + photo + n.fixer,
#                random = ~ 1 | NCRS.code, 
#                data = df, na.action = na.omit),
#   
#   ## Chi model
#   chi = lme(chi ~ beta + vpd4 + wn2_perc + soil.no3n + photo, 
#             random = ~ 1 | NCRS.code,
#             data = df, na.action = na.omit),
#   
#   ## Beta model
#   beta = lme(beta ~ soil.no3n + wn2_perc + vpd4 + photo + n.fixer,
#              random = ~ 1 | NCRS.code, data = df, 
#              na.action = na.omit),
#   
#   ## Soil N model
#   soiln = lme(soil.no3n ~ wn2_perc, random = ~ 1 | NCRS.code, 
#               data = df, na.action = na.omit),
#   
#   ## VPD soil moisture
#   vpd4 = lme(vpd4 ~ wn2_perc, random = ~ 1 | NCRS.code, 
#              data = df, na.action = na.omit))


## Corrected PSEM with added relationships from tests of directed separation:
narea_psem_full <- psem(

  ## Narea model
  narea = lme(narea ~ beta + chi + soil.no3n + wn2_perc + marea + n.leaf + photo,
              random = ~ 1 | NCRS.code,
              data = df, na.action = na.omit),

  ## Marea model
  marea = lme(marea ~ chi + soil.no3n + wn2_perc + photo + n.fixer,
              random = ~ 1 | NCRS.code,
              data = df, na.action = na.omit),

  ## Nmass model
  n.leaf = lme(n.leaf ~ beta + chi + soil.no3n + wn2_perc + marea + photo + n.fixer,
               random = ~ 1 | NCRS.code,
               data = df, na.action = na.omit),

  ## Chi model
  chi = lme(chi ~ beta + vpd4 + wn2_perc + soil.no3n + photo,
            random = ~ 1 | NCRS.code,
            data = df, na.action = na.omit),

  ## Beta model
  beta = lme(beta ~ soil.no3n + wn2_perc + vpd4 + photo + n.fixer,
             random = ~ 1 | NCRS.code, data = df,
             na.action = na.omit),

  ## Soil N model
  soiln = lme(soil.no3n ~ wn2_perc, random = ~ 1 | NCRS.code,
              data = df, na.action = na.omit),

  ## VPD soil moisture
  vpd4 = lme(vpd4 ~ wn2_perc + soil.no3n, random = ~ 1 | NCRS.code,
             data = df, na.action = na.omit))
summary(narea_psem_full)




line.thick <- data.frame(summary(narea_psem_reduced)$coefficients,
           line.thickness = abs(summary(
             narea_psem_reduced)$coefficients$Std.Estimate) * 18.75)


line.thick <- line.thick %>%
  mutate(line.thickness = round(line.thickness, digits = 2)) %>%
  dplyr::select(-Var.9)


ggplot(data=line.thick, aes(x = abs(Std.Estimate), 
                            y = line.thickness)) + 
  geom_smooth() +
  geom_point() +
  scale_y_continuous(limits = c(0,15), breaks = seq(0, 15, 5))





##########################################################################
## Tables
##########################################################################
## Table 2 (Coefficients + model results summary)
beta.coefs <- data.frame(summary(beta)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef) %>%
  filter(treatment == "(Intercept)" | treatment == "wn2_perc" | 
           treatment == "soil.no3n" | treatment == "wn2_perc:soil.no3n") %>%
  mutate(coef = ifelse(coef <0.001 & coef >= 0, "<0.001", coef)) %>%
  print(., row.names = FALSE)

table2 <- data.frame(Anova(beta)) %>%
  mutate(treatment = row.names(.),
         Chisq = round(Chisq, 3),
         P_value = ifelse(Pr..Chisq. < 0.001, "<0.001",
                          round(Pr..Chisq., 3))) %>%
  full_join(beta.coefs) %>%
  mutate(treatment = factor(treatment, levels = c("(Intercept)",
                                                  "wn2_perc",
                                                  "soil.no3n",
                                                  "pft",
                                                  "wn2_perc:soil.no3n",
                                                  "wn2_perc:pft",
                                                  "soil.no3n:pft",
                                                  "wn2_perc:soil.no3n:pft"))) %>%
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
  dplyr::select(treatment, coef.nobeta, se.nobeta, t.value.nobeta) %>%
  filter(treatment == "(Intercept)" | treatment == "vpd4" |
           treatment == "wn2_perc" | 
           treatment == "soil.no3n" | 
           treatment == "wn2_perc:soil.no3n") %>%
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
                            levels = c("(Intercept)", "vpd4", "wn2_perc",
                                       "soil.no3n", "pft", "wn2_perc:soil.no3n",
                                       "vpd4:pft",  "wn2_perc:pft",
                                       "soil.no3n:pft", "wn2_perc:soil.no3n:pft"))) %>%
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
           treatment == "wn2_perc" | 
           treatment == "soil.no3n:wn2_perc") %>%
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
                          "wn2_perc", "pft", "soil.no3n:wn2_perc",
                          "chi:pft", "soil.no3n:pft",
                          "wn2_perc:pft", "soil.no3n:wn2_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.narea, Chisq.narea, P_value.narea) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

nmass.coefs <- data.frame(summary(nmass)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.nmass = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef.nmass) %>%
  filter(treatment == "(Intercept)" | treatment == "chi" | 
           treatment == "soil.no3n" | 
           treatment == "wn2_perc" | 
           treatment == "soil.no3n:wn2_perc") %>%
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
                          "wn2_perc", "pft", "soil.no3n:wn2_perc",
                          "chi:pft", "soil.no3n:pft",
                          "wn2_perc:pft", "soil.no3n:wn2_perc:pft"))) %>%
  dplyr::select(treatment, df = Df, coef.nmass, Chisq.nmass, P_value.nmass) %>%
  arrange(treatment) %>%
  replace(is.na(.), "-")

marea.coefs <- data.frame(summary(marea)$coefficient) %>%
  mutate(treatment = row.names(.),
         coef.marea = format(Estimate, scientific = TRUE, digits = 3)) %>%
  dplyr::select(treatment, coef.marea) %>%
  filter(treatment == "(Intercept)" | 
           treatment == "chi" | treatment == "soil.no3n" | 
           treatment == "wn2_perc" | 
           treatment == "soil.no3n:wn2_perc") %>%
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
                          "wn2_perc", "pft", "soil.no3n:wn2_perc",
                          "chi:pft", "soil.no3n:pft",
                          "wn2_perc:pft", "soil.no3n:wn2_perc:pft"))) %>%
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
table5.coefs <- summary(narea_psem_reduced)$coefficients[, c(1:8)] %>%
  as.data.frame() %>%
  mutate(Std.Error = ifelse(Std.Error == "-", NA, Std.Error),
         across(Estimate:Std.Estimate, as.numeric),
         across(Estimate:Std.Estimate, round, 3),
         p_val = ifelse(P.Value < 0.001, "<0.001", P.Value),
         Std.Estimate = round(Std.Estimate, digits = 3)) %>%
  dplyr::select(resp = Response, pred = Predictor, std_est = Std.Estimate, 
                p_val)

table5 <- summary(narea_psem_reduced)$R2 %>%
  dplyr::select(resp = Response, r2_marg = Marginal,
                r2_cond = Conditional) %>%
  full_join(table5.coefs) %>%
  dplyr::select(resp, pred, r2_marg, r2_cond, std_est:p_val)


write.csv(table5, "../working_drafts/tables/TXeco_table5_SEMclean.csv", 
          row.names = FALSE)



###############################
# Indirect SEM path coefs
###############################
sm.vpd = summary(narea_psem_corrected)$coefficients$Std.Estimate[[34]]
vpd.cica = summary(narea_psem_corrected)$coefficients$Std.Estimate[[22]]
cica.beta = summary(narea_psem_corrected)$coefficients$Std.Estimate[[26]]
beta.narea = summary(narea_psem_corrected)$coefficients$Std.Estimate[[1]]
sm.no3n = summary(narea_psem_corrected)$coefficients$Std.Estimate[[31]]
no3n.beta = summary(narea_psem_corrected)$coefficients$Std.Estimate[[27]]
temp.no3n = summary(narea_psem_corrected)$coefficients$Std.Estimate[[32]]
temp.vpd = summary(narea_psem_corrected)$coefficients$Std.Estimate[[33]]


## Soil moisture -> soil NO3-N pathway
sm.no3n * no3n.beta * beta.narea

## Soil moisture -> vpd pathway
sm.vpd * vpd.cica * cica.beta * beta.narea

## Temp -> vpd pathway
temp.vpd * vpd.cica * cica.beta * beta.narea

## Temp -> no3n pathway
temp.no3n * no3n.beta * beta.narea

## Mean and standard deviation of beta
min(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
max(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
median(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
mean(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)
sd(subset(df, pft != "c4_nonlegume")$beta, na.rm = TRUE)

min(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
max(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
median(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
mean(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)
sd(subset(df, pft == "c4_nonlegume")$beta, na.rm = TRUE)






