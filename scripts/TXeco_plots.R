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

## Remove outliers from statistical models
df$beta[c(84)] <- NA
df$chi[c(62, 117, 315, 317, 481)] <- NA
df$chi[c(456, 483)] <- NA
df$chi[c(284, 292, 484)] <- NA

## Add general models
beta <- lmer(log(beta) ~ wn3 * soil.no3n * pft + (1 | NCRS.code), data = df)
chi <- lmer(chi ~ (vpd4 + tavg4 + (wn3 * soil.no3n)) * pft + (1 | NCRS.code), 
            data = df)
narea <- lmer(log(narea) ~ (beta + chi + soil.no3n) * pft + (1 | NCRS.code), 
              data = df)

##########################################################################
## Beta plots
##########################################################################
beta.soiln.bivariate <- lmer(log(beta) ~ soil.no3n + wn3 + pft + (1 | NCRS.code), 
                             data = df)
beta.sm.bivariate <- lmer(log(beta) ~ wn3 + (1 | NCRS.code), 
                          data = df)

## Extract data for slopes/intercepts
beta.no3n.pred <- data.frame(get_model_data(beta.soiln.bivariate, type = "pred", 
                                            terms = "soil.no3n"))
beta.h2o.pred <- data.frame(get_model_data(beta.sm.bivariate, type = "pred", 
                                           terms = "wn3"))
beta.h2o.inter <- data.frame(get_model_data(beta, type = "pred", 
                                            terms = c("wn3", "pft")))

## Soil N - beta
beta.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = beta.no3n.pred, 
              aes(x = x, y = predicted, ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25) +
  geom_line(data = beta.no3n.pred, size = 1,
            aes(x = x, y = log(predicted))) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil nitrogen availability (ppm NO"[3]~"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.no3n.ind

## Soil moisture - beta
beta.h2o.int <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn3, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = beta.h2o.pred, 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = "black") +
  geom_line(data = beta.h2o.pred, size = 1,
            aes(x = x, y = log(predicted)), color = "black") +
  geom_ribbon(data = subset(beta.h2o.inter, group == "c3_legume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[1]) +
  geom_line(data = subset(beta.h2o.inter, group == "c3_legume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[1], lty = 2) +
  geom_ribbon(data = subset(beta.h2o.inter, group == "c4_nonlegume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[2]) +
  geom_line(data = subset(beta.h2o.inter, group == "c4_nonlegume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[2]) +
  geom_ribbon(data = subset(beta.h2o.inter, group == "c3_nonlegume"), 
              aes(x = x, y = log(predicted), ymin = log(conf.low), 
                  ymax = log(conf.high)), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_line(data = subset(beta.h2o.inter, group == "c3_nonlegume"), size = 1,
            aes(x = x, y = log(predicted)), color = cbbPalette3[3], lty = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil moisture (mm)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.int

# Write plot
png("../working_drafts/figs/TXeco_fig2_beta.png",
    width = 12, height = 4, units = 'in', res = 600)
ggarrange(beta.h2o.int, beta.no3n.ind, ncol = 2, common.legend = TRUE,
          legend = "right", align = "hv", labels = "AUTO",
          font.label = list(size = 18))
dev.off()



##########################################################################
## Chi plots
##########################################################################

chi.no.inter <- lmer(chi~ vpd4 + tavg4 + soil.no3n + wn3 + pft + (1 | NCRS.code), data = df)

## Extract data for slopes/intercepts
chi.vpd.pred <- data.frame(get_model_data(chi.no.inter, 
                                          type = "pred", 
                                          terms = "vpd4"))
chi.vpd.inter <- data.frame(get_model_data(chi, 
                                           type = "pred", 
                                           terms = c("vpd4", "pft")))




chi.no3n.pred <- data.frame(get_model_data(chi.no.inter, 
                                            type = "pred", 
                                            terms = "soil.no3n"))
chi.no3n.inter <- data.frame(get_model_data(chi, 
                                           type = "pred", 
                                           terms = c("soil.no3n", "pft")))



chi.h2o.pred <- data.frame(get_model_data(chi.no.inter, 
                                           type = "pred", 
                                           terms = "wn3"))
chi.h2o.inter <- data.frame(get_model_data(chi, type = "pred", 
                                            terms = "wn3"))


chi.vpd.plot <- ggplot(data = df, aes(x = vpd4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = chi.vpd.pred, 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = "black") +
  geom_line(data = chi.vpd.pred, size = 1,
            aes(x = x, y = predicted), color = "black") +
  geom_ribbon(data = subset(chi.vpd.inter, group == "c3_legume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[1]) +
  geom_line(data = subset(chi.vpd.inter, group == "c3_legume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[1], lty = 2) +
  geom_ribbon(data = subset(chi.vpd.inter, group == "c4_nonlegume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[2]) +
  geom_line(data = subset(chi.vpd.inter, group == "c4_nonlegume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[2]) +
  geom_ribbon(data = subset(chi.vpd.inter, group == "c3_nonlegume"), 
              aes(x = x, y = predicted, ymin = conf.low, 
                  ymax = conf.high), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_line(data = subset(chi.vpd.inter, group == "c3_nonlegume"), size = 1,
            aes(x = x, y = predicted), color = cbbPalette3[3], lty = 1) +
  scale_fill_manual(values = c(cbbPalette3[1], cbbPalette3[3], cbbPalette3[2]), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0.8, 1.41), breaks = seq(0.8, 1.4, 0.2)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.vpd.plot  




