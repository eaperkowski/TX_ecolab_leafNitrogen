##########################################################################
## Load libraries and import data
##########################################################################
# Libraries
library(lme4)
library(emmeans)
library(tidyverse)
library(ggpubr)

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
cbbPalette3 <- c("#DDAA33", "#004488", "#BB5566")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_legume"])
length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c4_nonlegume"])

## Convert VPD from hPa (PRISM units) to kPa (standard)
df$vpd4 <- df$vpd4 / 10
df$wn3 <- df$wn3 / 150 # relative to maximum bucket size

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

##########################################################################
## Beta - soil N
##########################################################################
beta.0ppmN <- data.frame(soil.no3n = 0,
                         emmeans(beta, ~pft, at = list(soil.no3n = 0)))
beta.20ppmN <- data.frame(soil.no3n = 20,
                          emmeans(beta, ~pft, at = list(soil.no3n = 20)))
beta.40ppmN <- data.frame(soil.no3n = 40,
                          emmeans(beta, ~pft, at = list(soil.no3n = 40)))
beta.60ppmN <- data.frame(soil.no3n = 60,
                          emmeans(beta, ~pft, at = list(soil.no3n = 60)))
beta.80ppmN <- data.frame(soil.no3n = 80,
                          emmeans(beta, ~pft, at = list(soil.no3n = 80)))
beta.0ppmN.nopft <- data.frame(soil.no3n = 0,
                         emmeans(beta, ~1, at = list(soil.no3n = 0)))
beta.20ppmN.nopft <- data.frame(soil.no3n = 20,
                          emmeans(beta, ~1, at = list(soil.no3n = 20)))
beta.40ppmN.nopft <- data.frame(soil.no3n = 40,
                          emmeans(beta, ~1, at = list(soil.no3n = 40)))
beta.60ppmN.nopft <- data.frame(soil.no3n = 60,
                          emmeans(beta, ~1, at = list(soil.no3n = 60)))
beta.80ppmN.nopft <- data.frame(soil.no3n = 80,
                          emmeans(beta, ~1, at = list(soil.no3n = 80)))

beta.no3n.pft <- beta.0ppmN %>% full_join(beta.20ppmN) %>% full_join(beta.40ppmN) %>%
  full_join(beta.60ppmN) %>% full_join(beta.80ppmN)
beta.no3n <- beta.0ppmN.nopft %>% full_join(beta.20ppmN.nopft) %>% 
  full_join(beta.40ppmN.nopft) %>% full_join(beta.60ppmN.nopft) %>% 
  full_join(beta.80ppmN.nopft) %>% dplyr::select(soil.no3n, pft = X1, everything()) %>%
  full_join(beta.no3n.pft)

beta.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
   geom_jitter(aes(fill = pft),
               width = 0.5, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = beta.no3n, 
  #             aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = beta.no3n, size = 1,
  #           aes(x = soil.no3n, y = emmean, color = pft)) +
  geom_ribbon(data = subset(beta.no3n, pft == "overall"), 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), alpha = 0.25, fill = "black") +
  geom_line(data = subset(beta.no3n, pft == "overall"),
            aes(x = soil.no3n, y = emmean),
            color = "black", size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.no3n.ind

beta.no3n.int <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 4, alpha = 0.7, shape = 21) +
   geom_ribbon(data = subset(beta.no3n, pft != "overall"), 
               aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                   ymax = upper.CL, fill = pft), alpha = 0.25) +
   geom_line(data = subset(beta.no3n, pft != "overall"), size = 1,
              aes(x = soil.no3n, y = emmean, color = pft)) +
  # geom_ribbon(data = subset(beta.no3n, pft == "overall"), 
  #             aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), alpha = 0.25, fill = "black") +
  # geom_line(data = subset(beta.no3n, pft == "overall"),
  #           aes(x = soil.no3n, y = emmean),
  #           color = "black", size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  guides(color = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.no3n.int

##########################################################################
## Beta - soil moisture
##########################################################################
beta.0perc <- data.frame(wn = 0,
                         emmeans(beta, ~pft, at = list(wn3 = 0)))
beta.20perc <- data.frame(wn = 0.2,
                          emmeans(beta, ~pft, at = list(wn3 = 0.2)))
beta.40perc <- data.frame(wn = 0.4,
                          emmeans(beta, ~pft, at = list(wn3 = 0.4)))
beta.60perc <- data.frame(wn = 0.6,
                          emmeans(beta, ~pft, at = list(wn3 = 0.6)))
beta.80perc <- data.frame(wn = 0.8,
                          emmeans(beta, ~pft, at = list(wn3 = 0.8)))
beta.100perc <- data.frame(wn = 1,
                          emmeans(beta, ~pft, at = list(wn3 = 1)))
beta.sm.pft <- beta.0perc %>% full_join(beta.20perc) %>%
  full_join(beta.40perc) %>% full_join(beta.60perc) %>%
  full_join(beta.80perc) %>% full_join(beta.100perc)

beta.0perc.nopft <- data.frame(wn = 0,
                               emmeans(beta, ~1, at = list(wn3 = 0)))
beta.20perc.nopft <- data.frame(wn = 0.2,
                                emmeans(beta, ~1, at = list(wn3 = 0.2)))
beta.40perc.nopft <- data.frame(wn = 0.4,
                                emmeans(beta, ~1, at = list(wn3 = 0.4)))
beta.60perc.nopft <- data.frame(wn = 0.6,
                                emmeans(beta, ~1, at = list(wn3 = 0.6)))
beta.80perc.nopft <- data.frame(wn = 0.8,
                                emmeans(beta, ~1, at = list(wn3 = 0.8)))
beta.100perc.nopft <- data.frame(wn = 1,
                                 emmeans(beta, ~1, at = list(wn3 = 1)))
beta.sm <- beta.0perc.nopft %>% full_join(beta.20perc.nopft) %>%
  full_join(beta.40perc.nopft) %>% full_join(beta.60perc.nopft) %>%
  full_join(beta.80perc.nopft) %>% full_join(beta.100perc.nopft) %>%
  dplyr::select(wn, pft = X1, everything()) %>% full_join(beta.sm.pft)

beta.h2o.ind <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn3, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = subset(beta.sm, pft != "overall"), 
  #             aes(x = wn, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), 
  #             alpha = 0.3) +
  # geom_line(data = subset(beta.sm, pft == "c4_nonlegume"),
  #           aes(x = wn, y = emmean),
  #           size = 1, color = cbbPalette3[3]) +
  # geom_line(data = subset(beta.sm, pft == "c3_nonlegume"),
  #           aes(x = wn, y = emmean),
  #           size = 1, color = cbbPalette3[2], lty = 2) +
  # geom_line(data = subset(beta.sm, pft == "c3_legume"),
  #           aes(x = wn, y = emmean),
  #           size = 1, color = cbbPalette3[1], lty = 2) +
  geom_ribbon(data = subset(beta.sm, pft == "overall"),
              aes(x = wn, y = emmean, ymin = lower.CL, ymax = upper.CL),
              alpha = 0.25, fill = "black") +
  geom_line(data = subset(beta.sm, pft == "overall"),
            aes(x = wn, y = emmean),
            size = 1, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil moisture (%)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group",
       color = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.ind


beta.h2o.int <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn3, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(beta.sm, pft != "overall"), 
              aes(x = wn, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), 
              alpha = 0.3) +
  geom_line(data = subset(beta.sm, pft == "c4_nonlegume"),
            aes(x = wn, y = emmean),
            size = 1, color = cbbPalette3[3]) +
  geom_line(data = subset(beta.sm, pft == "c3_nonlegume"),
            aes(x = wn, y = emmean),
            size = 1, color = cbbPalette3[2], lty = 2) +
  geom_line(data = subset(beta.sm, pft == "c3_legume"),
            aes(x = wn, y = emmean),
            size = 1, color = cbbPalette3[1], lty = 2) +
  # geom_ribbon(data = subset(beta.sm, pft == "overall"), 
  #             aes(x = wn, y = emmean, ymin = lower.CL, ymax = upper.CL), 
  #             alpha = 0.25, fill = "black") +
  # geom_line(data = subset(beta.sm, pft == "overall"),
  #           aes(x = wn, y = emmean),  
  #           size = 1, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil moisture (%)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group",
       color = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
beta.h2o.int

##########################################################################
## Write beta plot
##########################################################################
# Write plot
png("../working_drafts/figs/TXeco_fig2_beta.png",
    width = 11.5, height = 4.5, units = 'in', res = 600)
ggarrange(beta.h2o.int, beta.no3n.ind,
          ncol = 2, nrow = 1, common.legend = TRUE,
          legend = "right", align = "hv", labels = "AUTO",
          font.label = list(size = 18))
dev.off()

##########################################################################
## Chi - VPD
##########################################################################
chi.vpd0.8 <- data.frame(vpd = 0.8,
                         emmeans(chi, ~pft, at = list(vpd4 = 0.8)))
chi.vpd0.9 <- data.frame(vpd = 0.9,
                          emmeans(chi, ~pft, at = list(vpd4 = 0.9)))
chi.vpd1.0 <- data.frame(vpd = 1.0,
                          emmeans(chi, ~pft, at = list(vpd4 = 1.0)))
chi.vpd1.1 <- data.frame(vpd = 1.1,
                          emmeans(chi, ~pft, at = list(vpd4 = 1.1)))
chi.vpd1.2 <- data.frame(vpd = 1.2,
                          emmeans(chi, ~pft, at = list(vpd4 = 1.2)))
chi.vpd1.3 <- data.frame(vpd = 1.3,
                          emmeans(chi, ~pft, at = list(vpd4 = 1.3)))
chi.vpd1.4 <- data.frame(vpd = 1.4,
                          emmeans(chi, ~pft, at = list(vpd4 = 1.4)))
chi.vpd0.8.nopft <- data.frame(vpd = 0.8,
                          emmeans(chi, ~1, at = list(vpd4 = 0.8)))
chi.vpd0.9.nopft <- data.frame(vpd = 0.9,
                          emmeans(chi, ~1, at = list(vpd4 = 0.9)))
chi.vpd1.0.nopft <- data.frame(vpd = 1.0,
                          emmeans(chi, ~1, at = list(vpd4 = 1.0)))
chi.vpd1.1.nopft <- data.frame(vpd = 1.1,
                          emmeans(chi, ~1, at = list(vpd4 = 1.1)))
chi.vpd1.2.nopft <- data.frame(vpd = 1.2,
                          emmeans(chi, ~1, at = list(vpd4 = 1.2)))
chi.vpd1.3.nopft <- data.frame(vpd = 1.3,
                          emmeans(chi, ~1, at = list(vpd4 = 1.3)))
chi.vpd1.4.nopft <- data.frame(vpd = 1.4,
                          emmeans(chi, ~1, at = list(vpd4 = 1.4)))

chi.vpd.pft <- chi.vpd0.8 %>% full_join(chi.vpd0.9) %>% full_join(chi.vpd1.0) %>%
  full_join(chi.vpd1.1) %>% full_join(chi.vpd1.2) %>% full_join(chi.vpd1.3) %>%
  full_join(chi.vpd1.4)
  
chi.vpd <- chi.vpd0.8.nopft %>% full_join(chi.vpd0.9.nopft) %>% 
  full_join(chi.vpd1.0.nopft) %>% full_join(chi.vpd1.1.nopft) %>% 
  full_join(chi.vpd1.2.nopft) %>% full_join(chi.vpd1.3.nopft) %>%
  full_join(chi.vpd1.4.nopft) %>% dplyr::select(vpd, pft = X1, everything()) %>%
  full_join(chi.vpd.pft)

chi.vpd.int <- ggplot(data = df, aes(x = vpd4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(chi.vpd, pft != "overall"), 
              aes(x = vpd, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.vpd, pft == "c3_legume"), 
              aes(x = vpd, y = emmean), 
            color = cbbPalette3[1], size = 1, lty = 2) +
  geom_line(data = subset(chi.vpd, pft == "c3_nonlegume"), 
            aes(x = vpd, y = emmean), 
            color = cbbPalette3[2], size = 1) +
  geom_line(data = subset(chi.vpd, pft == "c4_nonlegume"), 
            aes(x = vpd, y = emmean), 
            color = cbbPalette3[3], size = 1) +
  # geom_ribbon(data = subset(chi.vpd, pft == "overall"), 
  #             aes(x = vpd, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), fill = "black", alpha = 0.25) +
  # geom_line(data = subset(chi.vpd, pft == "overall"), 
  #           aes(x = vpd, y = emmean), 
  #           color = "black", size = 1) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0.8, 1.41), breaks = seq(0.8, 1.4, 0.15)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.vpd.int

chi.vpd.ind <- ggplot(data = df, aes(x = vpd4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = subset(chi.vpd, pft != "overall"), 
  #             aes(x = vpd, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = subset(chi.vpd, pft == "c3_legume"), 
  #           aes(x = vpd, y = emmean), 
  #           color = cbbPalette3[1], size = 1, lty = 2) +
  # geom_line(data = subset(chi.vpd, pft == "c3_nonlegume"), 
  #           aes(x = vpd, y = emmean), 
  #           color = cbbPalette3[2], size = 1) +
  # geom_line(data = subset(chi.vpd, pft == "c4_nonlegume"), 
  #           aes(x = vpd, y = emmean), 
  #           color = cbbPalette3[3], size = 1) +
  geom_ribbon(data = subset(chi.vpd, pft == "overall"),
              aes(x = vpd, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), fill = "black", alpha = 0.25) +
  geom_line(data = subset(chi.vpd, pft == "overall"),
            aes(x = vpd, y = emmean),
            color = "black", size = 1) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0.8, 1.41), breaks = seq(0.8, 1.4, 0.15)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))

##########################################################################
## Chi - Temp
##########################################################################
chi.tavg18 <- data.frame(tavg = 18,
                         emmeans(chi, ~pft, at = list(tavg4 = 18)))
chi.tavg19 <- data.frame(tavg = 19,
                         emmeans(chi, ~pft, at = list(tavg4 = 19)))
chi.tavg20 <- data.frame(tavg = 20,
                         emmeans(chi, ~pft, at = list(tavg4 = 20)))
chi.tavg21 <- data.frame(tavg = 21,
                         emmeans(chi, ~pft, at = list(tavg4 = 21)))
chi.tavg22 <- data.frame(tavg = 22,
                         emmeans(chi, ~pft, at = list(tavg4 = 22)))
chi.tavg18.nopft <- data.frame(tavg = 18,
                         emmeans(chi, ~1, at = list(tavg4 = 18)))
chi.tavg19.nopft <- data.frame(tavg = 19,
                         emmeans(chi, ~1, at = list(tavg4 = 19)))
chi.tavg20.nopft <- data.frame(tavg = 20,
                         emmeans(chi, ~1, at = list(tavg4 = 20)))
chi.tavg21.nopft <- data.frame(tavg = 21,
                         emmeans(chi, ~1, at = list(tavg4 = 21)))
chi.tavg22.nopft <- data.frame(tavg = 22,
                           emmeans(chi, ~1, at = list(tavg4 = 22)))

chi.tavg.pft <- chi.tavg18 %>% full_join(chi.tavg19) %>% full_join(chi.tavg20) %>%
  full_join(chi.tavg21) %>% full_join(chi.tavg22)

chi.tavg <- chi.tavg18.nopft %>% full_join(chi.tavg19.nopft) %>% 
  full_join(chi.tavg20.nopft) %>% full_join(chi.tavg21.nopft) %>% 
  full_join(chi.tavg22.nopft) %>% 
  dplyr::select(tavg, pft = X1, everything()) %>%
  full_join(chi.tavg.pft)

chi.temp.ind <- ggplot(data = df, aes(x = tavg4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = subset(chi.tavg, pft != "overall"), 
  #             aes(x = tavg, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = subset(chi.tavg, pft == "c3_legume"), 
  #           aes(x = tavg, y = emmean), 
  #           color = cbbPalette3[1], size = 1) +
  # geom_line(data = subset(chi.tavg, pft == "c3_nonlegume"), 
  #           aes(x = tavg, y = emmean), 
  #           color = cbbPalette3[2], size = 1, lty = 2) +
  # geom_line(data = subset(chi.tavg, pft == "c4_nonlegume"), 
  #           aes(x = tavg, y = emmean), 
  #           color = cbbPalette3[3], size = 1) +
  geom_ribbon(data = subset(chi.tavg, pft == "overall"),
              aes(x = tavg, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), fill = "black", alpha = 0.25) +
  geom_line(data = subset(chi.tavg, pft == "overall"),
            aes(x = tavg, y = emmean),
            color = "black", size = 1) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(18, 22), breaks = seq(18, 22, 1)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Air temperature ("*degree*"C)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.temp.ind

chi.temp.int <- ggplot(data = df, aes(x = tavg4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(chi.tavg, pft != "overall"), 
              aes(x = tavg, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.tavg, pft == "c3_legume"), 
            aes(x = tavg, y = emmean), 
            color = cbbPalette3[1], size = 1) +
  geom_line(data = subset(chi.tavg, pft == "c3_nonlegume"), 
            aes(x = tavg, y = emmean), 
            color = cbbPalette3[2], size = 1, lty = 2) +
  geom_line(data = subset(chi.tavg, pft == "c4_nonlegume"), 
            aes(x = tavg, y = emmean), 
            color = cbbPalette3[3], size = 1) +
  # geom_ribbon(data = subset(chi.tavg, pft == "overall"), 
  #             aes(x = tavg, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), fill = "black", alpha = 0.25) +
  # geom_line(data = subset(chi.tavg, pft == "overall"), 
  #           aes(x = tavg, y = emmean), 
  #           color = "black", size = 1) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(18, 22), breaks = seq(18, 22, 1)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Air temperature ("*degree*"C)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.temp.plot  

##########################################################################
## Chi - Soil moisture
##########################################################################
chi.sm0 <- data.frame(wn = 0,
                      emmeans(chi, ~pft, at = list(wn3 = 0)))
chi.sm0.2 <- data.frame(wn = 0.2,
                        emmeans(chi, ~pft, at = list(wn3 = 0.2)))
chi.sm0.4 <- data.frame(wn = 0.4,
                        emmeans(chi, ~pft, at = list(wn3 = 0.4)))
chi.sm0.6 <- data.frame(wn = 0.6,
                        emmeans(chi, ~pft, at = list(wn3 = 0.6)))
chi.sm0.8 <- data.frame(wn = 0.8,
                        emmeans(chi, ~pft, at = list(wn3 = 0.8)))
chi.sm1.0 <- data.frame(wn = 1.0,
                        emmeans(chi, ~pft, at = list(wn3 = 1.0)))
chi.sm0.nopft <- data.frame(wn = 0,
                      emmeans(chi, ~1, at = list(wn3 = 0)))
chi.sm0.2.nopft <- data.frame(wn = 0.2,
                        emmeans(chi, ~1, at = list(wn3 = 0.2)))
chi.sm0.4.nopft <- data.frame(wn = 0.4,
                        emmeans(chi, ~1, at = list(wn3 = 0.4)))
chi.sm0.6.nopft <- data.frame(wn = 0.6,
                        emmeans(chi, ~1, at = list(wn3 = 0.6)))
chi.sm0.8.nopft <- data.frame(wn = 0.8,
                        emmeans(chi, ~1, at = list(wn3 = 0.8)))
chi.sm1.0.nopft <- data.frame(wn = 1.0,
                        emmeans(chi, ~1, at = list(wn3 = 1.0)))

chi.sm.pft <- chi.sm0 %>% full_join(chi.sm0.2) %>% full_join(chi.sm0.4) %>%
  full_join(chi.sm0.6) %>% full_join(chi.sm0.6) %>% full_join(chi.sm1.0)

chi.sm <- chi.sm0.nopft %>% full_join(chi.sm0.2.nopft) %>% 
  full_join(chi.sm0.4.nopft) %>% full_join(chi.sm0.6.nopft) %>% 
  full_join(chi.sm0.6.nopft) %>% full_join(chi.sm1.0.nopft) %>% 
  dplyr::select(wn, pft = X1, everything()) %>%
  full_join(chi.sm.pft)

chi.sm.ind <- ggplot(data = df, aes(x = wn3, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = subset(chi.sm, pft != "overall"), 
  #             aes(x = wn, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = subset(chi.sm, pft == "c3_legume"), 
  #           aes(x = wn, y = emmean), 
  #           color = cbbPalette3[1], size = 1, lty = 2) +
  # geom_line(data = subset(chi.sm, pft == "c3_nonlegume"), 
  #           aes(x = wn, y = emmean), 
  #           color = cbbPalette3[2], size = 1, lty = 2) +
  # geom_line(data = subset(chi.sm, pft == "c4_nonlegume"), 
  #           aes(x = wn, y = emmean), 
  #           color = cbbPalette3[3], size = 1) +
  geom_ribbon(data = subset(chi.sm, pft == "overall"),
              aes(x = wn, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), fill = "black", alpha = 0.25) +
  geom_line(data = subset(chi.sm, pft == "overall"),
            aes(x = wn, y = emmean),
            color = "black", size = 1) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil moisture (%)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))

chi.sm.int <- ggplot(data = df, aes(x = wn3, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(chi.sm, pft != "overall"), 
              aes(x = wn, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.sm, pft == "c3_legume"), 
            aes(x = wn, y = emmean), 
            color = cbbPalette3[1], size = 1, lty = 2) +
  geom_line(data = subset(chi.sm, pft == "c3_nonlegume"), 
            aes(x = wn, y = emmean), 
            color = cbbPalette3[2], size = 1, lty = 2) +
  geom_line(data = subset(chi.sm, pft == "c4_nonlegume"), 
            aes(x = wn, y = emmean), 
            color = cbbPalette3[3], size = 1) +
  # geom_ribbon(data = subset(chi.sm, pft == "overall"), 
  #             aes(x = wn, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), fill = "black", alpha = 0.25) +
  # geom_line(data = subset(chi.sm, pft == "overall"), 
  #           aes(x = wn, y = emmean), 
  #           color = "black", size = 1) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil moisture (%)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.sm.int 

##########################################################################
## Chi- soil N
##########################################################################
chi.0ppmN <- data.frame(soil.no3n = 0,
                         emmeans(chi, ~pft, at = list(soil.no3n = 0)))
chi.20ppmN <- data.frame(soil.no3n = 20,
                          emmeans(chi, ~pft, at = list(soil.no3n = 20)))
chi.40ppmN <- data.frame(soil.no3n = 40,
                          emmeans(chi, ~pft, at = list(soil.no3n = 40)))
chi.60ppmN <- data.frame(soil.no3n = 60,
                          emmeans(chi, ~pft, at = list(soil.no3n = 60)))
chi.80ppmN <- data.frame(soil.no3n = 80,
                          emmeans(chi, ~pft, at = list(soil.no3n = 80)))
chi.0ppmN.nopft <- data.frame(soil.no3n = 0,
                               emmeans(chi, ~1, at = list(soil.no3n = 0)))
chi.20ppmN.nopft <- data.frame(soil.no3n = 20,
                                emmeans(chi, ~1, at = list(soil.no3n = 20)))
chi.40ppmN.nopft <- data.frame(soil.no3n = 40,
                                emmeans(chi, ~1, at = list(soil.no3n = 40)))
chi.60ppmN.nopft <- data.frame(soil.no3n = 60,
                                emmeans(chi, ~1, at = list(soil.no3n = 60)))
chi.80ppmN.nopft <- data.frame(soil.no3n = 80,
                                emmeans(chi, ~1, at = list(soil.no3n = 80)))

chi.no3n.pft <- chi.0ppmN %>% full_join(chi.20ppmN) %>% full_join(chi.40ppmN) %>%
  full_join(chi.60ppmN) %>% full_join(chi.80ppmN)

chi.no3n <- chi.0ppmN.nopft %>% full_join(chi.20ppmN.nopft) %>% 
  full_join(chi.40ppmN.nopft) %>% full_join(chi.60ppmN.nopft) %>% 
  full_join(chi.80ppmN.nopft) %>% dplyr::select(soil.no3n, pft = X1, everything()) %>%
  full_join(chi.no3n.pft)

chi.no3n.ind <- ggplot(data = df, aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = subset(chi.no3n, pft != "overall"), 
  #             aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = subset(chi.no3n, pft == "c3_legume"),
  #           aes(x = soil.no3n, y = emmean),
  #           color = cbbPalette3[1], size = 1, lty = 2) +
  # geom_line(data = subset(chi.no3n, pft == "c3_nonlegume"),
  #           aes(x = soil.no3n, y = emmean),
  #           color = cbbPalette3[2], size = 1, lty = 2) +
  # geom_line(data = subset(chi.no3n, pft == "c4_nonlegume"),
  #           aes(x = soil.no3n, y = emmean),
  #           color = cbbPalette3[3], size = 1) +
  geom_ribbon(data = subset(chi.no3n, pft == "overall"),
              aes(x = soil.no3n, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), fill = "black", alpha = 0.25) +
  geom_line(data = subset(chi.no3n, pft == "overall"),
            aes(x = soil.no3n, y = emmean),
            color = "black", size = 1, lty = 2) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))


chi.no3n.int <- ggplot(data = df, aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 4, alpha = 0.7, shape = 21) +
   geom_ribbon(data = subset(chi.no3n, pft != "overall"), 
               aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.no3n, pft == "c3_legume"),
            aes(x = soil.no3n, y = emmean),
            color = cbbPalette3[1], size = 1, lty = 2) +
  geom_line(data = subset(chi.no3n, pft == "c3_nonlegume"),
            aes(x = soil.no3n, y = emmean),
            color = cbbPalette3[2], size = 1, lty = 2) +
  geom_line(data = subset(chi.no3n, pft == "c4_nonlegume"),
            aes(x = soil.no3n, y = emmean),
            color = cbbPalette3[3], size = 1) +
  # geom_ribbon(data = subset(chi.no3n, pft == "overall"),
  #             aes(x = soil.no3n, y = emmean, ymin = lower.CL,
  #                 ymax = upper.CL), fill = "black", alpha = 0.25) +
  # geom_line(data = subset(chi.no3n, pft == "overall"),
  #           aes(x = soil.no3n, y = emmean),
  #           color = "black", size = 1, lty = 2) +
  scale_fill_manual(values = cbbPalette3, 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(chi)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.no3n.int  

##########################################################################
## Write chi plot
##########################################################################
png("../working_drafts/figs/TXeco_fig3_chi.png",
    width = 11.5, height = 9.5, units = 'in', res = 600)
ggarrange(chi.sm.int, chi.no3n.int,
          chi.vpd.int, chi.temp.int,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()

##########################################################################
## Narea - soil N
##########################################################################
narea.0ppmN <- data.frame(soil.no3n = 0,
                          emmeans(narea, ~pft, at = list(soil.no3n = 0), 
                                  type = "response"))
narea.20ppmN <- data.frame(soil.no3n = 20,
                           emmeans(narea, ~pft, at = list(soil.no3n = 20), 
                                   type = "response"))
narea.40ppmN <- data.frame(soil.no3n = 40,
                           emmeans(narea, ~pft, at = list(soil.no3n = 40), 
                                   type = "response"))
narea.60ppmN <- data.frame(soil.no3n = 60,
                           emmeans(narea, ~pft, at = list(soil.no3n = 60), 
                                   type = "response"))
narea.80ppmN <- data.frame(soil.no3n = 80,
                           emmeans(narea, ~pft, at = list(soil.no3n = 80), 
                                   type = "response"))
narea.0ppmN.nopft <- data.frame(soil.no3n = 0,
                                emmeans(narea, ~1, at = list(soil.no3n = 0), 
                                        type = "response"))
narea.20ppmN.nopft <- data.frame(soil.no3n = 20,
                                 emmeans(narea, ~1, at = list(soil.no3n = 20), 
                                         type = "response"))
narea.40ppmN.nopft <- data.frame(soil.no3n = 40,
                                 emmeans(narea, ~1, at = list(soil.no3n = 40), 
                                         type = "response"))
narea.60ppmN.nopft <- data.frame(soil.no3n = 60,
                                 emmeans(narea, ~1, at = list(soil.no3n = 60), 
                                         type = "response"))
narea.80ppmN.nopft <- data.frame(soil.no3n = 80,
                                 emmeans(narea, ~1, at = list(soil.no3n = 80), 
                                         type = "response"))

narea.no3n.pft <- narea.0ppmN %>% full_join(narea.20ppmN) %>% full_join(narea.40ppmN) %>%
  full_join(narea.60ppmN) %>% full_join(narea.80ppmN)
narea.no3n <- narea.0ppmN.nopft %>% full_join(narea.20ppmN.nopft) %>% 
  full_join(narea.40ppmN.nopft) %>% full_join(narea.60ppmN.nopft) %>% 
  full_join(narea.80ppmN.nopft) %>% dplyr::select(soil.no3n, pft = X1, everything()) %>%
  full_join(narea.no3n.pft) %>% dplyr::select(soil.no3n, pft, emmean = response, everything())

test(emtrends(narea, ~pft, "soil.no3n"))

narea.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = narea)) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = narea.no3n, 
  #             aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = narea.no3n, size = 1,
  #           aes(x = soil.no3n, y = emmean, color = pft)) +
  geom_ribbon(data = subset(narea.no3n, pft == "overall"), 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), alpha = 0.25, fill = "black") +
  geom_line(data = subset(narea.no3n, pft == "overall"),
            aes(x = soil.no3n, y = emmean),
            color = "black", size = 1, lty = "dashed") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(italic("N")["area"]*" (gN m"^"-2"~")")),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.no3n.ind

narea.no3n.int <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = soil.no3n, y = narea)) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 4, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(narea.no3n, pft != "overall"), 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(narea.no3n, pft != "overall"),
            aes(x = soil.no3n, y = emmean, color = pft),
            size = 1, lty = 2) +
  # geom_ribbon(data = subset(narea.no3n, pft == "overall"), 
  #             aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), alpha = 0.25, fill = "black") +
  # geom_line(data = subset(narea.no3n, pft == "overall"),
  #           aes(x = soil.no3n, y = emmean),
  #           color = "black", size = 1, lty = "dashed") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(italic("N")["area"]*" (gN m"^"-2"~")")),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.no3n.int

##########################################################################
## Narea - beta
##########################################################################
narea.beta0 <- data.frame(beta = 0,
                          emmeans(narea, ~pft, at = list(beta = 0), 
                                  type = "response"))
narea.beta100 <- data.frame(beta = 100,
                            emmeans(narea, ~pft, at = list(beta = 100), 
                                    type = "response"))
narea.beta200 <- data.frame(beta = 200,
                            emmeans(narea, ~pft, at = list(beta = 200), 
                                    type = "response"))
narea.beta300 <- data.frame(beta = 300,
                            emmeans(narea, ~pft, at = list(beta = 300), 
                                    type = "response"))
narea.beta400 <- data.frame(beta = 400,
                            emmeans(narea, ~pft, at = list(beta = 400), 
                                    type = "response"))
narea.beta500 <- data.frame(beta = 500,
                            emmeans(narea, ~pft, at = list(beta = 500), 
                                    type = "response"))
narea.beta600 <- data.frame(beta = 600,
                            emmeans(narea, ~pft, at = list(beta = 600), 
                                    type = "response"))
narea.beta0.nopft <- data.frame(beta = 0,
                                emmeans(narea, ~1, at = list(beta = 0), 
                                        type = "response"))
narea.beta100.nopft <- data.frame(beta = 100,
                                  emmeans(narea, ~1, at = list(beta = 100), 
                                          type = "response"))
narea.beta200.nopft <- data.frame(beta = 200,
                                  emmeans(narea, ~1, at = list(beta = 200), 
                                          type = "response"))
narea.beta300.nopft <- data.frame(beta = 300,
                                  emmeans(narea, ~1, at = list(beta = 300), 
                                          type = "response"))
narea.beta400.nopft <- data.frame(beta = 400,
                                  emmeans(narea, ~1, at = list(beta = 400), 
                                          type = "response"))
narea.beta500.nopft <- data.frame(beta = 500,
                                  emmeans(narea, ~1, at = list(beta = 500), 
                                          type = "response"))
narea.beta600.nopft <- data.frame(beta = 600,
                                  emmeans(narea, ~1, at = list(beta = 600), 
                                          type = "response"))
narea.beta.pft <- narea.beta0 %>% full_join(narea.beta100) %>% 
  full_join(narea.beta200) %>% full_join(narea.beta300) %>% 
  full_join(narea.beta400) %>% full_join(narea.beta500) %>% 
  full_join(narea.beta600)
narea.beta <- narea.beta0.nopft %>% full_join(narea.beta100.nopft) %>% 
  full_join(narea.beta200.nopft) %>% full_join(narea.beta300.nopft) %>% 
  full_join(narea.beta400.nopft) %>% full_join(narea.beta500.nopft) %>% 
  full_join(narea.beta600.nopft)%>% dplyr::select(beta, pft = X1, everything()) %>%
  full_join(narea.beta.pft) %>% dplyr::select(beta, pft, emmean = response, everything())

narea.beta.ind <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = beta, y = narea)) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 4, alpha = 0.7, shape = 21) +
  # geom_ribbon(data = subset(narea.beta, pft =="c4_nonlegume" & beta < 300),
  #             aes(x = beta, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), alpha = 0.25, fill = cbbPalette3[3]) +
  # geom_ribbon(data = subset(narea.beta, pft == "c3_legume" & beta <=500), 
  #             aes(x = beta, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_ribbon(data = subset(narea.beta, pft == "c3_nonlegume"), 
  #             aes(x = beta, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  # geom_line(data = subset(narea.beta, pft =="c4_nonlegume" & beta < 300),
  #           aes(x = beta, y = emmean), 
  #           color = cbbPalette3[3], size = 1, lty = 2) +
  # geom_line(data = subset(narea.beta, pft == "c3_legume" & beta <=500),
  #           aes(x = beta, y = emmean), color = cbbPalette3[1], size = 1) +
  # geom_line(data = subset(narea.beta, pft == "c3_nonlegume"),
  #           aes(x = beta, y = emmean), color = cbbPalette3[2], size = 1) +
  geom_ribbon(data = subset(narea.beta, pft == "overall"),
              aes(x = beta, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), alpha = 0.25, fill = "black") +
  geom_line(data = subset(narea.beta, pft == "overall"),
            aes(x = beta, y = emmean),
            color = "black", size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = expression(bold("Unit cost ratio ("*beta*")")),
       y = expression(bold(italic("N")["area"]*" (gN m"^"-2"~")")),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.beta.ind

narea.beta.int <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = beta, y = narea)) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 4, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(narea.beta, pft =="c4_nonlegume" & beta < 300),
              aes(x = beta, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), alpha = 0.25, fill = cbbPalette3[3]) +
  geom_ribbon(data = subset(narea.beta, pft == "c3_legume" & beta <=500), 
              aes(x = beta, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_ribbon(data = subset(narea.beta, pft == "c3_nonlegume"), 
              aes(x = beta, y = emmean, ymin = lower.CL, 
                 ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(narea.beta, pft =="c4_nonlegume" & beta < 300),
              aes(x = beta, y = emmean), 
            color = cbbPalette3[3], size = 1, lty = 2) +
  geom_line(data = subset(narea.beta, pft == "c3_legume" & beta <=500),
            aes(x = beta, y = emmean), color = cbbPalette3[1], size = 1) +
  geom_line(data = subset(narea.beta, pft == "c3_nonlegume"),
            aes(x = beta, y = emmean), color = cbbPalette3[2], size = 1, lty = 2) +
  # geom_ribbon(data = subset(narea.beta, pft == "overall"), 
  #             aes(x = beta, y = emmean, ymin = lower.CL, 
  #                 ymax = upper.CL), alpha = 0.25, fill = "black") +
  # geom_line(data = subset(narea.beta, pft == "overall"),
  #           aes(x = beta, y = emmean),
  #           color = "black", size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = expression(bold("Unit cost ratio ("*beta*")")),
       y = expression(bold(italic("N")["area"]*" (gN m"^"-2"~")")),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.beta.int

test(emtrends(narea, ~pft, "beta"))

##########################################################################
## Create Narea plots
##########################################################################
png("../working_drafts/figs/TXeco_fig4_narea.png",
    width = 11.5, height = 4.5, units = 'in', res = 600)
ggarrange(narea.no3n.ind, narea.beta.int,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()





