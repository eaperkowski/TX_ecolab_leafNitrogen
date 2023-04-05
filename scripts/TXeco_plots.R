##########################################################################
## Load libraries and import data
##########################################################################
# Libraries
library(lme4)
library(emmeans)
library(tidyverse)
library(ggpubr)
library(car)

# Turn off digit rounding in emmean args
emm_options(opt.digits = FALSE)

source("../../r_functions/propN_funcs.R")

# Load compiled datasheet
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(site != "Bell_2020_05" & 
           site != "Russel_2020_01") %>%
  filter(pft != "c3_shrub") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb",
                             "c3_nonlegume", 
                             ifelse(pft == "legume", 
                                    "c3_legume", 
                                    NA))),
         chi = ifelse(chi > 0.95 | chi < 0.10, NA, chi),
         marea = ifelse(marea > 1000, NA, marea))

## Add colorblind friendly palette
cbbPalette3 <- c("#DDAA33", "#004488", "#BB5566")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_legume"])
length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c4_nonlegume"])

## Convert VPD from hPa (PRISM units) to kPa (standard)
df$vpd4 <- df$vpd4 / 10

## Remove outliers from statistical models
df$beta[c(33, 277, 289, 465, 468)] <- NA
df$chi[c(371, 385, 485, 492, 499)] <- NA
df$narea[df$narea > 10] <- NA
df$narea[c(275, 493)] <- NA
df$n.leaf[c(360, 493)] <- NA
df$marea[df$marea > 1000] <- NA
df$marea[c(20, 21, 272, 275)] <- NA


## Add general models
beta <- lmer(log(beta) ~ wn3_perc * soil.no3n * pft + (1 | NCRS.code), data = df)
chi <- lmer(chi ~ (vpd4+ (wn3_perc * soil.no3n)) * pft + (1 | NCRS.code), data = df)
narea <- lmer(log(narea) ~ (chi + (soil.no3n * wn3_perc)) * pft + (1 | NCRS.code), data = df)
nmass <- lmer(log(n.leaf) ~ (chi + (soil.no3n * wn3_perc)) * pft + (1 | NCRS.code), data = df)
marea <- lmer(log(marea) ~ (chi + (soil.no3n * wn3_perc)) * pft + (1 | NCRS.code), data = df)

##########################################################################
## Beta - soil N
##########################################################################
Anova(beta)
test(emtrends(beta, ~pft, "soil.no3n"))


beta.no3n.ind <- data.frame(emmeans(beta, ~1, "soil.no3n",
                                    at = list(soil.no3n = seq(0,80,1))))
beta.no3n.pft <- data.frame(emmeans(beta, ~pft, "soil.no3n",
                                    at = list(soil.no3n = seq(0,80,1)))) %>%
  filter(pft == "c4_nonlegume")

## Plot
beta.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
  geom_jitter(aes(fill = pft),
               width = 0.5, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = beta.no3n.pft, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), fill = "#BB5566", alpha = 0.25) +
  geom_line(data = beta.no3n.pft, aes(x = soil.no3n, y = emmean), 
            size = 2, color = "#BB5566") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-10, 6.2), breaks = seq(-10, 6, 4)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold")) +
  guides(color = "none")
beta.no3n

##########################################################################
## Beta - soil moisture
##########################################################################
test(emtrends(beta, ~pft, "wn3_perc"))

beta.sm.pft <- data.frame(emmeans(beta, ~pft, "wn3_perc",
                                    at = list(wn3_perc = seq(0.1,1,0.01)))) %>%
  filter(pft == "c4_nonlegume")

beta.sm.ind <- data.frame(emmeans(beta, ~1, "wn3_perc",
                                  at = list(wn3_perc = seq(0.1,1,0.01))))

# Plot
beta.h2o <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn3_perc, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.01, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = beta.sm.pft,
              aes(x = wn3_perc, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), alpha = 0.25, fill = "#BB5566") +
  geom_line(data = beta.sm.pft, aes(x = wn3_perc, y = emmean),
            size = 2, color = "#BB5566") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.09, 1.0), breaks = seq(0.1, 1.0, 0.3),
                     labels = c(10, 40, 70, 100)) +
  scale_y_continuous(limits = c(-10, 6.2), breaks = seq(-10, 6, 4)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group",
       color = "Functional group") +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold"))
beta.h2o

##########################################################################
## Write beta plot
##########################################################################
# Write plot
jpeg("../working_drafts/figs/TXeco_fig2_beta.jpg",
    width = 12, height = 4.5, units = 'in', res = 600)
ggarrange(beta.no3n, beta.h2o,
          nrow = 1, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = c("(a)", "(b)"), font.label = list(size = 18))
dev.off()

##########################################################################
## Chi - VPD
##########################################################################
test(emtrends(chi, ~pft, "vpd4"))

chi.vpd.pft <- data.frame(emmeans(chi, ~pft, "vpd4",
                                  at = list(vpd4 = seq(0.9,1.3,0.01)))) %>%
  filter(pft == "c3_legume" | pft == "c3_nonlegume")
chi.vpd.ind <- data.frame(emmeans(chi, ~1, "vpd4",
                                  at = list(vpd4 = seq(0.9,1.3,0.01))))

chi.vpd <- ggplot(data = df, aes(x = vpd4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.01, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = chi.vpd.pft, 
              aes(x = vpd4, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = chi.vpd.pft, aes(x = vpd4, y = emmean, color = pft), 
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c("#DDAA33", "#004488"), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.9, 1.3), breaks = seq(0.9, 1.3, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold("C"["i"]*" : C"["a"])),
       fill = expression(bold("Functional group"))) +
  guides(color = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.vpd

##########################################################################
## Chi - soil N
##########################################################################
test(emtrends(chi, ~pft, "soil.no3n"))

chi.no3n.pft <- data.frame(emmeans(chi, ~pft, "soil.no3n",
                                  at = list(soil.no3n = seq(0,80,1)))) %>%
  filter(pft == "c4_nonlegume")

chi.no3n <- ggplot(data = df, aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = chi.no3n.pft, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), fill = "#BB5566", alpha = 0.25) +
  geom_line(data = chi.no3n.pft, aes(x = soil.no3n, y = emmean), 
            size = 2, color = "#BB5566") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold("C"["i"]*" : C"["a"])),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.no3n

##########################################################################
## Chi - Soil moisture
##########################################################################
Anova(chi)
test(emtrends(chi, ~pft, "wn3_perc"))

chi.sm <- ggplot(data = df, aes(x = wn3_perc, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.0075, size = 2.5, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.09, 1), breaks = seq(0.1, 1, 0.3),
                     labels = c(10, 40, 70, 100)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold("C"["i"]*" : C"["a"])),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25))
chi.sm

##########################################################################
## Write chi plot
##########################################################################
jpeg("../working_drafts/figs/TXeco_fig3_chi.jpg",
    width = 12, height = 9, units = 'in', res = 600)
ggarrange(chi.vpd, chi.sm, chi.no3n,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = c("(a)", "(b)", "(c)"), 
          font.label = list(size = 18))
dev.off()

##########################################################################
## Narea - chi
##########################################################################
Anova(narea)
test(emtrends(narea, ~pft, "chi"))

narea.chi.pft <- data.frame(emmeans(narea, ~pft, "chi",
                                     at = list(chi = seq(0.1, 1, 0.01)))) %>%
  filter(pft != "c4_nonlegume")
narea.chi.legume <- subset(narea.chi.pft, pft == "c3_legume" & chi > 0.50 & chi < 0.95)
narea.chi.c3non <- subset(narea.chi.pft, pft == "c3_nonlegume" & chi > 0.50 & chi < 0.95)
narea.chi.pft.cleaned <- narea.chi.legume %>%
  full_join(narea.chi.c3non)



narea.chi.ind <- data.frame(emmeans(narea, ~1, "chi",
                                    at = list(chi = seq(0.1, 1, 0.1))))

narea.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(narea))) +
  geom_point(aes(fill = pft),
             size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = narea.chi.pft.cleaned, 
              aes(x = chi, y = emmean, ymin = lower.CL, ymax = upper.CL,
                  fill = pft), 
              alpha = 0.25) +
  geom_line(data = narea.chi.pft.cleaned,
            aes(x = chi, y = emmean, color = pft),
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.09, 1), breaks = seq(0.1, 1, 0.3)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("C"["i"]*" : C"["a"])),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(color = "none")
narea.chi

##########################################################################
## Narea - soil N
##########################################################################
Anova(narea)

narea.no3n.ind <- data.frame(emmeans(narea, ~1, "soil.no3n",
                                     at = list(soil.no3n = seq(0, 80, 1))))

narea.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = soil.no3n, y = log(narea))) +
  geom_jitter(aes (fill = pft), width = 0.5, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = narea.no3n.ind, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25, fill = "black") +
  geom_line(data = narea.no3n.ind,
            aes(x = soil.no3n, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.no3n

##########################################################################
## Narea - soil moisture
##########################################################################
Anova(narea)

narea.sm.pft <- data.frame(emmeans(narea, ~1, "wn3_perc",
                                     at = list(wn3_perc = seq(0.1, 1, 0.01))))

narea.sm <- ggplot(data = subset(df, !is.na(pft)), 
                   aes(x = wn3_perc, y = log(narea))) +
  geom_jitter(aes(fill = pft),
              width = 0.0075, size = 2.5, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.09, 1), breaks = seq(0.1, 1, 0.3)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.sm

##########################################################################
## Nmass - chi
##########################################################################
Anova(nmass)
test(emtrends(nmass, ~pft, "chi"))


nmass.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(n.leaf))) +
  geom_point(aes(fill = pft), size = 2.5, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, 0.3)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("C"["i"]*" : C"["a"])),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) 
nmass.chi

##########################################################################
## Nmass - soil N
##########################################################################
Anova(nmass)

nmass.no3n.ind <- data.frame(emmeans(nmass, ~1, "soil.no3n",
                                 at = list(soil.no3n = seq(0, 80, 1)))) %>%
  dplyr::select(pft = X1, everything())

nmass.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = soil.no3n, y = log(n.leaf))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = nmass.no3n.ind,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = nmass.no3n.ind,
            aes(x = soil.no3n, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.no3n

##########################################################################
## Nmass - soil moisture
##########################################################################
Anova(nmass)

nmass.sm.ind <- data.frame(emmeans(nmass, ~1, "wn3_perc",
                                   at = list(wn3_perc = seq(0.1, 1, 0.01)))) %>%
  dplyr::select(pft = X1, everything())

nmass.sm <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = wn3_perc, y = log(n.leaf))) +
  geom_jitter(aes(fill = pft),
              width = 0.01, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.sm.ind,
              aes(x = wn3_perc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = nmass.sm.ind,
            aes(x = wn3_perc, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.09, 1), breaks = seq(0.1, 1, 0.3)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.sm

##########################################################################
## Marea - chi
##########################################################################
Anova(marea)
test(emtrends(marea, ~pft, "chi"))

marea.chi.pft <- data.frame(emmeans(marea, ~pft, "chi",
                                    at = list(chi = seq(0.1, 1, 0.01)))) %>%
  filter(pft != "c4_nonlegume")
marea.chi.legume <- subset(marea.chi.pft, pft == "c3_legume" & chi > 0.5 & chi < 0.95)
marea.chi.c3non <- subset(marea.chi.pft, pft == "c3_nonlegume" & chi > 0.5 & chi < 0.95)
marea.chi.pft.cleaned <- marea.chi.legume %>%
  full_join(marea.chi.c3non)

marea.chi.ind <- data.frame(emmeans(marea, ~1, "chi",
                                    at = list(chi = seq(0.1, 1, 0.01))))


marea.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(marea))) +
  geom_point(aes(fill = pft),
             size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = marea.chi.pft.cleaned,
              aes(x = chi, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL, fill = pft), 
              alpha = 0.25) +
  geom_line(data = marea.chi.pft.cleaned,
            aes(x = chi, y = emmean, color = pft),
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, 0.3)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("C"["i"]*" : C"["a"])),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(color = "none")
marea.chi

##########################################################################
## Marea - soil N
##########################################################################
test(emtrends(marea, ~pft, "soil.no3n"))
car::Anova(marea)

marea.no3n.pft <- data.frame(emmeans(marea, ~pft, "soil.no3n",
                                 at = list(soil.no3n = seq(0, 80, 1)))) %>%
  filter(pft != "c3_legume")

marea.no3n.c3non <- subset(marea.no3n.pft, pft == "c3_nonlegume" & chi > 0.5 & chi < 0.95)
marea.no3n.pft.cleaned <- marea.no3n.legume %>%
  full_join(marea.no3n.c3non)

marea.no3n.ind <- data.frame(emmeans(marea, ~1, "soil.no3n",
                                     at = list(soil.no3n = seq(0, 80, 1))))

marea.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = soil.no3n, y = log(marea))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = marea.no3n.pft,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL, fill = pft), 
              alpha = 0.25) +
  geom_line(data = marea.no3n.pft,
            aes(x = soil.no3n, y = emmean, 
                color = pft),
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c("#004488", "#BB5566"), 
                     labels = c(expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(color = "none")
marea.no3n

##########################################################################
## Marea - soil moisture
##########################################################################
test(emtrends(marea, ~pft, "wn3_perc"))
Anova(marea)

marea.sm.ind <- data.frame(emmeans(marea, ~1, "wn3_perc",
                                     at = list(wn3_perc = seq(0, 1, 0.01))))

marea.sm <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = wn3_perc, y = log(marea))) +
  geom_jitter(aes(fill = pft),
              width = 0.0075, size = 2.5, alpha = 0.75, shape = 21) +
  geom_ribbon(data = marea.sm.ind,
              aes(x = wn3_perc, y = emmean, ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = marea.sm.ind,
            aes(x = wn3_perc, y = emmean),
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.09, 1), breaks = seq(0.1, 1, 0.3)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(linetype = "none")

marea.sm

##########################################################################
## Create Narea plots
##########################################################################
jpeg("../working_drafts/figs/TXeco_fig4_narea.jpg",
    height = 12, width = 16, units = 'in', res = 600)
ggarrange(narea.chi, nmass.chi, marea.chi,
          narea.no3n, nmass.no3n, marea.no3n,
          narea.sm, nmass.sm, marea.sm,
          ncol = 3, nrow = 3, common.legend = TRUE, legend = "right", 
          align = "hv", labels = c("(a)", "(b)", "(c)", "(d)", "(e)",
                                   "(f)", "(g)", "(h)", "(i)"), 
          font.label = list(size = 18))
dev.off()


beta.var <- ggplot(data = df, aes(x = log(beta))) +
  geom_density(aes(fill = photo), alpha = 0.75) +
  scale_fill_manual(values = c("#004488", "#BB5566"),
                    labels = c(expression("C"[3]),
                               expression("C"[4]))) +
  scale_x_continuous(limits = c(-4, 6), breaks = seq(-4, 6, 2)) +
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.2)) +
  labs(x = expression(bold(ln~beta)),
       y = "Density",
       fill = "Photo. pathway") +
  theme_bw(base_size = 18)

png("../working_drafts/figs/TXeco_figS2_betaVar.png",
    height = 4.5, width = 8, units = 'in', res = 600)
beta.var
dev.off()




