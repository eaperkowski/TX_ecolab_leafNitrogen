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

# Load compiled datasheet
df <- read.csv("../../TXeco/data/TXeco_data.csv",
               na.strings = c("NA", "NaN")) %>%
  filter(site != "Fayette_2019_04") %>%
  mutate(pft = ifelse(pft == "c4_graminoid", 
                      "c4_nonlegume",
                      ifelse(pft == "c3_graminoid" | pft == "c3_forb",
                             "c3_nonlegume", 
                             ifelse(pft == "c3_legume", 
                                    "c3_legume", 
                                    ifelse(pft == "c3_shrub",
                                           "c3_nonlegume",
                                           NA)))),
         beta = ifelse(beta > 2000, NA, beta),
         beta = ifelse(pft == "c4_nonlegume" & beta > 400, NA, beta),
         beta = ifelse(pft == "c3_legume" & beta > 1000, NA, beta),
         marea = ifelse(marea > 1000, NA, marea),
         pft = factor(pft, 
                      levels = c("c3_legume", "c3_nonlegume", "c4_nonlegume"))) %>%
  filter(!is.na(pft))

## Add colorblind friendly palette
cbbPalette3 <- c("#DDAA33", "#4477AA", "#BB5566")

## Figure out sample sizes within each pft class
length(df$pft[df$pft == "c3_legume"])
length(df$pft[df$pft == "c3_nonlegume"])
length(df$pft[df$pft == "c4_nonlegume"])

## Remove outliers from statistical models
df$beta[c(403, 422)] <- NA
df$chi[c(134, 382, 412, 510)] <- NA
df$chi[c(451, 454, 503)] <- NA
df$chi[c(178, 445, 496)] <- NA
df$chi[c(233)] <- NA
df$narea[df$narea > 10] <- NA
df$narea[284] <- NA
df$n.leaf[504] <- NA
df$marea[df$marea > 1000] <- NA
df$marea[c(20, 21, 284, 318)] <- NA

## Add general models
beta <- lmer(sqrt(beta) ~ wn90_perc * soil.no3n * pft + (1 | NCRS.code), 
             data = df)
chi <- lmer(chi ~ (vpd90+ (wn90_perc * soil.no3n)) * pft + (1 | NCRS.code), 
            data = df)
narea <- lmer(log(narea) ~ (chi + (soil.no3n * wn90_perc)) * pft + 
                (1 | NCRS.code), data = df)
nmass <- lmer(log(n.leaf) ~ (chi + (soil.no3n * wn90_perc)) * pft + 
                (1 | NCRS.code), data = df)
marea <- lmer(log(marea) ~ (chi + (soil.no3n * wn90_perc)) * pft + 
                (1 | NCRS.code), data = df)

##########################################################################
## Beta - soil N
##########################################################################
Anova(beta)
test(emtrends(beta, ~pft, "soil.no3n"))

beta.no3n.ind <- data.frame(emmeans(beta, ~1, "soil.no3n",
                                    at = list(soil.no3n = seq(0, 80, 1))))

## Plot
beta.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                    aes(x = soil.no3n, y = sqrt(beta))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = beta.no3n.ind, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), fill = "black", alpha = 0.5) +
  geom_line(data = beta.no3n.ind, aes(x = soil.no3n, y = emmean), 
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 15)) +
  labs(x = expression(bold("N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(sqrt(beta))),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold")) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none")
beta.no3n

##########################################################################
## Beta - soil moisture parsed by PFT
##########################################################################
Anova(beta)
test(emtrends(beta, ~pft, "wn90_perc"))

beta.sm.pft <- data.frame(emmeans(beta, ~pft, "wn90_perc",
                                  at = list(wn90_perc = seq(0,1,0.01)))) %>%
  filter(pft == "c3_nonlegume")

# Plot
beta.h2o <- ggplot(data = subset(df, !is.na(pft)), 
                   aes(x = wn90_perc, y = sqrt(beta))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = beta.sm.pft,
              aes(x = wn90_perc, y = emmean, ymin = lower.CL,
                  ymax = upper.CL),
              alpha = 0.5, fill = "#4477AA") +
  geom_line(data = beta.sm.pft,
            aes(x = wn90_perc, y = emmean),
            size = 2, color = "#4477AA") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.125, 0.775), breaks = seq(0.15, 0.75, 0.15),
                     labels = seq(15, 75, 15)) +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 15)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(sqrt(beta))),
       fill = "Functional group",
       color = "Functional group") +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold"))
beta.h2o

##########################################################################
## Beta - soil moisture parsed by PFT
##########################################################################
beta.sm.ind <- data.frame(emmeans(beta, ~1, "wn90_perc",
                                  at = list(wn90_perc = seq(0,1,0.01))))

# Plot
beta.h2o.ind.plot <- ggplot(data = subset(df, !is.na(pft)),
                            aes(x = wn90_perc, y = sqrt(beta))) +
  geom_point(aes(fill = pft), size = 3, shape = 21) +
  geom_ribbon(data = beta.sm.ind,
              aes(x = wn90_perc, y = emmean, ymin = lower.CL,
                  ymax = upper.CL), alpha = 0.5) +
  geom_line(data = beta.sm.ind,
            aes(x = wn90_perc, y = emmean), linewidth = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.125, 0.775), breaks = seq(0.15, 0.75, 0.15),
                     labels = seq(15, 75, 15)) +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 15)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(sqrt(beta))),
       fill = "Functional group") +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25),
        legend.title = element_text(face = "bold"))
beta.h2o

##########################################################################
## Write beta plot
##########################################################################
# Write plot
jpeg("../../TX_ecolab_leafNitrogen/working_drafts/figs/TXeco_fig3_beta.jpg",
    width = 12, height = 4.5, units = 'in', res = 600)
ggarrange(beta.no3n, beta.h2o,
          nrow = 1, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = c("(a)", "(b)"), font.label = list(size = 18))
dev.off()

# Write plot for mid-term review
jpeg("../../TX_ecolab_leafNitrogen/working_drafts/figs/TXeco_fig3_beta_midterm.jpg",
     width = 12, height = 4.5, units = 'in', res = 600)
ggarrange(beta.no3n, beta.h2o.ind.plot,
          nrow = 1, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = c("(a)", "(b)"), font.label = list(size = 18))
dev.off()

##########################################################################
## Chi - VPD
##########################################################################
Anova(chi)
test(emtrends(chi, ~pft, "vpd90"))

chi.vpd.ind <- data.frame(
  emmeans(chi, ~pft, "vpd90", at = list(vpd90 = seq(0.9, 1.4, 0.01)))) %>%
  filter(pft != "c4_nonlegume")

chi.vpd <- ggplot(data = df, aes(x = vpd90, y = chi)) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = chi.vpd.ind, 
              aes(x = vpd90, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.5) +
  geom_line(data = chi.vpd.ind, aes(x = vpd90, y = emmean, color = pft), 
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.9, 1.4), breaks = seq(0.9, 1.4, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold("Leaf C"["i"]*" : C"["a"]*" (unitless)")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25)) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
chi.vpd

##########################################################################
## Chi - soil N
##########################################################################
Anova(chi)
test(emtrends(chi, ~pft, "soil.no3n"))

chi.no3n.pft <- data.frame(emmeans(chi, ~pft, "soil.no3n",
                                  at = list(soil.no3n = seq(0,80,1)))) %>%
  filter(pft == "c4_nonlegume")

chi.no3n <- ggplot(data = df, aes(x = soil.no3n, y = chi)) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = chi.no3n.pft, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.5) +
  geom_line(data = chi.no3n.pft, aes(x = soil.no3n, y = emmean, color = pft), 
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = cbbPalette3[3], 
                    labels = expression("C"[4]*" non-fixer")) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = expression(bold("N availability (ppm NO"[3]*"-N)")),
       y = expression(bold("Leaf C"["i"]*" : C"["a"]*" (unitless)")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25)) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
chi.no3n

##########################################################################
## Chi - Soil moisture
##########################################################################
Anova(chi)
test(emtrends(chi, ~pft, "wn90_perc"))

chi.sm.pft <- data.frame(
  emmeans(chi, ~pft, "wn90_perc", at = list(wn90_perc = seq(0.15,0.75,0.1)))) %>%
  filter(pft == "c4_nonlegume")

chi.sm <- ggplot(data = df, aes(x = wn90_perc, y = chi)) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = chi.sm.pft, 
              aes(x = wn90_perc, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.5) +
  geom_line(data = chi.sm.pft, aes(x = wn90_perc, y = emmean, color = pft), 
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = cbbPalette3[3], 
                    labels = expression("C"[4]*" non-fixer")) +
  scale_x_continuous(limits = c(0.125, 0.775), breaks = seq(0.15, 0.75, 0.15),
                     labels = seq(15, 75, 15)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold("Leaf C"["i"]*" : C"["a"]*" (unitless)")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25)) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
chi.sm

##########################################################################
## Write chi plot
##########################################################################
jpeg("../../TX_ecolab_leafNitrogen/working_drafts/figs/TXeco_fig4_chi.jpg",
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
                                     at = list(chi = seq(0, 1, 0.01)))) %>%
  filter(pft != "c4_nonlegume")
narea.chi.legume <- subset(narea.chi.pft, pft == "c3_legume" & chi > 0.50 & chi < 1)
narea.chi.c3non <- subset(narea.chi.pft, pft == "c3_nonlegume" & chi > 0.55 & chi < 1)
narea.chi.pft.cleaned <- narea.chi.legume %>%
  full_join(narea.chi.c3non)

narea.chi <- ggplot(data = subset(df, !is.na(pft)),
                    aes(x = chi, y = log(narea))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = narea.chi.pft.cleaned, 
              aes(x = chi, y = emmean, ymin = lower.CL, ymax = upper.CL,
                  fill = pft), alpha = 0.5) +
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
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("Leaf C"["i"]*" : C"["a"]*" (unitless)")),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"*")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
narea.chi

##########################################################################
## Narea - soil N
##########################################################################
Anova(narea)
test(emtrends(narea, ~pft, "soil.no3n"))

narea.no3n.ind <- data.frame(emmeans(narea, ~1, "soil.no3n",
                                     at = list(soil.no3n = seq(0, 80, 1))))

narea.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = soil.no3n, y = log(narea))) +
  geom_point(aes (fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = narea.no3n.ind, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.5, fill = "black") +
  geom_line(data = narea.no3n.ind,
            aes(x = soil.no3n, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"*")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
narea.no3n

##########################################################################
## Narea - soil moisture
##########################################################################
Anova(narea)

narea.sm.ind <- data.frame(emmeans(narea, ~1, "wn90_perc",
                                     at = list(wn90_perc = seq(0.15, 0.75, 0.01))))

narea.sm <- ggplot(data = subset(df, !is.na(pft)), 
                   aes(x = wn90_perc, y = log(narea))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = narea.sm.ind, 
              aes(x = wn90_perc, y = emmean, ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.5, fill = "black") +
  geom_line(data = narea.sm.ind,
            aes(x = wn90_perc, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.125, 0.775), breaks = seq(0.15, 0.75, 0.15),
                     labels = seq(15, 75, 15)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"*")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
narea.sm

##########################################################################
## Nmass - chi
##########################################################################
Anova(nmass)
test(emtrends(nmass, ~pft, "chi"))

nmass.chi.ind <- data.frame(
  emmeans(nmass, ~pft, "chi", at = list(chi = seq(0, 1, 0.01)))) %>%
  filter(pft == "c3_legume" & chi > 0.5)

nmass.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(n.leaf))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("Leaf C"["i"]*" : C"["a"]*" (unitless)")),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"*")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
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
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = nmass.no3n.ind,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.5) +
  geom_line(data = nmass.no3n.ind,
            aes(x = soil.no3n, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"*")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
nmass.no3n

##########################################################################
## Nmass - soil moisture
##########################################################################
Anova(nmass)

nmass.sm.ind <- data.frame(emmeans(nmass, ~1, "wn90_perc",
                                   at = list(wn90_perc = seq(0.1, 1, 0.01))))

nmass.sm <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = wn90_perc, y = log(n.leaf))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = nmass.sm.ind,
              aes(x = wn90_perc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.5) +
  geom_line(data = nmass.sm.ind,
            aes(x = wn90_perc, y = emmean),
            size = 2, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.125, 0.775), breaks = seq(0.15, 0.75, 0.15),
                     labels = seq(15, 75, 15)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"*")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
nmass.sm

##########################################################################
## Marea - chi
##########################################################################
Anova(marea)
test(emtrends(marea, ~pft, "chi"))

marea.chi.pft <- data.frame(emmeans(marea, ~pft, "chi",
                                    at = list(chi = seq(0, 1, 0.01)))) %>%
  filter(pft != "c4_nonlegume")
marea.chi.legume <- subset(marea.chi.pft, pft == "c3_legume" & chi > 0.5 & chi < 1)
marea.chi.c3non <- subset(marea.chi.pft, pft == "c3_nonlegume" & chi > 0.55 & chi < 1)
marea.chi.pft.cleaned <- marea.chi.legume %>%
  full_join(marea.chi.c3non)

marea.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(marea))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = marea.chi.pft.cleaned,
              aes(x = chi, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL, fill = pft), 
              alpha = 0.5) +
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
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Leaf C"["i"]*" : C"["a"]*" (unitless)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"*")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
marea.chi

##########################################################################
## Marea - soil N
##########################################################################
Anova(marea)
test(emtrends(marea, ~pft, "soil.no3n"))

marea.no3n.pft <- data.frame(emmeans(marea, ~pft, "soil.no3n",
                                     at = list(soil.no3n = seq(0, 80, 1)))) %>%
  filter(pft!= "c4_nonlegume")

marea.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = soil.no3n, y = log(marea))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  geom_ribbon(data = marea.no3n.pft,
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.5) +
  geom_line(data = marea.no3n.pft,
            aes(x = soil.no3n, y = emmean, color = pft),
            size = 2) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(3, 6), breaks = seq(3, 6, 1)) +
  labs(x = expression(bold("N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"*")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
marea.no3n

##########################################################################
## Marea - soil moisture
##########################################################################
Anova(marea)
test(emtrends(marea, ~pft, "wn90_perc"))

marea.sm.ind <- data.frame(
  emmeans(marea, ~1, "wn90_perc", at = list(wn90_perc = seq(0.15, 0.75, 0.01))))

marea.sm <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = wn90_perc, y = log(marea))) +
  geom_point(aes(fill = pft, shape = pft), size = 3, alpha = 0.5) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]*" N-fixer"),
                               expression("C"[3]*" non-fixer"),
                               expression("C"[4]*" non-fixer"))) +
  scale_shape_manual(values = c(22, 21, 23),
                     labels = c(expression("C"[3]*" N-fixer"),
                                expression("C"[3]*" non-fixer"),
                                expression("C"[4]*" non-fixer"))) +
  scale_x_continuous(limits = c(0.125, 0.775), breaks = seq(0.15, 0.75, 0.15)) +
  scale_y_continuous(limits = c(3, 6), breaks = seq(3, 6, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"*")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 23))),
         shape = "none", color = "none")
marea.sm

##########################################################################
## Create Narea plots
##########################################################################
jpeg("../../TX_ecolab_leafNitrogen/working_drafts/figs/TXeco_fig5_narea.jpg",
    height = 12, width = 16, units = 'in', res = 600)
ggarrange(narea.chi, narea.no3n, narea.sm, nmass.chi, nmass.no3n, nmass.sm,
          marea.chi, marea.no3n, marea.sm, ncol = 3, nrow = 3, 
          common.legend = TRUE, legend = "right", align = "hv", 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", 
                     "(f)", "(g)", "(h)", "(i)"), 
          font.label = list(size = 18))
dev.off()

##########################################################################
## Create density plot explaining variance in beta
##########################################################################
beta.var <- ggplot(data = df, aes(x = sqrt(beta), color = pft)) +
  geom_density(aes(color = photo), fill = NA, linewidth = 2) +
  scale_color_manual(values = c("#4477AA", "#BB5566"),
                     labels = c(expression("C"[3]),
                                expression("C"[4]))) +
  scale_x_continuous(limits = c(-2, 45), breaks = seq(0, 45, 15)) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
  labs(x = expression(bold(sqrt(beta))),
       y = "Density",
       color = "Photosynthetic\npathway") +
  theme_bw(base_size = 18)
beta.var

png("../working_drafts/figs/TXeco_figS2_betaVar.png",
    height = 4.5, width = 8, units = 'in', res = 600)
beta.var
dev.off()
