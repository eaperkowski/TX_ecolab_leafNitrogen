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
         chi = ifelse(chi > 0.95 & chi < 0.20, NA, chi))

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

## Remove outliers from statistical models
df$beta[c(16, 387, 436, 459, 515)] <- NA
df$beta[c(17, 456)] <- NA
df$beta[c(435)] <- NA
df$chi[c(16, 378, 417, 436, 515)] <- NA
df$chi[c(17, 387, 435)] <- NA
df$chi[c(416, 456, 508)] <- NA
df$chi[c(321, 325)] <- NA
df$chi[c(71, 402)] <- NA
df$narea[df$narea > 10] <- NA
df$narea[509] <- NA
df$n.leaf[c(375, 509)] <- NA
df$marea[c(20, 21)] <- NA

## Add general models
beta <- lmer(log(beta) ~ wn2_perc * soil.no3n * pft + (1 | NCRS.code), data = df)
chi <- lmer(chi ~ (vpd4+ (wn2_perc * soil.no3n)) * pft + 
              (1 | NCRS.code), data = df)
narea <- lmer(log(narea) ~ (chi + (soil.no3n * wn2_perc)) * pft + (1 | NCRS.code),
              data = df)

##########################################################################
## Beta - soil N
##########################################################################
test(emtrends(beta, ~pft, "soil.no3n"))

beta.no3n.pft <- data.frame(emmeans(beta, ~pft, "soil.no3n",
                                    at = list(soil.no3n = seq(0,80,1)))) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume", "dashed", "solid"))

## Plot
beta.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
   geom_jitter(aes(fill = pft),
               width = 0.5, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = beta.no3n.pft, 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = beta.no3n.pft,
            aes(x = soil.no3n, y = emmean, color = pft, linetype = linetype), 
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-3, 6), breaks = seq(-3, 6, 3)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group",
       color = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold")) +
  guides(linetype = "none")
beta.no3n

##########################################################################
## Beta - soil moisture
##########################################################################
test(emtrends(beta, ~pft, "wn3_perc"))

beta.sm.pft <- data.frame(emmeans(beta, ~pft, "wn2_perc",
                                    at = list(wn2_perc = seq(0,1,0.01))))

beta.sm <- data.frame(emmeans(beta, ~1, "wn2_perc",
                                at = list(wn2_perc = seq(0,1,0.01)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(beta.sm.pft) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume", "solid", "dashed"))

# Plot
beta.h2o <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn2_perc, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.01, size = 3, alpha = 0.5, shape = 21) +
  # geom_ribbon(data = subset(beta.sm, pft != "overall"),
  #             aes(x = wn2_perc, y = emmean, ymin = lower.CL,
  #                 ymax = upper.CL, fill = pft),
  #             alpha = 0.3) +
  # geom_line(data = subset(beta.sm, pft != "overall"),
  #           aes(x = wn2_perc, y = emmean, linetype = linetype, color = pft),
  #           size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(limits = c(-3, 6), breaks = seq(-3, 6, 3)) +
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
png("../working_drafts/figs/TXeco_fig2_beta.png",
    width = 12, height = 4.5, units = 'in', res = 600)
ggarrange(beta.no3n, beta.h2o,
          nrow = 1, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))

dev.off()

##########################################################################
## Chi - VPD
##########################################################################
test(emtrends(chi, ~pft, "vpd4"))

chi.vpd.pft <- data.frame(emmeans(chi, ~pft, "vpd4",
                                  at = list(vpd4 = seq(0.8,1.4,0.01)))) %>%
  mutate(linetype = ifelse(pft == "c3_nonlegume", "solid", "dashed"))

chi.vpd <- ggplot(data = df, aes(x = vpd4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = chi.vpd.pft, 
              aes(x = vpd4, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = chi.vpd.pft, 
            aes(x = vpd4, y = emmean, color = pft, linetype = linetype), 
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0.8, 1.41), breaks = seq(0.8, 1.4, 0.15)) +
  scale_y_continuous(limits = c(0.15, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold(chi)),
       fill = expression(bold("Functional group")), 
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25))
chi.vpd

##########################################################################
## Chi - soil N
##########################################################################
test(emtrends(chi, ~pft, "soil.no3n"))

chi.no3n.pft <- data.frame(emmeans(chi, ~pft, "soil.no3n",
                                  at = list(soil.no3n = seq(0,80,1)))) %>%
  mutate(linetype = ifelse(pft == "c3_legume", "solid", "dashed"))

chi.no3n <- ggplot(data = df, aes(x = soil.no3n, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = chi.no3n.pft,
              aes(x = soil.no3n, y = emmean, ymin = lower.CL,
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = chi.no3n.pft,
            aes(x = soil.no3n, y = emmean, color = pft, linetype = linetype), 
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0.15, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(chi)),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25))
chi.no3n

##########################################################################
## Chi - Soil moisture
##########################################################################
test(emtrends(chi, ~pft, "wn2_perc"))

chi.sm.pft <- data.frame(emmeans(chi, ~pft, "wn2_perc",
                                   at = list(wn2_perc = seq(0, 1, 0.01)))) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume", "solid", "dashed"))

chi.sm <- ggplot(data = df, aes(x = wn2_perc, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = chi.sm.pft, 
              aes(x = wn2_perc, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = chi.sm.pft, 
            aes(x = wn2_perc, y = emmean, color = pft, linetype = linetype),
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(limits = c(0.15, 1), breaks = seq(0.2, 1, 0.2)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(chi)),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25))
chi.sm

##########################################################################
## Write chi plot
##########################################################################
png("../working_drafts/figs/TXeco_fig3_chi.png",
    width = 12, height = 9, units = 'in', res = 600)
ggarrange(chi.vpd, chi.sm, chi.no3n,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()

##########################################################################
## Narea - chi
##########################################################################
test(emtrends(narea, ~pft, "chi"))
car::Anova(narea)

narea.chi.pft <- data.frame(emmeans(narea, ~pft, "chi",
                                     at = list(chi = seq(0.2, 1, 0.01)))) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume", "dashed", "solid"))


narea.chi.legume <- subset(narea.chi.pft, pft == "c3_legume" & chi > 0.55 & chi < 0.95)
narea.chi.c3non <- subset(narea.chi.pft, pft == "c3_nonlegume" & chi > 0.55 & chi < 0.95)
narea.chi.c4non <- subset(narea.chi.pft, pft == "c4_nonlegume" & chi < 0.95)


narea.chi.pft.cleaned <- narea.chi.legume %>%
  full_join(narea.chi.c3non) %>%
  full_join(narea.chi.c4non)

narea.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(narea))) +
  geom_point(aes(fill = pft),
             size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = narea.chi.pft.cleaned, 
              aes(x = chi, y = emmean, ymin = lower.CL, ymax = upper.CL,
                  fill = pft), 
              alpha = 0.25) +
  geom_line(data = narea.chi.pft.cleaned,
            aes(x = chi, y = emmean, linetype = linetype, color = pft),
            size = 0.9) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = expression(bold(chi)),
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(linetype = "none")
narea.chi

##########################################################################
## Narea - soil N
##########################################################################
narea.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(narea),
                            fill = factor(pft, levels = c("c3_legume",
                                                          "c4_nonlegume",
                                                          "c3_nonlegume")))) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  scale_fill_manual(values = cbbPalette3[1:3], 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
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
narea.sm <- ggplot(data = subset(df, !is.na(pft)), 
                   aes(x = wn2_perc, y = log(narea))) +
  geom_jitter(aes(fill = pft),
              width = 0.025, size = 3, alpha = 0.7, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
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
test(emtrends(nmass, ~pft, "chi"))

nmass.chi.pft <- data.frame(emmeans(nmass, ~pft, "chi",
                                    at = list(chi = seq(0.2, 1, 0.01)))) %>%
  mutate(linetype = ifelse(pft == "c3_nonlegume","solid", "dashed"))


nmass.chi.legume <- subset(nmass.chi.pft, pft == "c3_legume" & chi > 0.55 & chi < 0.95)
nmass.chi.c3non <- subset(nmass.chi.pft, pft == "c3_nonlegume" & chi > 0.55 & chi < 0.95)
nmass.chi.c4non <- subset(nmass.chi.pft, pft == "c4_nonlegume" & chi < 0.95)


nmass.chi.pft.cleaned <- nmass.chi.legume %>%
  full_join(nmass.chi.c3non) %>%
  full_join(nmass.chi.c4non)


nmass.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(n.leaf))) +
  geom_point(aes(fill = pft),
             size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.chi.pft.cleaned, 
              aes(x = chi, y = emmean, ymin = lower.CL, ymax = upper.CL,
                  fill = pft), 
              alpha = 0.25) +
  geom_line(data = nmass.chi.pft.cleaned,
            aes(x = chi, y = emmean, linetype = linetype, color = pft),
            size = 0.9) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold(chi)),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
guides(linetype = "none")
nmass.chi

##########################################################################
## Nmass - soil N
##########################################################################
nmass.no3n.ind <- data.frame(emmeans(nmass, ~1, "soil.no3n",
                                 at = list(soil.no3n = seq(0, 80, 1)))) %>%
  dplyr::select(pft = X1, everything())

nmass.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = soil.no3n, y = log(n.leaf))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.no3n.ind,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = nmass.no3n.ind,
            aes(x = soil.no3n, y = emmean),
            size = 0.9, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
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
nmass.sm.ind <- data.frame(emmeans(nmass, ~1, "wn2_perc",
                                     at = list(wn2_perc = seq(0, 1, 0.01)))) %>%
  dplyr::select(pft = X1, everything())

nmass.sm <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = wn2_perc, y = log(n.leaf))) +
  geom_jitter(aes(fill = pft),
              width = 0.025, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.sm.ind,
              aes(x = wn2_perc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = nmass.sm.ind,
            aes(x = wn2_perc, y = emmean),
            size = 0.9, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
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
test(emtrends(marea, ~pft, "chi"))

marea.chi.pft <- data.frame(emmeans(marea, ~pft, "chi",
                                    at = list(chi = seq(0.2, 1, 0.01)))) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume","dashed", "solid"))


marea.chi.legume <- subset(marea.chi.pft, pft == "c3_legume" & chi > 0.55 & chi < 0.95)
marea.chi.c3non <- subset(marea.chi.pft, pft == "c3_nonlegume" & chi > 0.55 & chi < 0.95)
marea.chi.c4non <- subset(marea.chi.pft, pft == "c4_nonlegume" & chi < 0.95)

marea.chi.pft.cleaned <- marea.chi.legume %>%
  full_join(marea.chi.c3non) %>%
  full_join(marea.chi.c4non)


marea.chi <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = chi, y = log(marea))) +
  geom_point(aes(fill = pft),
             size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.chi.pft.cleaned,
              aes(x = chi, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL, fill = pft), 
              alpha = 0.25) +
  geom_line(data = marea.chi.pft.cleaned,
            aes(x = chi, y = emmean, color = pft, linetype = linetype),
            size = 0.9) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold(chi)),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(linetype = "none")
marea.chi

##########################################################################
## Marea - soil N
##########################################################################
test(emtrends(marea, ~pft, "soil.no3n"))
car::Anova(marea)

marea.no3n.ind <- data.frame(emmeans(marea, ~pft, "soil.no3n",
                                 at = list(soil.no3n = seq(0, 80, 1)))) %>%
  mutate(linetype = ifelse(pft == "c3_legume", "dashed", "solid"))

marea.no3n <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = soil.no3n, y = log(marea))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.no3n.ind,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL, fill = pft), 
              alpha = 0.25) +
  geom_line(data = marea.no3n.ind,
            aes(x = soil.no3n, y = emmean, color = pft,
                linetype = linetype),
            size = 0.9) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(linetype = "none")
marea.no3n

##########################################################################
## Marea - soil moisture
##########################################################################
test(emtrends(marea, ~pft, "wn2_perc"))
car::Anova(marea)

marea.sm.ind <- data.frame(emmeans(marea, ~1, "wn2_perc",
                                     at = list(wn2_perc = seq(0, 1, 0.01))))

marea.sm <- ggplot(data = subset(df, !is.na(pft)), 
                     aes(x = wn2_perc, y = log(marea))) +
  geom_jitter(aes(fill = pft),
              width = 0.025, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.sm.ind,
              aes(x = wn2_perc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = marea.sm.ind,
            aes(x = wn2_perc, y = emmean),
            size = 0.9) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
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
png("../working_drafts/figs/TXeco_fig4_narea.png",
    height = 12, width = 16, units = 'in', res = 600)
ggarrange(narea.chi, nmass.chi, marea.chi,
          narea.no3n, nmass.no3n, marea.no3n,
          narea.sm, nmass.sm, marea.sm,
          ncol = 3, nrow = 3, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()


