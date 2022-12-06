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
df$beta[c(84, 117)] <- NA
df$chi[c(117, 322, 481)] <- NA
df$chi[c(62, 315)] <- NA
df$chi[c(456)] <- NA
df$chi[c(317, 483)] <- NA
df$chi[c(292, 484)] <- NA
df$chi[284] <- NA
df$chi[484] <- NA
df$narea[df$narea > 10] <- NA
df$narea[509] <- NA
df$n.leaf[c(375, 509)] <- NA
df$marea[c(20, 21)] <- NA

## Add general models
beta <- lmer(log(beta) ~ wn3_perc * soil.no3n * pft + (1 | NCRS.code), data = df)
chi <- lmer(chi ~ (vpd4 + tavg4 + (wn3_perc * soil.no3n)) * pft + 
              (1 | NCRS.code), data = df)
narea <- lmer(log(narea) ~ (beta + chi + (soil.no3n * wn3_perc)) * pft + (1 | NCRS.code),
              data = df)

##########################################################################
## Beta - soil N
##########################################################################
test(emtrends(beta, ~pft, "soil.no3n"))

beta.no3n.pft <- data.frame(emmeans(beta, ~pft, "soil.no3n",
                                    at = list(soil.no3n = seq(0,80,1))))

beta.no3n <- data.frame(emmeans(beta, ~1, "soil.no3n",
                                at = list(soil.no3n = seq(0,80,1)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(beta.no3n.pft) %>%
  mutate(linetype = "dashed")

## Plot
beta.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(beta))) +
   geom_jitter(aes(fill = pft),
               width = 0.5, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = subset(beta.no3n, pft == "overall"), 
              aes(x = soil.no3n, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL), alpha = 0.25, fill = "black") +
  geom_line(data = subset(beta.no3n, pft == "overall"),
            aes(x = soil.no3n, y = emmean),
            color = "black", size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[4]~"non-legume"),
                               expression("C"[3]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[4]~"non-legume"),
                                expression("C"[3]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold"))
beta.no3n.ind

##########################################################################
## Beta - soil moisture
##########################################################################
test(emtrends(beta, ~pft, "wn3_perc"))

beta.sm.pft <- data.frame(emmeans(beta, ~pft, "wn3_perc",
                                    at = list(wn3_perc = seq(0,1,0.01))))

beta.sm <- data.frame(emmeans(beta, ~1, "wn3_perc",
                                at = list(wn3_perc = seq(0,1,0.01)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(beta.sm.pft) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume", "solid", "dashed"))

# Plot
beta.h2o.int <- ggplot(data = subset(df, !is.na(pft)), 
                       aes(x = wn3_perc, y = log(beta))) +
  geom_jitter(aes(fill = pft),
              width = 0.01, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = subset(beta.sm, pft != "overall"),
              aes(x = wn3_perc, y = emmean, ymin = lower.CL,
                  ymax = upper.CL, fill = pft),
              alpha = 0.3) +
  geom_line(data = subset(beta.sm, pft != "overall"),
            aes(x = wn3_perc, y = emmean, linetype = linetype, color = pft),
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(limits = c(-2.5, 7.5), breaks = seq(-2.5, 7.5, 2.5)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(ln~beta)),
       fill = "Functional group",
       color = "Functional group") +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        legend.title = element_text(face = "bold"))
beta.h2o.int

##########################################################################
## Write beta plot
##########################################################################
# Write plot
png("../working_drafts/figs/TXeco_fig2_beta.png",
    width = 12, height = 4.5, units = 'in', res = 600)
ggarrange(beta.h2o.int, beta.no3n.ind,
          ncol = 2, nrow = 1, common.legend = TRUE,
          legend = "right", align = "hv", labels = "AUTO",
          font.label = list(size = 18))
dev.off()

##########################################################################
## Chi - VPD
##########################################################################
test(emtrends(chi, ~pft, "vpd4"))

chi.vpd.pft <- data.frame(emmeans(chi, ~pft, "vpd4",
                                  at = list(vpd4 = seq(0.8,1.4,0.01))))

chi.vpd <- data.frame(emmeans(chi, ~1, "vpd4",
                              at = list(vpd4 = seq(0.8,1.4,0.01)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(chi.vpd.pft) %>%
  mutate(linetype = ifelse(pft == "c3_legume", "dashed", "solid"))


chi.vpd.int <- ggplot(data = df, aes(x = vpd4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = subset(chi.vpd, pft != "overall"), 
              aes(x = vpd4, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.vpd, pft != "overall"), 
            aes(x = vpd4, y = emmean, color = pft, linetype = linetype), 
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0.8, 1.41), breaks = seq(0.8, 1.4, 0.15)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Vapor pressure deficit (kPa)")),
       y = expression(bold(chi)),
       fill = expression(bold("Functional group")), 
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25))
chi.vpd.int

##########################################################################
## Chi - Temp
##########################################################################
test(emtrends(chi, ~pft, "tavg4"))

chi.tavg.pft <- data.frame(emmeans(chi, ~pft, "tavg4",
                                  at = list(tavg4 = seq(18, 22,0.1))))

chi.tavg <- data.frame(emmeans(chi, ~1, "tavg4",
                              at = list(tavg4 = seq(18, 22,0.1)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(chi.tavg.pft) %>%
  mutate(linetype = ifelse(pft == "c3_nonlegume", "dashed", "solid"))

chi.temp.int <- ggplot(data = df, aes(x = tavg4, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = subset(chi.tavg, pft != "overall"),
              aes(x = tavg4, y = emmean, ymin = lower.CL,
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.tavg, pft != "overall"),
            aes(x = tavg4, y = emmean, color = pft, linetype = linetype), 
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(18, 22), breaks = seq(18, 22, 1)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  labs(x = expression(bold("Air temperature ("*degree*"C)")),
       y = expression(bold(chi)),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25))
chi.temp.int

##########################################################################
## Chi - Soil moisture
##########################################################################
test(emtrends(chi, ~pft, "wn3_perc"))

chi.sm.pft <- data.frame(emmeans(chi, ~pft, "wn3_perc",
                                   at = list(wn3_perc = seq(0, 1, 0.01))))

chi.sm <- data.frame(emmeans(chi, ~1, "wn3_perc",
                               at = list(wn3_perc = seq(0, 1, 0.01)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(chi.sm.pft) %>%
  mutate(linetype = ifelse(pft == "c4_nonlegume", "solid", "dashed"))

chi.sm.int <- ggplot(data = df, aes(x = wn3_perc, y = chi)) +
  geom_jitter(aes(fill = pft),
              width = 0.1, size = 3, alpha = 0.5, shape = 21) +
  geom_ribbon(data = subset(chi.sm, pft != "overall"), 
              aes(x = wn3_perc, y = emmean, ymin = lower.CL, 
                  ymax = upper.CL, fill = pft), alpha = 0.25) +
  geom_line(data = subset(chi.sm, pft != "overall"), 
            aes(x = wn3_perc, y = emmean, color = pft, linetype = linetype),
            size = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold(chi)),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  guides(linetype = "none") +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(linewidth = 1.25))
chi.sm.int 

##########################################################################
## Write chi plot
##########################################################################
png("../working_drafts/figs/TXeco_fig3_chi.png",
    width = 12, height = 9, units = 'in', res = 600)
ggarrange(chi.vpd.int, chi.temp.int,
          chi.sm.int,
          nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()

##########################################################################
## Narea - soil N
##########################################################################
test(emtrends(narea, ~pft, "soil.no3n"))
car::Anova(narea)

narea.no3n.pft <- data.frame(emmeans(narea, ~pft, "soil.no3n",
                                   at = list(soil.no3n = seq(0, 80, 1))))

narea.no3n <- data.frame(emmeans(narea, ~1, "soil.no3n",
                               at = list(soil.no3n = seq(0, 80, 1)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(narea.no3n.pft) %>%
  mutate(linetype = "dashed")


narea.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = soil.no3n, y = log(narea))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  #labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
  labs(x = NULL,
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
narea.no3n.ind

##########################################################################
## Narea - beta
##########################################################################
test(emtrends(narea, ~pft, "beta"))
car::Anova(narea)

narea.beta.pft <- data.frame(emmeans(narea, ~pft, "beta",
                                     at = list(beta = seq(0, 600, 1))))

narea.beta <- data.frame(emmeans(narea, ~1, "beta",
                                 at = list(beta = seq(0, 600, 1)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(narea.beta.pft) %>%
  mutate(linetype = ifelse(pft == "c3_legume", "solid", "dashed"))

narea.beta.ind <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = beta, y = log(narea))) +
  geom_point(aes(fill = pft),
              size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = subset(narea.beta, pft == "overall"),
              aes(x = beta, y = emmean, ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = subset(narea.beta, pft == "overall"),
            aes(x = beta, y = emmean, linetype = linetype),
            size = 0.9, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  #labs(x = expression(bold("Unit cost ratio ("*beta*")")),
  labs(x = NULL,
       y = expression(bold(ln*" N"["area"]*" (gN m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank()) +
  guides(linetype = "none")
narea.beta.ind

##########################################################################
## Nmass - beta
##########################################################################
test(emtrends(nmass, ~pft, "beta"))

nmass.beta.pft <- data.frame(emmeans(nmass, ~pft, "beta",
                                     at = list(wn3_perc = seq(0, 1, 0.01))))

nmass.beta <- data.frame(emmeans(nmass, ~1, "beta",
                                 at = list(list(wn3_perc = seq(0, 1, 0.01))))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(nmass.beta.pft) 

nmass.beta.ind <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = beta, y = log(n.leaf))) +
  geom_point(aes(fill = pft),
             size = 3, alpha = 0.7, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(-1, 3), breaks = seq(-1, 3, 1)) +
  labs(x = expression(bold("Unit cost ratio ("*beta*")")),
       y = expression(bold(ln*" N"["mass"]*" (g g"^"-1"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.beta.ind

##########################################################################
## Nmass - soil N
##########################################################################
test(emtrends(nmass, ~pft, "soil.no3n"))
car::Anova(nmass)

nmass.no3n.pft <- data.frame(emmeans(
  nmass, ~1, "soil.no3n",
  at = list(soil.no3n = seq(0, 80, 1))))

nmass.no3n <- data.frame(emmeans(nmass, ~1, "soil.no3n",
                                 at = list(soil.no3n = seq(0, 80, 1)))) %>%
  dplyr::select(pft = X1, everything()) %>%
  full_join(nmass.no3n.pft)

nmass.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = soil.no3n, y = log(n.leaf))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.no3n.pft,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = nmass.no3n.pft,
            aes(x = soil.no3n, y = emmean),
            size = 0.9, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" N"["mass"]*" (gN g"^"-1"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.no3n.ind

##########################################################################
## Marea - beta
##########################################################################
test(emtrends(marea, ~pft, "beta"))

marea.beta <- data.frame(emmeans(marea, ~1, "beta",
                                 at = list(beta = seq(0, 600, 1)))) %>%
  dplyr::select(pft = X1, everything()) 

marea.beta.ind <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = beta, y = log(marea))) +
  geom_point(aes(fill = pft),
             size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.beta,
              aes(x = beta, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = marea.beta,
            aes(x = beta, y = emmean),
            size = 0.9, color = "black") +
  
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = NULL,
    #x = expression(bold("Unit cost ratio ("*beta*")")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group")),
       color = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
marea.beta.ind

##########################################################################
## Marea - soil N
##########################################################################
test(emtrends(marea, ~pft, "soil.no3n"))
car::Anova(marea)

marea.no3n <- data.frame(emmeans(marea, ~1, "soil.no3n",
                                 at = list(soil.no3n = seq(0, 80, 1)))) %>%
  dplyr::select(pft = X1, everything())

marea.no3n.ind <- ggplot(data = subset(df, !is.na(pft)), 
                         aes(x = soil.no3n, y = log(marea))) +
  geom_jitter(aes(fill = pft),
              width = 0.5, size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.no3n,
              aes(x = soil.no3n, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              alpha = 0.25) +
  geom_line(data = marea.no3n,
            aes(x = soil.no3n, y = emmean),
            size = 0.9, color = "black") +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) +
  scale_x_continuous(limits = c(-1, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = NULL,
       x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold(ln*" M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
marea.no3n.ind

##########################################################################
## Create Narea plots
##########################################################################
png("../working_drafts/figs/TXeco_fig4_narea.png",
    height = 9, width = 16, units = 'in', res = 600)
ggarrange(narea.beta.ind, nmass.beta.ind, marea.beta.ind,
          narea.no3n.ind, nmass.no3n.ind, marea.no3n.ind,
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()


##########################################################################
## Supplemental Nmass plots
##########################################################################
nmass.soiln <- data.frame(emmeans(nmass, ~1, "soil.no3n",
                                  at = list(soil.no3n = seq(0,80,0.5))))

nmass.soiln.plot <- ggplot(data = subset(df, !is.na(pft)), 
                      aes(x = soil.no3n, y = n.leaf)) +
  geom_jitter(aes(fill = pft), width = 0.05,
              size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.soiln, aes(x=soil.no3n, y = emmean,
                                      ymin = lower.CL,
                                      ymax = upper.CL),
              alpha = 0.25) +
  geom_line(data = nmass.soiln, aes(x = soil.no3n, y = emmean), 
            linewidth = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) + 
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.5)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold("N"["mass"]*" (g g"^"-1"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.soiln.plot


nmass.sm <- data.frame(emmeans(nmass, ~1, "wn3_perc",
                                  at = list(wn3_perc = seq(0,1,0.01))))

nmass.sm.plot <- ggplot(data = subset(df, !is.na(pft)), 
                           aes(x = wn3_perc, y = n.leaf)) +
  geom_jitter(aes(fill = pft), width = 0.05,
              size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = nmass.sm, aes(x=wn3_perc, y = emmean,
                                      ymin = lower.CL,
                                      ymax = upper.CL),
              alpha = 0.25) +
  geom_line(data = nmass.sm, aes(x = wn3_perc, y = emmean), 
            linewidth = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.5)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold("N"["mass"]*" (g g"^"-1"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.sm.plot


nmass.beta <- data.frame(emmeans(nmass, ~1, "beta",
                               at = list(wn3_perc = seq(0,600,1))))

nmass.beta.plot <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = beta, y = n.leaf)) +
  geom_jitter(aes(fill = pft), width = 0.05,
              size = 3, alpha = 0.7, shape = 21) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) + 
  scale_x_continuous(limits = c(0, 600), breaks = seq(0,600,200)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.5)) +
  labs(x = expression(bold("Unit cost ratio ("*beta*")")),
       y = expression(bold("N"["mass"]*" (g g"^"-1"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
nmass.beta.plot

##########################################################################
## Supplemental Marea plots
##########################################################################
marea.soiln <- data.frame(emmeans(marea, ~1, "soil.no3n",
                                  at = list(soil.no3n = seq(0,80,0.5))))

marea.soiln.plot <- ggplot(data = subset(df, !is.na(pft)), 
                           aes(x = soil.no3n, y =log(marea))) +
  geom_jitter(aes(fill = pft), width = 0.05,
              size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.soiln, aes(x=soil.no3n, y = emmean,
                                      ymin = lower.CL,
                                      ymax = upper.CL),
              alpha = 0.25) +
  geom_line(data = marea.soiln, aes(x = soil.no3n, y = emmean), 
            linewidth = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) + 
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Soil N availability (ppm NO"[3]*"-N)")),
       y = expression(bold("M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
marea.soiln.plot


marea.sm <- data.frame(emmeans(marea, ~1, "wn3_perc",
                               at = list(wn3_perc = seq(0,1,0.01))))

marea.sm.plot <- ggplot(data = subset(df, !is.na(pft)), 
                        aes(x = wn3_perc, y = log(marea))) +
  geom_jitter(aes(fill = pft), width = 0.05,
              size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.sm, aes(x=wn3_perc, y = emmean,
                                   ymin = lower.CL,
                                   ymax = upper.CL),
              alpha = 0.25) +
  geom_line(data = marea.sm, aes(x = wn3_perc, y = emmean), 
            linewidth = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Soil moisture (% WHC)")),
       y = expression(bold("M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
marea.sm.plot


marea.beta <- data.frame(emmeans(marea, ~1, "beta",
                                 at = list(beta = seq(0,600,1))))

marea.beta.plot <- ggplot(data = subset(df, !is.na(pft)), 
                          aes(x = beta, y = log(marea))) +
  geom_jitter(aes(fill = pft), width = 0.05,
              size = 3, alpha = 0.7, shape = 21) +
  geom_ribbon(data = marea.beta, aes(x=beta, y = emmean,
                                   ymin = lower.CL,
                                   ymax = upper.CL),
              alpha = 0.25) +
  geom_line(data = marea.beta, aes(x = beta, y = emmean), 
            linewidth = 1) +
  scale_fill_manual(values = c(cbbPalette3), 
                    labels = c(expression("C"[3]~"legume"),
                               expression("C"[3]~"non-legume"),
                               expression("C"[4]~"non-legume"))) +
  scale_color_manual(values = c(cbbPalette3), 
                     labels = c(expression("C"[3]~"legume"),
                                expression("C"[3]~"non-legume"),
                                expression("C"[4]~"non-legume"))) + 
  scale_x_continuous(limits = c(0, 600), breaks = seq(0,600,150)) +
  scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, 1)) +
  labs(x = expression(bold("Unit cost ratio ("*beta*")")),
       y = expression(bold("M"["area"]*" (g m"^"-2"~")")),
       fill = expression(bold("Functional group"))) +
  theme_bw(base_size = 18) +
  theme(legend.text.align = 0,
        panel.border = element_rect(size = 1.25),
        panel.grid.minor.y = element_blank())
marea.beta.plot


##########################################################################
## Create Nmass/Marea supplemental plots
##########################################################################
png("../working_drafts/figs/TXeco_figS3_nmass_marea.png",
    height = 12, width = 12, units = 'in', res = 600)
ggpubr::ggarrange(nmass.soiln.plot, marea.soiln.plot,
          nmass.sm.plot, marea.sm.plot,
          nmass.beta.plot, marea.beta.plot,
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "right", 
          align = "hv", labels = "AUTO", font.label = list(size = 18))
dev.off()






