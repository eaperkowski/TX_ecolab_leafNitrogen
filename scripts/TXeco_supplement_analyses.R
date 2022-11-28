## Libraries
library(Hmisc)
library(ggcorrplot)
library(ggfortify)
library(dplyr)

## Load in soil characteristics file

soil <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv") %>%
  distinct(site, sampling.year, visit.type, .keep_all = TRUE) %>%
  dplyr::select(site, sampling.year, visit.type, soil.pH:soil.potassium, sg.bulkdens, sg.cec,
                sg.perc.gravel, sg.perc.clay, sg.ph.mean, sg.perc.sand, sg.perc.silt, sg.soc) %>%
  filter(site != "Bell_2020_05" & site != "Russel_2020_01")

## Correlation matrices
cor.test(soil$soil.no3n, soil$soil.phos)
cor.test(soil$soil.no3n, soil$soil.cec)
cor.test(soil$soil.no3n, soil$soil.pH)
cor.test(soil$soil.no3n, soil$soil.potassium)

## Create soil characteristic correlation matrix
soil.cor <- rcorr(as.matrix(soil[, c(4:8, 12, 14, 15)]), type = "pearson")

# Note: Pearson is for continuous vars and determines strength
# of linear relationships between vars

p.mat <- soil.cor$P

soil.corrplot <- ggcorrplot(soil.cor$r, type = "upper", method = "square", 
                            p.mat = p.mat, insig = "blank", hc.order = TRUE,
                            outline.color = "black", lab = TRUE,
                            colors = c("#DDAA33", "#FFFFFF", "#BB5566")) +
  scale_x_discrete(labels = c("EC",
                              expression("[NO"[3]*"-N]"),
                              "[P]",
                              "% sand",
                              "[K]",
                              "% silt",
                              "pH")) +
  scale_y_discrete(labels = c(expression("[NO"[3]*"-N]"),
                              "[P]",
                              "% sand",
                              "[K]",
                              "% silt",
                              "pH",
                              "% clay")) +
  labs(x = NULL, y = NULL, fill = "Pearson's r") +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"))

png("../working_drafts/figs/TXeco_FigS1_soil_correlations.png",
    width = 8, height = 5, units = 'in', res = 600)
soil.corrplot
dev.off()

## Create species summary table
spp <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv",
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
         chi = ifelse(chi > 0.95 | chi < 0.20, NA, chi)) %>%
  dplyr::select(pft:NCRS.code) %>%
  group_by(NCRS.code, photo, duration, n.fixer, pft) %>%
  summarize(no.sampled = length(NCRS.code))
write.csv(spp, "../working_drafts/tables/TXeco_tableS1_spp_data.csv",
          row.names = FALSE)


