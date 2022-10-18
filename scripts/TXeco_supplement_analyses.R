## Libraries
library(Hmisc)
library(ggcorrplot)
library(ggfortify)
library(dplyr)

## Load in soil characteristics file
soil <- read.csv("../data_sheets/TXeco_soil_characteristics.csv")

## Remove properties that were removed from original analysis
soil <- soil %>%
  filter(site != "2020eco_Bell_i" & site != "2020eco_Russel_i" &
           site != "2021eco_Menard_i")

## Correlation matrices
cor.test(soil$soil.no3n, soil$soil.phos)
cor.test(soil$soil.no3n, soil$soil.cec)
cor.test(soil$soil.no3n, soil$soil.pH)
cor.test(soil$soil.no3n, soil$soil.potassium)

## Create soil characteristic correlation matrix
soil.cor <- rcorr(as.matrix(soil[,2:6]), type = "pearson")

# Note: Pearson is for continuous vars and determines strength
# of linear relationships between vars

## Iridescent palette: 
iridescent <- c("#FEFBE9", "#FCF7D5", "#F5F3C1", "#EAF0B5",
                "#DDECBF", "#D0E7CA", "#C2E3D2", "#B5DDD8",
                "#A8D8DC", "#9BD2E1", "#8DCBE4", "#81C4E7",
                "#7BBCE7", "#7EB2E4", "#88A5DD", "#9398D2",
                "#9B8AC4", "#9D7DB2", "#9A709E", "#906388",
                "#805770", "#684957", "#46353A", "#000000")

soil.corrplot <- ggcorrplot(soil.cor$r, type = "upper", method = "square", 
           hc.order = TRUE,
           outline.color = "black", lab = TRUE) +
  scale_x_discrete(labels = c("pH", 
                              "Soil [K]", 
                              "Soil [P]",
                              "EC")) +
  scale_fill_gradientn(colors = iridescent,
                       limits = c(-1, 1), breaks = seq(-1, 1, 1)) +
  scale_y_discrete(labels = c("Soil [K]", 
                              "Soil [P]", 
                              "EC",
                              expression("Soil [NO"[3]*"-N]"))) +
  labs(x = NULL, y = NULL, fill = "Pearson's r") +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"))

png("../working_drafts/figs/TXeco_FigS1_soil_correlations.png",
    width = 8, height = 5, units = 'in', res = 600)
soil.corrplot
dev.off()




