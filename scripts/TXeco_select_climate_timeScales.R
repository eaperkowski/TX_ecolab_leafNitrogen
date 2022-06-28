###############################################################################
# Libraries
###############################################################################
library(dplyr)
library(car)
library(lme4)
library(MuMIn)

###############################################################################
# Load compiled data file
###############################################################################
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv")


###############################################################################
# Iterative models for growing season aridity
###############################################################################
df$narea[c(90, 170, 249, 290)] <- NA

ai.30 <- lmer(narea ~ ai.30 e), data = df)
shapiro.test(residuals(ai.30))
hist(residuals(ai.30))
outlierTest(ai.30)
Anova(ai.30)

ai.60 <- lmer(narea ~ ai.60 de), data = df)
shapiro.test(residuals(ai.60))
hist(residuals(ai.60))
outlierTest(ai.60)
Anova(ai.60)

ai.90 <- lmer(narea ~ ai.90 de), data = df)
shapiro.test(residuals(ai.90))
hist(residuals(ai.90))
outlierTest(ai.90)
Anova(ai.90)


AICc(ai.30, ai.60, ai.90)
BIC(ai.30, ai.60, ai.90)
## ai.90 is best model



###############################################################################
# Iterative models for precipitation
###############################################################################
prcp90 <- lmer(narea ~ prcp90 + (1 | NCRS.code), data = df)
prcp89 <- lmer(narea ~ prcp89 + (1 | NCRS.code), data = df)
prcp88 <- lmer(narea ~ prcp88 + (1 | NCRS.code), data = df)
prcp87 <- lmer(narea ~ prcp87 + (1 | NCRS.code), data = df)
prcp86 <- lmer(narea ~ prcp86 + (1 | NCRS.code), data = df)
prcp85 <- lmer(narea ~ prcp85 + (1 | NCRS.code), data = df)
prcp84 <- lmer(narea ~ prcp84 + (1 | NCRS.code), data = df)
prcp83 <- lmer(narea ~ prcp83 + (1 | NCRS.code), data = df)
prcp82 <- lmer(narea ~ prcp82 + (1 | NCRS.code), data = df)
prcp81 <- lmer(narea ~ prcp81 + (1 | NCRS.code), data = df)
prcp80 <- lmer(narea ~ prcp80 + (1 | NCRS.code), data = df)
prcp79 <- lmer(narea ~ prcp79 + (1 | NCRS.code), data = df)
prcp78 <- lmer(narea ~ prcp78 + (1 | NCRS.code), data = df)
prcp77 <- lmer(narea ~ prcp77 + (1 | NCRS.code), data = df)
prcp76 <- lmer(narea ~ prcp76 + (1 | NCRS.code), data = df)
prcp75 <- lmer(narea ~ prcp75 + (1 | NCRS.code), data = df)
prcp74 <- lmer(narea ~ prcp74 + (1 | NCRS.code), data = df)
prcp73 <- lmer(narea ~ prcp73 + (1 | NCRS.code), data = df)
prcp72 <- lmer(narea ~ prcp72 + (1 | NCRS.code), data = df)
prcp71 <- lmer(narea ~ prcp71 + (1 | NCRS.code), data = df)
prcp70 <- lmer(narea ~ prcp70 + (1 | NCRS.code), data = df)
prcp69 <- lmer(narea ~ prcp69 + (1 | NCRS.code), data = df)
prcp68 <- lmer(narea ~ prcp68 + (1 | NCRS.code), data = df)
prcp67 <- lmer(narea ~ prcp67 + (1 | NCRS.code), data = df)
prcp66 <- lmer(narea ~ prcp66 + (1 | NCRS.code), data = df)
prcp65 <- lmer(narea ~ prcp65 + (1 | NCRS.code), data = df)
prcp64 <- lmer(narea ~ prcp64 + (1 | NCRS.code), data = df)
prcp63 <- lmer(narea ~ prcp63 + (1 | NCRS.code), data = df)
prcp62 <- lmer(narea ~ prcp62 + (1 | NCRS.code), data = df)
prcp61 <- lmer(narea ~ prcp61 + (1 | NCRS.code), data = df)
prcp60 <- lmer(narea ~ prcp60 + (1 | NCRS.code), data = df)
prcp59 <- lmer(narea ~ prcp59 + (1 | NCRS.code), data = df)
prcp58 <- lmer(narea ~ prcp58 + (1 | NCRS.code), data = df)
prcp57 <- lmer(narea ~ prcp57 + (1 | NCRS.code), data = df)
prcp56 <- lmer(narea ~ prcp56 + (1 | NCRS.code), data = df)
prcp55 <- lmer(narea ~ prcp55 + (1 | NCRS.code), data = df)
prcp54 <- lmer(narea ~ prcp54 + (1 | NCRS.code), data = df)
prcp53 <- lmer(narea ~ prcp53 + (1 | NCRS.code), data = df)
prcp52 <- lmer(narea ~ prcp52 + (1 | NCRS.code), data = df)
prcp51 <- lmer(narea ~ prcp51 + (1 | NCRS.code), data = df)
prcp50 <- lmer(narea ~ prcp50 + (1 | NCRS.code), data = df)
prcp49 <- lmer(narea ~ prcp49 + (1 | NCRS.code), data = df)
prcp48 <- lmer(narea ~ prcp48 + (1 | NCRS.code), data = df)
prcp47 <- lmer(narea ~ prcp47 + (1 | NCRS.code), data = df)
prcp46 <- lmer(narea ~ prcp46 + (1 | NCRS.code), data = df)
prcp45 <- lmer(narea ~ prcp45 + (1 | NCRS.code), data = df)
prcp44 <- lmer(narea ~ prcp44 + (1 | NCRS.code), data = df)
prcp43 <- lmer(narea ~ prcp43 + (1 | NCRS.code), data = df)
prcp42 <- lmer(narea ~ prcp42 + (1 | NCRS.code), data = df)
prcp41 <- lmer(narea ~ prcp41 + (1 | NCRS.code), data = df)
prcp40 <- lmer(narea ~ prcp40 + (1 | NCRS.code), data = df)
prcp39 <- lmer(narea ~ prcp39 + (1 | NCRS.code), data = df)
prcp38 <- lmer(narea ~ prcp38 + (1 | NCRS.code), data = df)
prcp37 <- lmer(narea ~ prcp37 + (1 | NCRS.code), data = df)
prcp36 <- lmer(narea ~ prcp36 + (1 | NCRS.code), data = df)
prcp35 <- lmer(narea ~ prcp35 + (1 | NCRS.code), data = df)
prcp34 <- lmer(narea ~ prcp34 + (1 | NCRS.code), data = df)
prcp33 <- lmer(narea ~ prcp33 + (1 | NCRS.code), data = df)
prcp32 <- lmer(narea ~ prcp32 + (1 | NCRS.code), data = df)
prcp31 <- lmer(narea ~ prcp31 + (1 | NCRS.code), data = df)
prcp30 <- lmer(narea ~ prcp30 + (1 | NCRS.code), data = df)
prcp29 <- lmer(narea ~ prcp29 + (1 | NCRS.code), data = df)
prcp28 <- lmer(narea ~ prcp28 + (1 | NCRS.code), data = df)
prcp27 <- lmer(narea ~ prcp27 + (1 | NCRS.code), data = df)
prcp26 <- lmer(narea ~ prcp26 + (1 | NCRS.code), data = df)
prcp25 <- lmer(narea ~ prcp25 + (1 | NCRS.code), data = df)
prcp24 <- lmer(narea ~ prcp24 + (1 | NCRS.code), data = df)
prcp23 <- lmer(narea ~ prcp23 + (1 | NCRS.code), data = df)
prcp22 <- lmer(narea ~ prcp22 + (1 | NCRS.code), data = df)
prcp21 <- lmer(narea ~ prcp21 + (1 | NCRS.code), data = df)
prcp20 <- lmer(narea ~ prcp20 + (1 | NCRS.code), data = df)
prcp19 <- lmer(narea ~ prcp19 + (1 | NCRS.code), data = df)
prcp18 <- lmer(narea ~ prcp18 + (1 | NCRS.code), data = df)
prcp17 <- lmer(narea ~ prcp17 + (1 | NCRS.code), data = df)
prcp16 <- lmer(narea ~ prcp16 + (1 | NCRS.code), data = df)
prcp15 <- lmer(narea ~ prcp15 + (1 | NCRS.code), data = df)
prcp14 <- lmer(narea ~ prcp14 + (1 | NCRS.code), data = df)
prcp13 <- lmer(narea ~ prcp13 + (1 | NCRS.code), data = df)
prcp12 <- lmer(narea ~ prcp12 + (1 | NCRS.code), data = df)
prcp11 <- lmer(narea ~ prcp11 + (1 | NCRS.code), data = df)
prcp10 <- lmer(narea ~ prcp10 + (1 | NCRS.code), data = df)
prcp9 <- lmer(narea ~ prcp9 + (1 | NCRS.code), data = df)
prcp8 <- lmer(narea ~ prcp8 + (1 | NCRS.code), data = df)
prcp7 <- lmer(narea ~ prcp7 + (1 | NCRS.code), data = df)
prcp6 <- lmer(narea ~ prcp6 + (1 | NCRS.code), data = df)
prcp5 <- lmer(narea ~ prcp5 + (1 | NCRS.code), data = df)
prcp4 <- lmer(narea ~ prcp4 + (1 | NCRS.code), data = df)
prcp3 <- lmer(narea ~ prcp3 + (1 | NCRS.code), data = df)
prcp2 <- lmer(narea ~ prcp2 + (1 | NCRS.code), data = df)
prcp1 <- lmer(narea ~ prcp1 + (1 | NCRS.code), data = df)


prcp <- AICc(#prcp90, prcp89, prcp88, prcp87, prcp86, prcp85, prcp84, prcp83,
             #prcp82, prcp81, prcp80, prcp79, prcp78, prcp77, prcp76, prcp75,
             #prcp74, prcp73, prcp72, prcp71, prcp70, prcp69, prcp68, prcp67, 
             #prcp66, prcp65, prcp64, prcp63, prcp62, prcp61, prcp60, prcp59, 
             #prcp58, prcp57, prcp56, prcp55, prcp54, prcp53, prcp52, prcp51, 
             #prcp50, prcp49, prcp48, prcp47, prcp46, prcp45, prcp44, prcp43, 
             #prcp42, prcp41, prcp40, prcp39, prcp38, prcp37, prcp36, prcp35, 
             #prcp34, prcp33, prcp32, prcp31, 
             #prcp30, prcp29, prcp28, prcp27, 
             prcp26, prcp25, prcp24, prcp23, prcp22, prcp21, prcp20, prcp19,
             prcp18, prcp17, prcp16, prcp15, prcp14, prcp13, prcp12, prcp11, 
             prcp10,prcp9, prcp8, prcp7, prcp6, prcp5, prcp4, prcp3, prcp2, 
             prcp1) %>%
  arrange(AICc)
head(prcp)
# Two-day precipitation is most explanatory timescale

###############################################################################
# Iterative models for Tmean
###############################################################################
temp90 <- lmer(narea ~ tavg90 + (1 | NCRS.code), data = df)
temp89 <- lmer(narea ~ tavg89 + (1 | NCRS.code), data = df)
temp88 <- lmer(narea ~ tavg88 + (1 | NCRS.code), data = df)
temp87 <- lmer(narea ~ tavg87 + (1 | NCRS.code), data = df)
temp86 <- lmer(narea ~ tavg86 + (1 | NCRS.code), data = df)
temp85 <- lmer(narea ~ tavg85 + (1 | NCRS.code), data = df)
temp84 <- lmer(narea ~ tavg84 + (1 | NCRS.code), data = df)
temp83 <- lmer(narea ~ tavg83 + (1 | NCRS.code), data = df)
temp82 <- lmer(narea ~ tavg82 + (1 | NCRS.code), data = df)
temp81 <- lmer(narea ~ tavg81 + (1 | NCRS.code), data = df)
temp80 <- lmer(narea ~ tavg80 + (1 | NCRS.code), data = df)
temp79 <- lmer(narea ~ tavg79 + (1 | NCRS.code), data = df)
temp78 <- lmer(narea ~ tavg78 + (1 | NCRS.code), data = df)
temp77 <- lmer(narea ~ tavg77 + (1 | NCRS.code), data = df)
temp76 <- lmer(narea ~ tavg76 + (1 | NCRS.code), data = df)
temp75 <- lmer(narea ~ tavg75 + (1 | NCRS.code), data = df)
temp74 <- lmer(narea ~ tavg74 + (1 | NCRS.code), data = df)
temp73 <- lmer(narea ~ tavg73 + (1 | NCRS.code), data = df)
temp72 <- lmer(narea ~ tavg72 + (1 | NCRS.code), data = df)
temp71 <- lmer(narea ~ tavg71 + (1 | NCRS.code), data = df)
temp70 <- lmer(narea ~ tavg70 + (1 | NCRS.code), data = df)
temp69 <- lmer(narea ~ tavg69 + (1 | NCRS.code), data = df)
temp68 <- lmer(narea ~ tavg68 + (1 | NCRS.code), data = df)
temp67 <- lmer(narea ~ tavg67 + (1 | NCRS.code), data = df)
temp66 <- lmer(narea ~ tavg66 + (1 | NCRS.code), data = df)
temp65 <- lmer(narea ~ tavg65 + (1 | NCRS.code), data = df)
temp64 <- lmer(narea ~ tavg64 + (1 | NCRS.code), data = df)
temp63 <- lmer(narea ~ tavg63 + (1 | NCRS.code), data = df)
temp62 <- lmer(narea ~ tavg62 + (1 | NCRS.code), data = df)
temp61 <- lmer(narea ~ tavg61 + (1 | NCRS.code), data = df)
temp60 <- lmer(narea ~ tavg60 + (1 | NCRS.code), data = df)
temp59 <- lmer(narea ~ tavg59 + (1 | NCRS.code), data = df)
temp58 <- lmer(narea ~ tavg58 + (1 | NCRS.code), data = df)
temp57 <- lmer(narea ~ tavg57 + (1 | NCRS.code), data = df)
temp56 <- lmer(narea ~ tavg56 + (1 | NCRS.code), data = df)
temp55 <- lmer(narea ~ tavg55 + (1 | NCRS.code), data = df)
temp54 <- lmer(narea ~ tavg54 + (1 | NCRS.code), data = df)
temp53 <- lmer(narea ~ tavg53 + (1 | NCRS.code), data = df)
temp52 <- lmer(narea ~ tavg52 + (1 | NCRS.code), data = df)
temp51 <- lmer(narea ~ tavg51 + (1 | NCRS.code), data = df)
temp50 <- lmer(narea ~ tavg50 + (1 | NCRS.code), data = df)
temp49 <- lmer(narea ~ tavg49 + (1 | NCRS.code), data = df)
temp48 <- lmer(narea ~ tavg48 + (1 | NCRS.code), data = df)
temp47 <- lmer(narea ~ tavg47 + (1 | NCRS.code), data = df)
temp46 <- lmer(narea ~ tavg46 + (1 | NCRS.code), data = df)
temp45 <- lmer(narea ~ tavg45 + (1 | NCRS.code), data = df)
temp44 <- lmer(narea ~ tavg44 + (1 | NCRS.code), data = df)
temp43 <- lmer(narea ~ tavg43 + (1 | NCRS.code), data = df)
temp42 <- lmer(narea ~ tavg42 + (1 | NCRS.code), data = df)
temp41 <- lmer(narea ~ tavg41 + (1 | NCRS.code), data = df)
temp40 <- lmer(narea ~ tavg40 + (1 | NCRS.code), data = df)
temp39 <- lmer(narea ~ tavg39 + (1 | NCRS.code), data = df)
temp38 <- lmer(narea ~ tavg38 + (1 | NCRS.code), data = df)
temp37 <- lmer(narea ~ tavg37 + (1 | NCRS.code), data = df)
temp36 <- lmer(narea ~ tavg36 + (1 | NCRS.code), data = df)
temp35 <- lmer(narea ~ tavg35 + (1 | NCRS.code), data = df)
temp34 <- lmer(narea ~ tavg34 + (1 | NCRS.code), data = df)
temp33 <- lmer(narea ~ tavg33 + (1 | NCRS.code), data = df)
temp32 <- lmer(narea ~ tavg32 + (1 | NCRS.code), data = df)
temp31 <- lmer(narea ~ tavg31 + (1 | NCRS.code), data = df)
temp30 <- lmer(narea ~ tavg30 + (1 | NCRS.code), data = df)
temp29 <- lmer(narea ~ tavg29 + (1 | NCRS.code), data = df)
temp28 <- lmer(narea ~ tavg28 + (1 | NCRS.code), data = df)
temp27 <- lmer(narea ~ tavg27 + (1 | NCRS.code), data = df)
temp26 <- lmer(narea ~ tavg26 + (1 | NCRS.code), data = df)
temp25 <- lmer(narea ~ tavg25 + (1 | NCRS.code), data = df)
temp24 <- lmer(narea ~ tavg24 + (1 | NCRS.code), data = df)
temp23 <- lmer(narea ~ tavg23 + (1 | NCRS.code), data = df)
temp22 <- lmer(narea ~ tavg22 + (1 | NCRS.code), data = df)
temp21 <- lmer(narea ~ tavg21 + (1 | NCRS.code), data = df)
temp20 <- lmer(narea ~ tavg20 + (1 | NCRS.code), data = df)
temp19 <- lmer(narea ~ tavg19 + (1 | NCRS.code), data = df)
temp18 <- lmer(narea ~ tavg18 + (1 | NCRS.code), data = df)
temp17 <- lmer(narea ~ tavg17 + (1 | NCRS.code), data = df)
temp16 <- lmer(narea ~ tavg16 + (1 | NCRS.code), data = df)
temp15 <- lmer(narea ~ tavg15 + (1 | NCRS.code), data = df)
temp14 <- lmer(narea ~ tavg14 + (1 | NCRS.code), data = df)
temp13 <- lmer(narea ~ tavg13 + (1 | NCRS.code), data = df)
temp12 <- lmer(narea ~ tavg12 + (1 | NCRS.code), data = df)
temp11 <- lmer(narea ~ tavg11 + (1 | NCRS.code), data = df)
temp10 <- lmer(narea ~ tavg10 + (1 | NCRS.code), data = df)
temp9 <- lmer(narea ~ tavg9 + (1 | NCRS.code), data = df)
temp8 <- lmer(narea ~ tavg8 + (1 | NCRS.code), data = df)
temp7 <- lmer(narea ~ tavg7 + (1 | NCRS.code), data = df)
temp6 <- lmer(narea ~ tavg6 + (1 | NCRS.code), data = df)
temp5 <- lmer(narea ~ tavg5 + (1 | NCRS.code), data = df)
temp4 <- lmer(narea ~ tavg4 + (1 | NCRS.code), data = df)
temp3 <- lmer(narea ~ tavg3 + (1 | NCRS.code), data = df)
temp2 <- lmer(narea ~ tavg2 + (1 | NCRS.code), data = df)
temp1 <- lmer(narea ~ tavg1 + (1 | NCRS.code), data = df)


temp <- AICc(#temp90, temp89, temp88, temp87, temp86, temp85, temp84, temp83,
             #temp82, temp81, temp80, temp79, temp78, temp77, temp76, temp75,
             #temp74, temp73, temp72, temp71, temp70, temp69, temp68, temp67, 
             #temp66, temp65, temp64, temp63, temp62, temp61, temp60, temp59, 
             #temp58, temp57, temp56, temp55, temp54, temp53, temp52, temp51, 
             #temp50, temp49, temp48, temp47, temp46, temp45, temp44, temp43, 
             #temp42, temp41, temp40, temp39, temp38, temp37, temp36, temp35, 
             #temp34, temp33, temp32, temp31, 
             temp30, temp29, temp28, temp27, 
             temp26, temp25, temp24, temp23, temp22, temp21, temp20, temp19,
             temp18, temp17, temp16, temp15, temp14, temp13, temp12, temp11, 
             temp10, temp9, temp8, temp7, temp6, temp5, temp4, temp3, temp2, 
             temp1) %>%
  arrange(AICc)
head(temp)
## 30-day temperature is most explanatory temperature scale