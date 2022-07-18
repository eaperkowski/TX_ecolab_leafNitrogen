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
df$narea[c(245)] <- NA

ai.30 <- lmer(log(narea) ~ ai.30 + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.30))
hist(residuals(ai.30))
outlierTest(ai.30)
Anova(ai.30)

ai.60 <- lmer(log(narea) ~ ai.60 + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.60))
hist(residuals(ai.60))
outlierTest(ai.60)
Anova(ai.60)

ai.90 <- lmer(log(narea) ~ ai.90 + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.90))
hist(residuals(ai.90))
outlierTest(ai.90)
Anova(ai.90)


AICc(ai.30, ai.60, ai.90)
BIC(ai.30, ai.60, ai.90)
## ai.90 is best model

###############################################################################
# Precipitation AICc initial sites
###############################################################################
prcp30.initial <- lmer(log(narea) ~ prcp30 + visit.type + (1 | NCRS.code), data = df)
prcp29.initial <- lmer(log(narea) ~ prcp29 + visit.type + (1 | NCRS.code), data = df)
prcp28.initial <- lmer(log(narea) ~ prcp28 + visit.type + (1 | NCRS.code), data = df)
prcp27.initial <- lmer(log(narea) ~ prcp27 + visit.type + (1 | NCRS.code), data = df)
prcp26.initial <- lmer(log(narea) ~ prcp26 + visit.type + (1 | NCRS.code), data = df)
prcp25.initial <- lmer(log(narea) ~ prcp25 + visit.type + (1 | NCRS.code), data = df)
prcp24.initial <- lmer(log(narea) ~ prcp24 + visit.type + (1 | NCRS.code), data = df)
prcp23.initial <- lmer(log(narea) ~ prcp23 + visit.type + (1 | NCRS.code), data = df)
prcp22 <- lmer(log(narea) ~ prcp22 + visit.type + (1 | NCRS.code), data = df)
prcp21 <- lmer(log(narea) ~ prcp21 + visit.type + (1 | NCRS.code), data = df)
prcp20 <- lmer(log(narea) ~ prcp20 + visit.type + (1 | NCRS.code), data = df)
prcp19 <- lmer(log(narea) ~ prcp19 + visit.type + (1 | NCRS.code), data = df)
prcp18 <- lmer(log(narea) ~ prcp18 + visit.type + (1 | NCRS.code), data = df)
prcp17 <- lmer(log(narea) ~ prcp17 + visit.type + (1 | NCRS.code), data = df)
prcp16 <- lmer(log(narea) ~ prcp16 + visit.type + (1 | NCRS.code), data = df)
prcp15 <- lmer(log(narea) ~ prcp15 + visit.type + (1 | NCRS.code), data = df)
prcp14 <- lmer(log(narea) ~ prcp14 + visit.type + (1 | NCRS.code), data = df)
prcp13 <- lmer(log(narea) ~ prcp13 + visit.type + (1 | NCRS.code), data = df)
prcp12 <- lmer(log(narea) ~ prcp12 + visit.type + (1 | NCRS.code), data = df)
prcp11 <- lmer(log(narea) ~ prcp11 + visit.type + (1 | NCRS.code), data = df)
prcp10 <- lmer(log(narea) ~ prcp10 + visit.type + (1 | NCRS.code), data = df)
prcp9 <- lmer(log(narea) ~ prcp9 + visit.type + (1 | NCRS.code), data = df)
prcp8 <- lmer(log(narea) ~ prcp8 + visit.type + (1 | NCRS.code), data = df)
prcp7 <- lmer(log(narea) ~ prcp7 + visit.type + (1 | NCRS.code), data = df)
prcp6 <- lmer(log(narea) ~ prcp6 + visit.type + (1 | NCRS.code), data = df)
prcp5 <- lmer(log(narea) ~ prcp5 + visit.type + (1 | NCRS.code), data = df)
prcp4 <- lmer(log(narea) ~ prcp4 + visit.type + (1 | NCRS.code), data = df)
prcp3 <- lmer(log(narea) ~ prcp3 + visit.type + (1 | NCRS.code), data = df)
prcp2 <- lmer(log(narea) ~ prcp2 + visit.type + (1 | NCRS.code), data = df)
prcp1 <- lmer(log(narea) ~ prcp1 + visit.type + (1 | NCRS.code), data = df)


prcp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(prcp30, prcp29, prcp28, prcp27, prcp26, 
                             prcp25, prcp24, prcp23, prcp22, prcp21, 
                             prcp20, prcp19, prcp18, prcp17, prcp16, 
                             prcp15, prcp14, prcp13, prcp12, prcp11, 
                             prcp10,prcp9, prcp8, prcp7, prcp6, prcp5, 
                             prcp4, prcp3, prcp2, prcp1)) %>%
  arrange(AICc)

# 27-day precipitation is most explanatory timescale for Narea

###############################################################################
# Precipitation RMSE
###############################################################################
prcp30.rmse <- data.frame(day = 30, R2 = rmse(prcp30))
prcp29.rmse <- data.frame(day = 29, R2 = rmse(prcp29))
prcp28.rmse <- data.frame(day = 28, R2 = rmse(prcp28))
prcp27.rmse <- data.frame(day = 27, R2 = rmse(prcp27))
prcp26.rmse <- data.frame(day = 26, R2 = rmse(prcp26))
prcp25.rmse <- data.frame(day = 25, R2 = rmse(prcp25))
prcp24.rmse <- data.frame(day = 24, R2 = rmse(prcp24))
prcp23.rmse <- data.frame(day = 23, R2 = rmse(prcp23))
prcp22.rmse <- data.frame(day = 22, R2 = rmse(prcp22))
prcp21.rmse <- data.frame(day = 21, R2 = rmse(prcp21))
prcp20.rmse <- data.frame(day = 20, R2 = rmse(prcp20))
prcp19.rmse <- data.frame(day = 19, R2 = rmse(prcp19))
prcp18.rmse <- data.frame(day = 18, R2 = rmse(prcp18))
prcp17.rmse <- data.frame(day = 17, R2 = rmse(prcp17))
prcp16.rmse <- data.frame(day = 16, R2 = rmse(prcp16))
prcp15.rmse <- data.frame(day = 15, R2 = rmse(prcp15))
prcp14.rmse <- data.frame(day = 14, R2 = rmse(prcp14))
prcp13.rmse <- data.frame(day = 13, R2 = rmse(prcp13))
prcp12.rmse <- data.frame(day = 12, R2 = rmse(prcp12))
prcp11.rmse <- data.frame(day = 11, R2 = rmse(prcp11))
prcp10.rmse <- data.frame(day = 10, R2 = rmse(prcp10))
prcp9.rmse <- data.frame(day = 9, R2 = rmse(prcp9))
prcp8.rmse <- data.frame(day = 8, R2 = rmse(prcp8))
prcp7.rmse <- data.frame(day = 7, R2 = rmse(prcp7))
prcp6.rmse <- data.frame(day = 6, R2 = rmse(prcp6))
prcp5.rmse <- data.frame(day = 5, R2 = rmse(prcp5))
prcp4.rmse <- data.frame(day = 4, R2 = rmse(prcp4))
prcp3.rmse <- data.frame(day = 3, R2 = rmse(prcp3))
prcp2.rmse <- data.frame(day = 2, R2 = rmse(prcp2))
prcp1.rmse <- data.frame(day = 1, R2 = rmse(prcp1))


prcp.rmse <- prcp30.rmse %>% full_join(prcp29.rmse) %>% full_join(prcp28.rmse) %>% 
  full_join(prcp27.rmse) %>% full_join(prcp26.rmse) %>% full_join(prcp25.rmse) %>%
  full_join(prcp24.rmse) %>% full_join(prcp23.rmse) %>% full_join(prcp22.rmse) %>%
  full_join(prcp21.rmse) %>% full_join(prcp20.rmse) %>% full_join(prcp19.rmse) %>%
  full_join(prcp18.rmse) %>% full_join(prcp17.rmse) %>% full_join(prcp16.rmse) %>%
  full_join(prcp15.rmse) %>% full_join(prcp14.rmse) %>% full_join(prcp13.rmse) %>%
  full_join(prcp12.rmse) %>% full_join(prcp11.rmse) %>% full_join(prcp10.rmse) %>%
  full_join(prcp9.rmse) %>% full_join(prcp8.rmse) %>% full_join(prcp7.rmse) %>%
  full_join(prcp6.rmse) %>% full_join(prcp5.rmse) %>% full_join(prcp4.rmse) %>%
  full_join(prcp3.rmse) %>% full_join(prcp2.rmse) %>% full_join(prcp1.rmse) 

###############################################################################
# Precipitation R^2
###############################################################################
prcp30.r2 <- data.frame(day = 30, R2 = r.squaredGLMM(prcp30))
prcp29.r2 <- data.frame(day = 29, R2 = r.squaredGLMM(prcp29))
prcp28.r2 <- data.frame(day = 28, R2 = r.squaredGLMM(prcp28))
prcp27.r2 <- data.frame(day = 27, R2 = r.squaredGLMM(prcp27))
prcp26.r2 <- data.frame(day = 26, R2 = r.squaredGLMM(prcp26))
prcp25.r2 <- data.frame(day = 25, R2 = r.squaredGLMM(prcp25))
prcp24.r2 <- data.frame(day = 24, R2 = r.squaredGLMM(prcp24))
prcp23.r2 <- data.frame(day = 23, R2 = r.squaredGLMM(prcp23))
prcp22.r2 <- data.frame(day = 22, R2 = r.squaredGLMM(prcp22))
prcp21.r2 <- data.frame(day = 21, R2 = r.squaredGLMM(prcp21))
prcp20.r2 <- data.frame(day = 20, R2 = r.squaredGLMM(prcp20))
prcp19.r2 <- data.frame(day = 19, R2 = r.squaredGLMM(prcp19))
prcp18.r2 <- data.frame(day = 18, R2 = r.squaredGLMM(prcp18))
prcp17.r2 <- data.frame(day = 17, R2 = r.squaredGLMM(prcp17))
prcp16.r2 <- data.frame(day = 16, R2 = r.squaredGLMM(prcp16))
prcp15.r2 <- data.frame(day = 15, R2 = r.squaredGLMM(prcp15))
prcp14.r2 <- data.frame(day = 14, R2 = r.squaredGLMM(prcp14))
prcp13.r2 <- data.frame(day = 13, R2 = r.squaredGLMM(prcp13))
prcp12.r2 <- data.frame(day = 12, R2 = r.squaredGLMM(prcp12))
prcp11.r2 <- data.frame(day = 11, R2 = r.squaredGLMM(prcp11))
prcp10.r2 <- data.frame(day = 10, R2 = r.squaredGLMM(prcp10))
prcp9.r2 <- data.frame(day = 9, R2 = r.squaredGLMM(prcp9))
prcp8.r2 <- data.frame(day = 8, R2 = r.squaredGLMM(prcp8))
prcp7.r2 <- data.frame(day = 7, R2 = r.squaredGLMM(prcp7))
prcp6.r2 <- data.frame(day = 6, R2 = r.squaredGLMM(prcp6))
prcp5.r2 <- data.frame(day = 5, R2 = r.squaredGLMM(prcp5))
prcp4.r2 <- data.frame(day = 4, R2 = r.squaredGLMM(prcp4))
prcp3.r2 <- data.frame(day = 3, R2 = r.squaredGLMM(prcp3))
prcp2.r2 <- data.frame(day = 2, R2 = r.squaredGLMM(prcp2))
prcp1.r2 <- data.frame(day = 1, R2 = r.squaredGLMM(prcp1))




prcp.modelSelect <- prcp.aicc %>% full_join(prcp.rmse) %>% dplyr::select(-df)



library(ggplot2)
ggplot(data = prcp.modelSelect, aes(x = day, y = RMSE)) +
  geom_line()
  
  
  
  
  
  prcp30, prcp29, prcp28, prcp27, 
  prcp26, prcp25, prcp24, prcp23, prcp22, prcp21, prcp20, prcp19,
  prcp18, prcp17, prcp16, prcp15, prcp14, prcp13, prcp12, prcp11, 
  prcp10,prcp9, prcp8, prcp7, prcp6, prcp5, prcp4, prcp3, prcp2, 
  prcp1) %>%
  arrange(AICc)




###############################################################################
# Iterative models for Tmean
###############################################################################
temp30 <- lmer(log(narea) ~ tavg30 + (1 | NCRS.code), data = df)
temp29 <- lmer(log(narea) ~ tavg29 + (1 | NCRS.code), data = df)
temp28 <- lmer(log(narea) ~ tavg28 + (1 | NCRS.code), data = df)
temp27 <- lmer(log(narea) ~ tavg27 + (1 | NCRS.code), data = df)
temp26 <- lmer(log(narea) ~ tavg26 + (1 | NCRS.code), data = df)
temp25 <- lmer(log(narea) ~ tavg25 + (1 | NCRS.code), data = df)
temp24 <- lmer(log(narea) ~ tavg24 + (1 | NCRS.code), data = df)
temp23 <- lmer(log(narea) ~ tavg23 + (1 | NCRS.code), data = df)
temp22 <- lmer(log(narea) ~ tavg22 + (1 | NCRS.code), data = df)
temp21 <- lmer(log(narea) ~ tavg21 + (1 | NCRS.code), data = df)
temp20 <- lmer(log(narea) ~ tavg20 + (1 | NCRS.code), data = df)
temp19 <- lmer(log(narea) ~ tavg19 + (1 | NCRS.code), data = df)
temp18 <- lmer(log(narea) ~ tavg18 + (1 | NCRS.code), data = df)
temp17 <- lmer(log(narea) ~ tavg17 + (1 | NCRS.code), data = df)
temp16 <- lmer(log(narea) ~ tavg16 + (1 | NCRS.code), data = df)
temp15 <- lmer(log(narea) ~ tavg15 + (1 | NCRS.code), data = df)
temp14 <- lmer(log(narea) ~ tavg14 + (1 | NCRS.code), data = df)
temp13 <- lmer(log(narea) ~ tavg13 + (1 | NCRS.code), data = df)
temp12 <- lmer(log(narea) ~ tavg12 + (1 | NCRS.code), data = df)
temp11 <- lmer(log(narea) ~ tavg11 + (1 | NCRS.code), data = df)
temp10 <- lmer(log(narea) ~ tavg10 + (1 | NCRS.code), data = df)
temp9 <- lmer(log(narea) ~ tavg9 + (1 | NCRS.code), data = df)
temp8 <- lmer(log(narea) ~ tavg8 + (1 | NCRS.code), data = df)
temp7 <- lmer(log(narea) ~ tavg7 + (1 | NCRS.code), data = df)
temp6 <- lmer(log(narea) ~ tavg6 + (1 | NCRS.code), data = df)
temp5 <- lmer(log(narea) ~ tavg5 + (1 | NCRS.code), data = df)
temp4 <- lmer(log(narea) ~ tavg4 + (1 | NCRS.code), data = df)
temp3 <- lmer(log(narea) ~ tavg3 + (1 | NCRS.code), data = df)
temp2 <- lmer(log(narea) ~ tavg2 + (1 | NCRS.code), data = df)
temp1 <- lmer(log(narea) ~ tavg1 + (1 | NCRS.code), data = df)




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