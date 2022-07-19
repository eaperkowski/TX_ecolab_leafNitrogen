###############################################################################
# Libraries
###############################################################################
library(dplyr)
library(car)
library(lme4)
library(MuMIn)
library(nlme)
library(sjstats)
library(ggplot2)

###############################################################################
# Load compiled data file
###############################################################################
df <- read.csv("../data_sheets/TXeco_compiled_datasheet.csv")

###############################################################################
# Iterative models for growing season aridity and for log(narea)
###############################################################################
df$narea[c(244)] <- NA

ai.30 <- lmer(narea ~ ai.30 + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.30))
hist(residuals(ai.30))
outlierTest(ai.30)
Anova(ai.30)

ai.60 <- lmer(narea ~ ai.60 + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.60))
hist(residuals(ai.60))
outlierTest(ai.60)
Anova(ai.60)

ai.90 <- lmer(narea ~ ai.90 + (1 | NCRS.code), data = df)
shapiro.test(residuals(ai.90))
hist(residuals(ai.90))
outlierTest(ai.90)
Anova(ai.90)


AICc(ai.30, ai.60, ai.90)
BIC(ai.30, ai.60, ai.90)
## ai.90 is best model

###############################################################################
# Precipitation AICc for log(narea)
###############################################################################
prcp30 <- lmer(log(narea) ~ prcp30 + (1 | NCRS.code), data = df)
prcp29 <- lmer(log(narea) ~ prcp29 + (1 | NCRS.code), data = df)
prcp28 <- lmer(log(narea) ~ prcp28 + (1 | NCRS.code), data = df)
prcp27 <- lmer(log(narea) ~ prcp27 + (1 | NCRS.code), data = df)
prcp26 <- lmer(log(narea) ~ prcp26 + (1 | NCRS.code), data = df)
prcp25 <- lmer(log(narea) ~ prcp25 + (1 | NCRS.code), data = df)
prcp24 <- lmer(log(narea) ~ prcp24 + (1 | NCRS.code), data = df)
prcp23 <- lmer(log(narea) ~ prcp23 + (1 | NCRS.code), data = df)
prcp22 <- lmer(log(narea) ~ prcp22 + (1 | NCRS.code), data = df)
prcp21 <- lmer(log(narea) ~ prcp21 + (1 | NCRS.code), data = df)
prcp20 <- lmer(log(narea) ~ prcp20 + (1 | NCRS.code), data = df)
prcp19 <- lmer(log(narea) ~ prcp19 + (1 | NCRS.code), data = df)
prcp18 <- lmer(log(narea) ~ prcp18 + (1 | NCRS.code), data = df)
prcp17 <- lmer(log(narea) ~ prcp17 + (1 | NCRS.code), data = df)
prcp16 <- lmer(log(narea) ~ prcp16 + (1 | NCRS.code), data = df)
prcp15 <- lmer(log(narea) ~ prcp15 + (1 | NCRS.code), data = df)
prcp14 <- lmer(log(narea) ~ prcp14 + (1 | NCRS.code), data = df)
prcp13 <- lmer(log(narea) ~ prcp13 + (1 | NCRS.code), data = df)
prcp12 <- lmer(log(narea) ~ prcp12 + (1 | NCRS.code), data = df)
prcp11 <- lmer(log(narea) ~ prcp11 + (1 | NCRS.code), data = df)
prcp10 <- lmer(log(narea) ~ prcp10 + (1 | NCRS.code), data = df)
prcp9 <- lmer(log(narea) ~ prcp9 + (1 | NCRS.code), data = df)
prcp8 <- lmer(log(narea) ~ prcp8 + (1 | NCRS.code), data = df)
prcp7 <- lmer(log(narea) ~ prcp7 + (1 | NCRS.code), data = df)
prcp6 <- lmer(log(narea) ~ prcp6 + (1 | NCRS.code), data = df)
prcp5 <- lmer(log(narea) ~ prcp5 + (1 | NCRS.code), data = df)
prcp4 <- lmer(log(narea) ~ prcp4 + (1 | NCRS.code), data = df)
prcp3 <- lmer(log(narea) ~ prcp3 + (1 | NCRS.code), data = df)
prcp2 <- lmer(log(narea) ~ prcp2 + (1 | NCRS.code), data = df)
prcp1 <- lmer(log(narea) ~ prcp1 + (1 | NCRS.code), data = df)


prcp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(prcp30, prcp29, prcp28, prcp27, prcp26, 
                             prcp25, prcp24, prcp23, prcp22, prcp21, 
                             prcp20, prcp19, prcp18, prcp17, prcp16, 
                             prcp15, prcp14, prcp13, prcp12, prcp11, 
                             prcp10,prcp9, prcp8, prcp7, prcp6, prcp5, 
                             prcp4, prcp3, prcp2, prcp1)) %>%
  arrange(AICc)

# 15-day precipitation is most explanatory timescale for Narea

###############################################################################
# Precipitation RMSE for log(narea)
###############################################################################
prcp30.rmse <- data.frame(day = 30, RMSE = rmse(prcp30))
prcp29.rmse <- data.frame(day = 29, RMSE = rmse(prcp29))
prcp28.rmse <- data.frame(day = 28, RMSE = rmse(prcp28))
prcp27.rmse <- data.frame(day = 27, RMSE = rmse(prcp27))
prcp26.rmse <- data.frame(day = 26, RMSE = rmse(prcp26))
prcp25.rmse <- data.frame(day = 25, RMSE = rmse(prcp25))
prcp24.rmse <- data.frame(day = 24, RMSE = rmse(prcp24))
prcp23.rmse <- data.frame(day = 23, RMSE = rmse(prcp23))
prcp22.rmse <- data.frame(day = 22, RMSE = rmse(prcp22))
prcp21.rmse <- data.frame(day = 21, RMSE = rmse(prcp21))
prcp20.rmse <- data.frame(day = 20, RMSE = rmse(prcp20))
prcp19.rmse <- data.frame(day = 19, RMSE = rmse(prcp19))
prcp18.rmse <- data.frame(day = 18, RMSE = rmse(prcp18))
prcp17.rmse <- data.frame(day = 17, RMSE = rmse(prcp17))
prcp16.rmse <- data.frame(day = 16, RMSE = rmse(prcp16))
prcp15.rmse <- data.frame(day = 15, RMSE = rmse(prcp15))
prcp14.rmse <- data.frame(day = 14, RMSE = rmse(prcp14))
prcp13.rmse <- data.frame(day = 13, RMSE = rmse(prcp13))
prcp12.rmse <- data.frame(day = 12, RMSE = rmse(prcp12))
prcp11.rmse <- data.frame(day = 11, RMSE = rmse(prcp11))
prcp10.rmse <- data.frame(day = 10, RMSE = rmse(prcp10))
prcp9.rmse <- data.frame(day = 9, RMSE = rmse(prcp9))
prcp8.rmse <- data.frame(day = 8, RMSE = rmse(prcp8))
prcp7.rmse <- data.frame(day = 7, RMSE = rmse(prcp7))
prcp6.rmse <- data.frame(day = 6, RMSE = rmse(prcp6))
prcp5.rmse <- data.frame(day = 5, RMSE = rmse(prcp5))
prcp4.rmse <- data.frame(day = 4, RMSE = rmse(prcp4))
prcp3.rmse <- data.frame(day = 3, RMSE = rmse(prcp3))
prcp2.rmse <- data.frame(day = 2, RMSE = rmse(prcp2))
prcp1.rmse <- data.frame(day = 1, RMSE = rmse(prcp1))


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
# Precipitation R^2 for log(narea)
###############################################################################
prcp30.r2 <- data.frame(day = 30, r.squaredGLMM(prcp30))
prcp29.r2 <- data.frame(day = 29, r.squaredGLMM(prcp29))
prcp28.r2 <- data.frame(day = 28, r.squaredGLMM(prcp28))
prcp27.r2 <- data.frame(day = 27, r.squaredGLMM(prcp27))
prcp26.r2 <- data.frame(day = 26, r.squaredGLMM(prcp26))
prcp25.r2 <- data.frame(day = 25, r.squaredGLMM(prcp25))
prcp24.r2 <- data.frame(day = 24, r.squaredGLMM(prcp24))
prcp23.r2 <- data.frame(day = 23, r.squaredGLMM(prcp23))
prcp22.r2 <- data.frame(day = 22, r.squaredGLMM(prcp22))
prcp21.r2 <- data.frame(day = 21, r.squaredGLMM(prcp21))
prcp20.r2 <- data.frame(day = 20, r.squaredGLMM(prcp20))
prcp19.r2 <- data.frame(day = 19, r.squaredGLMM(prcp19))
prcp18.r2 <- data.frame(day = 18, r.squaredGLMM(prcp18))
prcp17.r2 <- data.frame(day = 17, r.squaredGLMM(prcp17))
prcp16.r2 <- data.frame(day = 16, r.squaredGLMM(prcp16))
prcp15.r2 <- data.frame(day = 15, r.squaredGLMM(prcp15))
prcp14.r2 <- data.frame(day = 14, r.squaredGLMM(prcp14))
prcp13.r2 <- data.frame(day = 13, r.squaredGLMM(prcp13))
prcp12.r2 <- data.frame(day = 12, r.squaredGLMM(prcp12))
prcp11.r2 <- data.frame(day = 11, r.squaredGLMM(prcp11))
prcp10.r2 <- data.frame(day = 10, r.squaredGLMM(prcp10))
prcp9.r2 <- data.frame(day = 9, r.squaredGLMM(prcp9))
prcp8.r2 <- data.frame(day = 8, r.squaredGLMM(prcp8))
prcp7.r2 <- data.frame(day = 7, r.squaredGLMM(prcp7))
prcp6.r2 <- data.frame(day = 6, r.squaredGLMM(prcp6))
prcp5.r2 <- data.frame(day = 5, r.squaredGLMM(prcp5))
prcp4.r2 <- data.frame(day = 4, r.squaredGLMM(prcp4))
prcp3.r2 <- data.frame(day = 3, r.squaredGLMM(prcp3))
prcp2.r2 <- data.frame(day = 2, r.squaredGLMM(prcp2))
prcp1.r2 <- data.frame(day = 1, r.squaredGLMM(prcp1))

prcp.r2 <- prcp30.r2 %>% full_join(prcp29.r2) %>% full_join(prcp28.r2) %>% 
  full_join(prcp27.r2) %>% full_join(prcp26.r2) %>% full_join(prcp25.r2) %>%
  full_join(prcp24.r2) %>% full_join(prcp23.r2) %>% full_join(prcp22.r2) %>%
  full_join(prcp21.r2) %>% full_join(prcp20.r2) %>% full_join(prcp19.r2) %>%
  full_join(prcp18.r2) %>% full_join(prcp17.r2) %>% full_join(prcp16.r2) %>%
  full_join(prcp15.r2) %>% full_join(prcp14.r2) %>% full_join(prcp13.r2) %>%
  full_join(prcp12.r2) %>% full_join(prcp11.r2) %>% full_join(prcp10.r2) %>%
  full_join(prcp9.r2) %>% full_join(prcp8.r2) %>% full_join(prcp7.r2) %>%
  full_join(prcp6.r2) %>% full_join(prcp5.r2) %>% full_join(prcp4.r2) %>%
  full_join(prcp3.r2) %>% full_join(prcp2.r2) %>% full_join(prcp1.r2)

prcp.modelSelect <- prcp.aicc %>% full_join(prcp.rmse) %>%
  full_join(prcp.r2) %>% dplyr::select(-df)

rmse.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = RMSE)) + 
  geom_vline(xintercept = 15, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Model RMSE") +
  theme_bw()

r2m.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2m)) + 
  geom_vline(xintercept = 15, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Marginal R"^2)) +
  theme_bw()

r2c.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2c)) + 
  geom_vline(xintercept = 15, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Conditional R"^2)) +
  theme_bw()

aicc.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = AICc)) + 
  geom_vline(xintercept = 15, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = expression("AIC"[c])) +
  theme_bw()


ggpubr::ggarrange(aicc.prcp.plot, rmse.prcp.plot,
                  r2m.prcp.plot, r2c.prcp.plot,
                  ncol = 2, nrow = 2, align = "hv")

###############################################################################
# Iterative models for Tmean and log(narea)
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


temp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(temp30, temp29, temp28, temp27, temp26, temp25, temp24, temp23, 
                             temp22, temp21, temp20, temp19, temp18, temp17, temp16, temp15, 
                             temp14, temp13, temp12, temp11, temp10, temp9, temp8, temp7, 
                             temp6, temp5, temp4, temp3, temp2, temp1))
head(temp.aicc)
## 1-day temperature is most explanatory temperature scale


###############################################################################
# Temperature RMSE for log(narea)
###############################################################################
temp30.rmse <- data.frame(day = 30, RMSE = rmse(temp30))
temp29.rmse <- data.frame(day = 29, RMSE = rmse(temp29))
temp28.rmse <- data.frame(day = 28, RMSE = rmse(temp28))
temp27.rmse <- data.frame(day = 27, RMSE = rmse(temp27))
temp26.rmse <- data.frame(day = 26, RMSE = rmse(temp26))
temp25.rmse <- data.frame(day = 25, RMSE = rmse(temp25))
temp24.rmse <- data.frame(day = 24, RMSE = rmse(temp24))
temp23.rmse <- data.frame(day = 23, RMSE = rmse(temp23))
temp22.rmse <- data.frame(day = 22, RMSE = rmse(temp22))
temp21.rmse <- data.frame(day = 21, RMSE = rmse(temp21))
temp20.rmse <- data.frame(day = 20, RMSE = rmse(temp20))
temp19.rmse <- data.frame(day = 19, RMSE = rmse(temp19))
temp18.rmse <- data.frame(day = 18, RMSE = rmse(temp18))
temp17.rmse <- data.frame(day = 17, RMSE = rmse(temp17))
temp16.rmse <- data.frame(day = 16, RMSE = rmse(temp16))
temp15.rmse <- data.frame(day = 15, RMSE = rmse(temp15))
temp14.rmse <- data.frame(day = 14, RMSE = rmse(temp14))
temp13.rmse <- data.frame(day = 13, RMSE = rmse(temp13))
temp12.rmse <- data.frame(day = 12, RMSE = rmse(temp12))
temp11.rmse <- data.frame(day = 11, RMSE = rmse(temp11))
temp10.rmse <- data.frame(day = 10, RMSE = rmse(temp10))
temp9.rmse <- data.frame(day = 9, RMSE = rmse(temp9))
temp8.rmse <- data.frame(day = 8, RMSE = rmse(temp8))
temp7.rmse <- data.frame(day = 7, RMSE = rmse(temp7))
temp6.rmse <- data.frame(day = 6, RMSE = rmse(temp6))
temp5.rmse <- data.frame(day = 5, RMSE = rmse(temp5))
temp4.rmse <- data.frame(day = 4, RMSE = rmse(temp4))
temp3.rmse <- data.frame(day = 3, RMSE = rmse(temp3))
temp2.rmse <- data.frame(day = 2, RMSE = rmse(temp2))
temp1.rmse <- data.frame(day = 1, RMSE = rmse(temp1))


temp.rmse <- temp30.rmse %>% full_join(temp29.rmse) %>% full_join(temp28.rmse) %>% 
  full_join(temp27.rmse) %>% full_join(temp26.rmse) %>% full_join(temp25.rmse) %>%
  full_join(temp24.rmse) %>% full_join(temp23.rmse) %>% full_join(temp22.rmse) %>%
  full_join(temp21.rmse) %>% full_join(temp20.rmse) %>% full_join(temp19.rmse) %>%
  full_join(temp18.rmse) %>% full_join(temp17.rmse) %>% full_join(temp16.rmse) %>%
  full_join(temp15.rmse) %>% full_join(temp14.rmse) %>% full_join(temp13.rmse) %>%
  full_join(temp12.rmse) %>% full_join(temp11.rmse) %>% full_join(temp10.rmse) %>%
  full_join(temp9.rmse) %>% full_join(temp8.rmse) %>% full_join(temp7.rmse) %>%
  full_join(temp6.rmse) %>% full_join(temp5.rmse) %>% full_join(temp4.rmse) %>%
  full_join(temp3.rmse) %>% full_join(temp2.rmse) %>% full_join(temp1.rmse) 

###############################################################################
# Temperature R^2 for log(narea)
###############################################################################
temp30.r2 <- data.frame(day = 30, r.squaredGLMM(temp30))
temp29.r2 <- data.frame(day = 29, r.squaredGLMM(temp29))
temp28.r2 <- data.frame(day = 28, r.squaredGLMM(temp28))
temp27.r2 <- data.frame(day = 27, r.squaredGLMM(temp27))
temp26.r2 <- data.frame(day = 26, r.squaredGLMM(temp26))
temp25.r2 <- data.frame(day = 25, r.squaredGLMM(temp25))
temp24.r2 <- data.frame(day = 24, r.squaredGLMM(temp24))
temp23.r2 <- data.frame(day = 23, r.squaredGLMM(temp23))
temp22.r2 <- data.frame(day = 22, r.squaredGLMM(temp22))
temp21.r2 <- data.frame(day = 21, r.squaredGLMM(temp21))
temp20.r2 <- data.frame(day = 20, r.squaredGLMM(temp20))
temp19.r2 <- data.frame(day = 19, r.squaredGLMM(temp19))
temp18.r2 <- data.frame(day = 18, r.squaredGLMM(temp18))
temp17.r2 <- data.frame(day = 17, r.squaredGLMM(temp17))
temp16.r2 <- data.frame(day = 16, r.squaredGLMM(temp16))
temp15.r2 <- data.frame(day = 15, r.squaredGLMM(temp15))
temp14.r2 <- data.frame(day = 14, r.squaredGLMM(temp14))
temp13.r2 <- data.frame(day = 13, r.squaredGLMM(temp13))
temp12.r2 <- data.frame(day = 12, r.squaredGLMM(temp12))
temp11.r2 <- data.frame(day = 11, r.squaredGLMM(temp11))
temp10.r2 <- data.frame(day = 10, r.squaredGLMM(temp10))
temp9.r2 <- data.frame(day = 9, r.squaredGLMM(temp9))
temp8.r2 <- data.frame(day = 8, r.squaredGLMM(temp8))
temp7.r2 <- data.frame(day = 7, r.squaredGLMM(temp7))
temp6.r2 <- data.frame(day = 6, r.squaredGLMM(temp6))
temp5.r2 <- data.frame(day = 5, r.squaredGLMM(temp5))
temp4.r2 <- data.frame(day = 4, r.squaredGLMM(temp4))
temp3.r2 <- data.frame(day = 3, r.squaredGLMM(temp3))
temp2.r2 <- data.frame(day = 2, r.squaredGLMM(temp2))
temp1.r2 <- data.frame(day = 1, r.squaredGLMM(temp1))

temp.r2 <- temp30.r2 %>% full_join(temp29.r2) %>% full_join(temp28.r2) %>% 
  full_join(temp27.r2) %>% full_join(temp26.r2) %>% full_join(temp25.r2) %>%
  full_join(temp24.r2) %>% full_join(temp23.r2) %>% full_join(temp22.r2) %>%
  full_join(temp21.r2) %>% full_join(temp20.r2) %>% full_join(temp19.r2) %>%
  full_join(temp18.r2) %>% full_join(temp17.r2) %>% full_join(temp16.r2) %>%
  full_join(temp15.r2) %>% full_join(temp14.r2) %>% full_join(temp13.r2) %>%
  full_join(temp12.r2) %>% full_join(temp11.r2) %>% full_join(temp10.r2) %>%
  full_join(temp9.r2) %>% full_join(temp8.r2) %>% full_join(temp7.r2) %>%
  full_join(temp6.r2) %>% full_join(temp5.r2) %>% full_join(temp4.r2) %>%
  full_join(temp3.r2) %>% full_join(temp2.r2) %>% full_join(temp1.r2)

temp.modelSelect <- temp.aicc %>% full_join(temp.rmse) %>%
  full_join(temp.r2) %>% dplyr::select(-df)

rmse.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = RMSE)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Model RMSE") +
  theme_bw()

r2m.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2m)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Marginal R"^2)) +
  theme_bw()

r2c.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2c)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Conditional R"^2)) +
  theme_bw()

aicc.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = AICc)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = expression("AIC"[c])) +
  theme_bw()

ggpubr::ggarrange(aicc.temp.plot, rmse.temp.plot,
                  r2m.temp.plot, r2c.temp.plot,
                  ncol = 2, nrow = 2, align = "hv")


###############################################################################
# Precipitation AICc for log(nmass)
###############################################################################
prcp30 <- lmer(n.leaf ~ prcp30 + (1 | NCRS.code), data = df)
prcp29 <- lmer(n.leaf ~ prcp29 + (1 | NCRS.code), data = df)
prcp28 <- lmer(n.leaf ~ prcp28 + (1 | NCRS.code), data = df)
prcp27 <- lmer(n.leaf ~ prcp27 + (1 | NCRS.code), data = df)
prcp26 <- lmer(n.leaf ~ prcp26 + (1 | NCRS.code), data = df)
prcp25 <- lmer(n.leaf ~ prcp25 + (1 | NCRS.code), data = df)
prcp24 <- lmer(n.leaf ~ prcp24 + (1 | NCRS.code), data = df)
prcp23 <- lmer(n.leaf ~ prcp23 + (1 | NCRS.code), data = df)
prcp22 <- lmer(n.leaf ~ prcp22 + (1 | NCRS.code), data = df)
prcp21 <- lmer(n.leaf ~ prcp21 + (1 | NCRS.code), data = df)
prcp20 <- lmer(n.leaf ~ prcp20 + (1 | NCRS.code), data = df)
prcp19 <- lmer(n.leaf ~ prcp19 + (1 | NCRS.code), data = df)
prcp18 <- lmer(n.leaf ~ prcp18 + (1 | NCRS.code), data = df)
prcp17 <- lmer(n.leaf ~ prcp17 + (1 | NCRS.code), data = df)
prcp16 <- lmer(n.leaf ~ prcp16 + (1 | NCRS.code), data = df)
prcp15 <- lmer(n.leaf ~ prcp15 + (1 | NCRS.code), data = df)
prcp14 <- lmer(n.leaf ~ prcp14 + (1 | NCRS.code), data = df)
prcp13 <- lmer(n.leaf ~ prcp13 + (1 | NCRS.code), data = df)
prcp12 <- lmer(n.leaf ~ prcp12 + (1 | NCRS.code), data = df)
prcp11 <- lmer(n.leaf ~ prcp11 + (1 | NCRS.code), data = df)
prcp10 <- lmer(n.leaf ~ prcp10 + (1 | NCRS.code), data = df)
prcp9 <- lmer(n.leaf ~ prcp9 + (1 | NCRS.code), data = df)
prcp8 <- lmer(n.leaf ~ prcp8 + (1 | NCRS.code), data = df)
prcp7 <- lmer(n.leaf ~ prcp7 + (1 | NCRS.code), data = df)
prcp6 <- lmer(n.leaf ~ prcp6 + (1 | NCRS.code), data = df)
prcp5 <- lmer(n.leaf ~ prcp5 + (1 | NCRS.code), data = df)
prcp4 <- lmer(n.leaf ~ prcp4 + (1 | NCRS.code), data = df)
prcp3 <- lmer(n.leaf ~ prcp3 + (1 | NCRS.code), data = df)
prcp2 <- lmer(n.leaf ~ prcp2 + (1 | NCRS.code), data = df)
prcp1 <- lmer(n.leaf ~ prcp1 + (1 | NCRS.code), data = df)


prcp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(prcp30, prcp29, prcp28, prcp27, prcp26, 
                             prcp25, prcp24, prcp23, prcp22, prcp21, 
                             prcp20, prcp19, prcp18, prcp17, prcp16, 
                             prcp15, prcp14, prcp13, prcp12, prcp11, 
                             prcp10,prcp9, prcp8, prcp7, prcp6, prcp5, 
                             prcp4, prcp3, prcp2, prcp1)) %>%
  arrange(AICc)

# 1-day precipitation is most explanatory timescale for Nmass

###############################################################################
# Precipitation RMSE for log(narea)
###############################################################################
prcp30.rmse <- data.frame(day = 30, RMSE = rmse(prcp30))
prcp29.rmse <- data.frame(day = 29, RMSE = rmse(prcp29))
prcp28.rmse <- data.frame(day = 28, RMSE = rmse(prcp28))
prcp27.rmse <- data.frame(day = 27, RMSE = rmse(prcp27))
prcp26.rmse <- data.frame(day = 26, RMSE = rmse(prcp26))
prcp25.rmse <- data.frame(day = 25, RMSE = rmse(prcp25))
prcp24.rmse <- data.frame(day = 24, RMSE = rmse(prcp24))
prcp23.rmse <- data.frame(day = 23, RMSE = rmse(prcp23))
prcp22.rmse <- data.frame(day = 22, RMSE = rmse(prcp22))
prcp21.rmse <- data.frame(day = 21, RMSE = rmse(prcp21))
prcp20.rmse <- data.frame(day = 20, RMSE = rmse(prcp20))
prcp19.rmse <- data.frame(day = 19, RMSE = rmse(prcp19))
prcp18.rmse <- data.frame(day = 18, RMSE = rmse(prcp18))
prcp17.rmse <- data.frame(day = 17, RMSE = rmse(prcp17))
prcp16.rmse <- data.frame(day = 16, RMSE = rmse(prcp16))
prcp15.rmse <- data.frame(day = 15, RMSE = rmse(prcp15))
prcp14.rmse <- data.frame(day = 14, RMSE = rmse(prcp14))
prcp13.rmse <- data.frame(day = 13, RMSE = rmse(prcp13))
prcp12.rmse <- data.frame(day = 12, RMSE = rmse(prcp12))
prcp11.rmse <- data.frame(day = 11, RMSE = rmse(prcp11))
prcp10.rmse <- data.frame(day = 10, RMSE = rmse(prcp10))
prcp9.rmse <- data.frame(day = 9, RMSE = rmse(prcp9))
prcp8.rmse <- data.frame(day = 8, RMSE = rmse(prcp8))
prcp7.rmse <- data.frame(day = 7, RMSE = rmse(prcp7))
prcp6.rmse <- data.frame(day = 6, RMSE = rmse(prcp6))
prcp5.rmse <- data.frame(day = 5, RMSE = rmse(prcp5))
prcp4.rmse <- data.frame(day = 4, RMSE = rmse(prcp4))
prcp3.rmse <- data.frame(day = 3, RMSE = rmse(prcp3))
prcp2.rmse <- data.frame(day = 2, RMSE = rmse(prcp2))
prcp1.rmse <- data.frame(day = 1, RMSE = rmse(prcp1))


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
# Precipitation R^2 for log(narea)
###############################################################################
prcp30.r2 <- data.frame(day = 30, r.squaredGLMM(prcp30))
prcp29.r2 <- data.frame(day = 29, r.squaredGLMM(prcp29))
prcp28.r2 <- data.frame(day = 28, r.squaredGLMM(prcp28))
prcp27.r2 <- data.frame(day = 27, r.squaredGLMM(prcp27))
prcp26.r2 <- data.frame(day = 26, r.squaredGLMM(prcp26))
prcp25.r2 <- data.frame(day = 25, r.squaredGLMM(prcp25))
prcp24.r2 <- data.frame(day = 24, r.squaredGLMM(prcp24))
prcp23.r2 <- data.frame(day = 23, r.squaredGLMM(prcp23))
prcp22.r2 <- data.frame(day = 22, r.squaredGLMM(prcp22))
prcp21.r2 <- data.frame(day = 21, r.squaredGLMM(prcp21))
prcp20.r2 <- data.frame(day = 20, r.squaredGLMM(prcp20))
prcp19.r2 <- data.frame(day = 19, r.squaredGLMM(prcp19))
prcp18.r2 <- data.frame(day = 18, r.squaredGLMM(prcp18))
prcp17.r2 <- data.frame(day = 17, r.squaredGLMM(prcp17))
prcp16.r2 <- data.frame(day = 16, r.squaredGLMM(prcp16))
prcp15.r2 <- data.frame(day = 15, r.squaredGLMM(prcp15))
prcp14.r2 <- data.frame(day = 14, r.squaredGLMM(prcp14))
prcp13.r2 <- data.frame(day = 13, r.squaredGLMM(prcp13))
prcp12.r2 <- data.frame(day = 12, r.squaredGLMM(prcp12))
prcp11.r2 <- data.frame(day = 11, r.squaredGLMM(prcp11))
prcp10.r2 <- data.frame(day = 10, r.squaredGLMM(prcp10))
prcp9.r2 <- data.frame(day = 9, r.squaredGLMM(prcp9))
prcp8.r2 <- data.frame(day = 8, r.squaredGLMM(prcp8))
prcp7.r2 <- data.frame(day = 7, r.squaredGLMM(prcp7))
prcp6.r2 <- data.frame(day = 6, r.squaredGLMM(prcp6))
prcp5.r2 <- data.frame(day = 5, r.squaredGLMM(prcp5))
prcp4.r2 <- data.frame(day = 4, r.squaredGLMM(prcp4))
prcp3.r2 <- data.frame(day = 3, r.squaredGLMM(prcp3))
prcp2.r2 <- data.frame(day = 2, r.squaredGLMM(prcp2))
prcp1.r2 <- data.frame(day = 1, r.squaredGLMM(prcp1))

prcp.r2 <- prcp30.r2 %>% full_join(prcp29.r2) %>% full_join(prcp28.r2) %>% 
  full_join(prcp27.r2) %>% full_join(prcp26.r2) %>% full_join(prcp25.r2) %>%
  full_join(prcp24.r2) %>% full_join(prcp23.r2) %>% full_join(prcp22.r2) %>%
  full_join(prcp21.r2) %>% full_join(prcp20.r2) %>% full_join(prcp19.r2) %>%
  full_join(prcp18.r2) %>% full_join(prcp17.r2) %>% full_join(prcp16.r2) %>%
  full_join(prcp15.r2) %>% full_join(prcp14.r2) %>% full_join(prcp13.r2) %>%
  full_join(prcp12.r2) %>% full_join(prcp11.r2) %>% full_join(prcp10.r2) %>%
  full_join(prcp9.r2) %>% full_join(prcp8.r2) %>% full_join(prcp7.r2) %>%
  full_join(prcp6.r2) %>% full_join(prcp5.r2) %>% full_join(prcp4.r2) %>%
  full_join(prcp3.r2) %>% full_join(prcp2.r2) %>% full_join(prcp1.r2)

prcp.modelSelect <- prcp.aicc %>% full_join(prcp.rmse) %>%
  full_join(prcp.r2) %>% dplyr::select(-df)

rmse.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = RMSE)) + 
  scale_y_continuous(limits = c(0.6375, 0.6425), breaks = seq(0.6375, 0.6425, 0.00125)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Model RMSE") +
  theme_bw()

r2m.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2m)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  scale_y_continuous(limits = c(0.010, 0.030), breaks = seq(0.01, 0.03, 0.005)) +
  labs(x = "Day prior to measurement", y = expression("Marginal R"^2)) +
  theme_bw()

r2c.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2c)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  scale_y_continuous(limits = c(0.54, 0.55), breaks = seq(0.54, 0.55, 0.0025)) +
  labs(x = "Day prior to measurement", y = expression("Conditional R"^2)) +
  theme_bw()

aicc.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = AICc)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  scale_y_continuous(limits = c(1206, 1216), breaks = seq(1206, 1216, 2.5)) +
  labs(x = NULL, y = expression("AIC"[c])) +
  theme_bw()


ggpubr::ggarrange(aicc.prcp.plot, rmse.prcp.plot,
                  r2m.prcp.plot, r2c.prcp.plot,
                  ncol = 2, nrow = 2, align = "hv")

###############################################################################
# Iterative models for Tmean and log(narea)
###############################################################################
temp30 <- lmer(log(n.leaf) ~ tavg30 + (1 | NCRS.code), data = df)
temp29 <- lmer(log(n.leaf) ~ tavg29 + (1 | NCRS.code), data = df)
temp28 <- lmer(log(n.leaf) ~ tavg28 + (1 | NCRS.code), data = df)
temp27 <- lmer(log(n.leaf) ~ tavg27 + (1 | NCRS.code), data = df)
temp26 <- lmer(log(n.leaf) ~ tavg26 + (1 | NCRS.code), data = df)
temp25 <- lmer(log(n.leaf) ~ tavg25 + (1 | NCRS.code), data = df)
temp24 <- lmer(log(n.leaf) ~ tavg24 + (1 | NCRS.code), data = df)
temp23 <- lmer(log(n.leaf) ~ tavg23 + (1 | NCRS.code), data = df)
temp22 <- lmer(log(n.leaf) ~ tavg22 + (1 | NCRS.code), data = df)
temp21 <- lmer(log(n.leaf) ~ tavg21 + (1 | NCRS.code), data = df)
temp20 <- lmer(log(n.leaf) ~ tavg20 + (1 | NCRS.code), data = df)
temp19 <- lmer(log(n.leaf) ~ tavg19 + (1 | NCRS.code), data = df)
temp18 <- lmer(log(n.leaf) ~ tavg18 + (1 | NCRS.code), data = df)
temp17 <- lmer(log(n.leaf) ~ tavg17 + (1 | NCRS.code), data = df)
temp16 <- lmer(log(n.leaf) ~ tavg16 + (1 | NCRS.code), data = df)
temp15 <- lmer(log(n.leaf) ~ tavg15 + (1 | NCRS.code), data = df)
temp14 <- lmer(log(n.leaf) ~ tavg14 + (1 | NCRS.code), data = df)
temp13 <- lmer(log(n.leaf) ~ tavg13 + (1 | NCRS.code), data = df)
temp12 <- lmer(log(n.leaf) ~ tavg12 + (1 | NCRS.code), data = df)
temp11 <- lmer(log(n.leaf) ~ tavg11 + (1 | NCRS.code), data = df)
temp10 <- lmer(log(n.leaf) ~ tavg10 + (1 | NCRS.code), data = df)
temp9 <- lmer(log(n.leaf) ~ tavg9 + (1 | NCRS.code), data = df)
temp8 <- lmer(log(n.leaf) ~ tavg8 + (1 | NCRS.code), data = df)
temp7 <- lmer(log(n.leaf) ~ tavg7 + (1 | NCRS.code), data = df)
temp6 <- lmer(log(n.leaf) ~ tavg6 + (1 | NCRS.code), data = df)
temp5 <- lmer(log(n.leaf) ~ tavg5 + (1 | NCRS.code), data = df)
temp4 <- lmer(log(n.leaf) ~ tavg4 + (1 | NCRS.code), data = df)
temp3 <- lmer(log(n.leaf) ~ tavg3 + (1 | NCRS.code), data = df)
temp2 <- lmer(log(n.leaf) ~ tavg2 + (1 | NCRS.code), data = df)
temp1 <- lmer(log(n.leaf) ~ tavg1 + (1 | NCRS.code), data = df)


temp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(temp30, temp29, temp28, temp27, temp26, temp25, temp24, temp23, 
                             temp22, temp21, temp20, temp19, temp18, temp17, temp16, temp15, 
                             temp14, temp13, temp12, temp11, temp10, temp9, temp8, temp7, 
                             temp6, temp5, temp4, temp3, temp2, temp1))
head(temp.aicc)
## 1-day temperature is most explanatory temperature scale


###############################################################################
# Temperature RMSE for log(nmass)
###############################################################################
temp30.rmse <- data.frame(day = 30, RMSE = rmse(temp30))
temp29.rmse <- data.frame(day = 29, RMSE = rmse(temp29))
temp28.rmse <- data.frame(day = 28, RMSE = rmse(temp28))
temp27.rmse <- data.frame(day = 27, RMSE = rmse(temp27))
temp26.rmse <- data.frame(day = 26, RMSE = rmse(temp26))
temp25.rmse <- data.frame(day = 25, RMSE = rmse(temp25))
temp24.rmse <- data.frame(day = 24, RMSE = rmse(temp24))
temp23.rmse <- data.frame(day = 23, RMSE = rmse(temp23))
temp22.rmse <- data.frame(day = 22, RMSE = rmse(temp22))
temp21.rmse <- data.frame(day = 21, RMSE = rmse(temp21))
temp20.rmse <- data.frame(day = 20, RMSE = rmse(temp20))
temp19.rmse <- data.frame(day = 19, RMSE = rmse(temp19))
temp18.rmse <- data.frame(day = 18, RMSE = rmse(temp18))
temp17.rmse <- data.frame(day = 17, RMSE = rmse(temp17))
temp16.rmse <- data.frame(day = 16, RMSE = rmse(temp16))
temp15.rmse <- data.frame(day = 15, RMSE = rmse(temp15))
temp14.rmse <- data.frame(day = 14, RMSE = rmse(temp14))
temp13.rmse <- data.frame(day = 13, RMSE = rmse(temp13))
temp12.rmse <- data.frame(day = 12, RMSE = rmse(temp12))
temp11.rmse <- data.frame(day = 11, RMSE = rmse(temp11))
temp10.rmse <- data.frame(day = 10, RMSE = rmse(temp10))
temp9.rmse <- data.frame(day = 9, RMSE = rmse(temp9))
temp8.rmse <- data.frame(day = 8, RMSE = rmse(temp8))
temp7.rmse <- data.frame(day = 7, RMSE = rmse(temp7))
temp6.rmse <- data.frame(day = 6, RMSE = rmse(temp6))
temp5.rmse <- data.frame(day = 5, RMSE = rmse(temp5))
temp4.rmse <- data.frame(day = 4, RMSE = rmse(temp4))
temp3.rmse <- data.frame(day = 3, RMSE = rmse(temp3))
temp2.rmse <- data.frame(day = 2, RMSE = rmse(temp2))
temp1.rmse <- data.frame(day = 1, RMSE = rmse(temp1))


temp.rmse <- temp30.rmse %>% full_join(temp29.rmse) %>% full_join(temp28.rmse) %>% 
  full_join(temp27.rmse) %>% full_join(temp26.rmse) %>% full_join(temp25.rmse) %>%
  full_join(temp24.rmse) %>% full_join(temp23.rmse) %>% full_join(temp22.rmse) %>%
  full_join(temp21.rmse) %>% full_join(temp20.rmse) %>% full_join(temp19.rmse) %>%
  full_join(temp18.rmse) %>% full_join(temp17.rmse) %>% full_join(temp16.rmse) %>%
  full_join(temp15.rmse) %>% full_join(temp14.rmse) %>% full_join(temp13.rmse) %>%
  full_join(temp12.rmse) %>% full_join(temp11.rmse) %>% full_join(temp10.rmse) %>%
  full_join(temp9.rmse) %>% full_join(temp8.rmse) %>% full_join(temp7.rmse) %>%
  full_join(temp6.rmse) %>% full_join(temp5.rmse) %>% full_join(temp4.rmse) %>%
  full_join(temp3.rmse) %>% full_join(temp2.rmse) %>% full_join(temp1.rmse) 

###############################################################################
# Temperature R^2 for log(narea)
###############################################################################
temp30.r2 <- data.frame(day = 30, r.squaredGLMM(temp30))
temp29.r2 <- data.frame(day = 29, r.squaredGLMM(temp29))
temp28.r2 <- data.frame(day = 28, r.squaredGLMM(temp28))
temp27.r2 <- data.frame(day = 27, r.squaredGLMM(temp27))
temp26.r2 <- data.frame(day = 26, r.squaredGLMM(temp26))
temp25.r2 <- data.frame(day = 25, r.squaredGLMM(temp25))
temp24.r2 <- data.frame(day = 24, r.squaredGLMM(temp24))
temp23.r2 <- data.frame(day = 23, r.squaredGLMM(temp23))
temp22.r2 <- data.frame(day = 22, r.squaredGLMM(temp22))
temp21.r2 <- data.frame(day = 21, r.squaredGLMM(temp21))
temp20.r2 <- data.frame(day = 20, r.squaredGLMM(temp20))
temp19.r2 <- data.frame(day = 19, r.squaredGLMM(temp19))
temp18.r2 <- data.frame(day = 18, r.squaredGLMM(temp18))
temp17.r2 <- data.frame(day = 17, r.squaredGLMM(temp17))
temp16.r2 <- data.frame(day = 16, r.squaredGLMM(temp16))
temp15.r2 <- data.frame(day = 15, r.squaredGLMM(temp15))
temp14.r2 <- data.frame(day = 14, r.squaredGLMM(temp14))
temp13.r2 <- data.frame(day = 13, r.squaredGLMM(temp13))
temp12.r2 <- data.frame(day = 12, r.squaredGLMM(temp12))
temp11.r2 <- data.frame(day = 11, r.squaredGLMM(temp11))
temp10.r2 <- data.frame(day = 10, r.squaredGLMM(temp10))
temp9.r2 <- data.frame(day = 9, r.squaredGLMM(temp9))
temp8.r2 <- data.frame(day = 8, r.squaredGLMM(temp8))
temp7.r2 <- data.frame(day = 7, r.squaredGLMM(temp7))
temp6.r2 <- data.frame(day = 6, r.squaredGLMM(temp6))
temp5.r2 <- data.frame(day = 5, r.squaredGLMM(temp5))
temp4.r2 <- data.frame(day = 4, r.squaredGLMM(temp4))
temp3.r2 <- data.frame(day = 3, r.squaredGLMM(temp3))
temp2.r2 <- data.frame(day = 2, r.squaredGLMM(temp2))
temp1.r2 <- data.frame(day = 1, r.squaredGLMM(temp1))

temp.r2 <- temp30.r2 %>% full_join(temp29.r2) %>% full_join(temp28.r2) %>% 
  full_join(temp27.r2) %>% full_join(temp26.r2) %>% full_join(temp25.r2) %>%
  full_join(temp24.r2) %>% full_join(temp23.r2) %>% full_join(temp22.r2) %>%
  full_join(temp21.r2) %>% full_join(temp20.r2) %>% full_join(temp19.r2) %>%
  full_join(temp18.r2) %>% full_join(temp17.r2) %>% full_join(temp16.r2) %>%
  full_join(temp15.r2) %>% full_join(temp14.r2) %>% full_join(temp13.r2) %>%
  full_join(temp12.r2) %>% full_join(temp11.r2) %>% full_join(temp10.r2) %>%
  full_join(temp9.r2) %>% full_join(temp8.r2) %>% full_join(temp7.r2) %>%
  full_join(temp6.r2) %>% full_join(temp5.r2) %>% full_join(temp4.r2) %>%
  full_join(temp3.r2) %>% full_join(temp2.r2) %>% full_join(temp1.r2)

temp.modelSelect <- temp.aicc %>% full_join(temp.rmse) %>%
  full_join(temp.r2) %>% dplyr::select(-df)

rmse.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = RMSE)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Model RMSE") +
  theme_bw()

r2m.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2m)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Marginal R"^2)) +
  theme_bw()

r2c.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2c)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Conditional R"^2)) +
  theme_bw()

aicc.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = AICc)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = expression("AIC"[c])) +
  theme_bw()

ggpubr::ggarrange(aicc.temp.plot, rmse.temp.plot,
                  r2m.temp.plot, r2c.temp.plot,
                  ncol = 2, nrow = 2, align = "hv")

###############################################################################
# Precipitation AICc for log(nmass)
###############################################################################
prcp30 <- lmer(sla ~ prcp30 + (1 | NCRS.code), data = df) 
prcp29 <- lmer(sla ~ prcp29 + (1 | NCRS.code), data = df)
prcp28 <- lmer(sla ~ prcp28 + (1 | NCRS.code), data = df)
prcp27 <- lmer(sla ~ prcp27 + (1 | NCRS.code), data = df)
prcp26 <- lmer(sla ~ prcp26 + (1 | NCRS.code), data = df)
prcp25 <- lmer(sla ~ prcp25 + (1 | NCRS.code), data = df)
prcp24 <- lmer(sla ~ prcp24 + (1 | NCRS.code), data = df)
prcp23 <- lmer(sla ~ prcp23 + (1 | NCRS.code), data = df)
prcp22 <- lmer(sla ~ prcp22 + (1 | NCRS.code), data = df)
prcp21 <- lmer(sla ~ prcp21 + (1 | NCRS.code), data = df)
prcp20 <- lmer(sla ~ prcp20 + (1 | NCRS.code), data = df)
prcp19 <- lmer(sla ~ prcp19 + (1 | NCRS.code), data = df)
prcp18 <- lmer(sla ~ prcp18 + (1 | NCRS.code), data = df)
prcp17 <- lmer(sla ~ prcp17 + (1 | NCRS.code), data = df)
prcp16 <- lmer(sla ~ prcp16 + (1 | NCRS.code), data = df)
prcp15 <- lmer(sla ~ prcp15 + (1 | NCRS.code), data = df)
prcp14 <- lmer(sla ~ prcp14 + (1 | NCRS.code), data = df)
prcp13 <- lmer(sla ~ prcp13 + (1 | NCRS.code), data = df)
prcp12 <- lmer(sla ~ prcp12 + (1 | NCRS.code), data = df)
prcp11 <- lmer(sla ~ prcp11 + (1 | NCRS.code), data = df)
prcp10 <- lmer(sla ~ prcp10 + (1 | NCRS.code), data = df)
prcp9 <- lmer(sla ~ prcp9 + (1 | NCRS.code), data = df)
prcp8 <- lmer(sla ~ prcp8 + (1 | NCRS.code), data = df)
prcp7 <- lmer(sla ~ prcp7 + (1 | NCRS.code), data = df)
prcp6 <- lmer(sla ~ prcp6 + (1 | NCRS.code), data = df)
prcp5 <- lmer(sla ~ prcp5 + (1 | NCRS.code), data = df)
prcp4 <- lmer(sla ~ prcp4 + (1 | NCRS.code), data = df)
prcp3 <- lmer(sla ~ prcp3 + (1 | NCRS.code), data = df)
prcp2 <- lmer(sla ~ prcp2 + (1 | NCRS.code), data = df)
prcp1 <- lmer(sla ~ prcp1 + (1 | NCRS.code), data = df)


prcp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(prcp30, prcp29, prcp28, prcp27, prcp26, 
                             prcp25, prcp24, prcp23, prcp22, prcp21, 
                             prcp20, prcp19, prcp18, prcp17, prcp16, 
                             prcp15, prcp14, prcp13, prcp12, prcp11, 
                             prcp10,prcp9, prcp8, prcp7, prcp6, prcp5, 
                             prcp4, prcp3, prcp2, prcp1)) %>%
  arrange(AICc)

# 1-day precipitation is most explanatory timescale for Narea

###############################################################################
# Precipitation RMSE for log(narea)
###############################################################################
prcp30.rmse <- data.frame(day = 30, RMSE = rmse(prcp30))
prcp29.rmse <- data.frame(day = 29, RMSE = rmse(prcp29))
prcp28.rmse <- data.frame(day = 28, RMSE = rmse(prcp28))
prcp27.rmse <- data.frame(day = 27, RMSE = rmse(prcp27))
prcp26.rmse <- data.frame(day = 26, RMSE = rmse(prcp26))
prcp25.rmse <- data.frame(day = 25, RMSE = rmse(prcp25))
prcp24.rmse <- data.frame(day = 24, RMSE = rmse(prcp24))
prcp23.rmse <- data.frame(day = 23, RMSE = rmse(prcp23))
prcp22.rmse <- data.frame(day = 22, RMSE = rmse(prcp22))
prcp21.rmse <- data.frame(day = 21, RMSE = rmse(prcp21))
prcp20.rmse <- data.frame(day = 20, RMSE = rmse(prcp20))
prcp19.rmse <- data.frame(day = 19, RMSE = rmse(prcp19))
prcp18.rmse <- data.frame(day = 18, RMSE = rmse(prcp18))
prcp17.rmse <- data.frame(day = 17, RMSE = rmse(prcp17))
prcp16.rmse <- data.frame(day = 16, RMSE = rmse(prcp16))
prcp15.rmse <- data.frame(day = 15, RMSE = rmse(prcp15))
prcp14.rmse <- data.frame(day = 14, RMSE = rmse(prcp14))
prcp13.rmse <- data.frame(day = 13, RMSE = rmse(prcp13))
prcp12.rmse <- data.frame(day = 12, RMSE = rmse(prcp12))
prcp11.rmse <- data.frame(day = 11, RMSE = rmse(prcp11))
prcp10.rmse <- data.frame(day = 10, RMSE = rmse(prcp10))
prcp9.rmse <- data.frame(day = 9, RMSE = rmse(prcp9))
prcp8.rmse <- data.frame(day = 8, RMSE = rmse(prcp8))
prcp7.rmse <- data.frame(day = 7, RMSE = rmse(prcp7))
prcp6.rmse <- data.frame(day = 6, RMSE = rmse(prcp6))
prcp5.rmse <- data.frame(day = 5, RMSE = rmse(prcp5))
prcp4.rmse <- data.frame(day = 4, RMSE = rmse(prcp4))
prcp3.rmse <- data.frame(day = 3, RMSE = rmse(prcp3))
prcp2.rmse <- data.frame(day = 2, RMSE = rmse(prcp2))
prcp1.rmse <- data.frame(day = 1, RMSE = rmse(prcp1))


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
# Precipitation R^2 for log(narea)
###############################################################################
prcp30.r2 <- data.frame(day = 30, r.squaredGLMM(prcp30))
prcp29.r2 <- data.frame(day = 29, r.squaredGLMM(prcp29))
prcp28.r2 <- data.frame(day = 28, r.squaredGLMM(prcp28))
prcp27.r2 <- data.frame(day = 27, r.squaredGLMM(prcp27))
prcp26.r2 <- data.frame(day = 26, r.squaredGLMM(prcp26))
prcp25.r2 <- data.frame(day = 25, r.squaredGLMM(prcp25))
prcp24.r2 <- data.frame(day = 24, r.squaredGLMM(prcp24))
prcp23.r2 <- data.frame(day = 23, r.squaredGLMM(prcp23))
prcp22.r2 <- data.frame(day = 22, r.squaredGLMM(prcp22))
prcp21.r2 <- data.frame(day = 21, r.squaredGLMM(prcp21))
prcp20.r2 <- data.frame(day = 20, r.squaredGLMM(prcp20))
prcp19.r2 <- data.frame(day = 19, r.squaredGLMM(prcp19))
prcp18.r2 <- data.frame(day = 18, r.squaredGLMM(prcp18))
prcp17.r2 <- data.frame(day = 17, r.squaredGLMM(prcp17))
prcp16.r2 <- data.frame(day = 16, r.squaredGLMM(prcp16))
prcp15.r2 <- data.frame(day = 15, r.squaredGLMM(prcp15))
prcp14.r2 <- data.frame(day = 14, r.squaredGLMM(prcp14))
prcp13.r2 <- data.frame(day = 13, r.squaredGLMM(prcp13))
prcp12.r2 <- data.frame(day = 12, r.squaredGLMM(prcp12))
prcp11.r2 <- data.frame(day = 11, r.squaredGLMM(prcp11))
prcp10.r2 <- data.frame(day = 10, r.squaredGLMM(prcp10))
prcp9.r2 <- data.frame(day = 9, r.squaredGLMM(prcp9))
prcp8.r2 <- data.frame(day = 8, r.squaredGLMM(prcp8))
prcp7.r2 <- data.frame(day = 7, r.squaredGLMM(prcp7))
prcp6.r2 <- data.frame(day = 6, r.squaredGLMM(prcp6))
prcp5.r2 <- data.frame(day = 5, r.squaredGLMM(prcp5))
prcp4.r2 <- data.frame(day = 4, r.squaredGLMM(prcp4))
prcp3.r2 <- data.frame(day = 3, r.squaredGLMM(prcp3))
prcp2.r2 <- data.frame(day = 2, r.squaredGLMM(prcp2))
prcp1.r2 <- data.frame(day = 1, r.squaredGLMM(prcp1))

prcp.r2 <- prcp30.r2 %>% full_join(prcp29.r2) %>% full_join(prcp28.r2) %>% 
  full_join(prcp27.r2) %>% full_join(prcp26.r2) %>% full_join(prcp25.r2) %>%
  full_join(prcp24.r2) %>% full_join(prcp23.r2) %>% full_join(prcp22.r2) %>%
  full_join(prcp21.r2) %>% full_join(prcp20.r2) %>% full_join(prcp19.r2) %>%
  full_join(prcp18.r2) %>% full_join(prcp17.r2) %>% full_join(prcp16.r2) %>%
  full_join(prcp15.r2) %>% full_join(prcp14.r2) %>% full_join(prcp13.r2) %>%
  full_join(prcp12.r2) %>% full_join(prcp11.r2) %>% full_join(prcp10.r2) %>%
  full_join(prcp9.r2) %>% full_join(prcp8.r2) %>% full_join(prcp7.r2) %>%
  full_join(prcp6.r2) %>% full_join(prcp5.r2) %>% full_join(prcp4.r2) %>%
  full_join(prcp3.r2) %>% full_join(prcp2.r2) %>% full_join(prcp1.r2)

prcp.modelSelect <- prcp.aicc %>% full_join(prcp.rmse) %>%
  full_join(prcp.r2) %>% dplyr::select(-df)

rmse.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = RMSE)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Model RMSE") +
  theme_bw()

r2m.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2m)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Marginal R"^2)) +
  theme_bw()

r2c.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2c)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Conditional R"^2)) +
  theme_bw()

aicc.prcp.plot <- ggplot(data = prcp.modelSelect, aes(x = day)) +
  geom_line(aes(y = AICc)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = expression("AIC"[c])) +
  theme_bw()


ggpubr::ggarrange(aicc.prcp.plot, rmse.prcp.plot,
                  r2m.prcp.plot, r2c.prcp.plot,
                  ncol = 2, nrow = 2, align = "hv")

###############################################################################
# Iterative models for Tmean and log(narea)
###############################################################################
temp30 <- lmer(sla ~ tavg30 + (1 | NCRS.code), data = df)
temp29 <- lmer(sla ~ tavg29 + (1 | NCRS.code), data = df)
temp28 <- lmer(sla ~ tavg28 + (1 | NCRS.code), data = df)
temp27 <- lmer(sla ~ tavg27 + (1 | NCRS.code), data = df)
temp26 <- lmer(sla ~ tavg26 + (1 | NCRS.code), data = df)
temp25 <- lmer(sla ~ tavg25 + (1 | NCRS.code), data = df)
temp24 <- lmer(sla ~ tavg24 + (1 | NCRS.code), data = df)
temp23 <- lmer(sla ~ tavg23 + (1 | NCRS.code), data = df)
temp22 <- lmer(sla ~ tavg22 + (1 | NCRS.code), data = df)
temp21 <- lmer(sla ~ tavg21 + (1 | NCRS.code), data = df)
temp20 <- lmer(sla ~ tavg20 + (1 | NCRS.code), data = df)
temp19 <- lmer(sla ~ tavg19 + (1 | NCRS.code), data = df)
temp18 <- lmer(sla ~ tavg18 + (1 | NCRS.code), data = df)
temp17 <- lmer(sla ~ tavg17 + (1 | NCRS.code), data = df)
temp16 <- lmer(sla ~ tavg16 + (1 | NCRS.code), data = df)
temp15 <- lmer(sla ~ tavg15 + (1 | NCRS.code), data = df)
temp14 <- lmer(sla ~ tavg14 + (1 | NCRS.code), data = df)
temp13 <- lmer(sla ~ tavg13 + (1 | NCRS.code), data = df)
temp12 <- lmer(sla ~ tavg12 + (1 | NCRS.code), data = df)
temp11 <- lmer(sla ~ tavg11 + (1 | NCRS.code), data = df)
temp10 <- lmer(sla ~ tavg10 + (1 | NCRS.code), data = df)
temp9 <- lmer(sla ~ tavg9 + (1 | NCRS.code), data = df)
temp8 <- lmer(sla ~ tavg8 + (1 | NCRS.code), data = df)
temp7 <- lmer(sla ~ tavg7 + (1 | NCRS.code), data = df)
temp6 <- lmer(sla ~ tavg6 + (1 | NCRS.code), data = df)
temp5 <- lmer(sla ~ tavg5 + (1 | NCRS.code), data = df)
temp4 <- lmer(sla ~ tavg4 + (1 | NCRS.code), data = df)
temp3 <- lmer(sla ~ tavg3 + (1 | NCRS.code), data = df)
temp2 <- lmer(sla ~ tavg2 + (1 | NCRS.code), data = df)
temp1 <- lmer(sla ~ tavg1 + (1 | NCRS.code), data = df)


temp.aicc <- data.frame(day = seq(30, 1, -1),
                        AICc(temp30, temp29, temp28, temp27, temp26, temp25, temp24, temp23, 
                             temp22, temp21, temp20, temp19, temp18, temp17, temp16, temp15, 
                             temp14, temp13, temp12, temp11, temp10, temp9, temp8, temp7, 
                             temp6, temp5, temp4, temp3, temp2, temp1))
head(temp.aicc)
## 1-day temperature is most explanatory temperature scale


###############################################################################
# Temperature RMSE for log(nmass)
###############################################################################
temp30.rmse <- data.frame(day = 30, RMSE = rmse(temp30))
temp29.rmse <- data.frame(day = 29, RMSE = rmse(temp29))
temp28.rmse <- data.frame(day = 28, RMSE = rmse(temp28))
temp27.rmse <- data.frame(day = 27, RMSE = rmse(temp27))
temp26.rmse <- data.frame(day = 26, RMSE = rmse(temp26))
temp25.rmse <- data.frame(day = 25, RMSE = rmse(temp25))
temp24.rmse <- data.frame(day = 24, RMSE = rmse(temp24))
temp23.rmse <- data.frame(day = 23, RMSE = rmse(temp23))
temp22.rmse <- data.frame(day = 22, RMSE = rmse(temp22))
temp21.rmse <- data.frame(day = 21, RMSE = rmse(temp21))
temp20.rmse <- data.frame(day = 20, RMSE = rmse(temp20))
temp19.rmse <- data.frame(day = 19, RMSE = rmse(temp19))
temp18.rmse <- data.frame(day = 18, RMSE = rmse(temp18))
temp17.rmse <- data.frame(day = 17, RMSE = rmse(temp17))
temp16.rmse <- data.frame(day = 16, RMSE = rmse(temp16))
temp15.rmse <- data.frame(day = 15, RMSE = rmse(temp15))
temp14.rmse <- data.frame(day = 14, RMSE = rmse(temp14))
temp13.rmse <- data.frame(day = 13, RMSE = rmse(temp13))
temp12.rmse <- data.frame(day = 12, RMSE = rmse(temp12))
temp11.rmse <- data.frame(day = 11, RMSE = rmse(temp11))
temp10.rmse <- data.frame(day = 10, RMSE = rmse(temp10))
temp9.rmse <- data.frame(day = 9, RMSE = rmse(temp9))
temp8.rmse <- data.frame(day = 8, RMSE = rmse(temp8))
temp7.rmse <- data.frame(day = 7, RMSE = rmse(temp7))
temp6.rmse <- data.frame(day = 6, RMSE = rmse(temp6))
temp5.rmse <- data.frame(day = 5, RMSE = rmse(temp5))
temp4.rmse <- data.frame(day = 4, RMSE = rmse(temp4))
temp3.rmse <- data.frame(day = 3, RMSE = rmse(temp3))
temp2.rmse <- data.frame(day = 2, RMSE = rmse(temp2))
temp1.rmse <- data.frame(day = 1, RMSE = rmse(temp1))


temp.rmse <- temp30.rmse %>% full_join(temp29.rmse) %>% full_join(temp28.rmse) %>% 
  full_join(temp27.rmse) %>% full_join(temp26.rmse) %>% full_join(temp25.rmse) %>%
  full_join(temp24.rmse) %>% full_join(temp23.rmse) %>% full_join(temp22.rmse) %>%
  full_join(temp21.rmse) %>% full_join(temp20.rmse) %>% full_join(temp19.rmse) %>%
  full_join(temp18.rmse) %>% full_join(temp17.rmse) %>% full_join(temp16.rmse) %>%
  full_join(temp15.rmse) %>% full_join(temp14.rmse) %>% full_join(temp13.rmse) %>%
  full_join(temp12.rmse) %>% full_join(temp11.rmse) %>% full_join(temp10.rmse) %>%
  full_join(temp9.rmse) %>% full_join(temp8.rmse) %>% full_join(temp7.rmse) %>%
  full_join(temp6.rmse) %>% full_join(temp5.rmse) %>% full_join(temp4.rmse) %>%
  full_join(temp3.rmse) %>% full_join(temp2.rmse) %>% full_join(temp1.rmse) 

###############################################################################
# Temperature R^2 for log(narea)
###############################################################################
temp30.r2 <- data.frame(day = 30, r.squaredGLMM(temp30))
temp29.r2 <- data.frame(day = 29, r.squaredGLMM(temp29))
temp28.r2 <- data.frame(day = 28, r.squaredGLMM(temp28))
temp27.r2 <- data.frame(day = 27, r.squaredGLMM(temp27))
temp26.r2 <- data.frame(day = 26, r.squaredGLMM(temp26))
temp25.r2 <- data.frame(day = 25, r.squaredGLMM(temp25))
temp24.r2 <- data.frame(day = 24, r.squaredGLMM(temp24))
temp23.r2 <- data.frame(day = 23, r.squaredGLMM(temp23))
temp22.r2 <- data.frame(day = 22, r.squaredGLMM(temp22))
temp21.r2 <- data.frame(day = 21, r.squaredGLMM(temp21))
temp20.r2 <- data.frame(day = 20, r.squaredGLMM(temp20))
temp19.r2 <- data.frame(day = 19, r.squaredGLMM(temp19))
temp18.r2 <- data.frame(day = 18, r.squaredGLMM(temp18))
temp17.r2 <- data.frame(day = 17, r.squaredGLMM(temp17))
temp16.r2 <- data.frame(day = 16, r.squaredGLMM(temp16))
temp15.r2 <- data.frame(day = 15, r.squaredGLMM(temp15))
temp14.r2 <- data.frame(day = 14, r.squaredGLMM(temp14))
temp13.r2 <- data.frame(day = 13, r.squaredGLMM(temp13))
temp12.r2 <- data.frame(day = 12, r.squaredGLMM(temp12))
temp11.r2 <- data.frame(day = 11, r.squaredGLMM(temp11))
temp10.r2 <- data.frame(day = 10, r.squaredGLMM(temp10))
temp9.r2 <- data.frame(day = 9, r.squaredGLMM(temp9))
temp8.r2 <- data.frame(day = 8, r.squaredGLMM(temp8))
temp7.r2 <- data.frame(day = 7, r.squaredGLMM(temp7))
temp6.r2 <- data.frame(day = 6, r.squaredGLMM(temp6))
temp5.r2 <- data.frame(day = 5, r.squaredGLMM(temp5))
temp4.r2 <- data.frame(day = 4, r.squaredGLMM(temp4))
temp3.r2 <- data.frame(day = 3, r.squaredGLMM(temp3))
temp2.r2 <- data.frame(day = 2, r.squaredGLMM(temp2))
temp1.r2 <- data.frame(day = 1, r.squaredGLMM(temp1))

temp.r2 <- temp30.r2 %>% full_join(temp29.r2) %>% full_join(temp28.r2) %>% 
  full_join(temp27.r2) %>% full_join(temp26.r2) %>% full_join(temp25.r2) %>%
  full_join(temp24.r2) %>% full_join(temp23.r2) %>% full_join(temp22.r2) %>%
  full_join(temp21.r2) %>% full_join(temp20.r2) %>% full_join(temp19.r2) %>%
  full_join(temp18.r2) %>% full_join(temp17.r2) %>% full_join(temp16.r2) %>%
  full_join(temp15.r2) %>% full_join(temp14.r2) %>% full_join(temp13.r2) %>%
  full_join(temp12.r2) %>% full_join(temp11.r2) %>% full_join(temp10.r2) %>%
  full_join(temp9.r2) %>% full_join(temp8.r2) %>% full_join(temp7.r2) %>%
  full_join(temp6.r2) %>% full_join(temp5.r2) %>% full_join(temp4.r2) %>%
  full_join(temp3.r2) %>% full_join(temp2.r2) %>% full_join(temp1.r2)

temp.modelSelect <- temp.aicc %>% full_join(temp.rmse) %>%
  full_join(temp.r2) %>% dplyr::select(-df)

rmse.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = RMSE)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Model RMSE") +
  theme_bw()

r2m.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2m)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Marginal R"^2)) +
  theme_bw()

r2c.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = R2c)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = "Day prior to measurement", y = expression("Conditional R"^2)) +
  theme_bw()

aicc.temp.plot <- ggplot(data = temp.modelSelect, aes(x = day)) +
  geom_line(aes(y = AICc)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = expression("AIC"[c])) +
  theme_bw()

ggpubr::ggarrange(aicc.temp.plot, rmse.temp.plot,
                  r2m.temp.plot, r2c.temp.plot,
                  ncol = 2, nrow = 2, align = "hv")





