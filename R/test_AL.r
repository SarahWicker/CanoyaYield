library(readxl)
data_set <- read_excel("../../cours/R/01_HG_Canola yield data_Shafii.xls",
                       sheet = "full data")
View(data_set)

mean_data_set<-read_excel("../../cours/R/01_HG_Canola yield data_Shafii.xls",
                          sheet = "GxE means")
View(mean_data_set)


library(tidyverse)
dat <- mean_data_set %>%
  select(LOC,YLD_Mean)
summary(dat)


#several things to test, if the rendement is dependant on the year/location(environnement)/genetic
library(ggplot2)
ggplot(dat) +
  aes(x=LOC, y= YLD_Mean, color = LOC) +
  geom_jitter() +
  theme(legend.position = "none")

#test of the normality of the data

#compute the ANOVA
res_aov <- aov(YLD_Mean ~ LOC,
               data = dat
               )
#check normality visualy
par(mfrow = c(1,2)) #combine 2 plots
hist(res_aov$residuals) # histogram
library(car)
qqPlot(res_aov$residuals,
       id = FALSE
       ) # FALSE remove identification point

#test normality also with normality test
shapiro.test(res_aov$residuals) # observe the p value given as result if >5% hypothesis that residuals follow normal distribution is not rejected
#here p-value = 0,00117.... rejected

