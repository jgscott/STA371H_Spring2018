# Read in 10 mile race data
library(mosaic)
library(mosaicData)
data(TenMileRace)

# The model aggregrating men and women
plot(net~age,data=TenMileRace, col='grey')
lm1 = lm(net~age,data=TenMileRace)
abline(lm1)
summary(lm1)

# Now disaggregating
lmM = lm(net~age,data=subset(TenMileRace,sex=="M"))
lmF = lm(net~age,data=subset(TenMileRace,sex=="F"))
coef(lmM)
coef(lmF)

mean(net ~ sex, data=TenMileRace)

# Clearly we get different effects due to age when we disaggregate
plot(net~age,data=TenMileRace, col='grey', pch=19, cex=0.5)
abline(lm1, col='black')
abline(lmM, col='red')
abline(lmF, col='blue')

# We can model this with main effects for age and sex
lm2 = lm(net ~ age + sex, data= TenMileRace)
coef(lm2)

# A simple way to visualize the fit
plotModel(lm2)

# With an interaction
lm3 = lm(net ~ age + sex + age:sex, data= TenMileRace)
coef(lm3)

# Visualize the fit
plotModel(lm3)

# An ANOVA table
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')
simple_anova(lm3)

