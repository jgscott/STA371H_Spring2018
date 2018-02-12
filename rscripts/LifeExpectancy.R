library(mosaic)
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')

LifeExpectancy = read.csv('../data/LifeExpectancy.csv')

plot(LifeExp ~ PPGDP, data=LifeExpectancy)

# A log transformation of GDP?
hist(LifeExpectancy$PPGDP)

# Still looks nonlinear
plot(LifeExp ~ log(PPGDP), data=LifeExpectancy)

# What about stratifying by group?
xyplot(LifeExp ~ log(PPGDP) | Group, data=LifeExpectancy)

# Use a model with dummy variables
lm1 = lm(LifeExp ~ log(PPGDP) + Group, data=LifeExpectancy)

plotModel(lm1, pch=19)

# With an interaction? Doesn't look necessary
lm2 = lm(LifeExp ~ log(PPGDP) + Group + log(PPGDP):Group, data=LifeExpectancy)
simple_anova(lm2)

# On the transformed scale
plot(LifeExp ~ log(PPGDP), data=LifeExpectancy)
points(fitted(lm1) ~ log(PPGDP), data=LifeExpectancy,
      col='blue', pch=19)

plot(LifeExp ~ log(PPGDP), data=LifeExpectancy)
points(fitted(lm2) ~ log(PPGDP), data=LifeExpectancy,
      col='red', pch=19)

# Prediction interval
new_data = data.frame(PPGDP = 20000, Group ='oecd')
predict(lm1, new_data, interval='prediction', level = 0.95)

## Bootstrapping
boot1 = do(2500)*{
	lm_boot = lm(LifeExp ~ log(PPGDP) + Group, data=resample(LifeExpectancy))
	lm_boot
}

boot2 = do(2500)*{
  lm_boot = lm(LifeExp ~ log(PPGDP) + Group, data=resample(LifeExpectancy))
  lm_boot
}

# First few lines
head(boot1)

# Confidence intervals
confint(boot1)

# standard errors (std deviation of each column
sd_cols(boot1)  # this function is in class_utils.R script from website


# Using the normal linear regression model
summary(lm1)
confint(lm1)
confint(boot1)  # compare
coef_table(lm1)  # this function is in class_utils.R script from website

# Residuals look somewhat non-normal
hist(resid(lm1), 20)

# Code for comparing vs normal dist
hist(resid(lm1), breaks = 20, prob=TRUE)
curve(dnorm(x, 0, sd(resid(lm1))), add=TRUE)

