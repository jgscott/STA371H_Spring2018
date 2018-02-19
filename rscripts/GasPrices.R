library(mosaic)
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')

GasPrices = read.csv('../data/GasPrices.csv')

summary(GasPrices)

# Use Cents as a Y variable, to make coefficients easier to interpret
GasPrices$Cents = GasPrices$Price*100

# ZIP code?
# Some ZIP codes have only 3-4 stations,
# so won't include ZIP as a predictor due to overfitting concerns
summary(factor(GasPrices$Zipcode))


# Income differences
plot(Cents ~ Income, data=GasPrices)

# Competitors?
bwplot(Cents ~ Competitors, data=GasPrices)

# Highway differences
bwplot(Cents ~ Highway, data=GasPrices)

# Carwash differences
bwplot(Cents ~ CarWash, data=GasPrices)

# Brand differences
bwplot(Cents ~ Brand, data=GasPrices)

# stoplight? maybe
bwplot(Cents ~ Stoplight, data=GasPrices)


bwplot(Cents ~ Restaurant, data=GasPrices)


# Interactions?
xyplot(Cents ~ Income | Highway, data=GasPrices)
xyplot(Cents ~ Income | Competitors, data=GasPrices)

# Note: I didn't notice any more obvious effects or confounders.  Did you?

# Baseline model
lm_base = lm(Cents ~ Competitors + Highway + CarWash + Brand + Income + Stoplight +
	Income:Highway + Income:Competitors, data=GasPrices)

# Most terms look to be pulling at least some weight,
# except the Stoplight and Competitors:Income variables
simple_anova(lm_base)


# Bootstrap to get confidence intervals
boot_base = do(1000)*{
	lm_boot = lm(Cents ~ Competitors + Highway + CarWash + Brand + Income + Stoplight +
		Income:Highway + Income:Competitors, data=resample(GasPrices))
	lm_boot
}


# Confidence intervals on the interaction term:
confint(boot_base)


###
# Answering the questions
###

# A) Gas stations charge more if they lack direct competition in sight, all else being equal.
confint(boot_base)

# Zero (no difference) is a plausible value in the 95% CI
# So no "statistically significant" difference at 5% significance level
# But large negative numbers (e.g. 5-10 cents cheaper per gallon) are also plausible!
hist(boot_base$CompetitorsY, 20)


# B) Gas stations in richer areas charge more, all else being equal.

# Per extra dollar of income
hist(boot_base$Income, 20)

# Per extra $10,000 of income:
# looks like a meaningful effect of 1-2 cents per extra 10K of income,
# adjusting for other variables
hist(boot_base$Income*10000, 20)

# Subtle point: this holds only for gas stations not on highways
# Bonus finding: the income-Price relationship disappears for stations on highways
hist(boot_base$Income*10000, 20)  # "baseline slope" for non-highway stations
hist(boot_base$HighwayY.Income*10000, 20) # interaction term = offset to slope

# Baseline + offset to get slope for highway stations
# Mostly centered around zero!
hist((boot_base$Income + boot_base$HighwayY.Income)*10000, 20)



# C) Gas stations from the major brands charge more, all else being equal.
# Remember, the baseline is ChevronTexaco

# ChevronTexaco is probably a bit more expensive than "Other"
# But again, zero (no effect) is a plausible value
hist(boot_base$BrandOther, 20)

# And the dummy variable coefs for Shell and ExxonMobil
# all have similar offsets from Chevron as "Other" (non-branded) stations
hist(boot_base$BrandExxonMobil, 20)

# Conclusion: no strong evidence for a big effect,
# But Chevron might be higher than the others



# D) If a gas station doesn't have direct competition within sight,
# then it will obviously charge more in richer areas.
# But if it has direct competition, then the correlation between
# Price and income will disappear.

# Solution: look at the interaction term
hist(boot_base$CompetitorsY.Income, 20)

# as before, easier to interpret in units of $10,000 of extra income
# Sampling distribution pretty much centered around zero (no difference)
# with pretty small standard error (~1 cent per gallon per extra 10K)
hist(boot_base$CompetitorsY.Income*10000, 20)


# E) Gas stations at stoplights can charge more, all else being equal.

# Sampling distribution pretty much centered around zero (no difference)
# with pretty small standard error (~2 cent per gallon)
# No evidence for a big positive effect,
# and the effect could even be negative (though small)
hist(boot_base$StoplightY, 20)



# F) Gas stations with direct highway access can charge more, all else being equal.
# This one looks like a slam dunk: adjusting for all other variables,
# Gas stations on highways seem to charge 10-20 cents per gallon more, on average
hist(boot_base$HighwayY, 20)

# This looks like the biggest effect we've found, in practical terms:
# 15-gallon fillup costs $1.50 - $3.00 more on a highway on average, all else equal
# (could be a little more or a little less)





