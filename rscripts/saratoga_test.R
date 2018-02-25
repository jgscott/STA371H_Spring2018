library(mosaic)
data(SaratogaHouses)
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')

# Baseline model
lm0 = lm(price ~ livingArea + lotSize + fireplaces, data=SaratogaHouses)
rsquared(lm0)

# Add a useless variable!
SaratogaHouses$useless = rnorm(1728, 0, 1)

# R squared with a useless variable: gets better
lm_withuseless = lm(price ~ livingArea + lotSize + fireplaces + useless, data=SaratogaHouses)
rsquared(lm_withuseless)

# R squared with a shuffled useless variable: still useless!
lm_shuffleduseless = lm(price ~ livingArea + lotSize + fireplaces + shuffle(useless), data=SaratogaHouses)
rsquared(lm_shuffleduseless)

# Shuffle living area instead
lm_shuffledlivingArea = lm(price ~ shuffle(livingArea) + lotSize + fireplaces + useless, data=SaratogaHouses)
rsquared(lm_shuffledlivingArea)


# Question 1: is there a partial relationship between fuel system type
# and price, adjusting for living area, lotsize, and fireplaces?

lm1 = lm(price ~ livingArea + lotSize + fireplaces + fuel, data=SaratogaHouses)
coef(lm1)
rsquared(lm1)

boot1 = do(1000)*{
  lm(price ~ livingArea + lotSize + fireplaces + fuel, data=resample(SaratogaHouses))
}

confint(boot1)


# one shuffled model
lm_shuff = lm(price ~ livingArea + lotSize + fireplaces + shuffle(fuel), data=SaratogaHouses)
rsquared(lm_shuff)

rsquared(lm1)

head(SaratogaHouses)
head(shuffle(SaratogaHouses$fuel)) 


# permutation test: is the fuel variable significant?
perm1 = do(2000)*{
	lm(price ~ livingArea + lotSize + fireplaces + shuffle(fuel), data=SaratogaHouses)
}
head(perm1)

hist(perm1$r.squared, 20)

# The quantile
qdata(perm1$r.squared, p = 0.95)

# the p-value
sum(perm1$r.squared >= rsquared(lm1))



# Question 2: is there an interaction between fireplaces and fuel system?

lm2 = lm(price ~ livingArea + lotSize + fireplaces + fuel + fireplaces:fuel, data=SaratogaHouses)
coef(lm2)
rsquared(lm2)

perm2 = do(2000)*{
	lm(price ~ livingArea + lotSize + fireplaces + fuel + shuffle(fireplaces):shuffle(fuel), data=SaratogaHouses)
}

hist(perm2$r.squared, 30)
abline(v=rsquared(lm2), col='red')

# p-value
sum(perm2$r.squared >= rsquared(lm2))
sum(perm2$r.squared >= rsquared(lm2))/2000

# Compare with p-values from ANOVA table
simple_anova(lm2)
