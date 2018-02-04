library(mosaic)
sclass = read.csv('../data/sclass.csv')

# Reminder of the variables involved
summary(sclass)

s350_2012 = subset(sclass, trim == '350' & year == 2012 & condition == 'Used')

# Look at a specific subset
plot(price ~ mileage, data=s350_2012)

# fit a model and show the line
lm1 = lm(price ~ mileage, data = s350_2012)
abline(lm1)

# pick an arbitrary car
s350_2012[71,]

# price = 54991
hist(s350_2012$price, 30)

resid(lm1)[71]
hist(resid(lm1))
sigma = sd(resid(lm1))

# a prediction interval
plot(price ~ mileage, data=s350_2012)
mybeta = coef(lm1)
abline(lm1)

# can also call abline with an intercept and a slope
# move up/down 1 residual standard deviation
abline(mybeta[1] - sigma, mybeta[2], col='grey')
abline(mybeta[1] + sigma, mybeta[2], col='grey')
abline(mybeta[1] - 2*sigma, mybeta[2], col='red')
abline(mybeta[1] + 2*sigma, mybeta[2], col='red')

