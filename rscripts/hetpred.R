library(mosaic)
library(quantreg)

# Read in using Import Dataset, or use this:
afc = read.csv('../data/afc.csv')

# Plot the data and fit the model
plot(Price ~ FoodScore, data = afc)
lm1 = lm(Price ~ FoodScore, data = afc)
abline(lm1)

# Form "postdiction" intervals, i.e. prediction interval on the original data
postdiction = predict(lm1, newdata=afc, interval = 'prediction', level = 0.8)

# Now let's add the prediction intervals to the plot:
plot(Price ~ FoodScore, data = afc, ylim=c(0, 130))
points(postdiction[,2] ~ FoodScore, data=afc, col='red', pch=19) # lower bound
points(postdiction[,3] ~ FoodScore, data=afc, col='red', pch=19) # upper bound

# These look really ugly!
# Too wide for low food scores.
# Too narrow for high food scores.

# The problem: heteroskedasticity, i.e. nonconstant variance
# Notice the "fan" shape in the plot of residuals vs fitted values
# Our basic prediction intervals will not work if the residuals look like this
plot(resid(lm1) ~ FoodScore, data=afc)
plot(resid(lm1) ~ fitted(lm1), data=afc)


##
# A simple fix: prediction intervals using quantile regression
##

# Fit quantile regression models for the 10th and 90th percentile
# Need the quantreg package
# Must fit separate models for lower and upper bounds
q10 = rq(Price ~ FoodScore, tau = 0.1, data= afc)
q90 = rq(Price ~ FoodScore, tau = 0.9, data= afc)

# Plot the fitted 10th and 90th percentile
# Now our prediction intervals capture the fan.
# Beautiful!
plot(Price ~ FoodScore, data = afc)
points(fitted(q10) ~ FoodScore, data=afc, col='blue', pch=19) # lower bound
points(fitted(q90) ~ FoodScore, data=afc, col='blue', pch=19) # upper bound

# The moral of the story:
# if you see heteroskedasticity and need a prediction interval,
# use quantile regression instead.

# Could also predict on newdata
my_newdata = data.frame(FoodScore = c(4.0, 7.5))
predict(q10, newdata= my_newdata)
predict(q90, newdata= my_newdata)

# 80% prediction interval for 4.0 restaurant is (10.38, 42.59)
# 80% prediction interval for 7.5 restaurant is (23.85, 94.44), which is much wider
