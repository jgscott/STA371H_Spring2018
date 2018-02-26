library(mosaic)

# Problem 1
cheese = read.csv('../data/cheese.csv')


# Clearly volume is correlated with displays
boxplot(vol~disp, data=cheese)

# Part A: % increase in sales volume under displays

# One way: bootstrap the means
mu = mean(vol~disp, data=cheese)
mu[2]/mu[1]

# Bootstrap this ratio
boot_mean = do(1000)*{
	muboot = mean(vol~disp, data=resample(cheese))
	muboot[2]/muboot[1]	
}
head(boot_mean)
hist(boot_mean$X1)
confint(boot_mean$X1, level=0.95)
confint(boot_mean$X1)  # 95% by default




# However, this analysis misses something... look store by store
boxplot(log(vol)~disp, data=subset(cheese, store=='BALTI/WASH - SAFEWAY'))
boxplot(log(vol)~disp, data=subset(cheese, store=='SYRACUSE - PRICE CHOPPER'))

# Different stores have very different sales volumes,
# and different patterns of display activity.
# This might be a confounder.
# We can account for this using a model:
#  - put in dummy variables store by store
#  - work on the log scale (since log scale gives
#    us multiplicative change)
lm2 = lm(log(vol)~disp + store, data=cheese)
coef(lm2)


# Bootstrap this model
boot2 = do(1000)*{
	lm(log(vol)~disp + store, data=resample(cheese))
}
confint(boot2)

# Displays now have a lower estimated effect.
# Therefore important to account for store-level variability.
exp(confint(boot2$disp))
# This is the best answer for Part A: 35-43%



# Part B
# Now look at price

# lower prices during display weeks
# this is certainly a confounder: people bought more
# cheese at least partially due to lower prices.
boxplot(price ~ disp, data=cheese)

# Fit a model that accounts for price:
# this means fitting a demand curve, like in the milk data set...
# Except now we have dummies for store and display that shift the curve up/down
lm3 = lm(log(vol)~log(price) + store + disp, data=cheese)

# Notice the large negative coefficient on price: highly elastic
coef(lm3)

# Spot check the fit for a single store
# Specifically looking at Kroger DFW
dfwkroger = subset(cheese, store=='DALLAS/FT. WORTH - KROGER CO')
plot(vol~price, data=dfwkroger, las=1)
points(vol~price, data=subset(dfwkroger, disp==1), col='blue', pch=19)
points(vol~price, data=subset(dfwkroger, disp==0), col='red', pch=19)

# Go back and make the curves from the fitted coefficients
# No displays
curve(exp(9.37579 + 1.43461)*x^(-2.53159), add=TRUE, col='red')
# With displays
curve(exp(9.37579 + 1.43461 + 0.18540)*x^(-2.53159), add=TRUE, col='blue')


# Get a confidence interval for the display coefficient
boot3 = do(1000)*{
	lm(log(vol)~log(price) + store + disp, data=resample(cheese))
}

# Confidence interval for the % change due to display
exp(confint(boot3$disp))


# Part C: does the elasticity, i.e. the slope on log(price), change
# in the presence of an in-store display?

# Try a model with an interaction
lm4 = lm(log(vol)~log(price) + store + disp + disp:log(price), data=cheese)
coef(lm4)

# Get a confidence interval for the interaction coefficient
boot4 = do(1000)*{
	lm(log(vol)~log(price) + store + disp + disp:log(price), data=resample(cheese))
}
names(boot4)

# Is zero a plausible value for the interaction coefficient?
# Maybe, not probably not.
confint(boot4$log.price..disp)



plot(vol~price, data=dfwkroger, las=1)
points(vol~price, data=subset(dfwkroger, disp==1), col='blue', pch=19)
points(vol~price, data=subset(dfwkroger, disp==0), col='red', pch=19)

# Go back and make the curves from the fitted coefficients
# No displays
curve(exp(9.2609507 + 1.4292030)*x^(-2.4258328), add=TRUE, col='red')
# With displays
curve(exp(9.2609507 + 1.4292030 + 0.3415304)*x^(-2.4258328 - 0.1476102), add=TRUE, col='blue')


######
# Advanced optional stuff:
# defining and optimizing your own functions
######

# Profit equation when cost = 1.50
# blue = display weeks
# red = no-display weeks
my_cost = 1.5

# Define function for the expected profit under different possible conditions
profit = function(price, store, disp, cost=1.5, model=lm4) {
	predictors = data.frame(price= price, store=rep(store, length(price)), disp=disp)
	log_yhat = predict(model, newdata=predictors)
	exp(log_yhat)*(price-cost)
	# exp(9.2609507 + 1.4292030 + 0.3415304)*x^(-2.4258328 - 0.1476102) * (x-cost)
}



# Show the profit curves for DFW Kroger
curve(profit(x, store='DALLAS/FT. WORTH - KROGER CO', disp=1, cost=my_cost), col='blue', from=1.5, to=4)
curve(profit(x, store='DALLAS/FT. WORTH - KROGER CO', disp=0, cost=my_cost), col='red', add=TRUE)

out1 = optimize(profit, interval=c(0, 10), cost=my_cost, disp=1, store='DALLAS/FT. WORTH - KROGER CO', maximum=TRUE)
out1$maximum

out2 = optimize(profit, interval=c(0, 10), cost=my_cost, disp=0, store='DALLAS/FT. WORTH - KROGER CO', maximum=TRUE)
out2$maximum


# Show the profit curves with optimal prices
abline(v=out1$maximum, col='blue')
abline(v=out2$maximum, col='red')

