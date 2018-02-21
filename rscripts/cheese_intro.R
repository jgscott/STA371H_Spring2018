library(mosaic)
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')
cheese = read.csv('../data/cheese.csv')

boxplot(vol ~ disp, data=cheese)

# First "obvious" thing to do:
# fit a model assuming an additive effect of display on volume
lm0 = lm(vol ~ disp, data=cheese)
coef(lm0)

# baseline = 3244
# dummy variable coefficient = 2365 more 
# 		units of cheese in display weeks
# What's wrong with this answer?
boxplot(vol ~ disp, data=subset(cheese, store == "HOUSTON - KROGER CO"))
boxplot(vol ~ disp, data=subset(cheese, store == "ORLANDO,FL - FOOD LION"))

# OK, second thought, what about a multiplicative change?
# key insight: a regression model on the log(y) scale
# implies a multiplicative effect on the original scale
lm1 = lm(log(vol) ~ disp, data=cheese)
coef(lm1)
# so on average, log(vol) = 7.84 + 0.44*disp
# or vol = exp(7.84) * exp(0.44*disp)

# But this answer is confounded by store.
# Store is correlated with vol (the Y variable):
mean(vol ~ store, data=cheese)

# And it's also correlated with disp (the X variable):
# some stores use the display more frequently
mean(disp ~ store, data=cheese)

# Part A asks: how can you estimate the partial relationship
# between display and sales volume, adjusting for differences
# among stores? 

# And what about price? Is it a confounder, too?
