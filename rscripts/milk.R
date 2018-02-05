library(mosaic)

# You don't need this line if you use the Import Dataset button
milk = read.csv('../data/milk.csv')

# 1) Our equation for net profit N is 
# N = Q*(P - unit_cost)
# where Q is quantity sold and P is per_unit price

##
# 2) Can you fit an equation for Q versus P?
##

# Looks nonlinear
plot(sales ~ price, data=milk)

# A log-log transformations works best here
# This implies a power law (also supported by economic theory)
plot(log(sales) ~ log(price), data=milk)

# Let's fit the power law
lm1 = lm(log(sales) ~ log(price), data=milk)
coef(lm1)

# Thus our fitted equation is
# log(Q) = 4.72 - 1.62*log(P)
# or Q = exp(4.72) * P^(-1.61)


##
# 3) Can you substitute this back in to the profit equation
# to get an equation for profit in terms of P and unit_cost alone?
##

# Sub in to define the profit function
# Once you have this script set up, it will work for any unit cost
unit_cost = 1
profit = makeFun( exp(4.7206) * P^(-1.6186) * (P - unit_cost) ~ P)

## 4) Optimize the profit function

# Plot and point
curve(profit, from = 1, to = 5)
curve(profit, from = 2, to = 3)
curve(profit, from = 2.5, to = 2.7)

# Using optimize
optimize(profit, lower=0, upper = 10, maximum=TRUE)


####
# Prediction intervals for log-transformed y variables
####

# Suppose we charge 2.62 for milk
# What is the expected range of values for sales?
# Key fact: prediction intervals from our fitted model correspond to log(sales)
# We want an intervals for sales, not log(sales).
# What to do?

# Step 1: Form the new data frame whose y values we want to predict
my_newdata = data.frame(price=2.62)

# Step 2: form the prediction interval for this data on the log scale
pred_logsales = predict(lm1, newdata = my_newdata, interval = 'prediction', level = 0.95)

# These predictions are for log(sales)
pred_logsales

# Step 3: Undo the transformation,
# by exponentiating the lower/upper bounds of the interval
exp(pred_logsales)

# Looks like we expect to sell between 14 and 40 units per day,
# with 95% confidence

# Here's a way to visualize the intervals on the original scale
price_grid = data.frame(price = seq(1, 5, by = 0.01))
logsales_grid =  predict(lm1, newdata = price_grid, interval = 'prediction', level = 0.95)
sales_grid = exp(logsales_grid)

head(sales_grid)

# Plot the data + lower/upper bounds calculated from the grid
plot(sales ~ price, data=milk)
lines( sales_grid[,2] ~ price, data=price_grid, col='red')
lines( sales_grid[,3] ~ price_grid$price, col='red')
lines( sales_grid[,1] ~ price_grid$price, col='blue')
