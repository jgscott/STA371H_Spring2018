library(mosaic)

# Modeling a risky asset with a positive expected return
ReturnAvg = 0.0653
ReturnSD = 0.195
Horizon = 40


# Simulate 40 years of investment in the stock market
# Try the following block of code several times

# Initial wealth
Wealth = 10000
# Sweep through each year.
# Simulate a random interest rate
# and update the value of wealth.
for(year in 1:Horizon) {
	# Generate a random return
	ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
	
	# Update wealth using simple interest formula
	Wealth = Wealth * (1 + ThisReturn)
}

# Final value of wealth
Wealth


# Now the same simulation, but tracking wealth along the way
# Try the following code block several times.

Wealth = 10000  # initial wealth
WealthOverTime = rep(0, Horizon)  # Placeholder
# Sweep through each year and update the value of wealth
for(year in 1:Horizon) {
  ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
  Wealth = Wealth * (1 + ThisReturn)
  WealthOverTime[year] = Wealth
}
Wealth
plot(WealthOverTime, type='l')



####
# Now a Monte Carlo simulation
# 1000 simulated trajectories
####

# Outer loop
sim1 = do(1000)*{
  Wealth = 10000  # Reset initial wealth
  WealthOverTime = rep(0, Horizon)  # Placeholder
  # Sweep through each year and update the value of wealth
  for(year in 1:Horizon) {
    ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
    Wealth = Wealth * (1 + ThisReturn)
    WealthOverTime[year] = Wealth
  }
  WealthOverTime
}
head(sim1)

# Plot a few simulated scenarios
plot(1:Horizon, sim1[1,], type='l')
lines(1:Horizon, sim1[2,], type='l')
lines(1:Horizon, sim1[3,], type='l')

# The probability distribution over final wealth
hist(sim1[,Horizon], breaks=200, xlim=c(0, 500000))

# What fraction of the simulated futures lost money
# at the end of the investment horizon?
sum(sim1[,Horizon] < 10000) / 1000

# How many were losing money after 10 years?
sum(sim1[,10] < 10000) / 1000

# How many made more than $1 million?
sum(sim1[,Horizon] > 1e6) / 1000



######
# Optional: reproduce the plot in the course packet
######

layout(matrix(c(1,1,1,2,2), nrow=1))
plot(1:Horizon, colMeans(sim1), type='l', col='red',
	xlab="Years into future",
	ylab="Value of portfolio",
	main="Simulated growth of a stock portfolio over 40 years",
	las=1, cex.axis = 0.85,
	ylim=c(10000, 2*max(colMeans(sim1))))
	
for(sim in 1:nrow(sim1)) {
  lines(1:Horizon, sim1[sim,], type='l', col=rgb(0.5,0.5,0.5,0.05))
}
lines(1:Horizon, colMeans(sim1), lwd=3, col='red')
legend('topleft',
	legend=c("Average trajectory", "All simulations", "Example trajectories"),
	lwd=1, col=c("red", "grey", "blue"))
lines(1:Horizon, sim1[90,], col='blue')
lines(1:Horizon, sim1[91,], col='blue')



# Now look at terminal wealth (the last column)
hist(sim1[,40], 400, main="Value of portfolio after 40 years",
	col='grey', las=1, xlim=c(0, 0.5e6),
	xlab="Dollars")

