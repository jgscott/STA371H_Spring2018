#######
# Example 1: binomial
#######

# Airline no shows
P = 0.09
N = 140

# How likely are 5 no-shows?
# about 0.7%... use dbinom
dbinom(5, size=N, prob = P)

# How like is each of 0 through 5 no-shows?
dbinom(0:5, size=N, prob = P)

# What is the sum of these probabilities?
sum(dbinom(0:5, size=N, prob = P))

# An easier way: compute a binomial left tail area using pbinom
# This calculates the sum of the probabilities of all outcomes
# with 5 successes or fewer
pbinom(5, size=N, prob=P)

# You are 16th on the stand-by list on a full flight.
# What is the probability of 16 or more no-shows?
# This is a right tail area.
# Subtact the corresponding left tail area from 1.

# Probability of 15 or fewer
pbinom(15, size=N, prob=P)

# Probability of 16 or more
1 - pbinom(15, size=N, prob=P)

# Make a barplot of binomial probabilities
x_grid = seq(0, 30, by=1)
barplot(dbinom(x_grid, N, P), names.arg = x_grid)
sum(dbinom(0:5, N, P))


#######
# Example 2: Poisson
#######

# Poisson probabilities: X_A ~ Poisson(1.6)
# Under the Poisson model, what is the probability
# that Arsenal will score exactly 3 goals in a game?
dpois(3, lambda=1.6)

# A Poisson left tail area:
# Under the Poisson model, what is the probability
# that Arsenal will score 2 or fewer goals in a game?
ppois(2, lambda=1.6)

# What about for Man U?  X_M ~ Poisson(1.3)
ppois(2, lambda=1.3)

# What is the probability of a 1-1 tie, assuming independence?
# Multiply the individual probabilities for the joint probability.
dpois(1, lambda=1.6) * dpois(1, lambda=1.3)

# A simple way to calculate lots of probabilities at once:
# simulate 100000 games by randomly simulating goals from the Poisson model.
NMC = 100000

# Game scores for Arsenal
arsenal = rpois(NMC, 1.6)

# Game scores for Man U
ManU = rpois(NMC, 1.3)

# Look at the first several simulated games:
head(cbind(arsenal, ManU), 10)

# Tabulate the scores and make a probability matrix:
results_matrix = xtabs(~arsenal + ManU)/NMC
results_matrix = round(results_matrix, 3)

results_matrix

# Monte Carlo estimates are now available for several quantities.
# The probability of an Arsenal win:
sum(arsenal > ManU)/NMC

# The probability of a draw:
sum(arsenal == ManU)/NMC

# The probability of a Man U win:
sum(arsenal < ManU)/NMC


#####
# Optional fancy plot
####

# only look at games with a score of 5-5 or less
results_matrix = results_matrix[1:6, 1:6]

my_cols = grey(seq(1, 0.5, length=15))
my_breaks = seq(0, 0.12, length=16)
image(0:5, 0:5, results_matrix,
	col=my_cols, breaks=my_breaks,
	las=1, bty='n',
	main="Probability of outcomes for the \nArsenal vs. Manchester United match",
	ylab='Manchester United goals',
	xlab="Arsenal goals")
	
# Add text labels
for(i in 1:6) {
	for(j in 1:6) {
		text(i-1, j-1, results_matrix[i,j])
	}
}


#######
# Example 3: Normal
#######

# Mean and standard deviation of historical stock-market returns
mu = 0.0653
sigma = 0.195

# Normal left tail area: how likely is a return worse than -5%:
pnorm(-0.05, mu, sigma)

# How likely is any negative return?
pnorm(0, mu, sigma)

# Normal right tail area:
# How likely is a return better than 20%:
# (the lower.tail argument is true by default)
pnorm(0.2, mu, sigma, lower.tail=FALSE)

# Difference of two left tail areas:
# How likely is a return between 5% and 15%?
pnorm(0.15, mu, sigma) - pnorm(0.05, mu, sigma)


# simulate random normal variables
my_sample = rnorm(100, mu, sigma)
hist(my_sample, breaks=20)
abline(v=mu, col='red', lwd=3)

# A much bigger sample
my_sample2 = rnorm(10000, mu, sigma)
hist(my_sample2, breaks=50)


# Normal quantiles: what return corresponds to a 25% left tail area?
qnorm(0.25, mu, sigma, lower.tail=TRUE)

# What return corresponds to a 10% right tail area?
qnorm(0.1, mu, sigma, lower.tail=FALSE)

# Same as this: 
qnorm(1-0.1, mu, sigma, lower.tail=TRUE)
