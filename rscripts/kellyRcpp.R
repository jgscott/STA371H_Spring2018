library(Rcpp)
library(mosaic)
library(foreach)

# A C++ function for simulating a single trajectory of wealth
# Written in C++ to make it fast, since we're repeating lots of times
cppFunction('
NumericVector sim_path(int N, double frac, double p_win, double W0) {
	// N = number of rounds
	// frac = fraction of wealth to bet on each round
	// p_win = probability of winning each round
	// W0 = initial wealth
	
	// Simulate all the wins and losses in advance
	NumericVector wins = rbinom(N, 1, p_win);
	
	double current_wealth = W0;
	NumericVector running_wealth(N, 0.0);
	double delta, stake;
	for(int i=0; i<N; i++) {
		stake = frac*current_wealth;
		if(wins[i] == 1) delta = stake;
		else delta = -stake;
		current_wealth += delta;
		running_wealth[i] = current_wealth;
	}
	return running_wealth;
}
')

# Parameters of simulation
n_rounds = 10000
frac = 0.10
p_win = 0.52
W0 = 10000

# simulate a single wealth trajectory
this_path = sim_path(n_rounds, frac, p_win, W0)
plot(this_path)

# Extract the final wealth
final_wealth = this_path[n_rounds]
final_wealth

# calculate the implied daily growth rate
(final_wealth/W0)^(1/n_rounds) - 1

# a more numerically stable calculation for large N
gr_daily = exp((1/n_rounds)*(log(final_wealth) - log(W0))) - 1
gr_daily

# express as a yearly rate, assuming 250 trading days/year
gr_yearly = (1+ gr_daily)^250 - 1
gr_yearly


# 2500 simulated trajectories
this_sim = foreach(sim=1:2500, .combine='c') %do% {
	this_path = sim_path(n_rounds, frac, p_win, W0)
	final_wealth = this_path[n_rounds]
	gr_daily = exp((1/n_rounds)*(log(final_wealth) - log(W0))) - 1
	gr_yearly = (1+ gr_daily)^250 - 1
	gr_yearly
}

hist(this_sim, breaks=20)


# Now look at expected growth rate across a range of bet sizes
edge = p_win - (1-p_win)
c_max = 3*edge
c_grid = seq(0, c_max, length=50)
growth_grid = rep(0, length(c_grid))

# Loop over the values in c_grid
for(i in seq_along(c_grid)) {
	frac = c_grid[i]
	cat("c = ", frac, "\n")
	this_sim = foreach(sim=1:500, .combine='c') %do% {
		# simulate a single wealth trajectory
		this_path = sim_path(n_rounds, frac, p_win, W0)
		final_wealth = tail(this_path,1)
		# calculate the implied daily growth rate
		gr_daily = exp((1/n_rounds)*(log(final_wealth) - log(W0))) - 1
		# express as a yearly rate, assuming 250 trading days/year
		gr_yearly = (1+ gr_daily)^250 - 1
		gr_yearly
	}
	gr_yearly_avg = mean(this_sim)
	growth_grid[i] = gr_yearly_avg
}

plot(c_grid, growth_grid)

# The maximum as predicted by theory
abline(v=edge)

