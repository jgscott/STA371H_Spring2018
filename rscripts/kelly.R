# Simulating 10000 rounds of the favorable bet

# Parameters for initial wealth and number of rounds
initial_wealth = 10000
rounds = 10000

frac = 0.04 # how much of wealth is wagered each round
runningwealth = rep(0, rounds) # a placeholder for the result

# Simulate 10000 rounds of betting
current_wealth = initial_wealth
for(i in 1:rounds) {
  bet = frac* current_wealth  # the size of the bet
  coin = sample(c(1,-1), 1, prob = c(0.52, 0.48))  # the result  
  current_wealth = current_wealth + coin*bet # update current wealth
  runningwealth[i] = current_wealth
}
plot(runningwealth)
current_wealth

# implied interest rate
# under compound interest: final = initial * (1 + R)^N
# where R = interest rate and N = periods
R =  (current_wealth/initial_wealth)^{1/rounds} - 1

# Implied yearly interest rate
(1+R)^{250} - 1
