library(mosaic)

# 1) Choose H0: the coin toss winner has a 50% chance of winning the game

# 2) Choose a test statistic: observed fraction of home-team wins (38/70)

# 3) Simulate P(test stat | H0 )

montecarlo1 = do(10000)*nflip(70)
head(montecarlo1)

hist(montecarlo1$nflip, 40)

sum(montecarlo1$nflip >= 38)
