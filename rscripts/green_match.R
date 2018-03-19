library(mosaic)
library(MatchIt)

green = read.csv("../data/green.csv", header=TRUE)

# quick summary
summary(green)

# Define revenue per square foot measure
green$RevPSF = green$Rent * green$leasing_rate / 100
hist(green$RevPSF, 30)

# Means of each group
mean(RevPSF ~ green_rating, data=green)

# But look at the confounders...
mean(age ~ green_rating, data=green)
mean(class_a ~ green_rating, data=green)
mean(class_b ~ green_rating, data=green)

###
# Matching to estimate the causal effect
# of a green certification
###

# Step 1:
# Find matching pairs based on age and building class
mymatch = matchit(green_rating ~ age + class_a + class_b, data = green)

# Step 2:
# Check covariate balance
summary(mymatch)

# Extract only the matched pairs
green_matched = match.data(mymatch)
	
# Step 3: run an analysis on the matched data.
# Here we perform a simple difference-of-means 
# analysis on the matched data only
mean(RevPSF ~ green_rating, data= green_matched)
t.test(RevPSF ~ green_rating, data= green_matched)

# The catch:
# Confounders that weren't matched on may still be unbalanced!
# This means the above answer is probably not trustworthy
mean(amenities ~ green_rating, data= green_matched)
mean(renovated ~ green_rating, data= green_matched)
mean(heating_costs ~ green_rating, data= green_matched)
mean(cluster_rent ~ green_rating, data= green_matched)

# Generally better for step 3:
# Run a regression on the matched data,
# to further adjust for any confounding/imbalances
lm1 = lm(RevPSF ~ age + class_a + class_b + cluster_rent + green_rating, data = green_matched)
summary(lm1)
