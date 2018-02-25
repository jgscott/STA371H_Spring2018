library(mosaic)
data(SaratogaHouses)
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')

# Baseline model
lm_small = lm(price ~ livingArea + lotSize + fireplaces, data=SaratogaHouses)
simple_anova(lm_small)

# 11 main effects
lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
		fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=SaratogaHouses)

# Sometimes it's easier to name the variables we want to leave out
# The command below yields exactly the same model.
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
lm_medium2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=SaratogaHouses)

coef(lm_medium)
coef(lm_medium2)
simple_anova(lm_medium)

# All interactions
# the ()^2 says "include all pairwise interactions"
lm_big = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=SaratogaHouses)
simple_anova(lm_big)

# Stepwise selection
lm_step = step(lm_big)

# Which variables are in?
simple_anova(lm_step)

# Compare size versus big model
length(coef(lm_step))
length(coef(lm_big))

###
# Different ways to do stepwise selection
###

# Option 1: Starting from a large model and deleting variables
# Here the scope is all the variables in the original model.
lm_big = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=SaratogaHouses)
lm_step = step(lm_big)

# Option 2: Starting from a smaller model,
# explicitly listing variables in the scope for selection,
# and adding/deleting variables from the scope
lm_start = lm(price ~ lotSize + age + livingArea, data=SaratogaHouses)
lm_step2 = step(lm_start, scope = price ~ (lotSize + age + livingArea + pctCollege + bedrooms + 
		fireplaces + bathrooms + rooms + heating + fuel + centralAir)^2, data=SaratogaHouses)


# These different starting points lead to different selected models
summary(lm_step)  
summary(lm_step2)


####
# Compare out-of-sample predictive performance
####

# Split into training and testing sets
n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
saratoga_train = SaratogaHouses[train_cases,]
saratoga_test = SaratogaHouses[test_cases,]
	
# Fit to the training data
lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
lm2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
lm3 = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)

# Refit the stepwise-selected model to the training set
# We could rename all the variables but this is more concise.
# Note: do not run stepwise selection again on the the training data.
# Stepwise selection is usually run on the full data set.
lm4 = update(lm_step, data=saratoga_train)

# Predictions out of sample
yhat_test1 = predict(lm1, saratoga_test)
yhat_test2 = predict(lm2, saratoga_test)
yhat_test3 = predict(lm3, saratoga_test)
yhat_test4 = predict(lm4, saratoga_test)

# Root mean-squared prediction error
sqrt( mean( (saratoga_test$price - yhat_test1)^2 ) )
sqrt( mean( (saratoga_test$price - yhat_test2)^2 ) )
sqrt( mean( (saratoga_test$price - yhat_test3)^2 ) )
sqrt( mean( (saratoga_test$price - yhat_test4)^2 ) )	
	
		