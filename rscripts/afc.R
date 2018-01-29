library(mosaic)

# sanity check the first few lines 
head(afc)

# stratify scatterplot of price vs food score by nbhd
xyplot(Price ~ FoodScore | Neighborhood, data=afc)

# hard to tell by eye; compute means by group
mean(Price ~ Neighborhood, data=afc)


# food or atmosphere?
cor(Price ~ FeelScore, data = afc)
cor(Price ~ FoodScore, data = afc)

# could also look at residuals from regression models
plot(Price ~ FoodScore, data = afc)
lm1 = lm(Price ~ FoodScore, data = afc)
lm2 = lm(Price ~ FeelScore, data = afc)
sd(resid(lm1))
sd(resid(lm2))

# Adjust price for food score
adjusted_price = resid(lm1)
boxplot(adjusted_price ~ Neighborhood, data=afc)

# groupwise means
mean(adjusted_price ~ Neighborhood, data=afc)

# some code for change the axis labels
boxplot(adjusted_price ~ Neighborhood, data=afc,
        las=2, cex.axis=0.5)

# cex = "character expansion" (default = 1.0)
# las = text angle (1 = horizontal, 2 = perpendicular to axis, etc)
# type ?par for lots of options, or just email me with 
# "how do I make an R plot do X" question?

