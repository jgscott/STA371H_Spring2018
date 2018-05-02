library(mosaic)

# Read in the challenger data
challenger = read.csv("../data/challenger.csv", header=T)

# Plot presence/absence of erosion event vs temperature
plot(Erosion ~ Temp, data = challenger)


# Extend the x axis
plot(Erosion ~ Temp, data = challenger, xlim=c(20, 85))

# Add a line for the temp at 9 AM on 1/28/86
abline(v=29)

### Fit a logit model
glm1 = glm(Erosion ~ Temp, data=challenger, family=binomial(link="logit"))
summary(glm1)

# Plot the fitted model
plotModel(glm1)

# With the extended x axis
plotModel(glm1, xlim=c(20, 85))
abline(v=29)

curve(exp(1 + -1.5*x)/(1+exp(1 -1.5*x)), from=-5, to = 5)


# Temp at 2PM launch: 38F
# Estimate P(erosion | temp = 38F)
coef(glm1)
psi2pm = 23.87 - 0.3682 * 38
prob2pm = exp(psi2pm) / (1 + exp(psi2pm))

# A little more concise
xstar = data.frame(Temp = c(38, 60, 85))
predict(glm1, newdata = xstar, type='response')


# A prettier plot
plot(Erosion ~ Temp, data = challenger,
	pch=19, axes=F, xlab="Temperature at Launch", ylab='O-Ring Erosion?', cex.lab=1.5, 	cex.axis=1.5, main="Erosion Incidents on Previous Shuttle Flights", cex.main=1.3, 	xlim=c(20, 85))
axis(1, cex.axis=1.5)
axis(2, at = c(0, 1), labels=c("No", "Yes"), cex.axis=1.5)

arrows(29,0.39, 29, 0)
text(29, 0.4, "Temperature at 9 AM, 1/28/86", pos=3)



### Fit a logit model

glm1 = glm(Erosion ~ Temp, data=challenger, family=binomial(link="logit"))
summary(glm1)

### Temperature at 9 AM on 1/28/1986 = 29F
mu9am = 23.87 - 0.3682 * 29

prob9am = exp(mu9am) / (1 + exp(mu9am))

### Temperature at 2 PM on 1/28/1986 = 38F
psi2pm = 23.87 - 0.3682 * 38
prob2pm = exp(psi2pm) / (1 + exp(psi2pm))
