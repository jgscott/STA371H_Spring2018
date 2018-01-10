library(mosaic)
sclass = read.csv('../data/sclass.csv')

# Reminder of the variables involved
summary(sclass)

options(scipen=5) # suppress scientific notation in the axis labels
barplot(mean(price ~ condition, data = sclass), las=1,
	main="Mercedes S-class: price by condition",
	axes=FALSE)
axis(2, at = seq(0, 120000, by=10000), las=1)


# And now a silly version of the plot:
# needlessly truncating the y axis distorts relative size of the bars
barplot(mean(price-25000 ~ condition, data = sclass), las=1,
	main="Mercedes S-class: price by condition", axes=FALSE)
axis(2, at = seq(0, 120000, by=10000), labels = seq(0, 120000, by=10000) + 25000, las=1)
