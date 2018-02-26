library(mosaic)

predimed = read.csv('../data/predimed.csv')

# Quick look at the data
head(predimed)
xtabs(~group + event, data=predimed)

# create a new varible
# can also easily do this in Excel and create a new csv file
predimed$MedDietAny = ifelse(predimed$group == "Control", "No", "Yes")

# Make a table using the combined variable
t1 = xtabs(~ MedDietAny + event, data=predimed)
p1 = prop.table(t1, margin=1)
RR = p1[2,2]/p1[1,2]
RR

# permutation test
perm1 = do(1000)*{
	t1_shuff = xtabs(~ shuffle(MedDietAny) + event, data=predimed)
	p1_shuff = prop.table(t1_shuff, margin=1)
	RR_shuff = p1_shuff[2,2]/p1_shuff[1,2]
	RR_shuff
}

hist(perm1$result, 20)
abline(v=RR, col='red')
sum(perm1$result <=  RR)/1000


# a more nuanced answer here is to get a confidence interval
boot1 = do(1000)*{
	t1_boot = xtabs(~ MedDietAny + event, data=resample(predimed))
	p1_boot= prop.table(t1_boot, margin=1)
	RR_boot = p1_boot[2,2]/p1_boot[1,2]
	RR_boot
}
hist(boot1$result)
confint(boot1$result)


# a second version that doesn't involve collapsing categories
# one idea: use R2 for a group-wise model as a test statistic
# (Note: far from the only test statistic you could have used here)
xtabs(~group + event, data=predimed)
predimed$event_dummy = ifelse(predimed$event == "Yes", 1, 0) 
lm2 = lm(event_dummy ~ group, data=predimed)
rsquared(lm2)

# permutation test
perm2 = do(1000)*{
	lm_shuff = lm(event_dummy ~ shuffle(group), data=predimed)
	rsquared(lm_shuff)
}
hist(perm2$result)
abline(v=rsquared(lm2), col='red')
sum(perm2$result >= rsquared(lm2))/1000


