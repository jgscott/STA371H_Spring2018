library(mosaic)


# problem 1
ut2000 = read.csv("../data/ut2000.csv")

# part a
arch = subset(ut2000, School == "ARCHITECTURE")
lib_arts = subset(ut2000, School == "LIBERAL ARTS")

mean(arch$SAT.Q)
mean(lib_arts$SAT.Q)

mean(resample(arch)$SAT.Q)
mean(resample(lib_arts)$SAT.Q)

out1 = do(1000)*{
	mean(resample(arch)$SAT.Q) - mean(resample(lib_arts)$SAT.Q)
}

confint(out1)

# part b
ut2000$GPA100 = 100*ut2000$GPA
lm2 = lm(GPA100 ~ SAT.C + School, data=ut2000)
coef(lm2)

# Bootstrap the model
boot2 = do(1000)*{
	lm(GPA100 ~ SAT.C + School, data=resample(ut2000))
}
confint(boot2)
confint(lm2) # What's the difference in assumptions?

hist(boot2$SAT.C)
