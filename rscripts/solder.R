summary(solder)

# An ANOVA table
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')

lm0 = lm(skips ~ Opening + Solder, data= solder)
coef(lm0)

lm1 = lm(skips ~ Opening + Solder + Opening:Solder, data= solder)
coef(lm1)

boxplot(skips ~ Solder:Opening, data=solder)

lm2 = lm(skips ~ Opening + Solder + Mask + Opening:Mask + Mask:Solder, data= solder)
coef(lm2)

boxplot(skips ~ Solder:Opening, data = solder)

coef(lm1)
simple_anova(lm1)

simple_anova(lm2)
