library(mosaic)
colon_cancer = read.csv('../data/colon_cancer.csv')
	
	
# first few lines
head(colon_cancer)

# a two-way table showing recurrence and treatment
xtabs(~recur + chemo, data=colon_cancer)

# calculating relative risk by eye
123/(181+123)
329/(296+329)
.5264/.4046

# Let's construct the table and pick out the corresponding entries
tab_recur = xtabs(~chemo + recur, data=colon_cancer)
risk_control = tab_recur[1,2]/( tab_recur[1,1] + tab_recur[1,2] )
risk_chemo = tab_recur[2,2]/( tab_recur[2,1] + tab_recur[2,2] )
risk_control/risk_chemo

# How to assess whether this result is "surprisingly different" from zero?
# idea: "shuffle the cards" and see how variable the relative risk is
tab_shuffle = xtabs(~chemo + shuffle(recur), data=colon_cancer)
tab_shuffle
risk_control_shuff = tab_shuffle[1,2]/( tab_shuffle[1,1] + tab_shuffle[1,2] )
risk_chemo_shuff = tab_shuffle[2,2]/( tab_shuffle[2,1] + tab_shuffle[2,2] )
risk_control_shuff/risk_chemo_shuff
