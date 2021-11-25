# Problem 10.
# We are working with cheese producers. The milk can be pasteurized or not. 
# Can also add CaCl2 or not add it to the milk. 
# We also have 3 types of milk. Which treatment is the best? (in terms of economical gains)
# We have four different ways in making chees (combinations of additive/not and plane/pasteurized)
# Which gives the best cheese as a result?

library(doBy)
library(car)
library(emmeans)
library(tables)
library(lsmeans)
library(multcomp)
library(RcmdrMisc)

setwd("/home/ajo/gitRepos/glm/Practical4")
data <- read.csv2("cheese.csv")
head(data)
str(data)
summary(data)
data # small data set, print all of it. 

data$THERMIC <- as.factor(data$THERMIC)
data$CaCl2 <- as.factor(data$CaCl2)

# We will do the analysis with cow milk here. The same can/should be done with the two other milk-types.

tabular((COW+GOAT+SHEEP)*(THERMIC+1)~(CaCl2+1)*(n=1), data=data)

tabular((COW+GOAT+SHEEP)*(THERMIC+1)~(CaCl2+1)*mean, data=data)
tabular((COW+GOAT+SHEEP)*(THERMIC)~(CaCl2)*sd, data=data)

with(data, RcmdrMisc::plotMeans(COW, THERMIC, CaCl2, error.bars = "conf.int", level = 0.95, connect = T))
# We are facing a true Two-Way ANOVA. The two factors have an interaction, as seen in the plot above. 
# E.g.: Adding CaCl2 can either be good or bad, depending on if the milk if pasteurized or not. 

with(data, RcmdrMisc::plotMeans(COW, CaCl2, THERMIC, error.bars = "conf.int", level = 0.95, connect = T))

# Means table.
tabular(THERMIC ~ COW*CaCl2*mean, data = data)

mcow <- lm(COW~THERMIC*CaCl2, data = data)
summary(mcow)
anova(mcow)
Anova(mcow, t=3)
# Now we can do two things:
# 1. Eliminate the main term that is not significant. 
# 2. Leave the main effects because the interaction is significant. Not leaving the main effects makes the interpretation
# of the interaction term more difficult. Thus, we leave the main effects in here, since this makes interpretation more simple. 

# Compact letter display. Tukey method. 
multcomp::cld(emmeans(mcow, ~THERMIC), Letters = letters, reversed = T) # This is misleading because of the significant interaction
# As the warning points out. Therefore, add the |CaCl2 below. 
multcomp::cld(emmeans(mcow, ~THERMIC|CaCl2), Letters = letters, reversed = T)
# We perform the Tukey method comparing two means when CaCl2 is added and comparing two means when CaCl2 is not added.
# We fix the additive value and the compare the means of plane vs pasteurized in each case. 

# Below we fix the thermic effect (plane or pasteurized) and we compare the means of adding CaCl2 or not in each case. 
multcomp::cld(emmeans(mcow, ~CaCl2|THERMIC), Letters = letters, reversed = T)

# Below we do both things at the same time. 
multcomp::cld(emmeans(mcow, ~THERMIC*CaCl2), Letters = letters, reversed = T)
# Should reach the same conclusions. Almost all of the are the same, but not all. See below. 
# The quantiles in the Student Rank are different in the tables, when some are fixed and when all four means are compared, 
# since the tests are done slightly different.  
# That is why the conclusion about plane vs pasteurized with CaCl2 is different in the first table (where CaCl2 is fixed) 
# (where they are put in two different groups) vs the table where all four are compared (where they are not put in different groups).

# Plot residuals. 
par(mfrow = c(2,2))
plot(mcow, ask = F)
par(mfrow = c(1,1))

# More model testing plots are done in the pdf that she will upload after. Check this file later. 
