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
