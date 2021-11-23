# Two way ANOVA
# Test which pate is the best. 

library(doBy)
library(car)
library(emmeans)
library(tables)
library(lsmeans)
library(multcomp)

setwd("/home/ajo/gitRepos/glm/Practical3")
data <- read.csv2("PATE.csv")
head(data)
dim(data)
str(data)
summary(data)

data$per <- as.factor(data$per)
data$pate <- as.factor(data$pate)

# Some EDA first (shown in the pdf in Atenea after the lecture).

# a) Find the appropriate linear model assuming the assumptions of the ANOVA model are satisfied. 
#    Perform the corresponding tests and interpret them, including multiple comparisons. Is it
#    necessary to apply any kind of data transformation?.

# As explanatory variables we can use the person and the pate. 
# As response variables we can use color, smell, texture or taste. 

tables::tabular(pate~color*((n=1)+mean+sd), data = data)

tables::tabular(pate~mean*color*per, data = data)

# One-way ANOVA.
model1 <- lm(color~pate, data = data)
summary(model1)

stats::anova(model1)

car::Anova(model1, ty = 3) # Type III anova test. 

par(mfrow = c(2,2))
plot(model1, ask = F)

# Two-way ANOVA. 
model2 <- lm(color~pate+per, data = data)
summary(model2)

Anova(model2, ty = 3) # Type III anova test. 

plot(model2, ask = F)
par(mfrow = c(1,1))

# Need at least two data points in each "cell" to consider interactions, and here we only have one in each "cell".

# Plot residuals also (standardized and studentized)

car::qqPlot(model2)

# Perform Tukey to compare pates and persons. 
# Estimated Marginal Means (emmeans).
emmeans(model1, ~pate)
emmeans(model2, ~pate)

# Compact Letter Display (for Tukey method).
multcomp::cld(emmeans(model1, ~pate), Letters = letters, reversed = T)
multcomp::cld(emmeans(model2, ~pate), Letters = letters, reversed = T)

# Increasing the confidence level.
multcomp::cld(emmeans(model1, ~pate), Letters = letters, reversed = T, alpha = 0.01)
multcomp::cld(emmeans(model2, ~pate), Letters = letters, reversed = T, alpha = 0.01)

# Comparisons in the file she will upload on Atenea later!

# b) Compare the results with the ones obtained considering that it may also exists a taster effect.

# c) Are the ANOVA conditions verified?
