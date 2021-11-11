# Problem 6: One Way ANOVA
# Pdf with answers will be uploaded later. 

library(HH)
library(emmeans)
library(tables)
library(RcmdrMisc)
library(car)
library(multcomp)
library(multcompView)
library(ggplot2)
library(broom)
library(lsmeans)

data <- read.csv2("ADG.csv")
head(data)
# ADG = Average Daily Gain. We want to model this based on the daily does of sweetener.
summary(data) 
str(data)

data$DOSEFact <- as.factor(data$DOSE) # Define the dose as a factor. 
summary(data)

# Some EDA (Descriptive).
boxplot(data$ADG ~ data$DOSEFact, xlab = "Dose level", ylab = "Average Daily Gain")
with(data, plotMeans(ADG, DOSEFact, error.bars = "conf.int", level = 0.95, connect = T))


# a) Define and fit the linear model appropriate to this situation. 
lm.fit <- lm(ADG ~ DOSEFact, data = data)
summary(lm.fit)

# b) Perform the ANOVA test and obtain conclusions from it.
anova(lm.fit)
Anova(lm.fit)
# Both these methods give the same in this case, since we only have one factor. 

# c) Use the Tukey method at 5% 
emm <- emmeans(lm.fit, ~DOSEFact)
emm
pairs(emm)

# Another way of doing Tukey (found online).
aov.fit <- aov(ADG ~ DOSEFact, data = data) # Another way to perform 
summary(aov.fit)
tuk <- TukeyHSD(aov.fit)
tuk
plot(tuk, las = 2)

# Compact letter display (CLD) - shows the same infor as earlier more concisely.  
cld(emm, alpha = 0.05)

# Confidence intervals using Student Range Distribution, with 99% confidence. 
plot(emm, level = 0.99, adjust = "tukey") # adjust = "tukey" gives use of quantiles from Student Range, instead of regular student. 
confint(emm, level = 0.99, adjust = "tukey")

# d) Check if the linear model assumptions can be assumed to be true by performing the model
# adequacy checking. 

# Model adequance checking is done by plotting raw and studentized residuals, etc. 
par(mfrow = c(1,1))
plot(predict(lm.fit), resid(lm.fit))
abline(h=0, lty =2)

par(mfrow = c(2,2))
plot(lm.fit, ask = F)

# Tests for homogeinity of variances. 
# Checking H0 that all variances are equal vs H1 at least two groups that have different variances. 
# A generalization of the F-test for checking two variances, to testing more than two variances (groups).
leveneTest(lm.fit)
bartlett.test(ADG ~ DOSE, data = data)
