# ANCOVA exercise: Vitamin C level. 
# Problem 15.

# The juices can be:
# 1) Packed differently.
# 2) Have different conservation temperatures. 

# 3 conservation methods are considered: a, b and c. Want to find the best of these 3 treatments
# where each are a combination of package methods and conservation temperatures. 
# Want to find which treatment gives the highest vitamin c level. 
# In each week, two juices are followed for each treatment. The study goes for 12 weeks. 
# Hence we have 2*12 data points for each treatment, 24*3 = 72 rows in total. 
library(car)
library(emmeans)
setwd("/home/ajo/gitRepos/glm/Practical4")
data <- read.csv2("vitc.csv")
dim(data)
head(data)
data$logvitc <- log(data$vitc)
summary(data)

# EDA.
scatterplot(logvitc ~ week|treat, smooth=F, dat = data)


model1 <- lm(logvitc ~ week*treat, data = data)
summary(model1)

Anova(model1, ty = 3)
# Treat is not significant. 

scatterplot(predict(model1)~week|treat,dat = data)

# Want to make sure that the Vitamin C levels are the same at the beginnin, i.e. at week 0. 
emm0 <- emmeans(model1, ~treat|week, at=list(week = c(0)))
print(pairs(emm0))
# Since the p-values are large, we do not reject the H0 that says that the Vitamin C levels are the same. 
# Thus we conclude that the levels are the same at the beginning, which means that we can carry on with our analysis. 

# Next we want to compare the slopes of the treatments. 
# a-b and b-c are significant, which means that they are different, except for a - c, which is as expected based on the first scatterplot.
emm <- emmeans::emtrends(model1, ~treat, var = "week")
print(pairs(emm))

par(mfrow=c(2,2))
plot(model1, ask = F)
par(mfrow=c(1,1))

model2 <- lm(logvitc~week+treat:week, data = data) # 2 parameters less, since we remove the trend (and the trend interaction with week)
summary(model2)

# # Method a)
# \begin{equation}
#   log(vitc) = 3.8293 - 0.1648*Week
# \end{equation}
#
# \begin{equation}
#   vitc = exp(3.8293 - 0.1648*Week) = exp(3.8293)exp(-0.1648*Week)
# \end{equation}
# etc
# # Method b)
# \begin{equation}
#   log(vitc) = 3.8293 + (- 0.1648 + 0.12)*Week
# \end{equation}
#   
#   # Method b)
# \begin{equation}
#   log(vitc) = 3.8293 + (-0.1648 + 0.04778)*Week
# \end{equation}

# All th three slopes are different in this model. 
emmm <- emtrends(model2, ~treat, var = "week")
print(pairs(emmm))

par(mfrow=c(2,2))
plot(model2, ask = F)
par(mfrow=c(1,1))

  