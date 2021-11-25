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
setwd("/home/ajo/gitRepos/glm/Practical4")
data <- read.csv2("vitc.csv")
dim(data)
head(data)
data$logvitc <- log(data$vitc)
summary(data)

# EDA.
scatterplot(logvitc ~ week|treat, smooth=F, dat = data)


lm.fit <- lm(logvitc ~ week*treat, data = data)
summary(lm.fit)
