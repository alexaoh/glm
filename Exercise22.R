# Clusters-data: Is the probability of cancer illness different depending on the distance from where people live to a nuclear plant.

# Fixed the period of time and count how much cancer has been detected --> Poisson.

setwd("/home/ajo/gitRepos/glm")
data <- read.csv2("clusters.csv")
head(data)
str(data)

plot(data$Distance, data$Cancers)
non.null <- which(data$Cancers > 0)
plot(data$Distance[non.null], log(data$Cancers[non.null]))
plot(data$Distance, log(data$Cancers))
# Burde være en linear relationship her et sted.

# Fit the appropriate GLM.
model <- glm(Cancers ~ Distance, family = poisson(link = "log"), data = data)
summary(model)

# Check if there is overdispersion.
# Estimate the \phi-parameter. 
# What is it for the poisson?
