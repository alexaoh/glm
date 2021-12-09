# Mortality produced by different dosage of insecticide. 
# GLM with binomial response in each of the groups we study.

library(car)
library(RcmdrMisc)
setwd("/home/ajo/gitRepos/glm")

data <- read.csv2("insecticide.csv")
str(data)
data
summary(data)

# EDA
scatterplot(DIED/T~log(DOSE), smooth = F, boxplots = F, data = data) # "S-shape".

# Do the inverse normal CDF transformation to see if p is linear in this function.
scatterplot(qnorm(DIED/T)~log(DOSE), smooth = F, boxplots = F, data = data[2:10,])

# We will fit different models in order to compare the results. 
# GLMs with different links and the binomial distribution for Y. 

# Model1: Assume normal response with the qnorm() transformation. 
m0 <- lm(qnorm(DIED/T)~log(DOSE), data = data[which(data$DIED>0), ])
summary(m0)
par(mfrow = c(2,2))
plot(m0, ask = F)
par(mfrow = c(0,0))

# Model2: Response binomial. probit link (same as above, but do not assume normal response.)

# Model3: Response binomial. logit link.

# Model4: Response binomial. complementary log-log link. 
