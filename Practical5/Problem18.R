# Milk diary production. GLM.
# It is known that the production (PROD) follows a Gamma distribution. 
# Thus we use the Gamma distribution as the distribution of the production 
# and, because of the statement about the expected value in the problem, 
# we use the log() as a link function. 
setwd("~/gitRepos/glm/Practical5")
data <- read.csv2("diaryp.csv")
data$logPROD <- log(data$PROD)
head(data)
str(data)
summary(data)
data

model1 <- lm(PROD~Days+log(Days), data = data)
(s1 <- summary(model1))
par(mfrow=c(2,2))
plot(model1, ask = F) # Clearly not a good fit.

par(mfrow = c(1,1))
predicted.model1 <- predict(model1)
with(data, plot(Days, predicted.model1), type = "l")
with(data, points(Days, PROD, col = "red", pch = "+"))

model2 <- lm(log(PROD)~Days+log(Days), data = data)
(s2 <- summary(model2))
par(mfrow=c(2,2))
plot(model2, ask = F) # Clearly not a good fit either, but perhaps slightly better than model1.

par(mfrow = c(1,1))
predicted.model2 <- predict(model2)
with(data, plot(Days, exp(predicted.model2)), type = "l")
with(data, points(Days, PROD, col = "red", pch = "+"))
plot(data$Days, resid(model2))
plot(predicted.model2, resid(model2))
abline(h=0, lty = 2)
     
model3 <- glm(PROD~Days+log(Days), family = Gamma(link = "log"), data = data) 
(s3 <- summary(model3))
par(mfrow=c(2,2))
plot(model3, ask = F) # Pretty much the same as earlier!

par(mfrow = c(1,1))
predicted.model3 <- predict(model3, ty = "response")
with(data, plot(Days, predicted.model3), lty = 1)
with(data, points(Days, PROD, col = "red", pch = "+"))
plot(predict(model3),resid(model3))
abline(h=0,lty=2)
residualPlot(model3) # The linear predictor is the deterministic part of GLM. 


# The gaussian without the log-link (which is the canonical identity) should give the same fit as model1
model4 <- glm(PROD~Days+log(Days), family = gaussian(link = "log"), data = data) 
# Dispersion parameter for gaussian is \sigma^2 (variance)
(s4 <- summary(model4)) 
par(mfrow=c(2,2))
plot(model4, ask = F)

par(mfrow = c(1,1))
predicted.model4 <- predict(model4, ty = "response")
with(data, plot(Days, predicted.model4), lty = 1)
with(data, points(Days, PROD, col = "red", pch = "+"))
plot(predict(model4),resid(model4))
abline(h=0,lty=2)
residualPlot(model4) # The linear predictor is the deterministic part of GLM. 

model5 <- glm(PROD~Days+log(Days), family = poisson(link = "log"), data = data) # not discrete production (output), yields numerical problems. 
(s5 <- summary(model5))
plot(model5, ask = F)

df <- data.frame("model1"=c(s1$r.squared, s1$sigma), "model2"=c(s2$r.squared, s2$sigma), "model4"=c(NULL, sqrt(s4$dispersion)))
rownames(df) <- c("R-squared","Res. Stand. Err.")
df

# She did some more comparisons of three of the models in the pdf in Atenea. Uses R^2 for lm and the ratio of deviances in glm, 
# in order to compare the models. These values apparently play a similar role. 
