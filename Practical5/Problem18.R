# Milk diary production. GLM.
# It is known that the production (PROD) follows a Gamma distribution. 
# Thus we use the Gamma distribution as the distribution of the production 
# and, because of the statement about the expected value in the problem, 
# we use the log() as a link function. 
setwd("~/gitRepos/glm/Practical5")
data <- read.csv2("diaryp.csv")
head(data)
str(data)
summary(data)
data

model1 <- lm(PROD~Days+log(Days), data = data)
(s1 <- summary(model1))
par(mfrow=c(2,2))
plot(model1, ask = F) # Clearly not a good fit.

model2 <- lm(log(PROD)~Days+log(Days), data = data)
(s2 <- summary(model2))
plot(model2, ask = F) # Clearly not a good fit either, but perhaps slightly better than model1.

model3 <- glm(PROD~Days+log(Days), family = Gamma(link = "log"), data = data) 
(s3 <- summary(model3))
plot(model3, ask = F) # Pretty much the same as earlier!

# The gaussian without the log-link (which is the canonical identity) should give the same fit as model1
model4 <- glm(PROD~Days+log(Days), family = gaussian(link = "log"), data = data) 
(s4 <- summary(model4)) 
plot(model4, ask = F)

model5 <- glm(PROD~Days+log(Days), family = poisson(link = "log"), data = data) # non-integer!
(s5 <- summary(model5))
plot(model5, ask = F)

df <- data.frame("model1"=c(s1$r.squared, s1$sigma), "model2"=c(s2$r.squared, s2$sigma))
rownames(df) <- c("R-squared","Res. Stand. Err.")
df
