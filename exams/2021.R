# Practical part of exam 2021 (practice for exam 20.01.22)

germ.temp <- c(rep(c(11,11,11), 3),rep(c(21,21,21), 3))
moist.level <- c(rep("low", 3), rep("med", 3), rep("high",3),
                 rep("low", 3), rep("med", 3), rep("high",3))
storage.temp <- rep(c(21,42,62), 6)
Y <- c(98,96,62,94,79,3,92,41,1,
       94,93,65,94,71,2,91,30,1)

df <- data.frame(germ.temp = as.factor(germ.temp), moist.level = as.factor(moist.level), 
                 storage.temp = as.factor(storage.temp), Y = Y)
str(df)
head(df)

model1 <- glm(cbind(Y, 100-Y)~.+moist.level*storage.temp, family = binomial, data = df)
summary(model1)

# Maximum standardized deviance residual.
max(rstandard(model1))

# Prediction.
pih<-predict(model1,data.frame(moist.level="med", germ.temp="11", storage.temp="42"),type="response")
pih

# Pearson.
PS<-sum(residuals(model1,type="pearson")^2)
PS

# Estimation of dispersion parameter. 
PS/model1$df.res

# Include more terms and fit new model
model2 <- glm(cbind(Y, 100-Y)~.+moist.level*storage.temp+germ.temp*storage.temp, family = binomial, data = df)
summary(model2)

# Which model is best?
D <- summary(model1)$deviance - summary(model2)$deviance
D > qchisq(0.95, df = 2)
# Since H0 is not rejected, we conclude that model1 is better than model2.
