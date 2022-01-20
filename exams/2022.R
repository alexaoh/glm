# Code from part 2 of exam 2022.
library(dplyr)
setwd("/home/ajo/gitRepos/glm/exams")
data <- read.csv2("DadesFinalPie2-B-2022.csv")

head(data)
str(data)
summary(data)
data$Especie <- as.factor(data$Especie)

#cbind(data[data$Especie == "Exclamationis", ], data[data$Especie == "Niveus", ]


# 1)
boxplot(data[data$Especie == "Niveus", "Pulsacions"])
boxplot(data[data$Especie =="Exclamationis", "Pulsacions"])
boxplot(data[,"Pulsacions"])
boxplot(Pulsacions~Especie, data)

#2) 
cor(data$Pulsacions, data$Temp)

#3) 
mean(data[data$Especie == "Exclamationis", "Pulsacions"])
var(data[data$Especie == "Exclamationis", "Pulsacions"])
mean(data[data$Especie == "Niveus", "Pulsacions"])
var(data[data$Especie == "Niveus", "Pulsacions"])

#4) 
slm <- lm(Pulsacions~Temp, data = data)
summary(slm)
sse <- sum((fitted(slm) - data$Pulsacions)^2)
ssr <- sum((fitted(slm) - mean(data$Pulsacions))^2)
sst <- sse+ssr
sst

#5) 
4.15

#6) Done on paper.

#7 
plot(slm, ask = F)
par(mfrow = c(2,2))
plot(slm, ask = F)
par(mfrow = c(1,1))
# Largest leverage value:
max(hatvalues(slm))
summary(hatvalues(slm))
# Influence?
summary(cooks.distance(slm))
4/31

#8)
plot(rstandard(slm), ylim = c(-3,3))
abline(h=c(-1.96,0,1.96),lty=2)
summary(rstandard(slm))

#9)
addmod <- lm(Pulsacions~Temp+Especie, data = data)
summary(addmod)
sse <- sum((fitted(addmod) - data$Pulsacions)^2)
ssr <- sum((fitted(addmod) - mean(data$Pulsacions))^2)
sst <- sse+ssr
sst
ssr/sst

#10)

#11)

#12)

#13)
plot(addmod, ask = F)
par(mfrow = c(2,2))
plot(slm, ask = F)
par(mfrow = c(1,1))
# Largest leverage value:
max(hatvalues(addmod))
summary(hatvalues(addmod))
# Influence?
summary(cooks.distance(addmod))
4/31

#14)
plot(rstandard(addmod), ylim = c(-3,3))
abline(h=c(-1.96,0,1.96),lty=2)
summary(rstandard(addmod))
rst <- rstandard(addmod)
sum(rst>1.96) + sum(rst<(-1.96))

#15)
fact <- lm(Pulsacions~Temp*Especie, data = data)
summary(fact)

#17) Comments:
plot(fact, ask = F)
par(mfrow = c(2,2))
plot(slm, ask = F)
par(mfrow = c(1,1))
# Largest leverage value:
max(hatvalues(fact))
summary(hatvalues(fact))
# Influence?
summary(cooks.distance(fact))
4/31
plot(rstandard(fact), ylim = c(-3,3))
abline(h=c(-1.96,0,1.96),lty=2)
summary(rstandard(fact))

#18

#19)
new.data <- data.frame(Especie = "Exclamationis", Temp = 27)
predict(addmod, new.data, interval = "confidence")
predict(addmod, new.data, interval = "prediction")
