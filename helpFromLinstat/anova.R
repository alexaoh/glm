## Concrete example - one factor with five levels
# From Walpole, Myers, Myers, Ye, 9th ed., Table 13.1

# Does mean absorption of moisture in concrete ("betong") vary among five
# different concrete aggregates ("tilslag") (main part of concrete, e.g.
# mixtures of sand, gravel, stone, in addition to cement and water)?

## 11 March 2021

concrete.data <- read.csv("https://www.math.ntnu.no/emner/TMA4267/2021v/AGGREGATES.CSV",sep=";")
concrete.data

attach(concrete.data)

n <- length(moisture)
p <- length(unique(aggregate)) # or p <- length(table(aggregate))

origcoding<-getOption("contrasts")
origcoding
contr.treatment(p) # Dummy coding. Post-multiply by new parameters to get original

fit0 <-lm(moisture~as.factor(aggregate)) # dummy variables created automatically for factors
summary(fit0)
model.matrix(fit0)

options(contrasts=c("contr.sum","contr.sum"))
contr.sum(p) # Effect coding. Post-multiply by new parameters to get original

fit <-lm(moisture~as.factor(aggregate)) 
summary(fit)
model.matrix(fit)
fit0$fitted.values
fit$fitted.values # the same

# Manually:

x<-matrix(rep(0,150),nrow=30)
x[,1]<-1
for(j in 1:4) for(i in 1:30){
  if(aggregate[i]==j) x[i,1+j]<-1
  if(aggregate[i]==5) x[i,1+j]<--1
}
x
fit2<-lm(moisture~x[,-1]) # R adds the intercept
# lm(moisture~x-1) to avoid R adding a new intercept is dangerous:
# https://www.r-bloggers.com/2014/06/be-careful-with-using-model-design-in-r/
summary(fit2)
model.matrix(fit2)

demand<-BOD$demand
mm<-model.matrix(~Time,BOD)
summary(lm(demand~mm+0))

# Test of treatments effect:

restricted<-lm(moisture~1) # only intercept
sse0<-(n-1)*summary(restricted)$sigma^2
sse<-(n-p)*summary(fit)$sigma^2
f<-(sse0-sse)/(p-1)/(sse/(n-p))
pf(f,p-1,n-p,lower.tail=FALSE) # p-value

anova(restricted,fit) # easier
anova(fit) # even easier
anova(fit0) # dummy or effect coding doesn't matter
ssr<-summary(fit)$r.squared*(n-1)*var(moisture) # find sse and ssr in anova output

## Machine operator example

ds <- matrix(c(42.5,39.8,40.2,41.3,
               39.3,40.1,40.5,42.2,
               39.6,40.5,41.3,43.5,
               39.9,42.3,43.4,44.2,
               42.9,42.5,44.9,45.9,
               43.6,43.1,45.1,42.3),ncol=6,nrow=4)
dsmat <- data.frame(cbind(c(ds),rep(1:6,each=4),rep(1:4,6)))
colnames(dsmat) <- c("time","operator","machine")
dsmat # time to assemble product, 6 operators, 4 machines

fit <- lm(time~as.factor(machine),data=dsmat) # reduced model
summary(fit)

fit2 <- lm(time~as.factor(machine)+as.factor(operator),data=dsmat) # full model
summary(fit2)
anova(fit,fit2) # effect of operator: p-value 0.005328
anova(fit2) # also get effect of machine

## Age and memory data

ds <- read.csv("https://www.math.ntnu.no/emner/TMA4267/2018v/eysenck.txt",sep="\t")
ds
# Number of words remembered out of 27 words. 2 age groups, 5 methods
# (processes) for remembering
ds[1:15,]
attach(ds)

labnames <- c("OA","YA","OC","YC","OIm", "YIm", "OIn","YIn","OR","YR")
boxplot(Words~Age*Process,col=rep(c(2,4),5),names=labnames)

summary(lm(Words~Age+Process+Age:Process)) # in model formulas: ':' generates interactions
res <- lm(Words~Age*Process) # '*' generates both main effects and interactions
summary(res)
model.matrix(res)[1:15,]

anova(res)

## Came here 11 March 2021

## Exam 2018 Problem 3

set.seed(1)
n<-15
x<-c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))
x<-as.factor(x)
mu<-c(.7,0,1,0)
y<-rnorm(4*n,mu[x])
fit<-lm(y~x-1) # -1: no intercept
model.matrix(fit)
summary(fit)

c<-matrix(c(0,0,1,-1),byrow=TRUE,nrow=1)
r<-1 # rank of c
p<-4
f<-t(c%*%fit$coefficients)%*%solve(c%*%solve(t(model.matrix(fit))%*%model.matrix(fit))%*%t(c))%*%
  c%*%fit$coefficients/r/
  summary(fit)$sigma^2
f
pf(f,r,4*n-p,lower.tail=FALSE)

# The test performed for all pairs of coefficients:
for(i in 1:3) for(j in (i+1):4)
  cat(i,j,pf((fit$coeff[i]-fit$coeff[j])^2*30/4/(43.04524/56),r,4*n-p,lower=F),"\n")
# Another way to perform these tests, not using linear regression model:
pairwise.t.test(y,x,p.adjust.method="none")

# Bonferroni-adjusted significance level
.05/6
# Null hypotheses 4 and 6 should be rejected

# Alternative: Adjust p-values:
pairwise.t.test(y,x,p.adjust.method="bonf")
6*pairwise.t.test(y,x,p.adjust.method="none")$p.value

# This gives different results since variance estimate is based only on half the observations:
t.test(y[x==3],y[x==4],var.equal=TRUE)
