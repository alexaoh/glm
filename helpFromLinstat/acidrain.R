# How does pH in Norwegian lakes depend on sulphate, nitrate, calcium, aluminium
# and organic content (x1, ..., x5), area of lake (x6) and location (x7 = 0,
# Telemark, or x7 = 1, Trøndelag)? Data from Statens forurensningstilsyn (1986).
# Here 26 random lakes from Telemark and Trøndelag out of 1005 lakes have been
# drawn

## 12 February 2021

acidrain <- read.table("https://www.math.ntnu.no/emner/TMA4267/2021v/acidrain.txt",header=TRUE)
acidrain

y<-acidrain$y # responses
y
n<-length(y)
n
x<-acidrain[,2:8] # design matrix
x # intercept missing
x<-cbind(rep(1,n),x)
x # now with intercept
names(x)[1] <- 1 # nicer name for intercept
x<-as.matrix(x)
x

## coefficient estimates
bhat<-solve(t(x)%*%x)%*%t(x)%*%y
# t: transpose; %*%: matrix multiplication; solve: invert
# y is promoted to column matrix to make arguments conformable in last multiplication
bhat

## By R function:
fit <- lm(y~.,data=acidrain) # lm: linear model
# or
attach(acidrain)
fit<-lm(y~x1+x2+x3+x4+x5+x6+x7)
# 1 is added by R as a covariate for both alternatives
summary(fit) # see "Estimate" column
summary(lm(y~x))
# Dangerous! See https://www.r-bloggers.com/2014/06/be-careful-with-using-model-design-in-r/
fit$coefficients # extract coefficient estimates
model.matrix(fit) # another way to obtain design matrix

## 18 February 2021

## Predictions
pred<-x%*%fit$coefficients
pred
h <- x%*%solve(t(x)%*%x)%*%t(x) # hat matrix
h%*%y # the same as pred
fit$fitted.values # from the lm object

## Residuals
y-pred # or
(diag(n)-h)%*%y # (diag(n) is identity matrix) or
fit$residuals

## SSE
p<-8 # no. of coefficients
sse<-sum(fit$residuals^2)
sse # or
y%*%(diag(n)-h)%*%y # (diag(n) is identity matrix) or
(n-p)*(summary(fit)$sigma)^2 # (see below)

## sigma^2 estimate
s2<-sse/(n-p)
s2
sqrt(s2)
summary(fit) # see "Residual standard error on …" n - p = 18 "… degrees of freedom"
summary(fit)$sigma

## Standard errors of coefficient estimates
sqrt(s2*diag(solve(t(x)%*%x))) # diag picks out diagonal
summary(fit) # see "Std. Error" column
summary(fit)$coefficients[,"Std. Error"]
summary(fit)$coefficients[,2]

## Inference about coefficients
c<-diag(solve(t(x)%*%x)) # needed to do inference about the coefficients
sqrt(s2*c) # same as "Std. Error" column of summary(fit)
bhat/(sqrt(s2*c)) # same as "t value" column of summary(fit)
2*pt(abs(bhat)/(sqrt(s2*c)),n-p,lower.tail = FALSE)
# pt = cdf of t-distribution. Same as "Pr(>|t|)" column of summary(fit)
t<-qt(.975,n-p) # upper 0.025 quantile of t with df = n - p = 18
bhat[2]-t*sqrt(s2*c[2]) # lower bound 95% conf. int. for coeff. for sulphate
bhat[2]+t*sqrt(s2*c[2]) # upper bound 95% conf. int. for coeff. for sulphate
# Indexing starts at 1 in R, so bhat[2] is estimate of what is usually called beta_1
# 0 not in c.i. agrees with rejection of H0: beta_1 = 0 (se Pr(>|t|) for x1)
confint(fit)

## 19 February 2021

## R^2
sst<-(n-1)*var(y)
sst # or
sum((y-mean(y))^2)
1-sse/sst
# same as "Multiple R-squared" in summary(fit)

## F-test of model
ssr<-sst-sse
f<-ssr/(p-1)/(sse/(n-p))
f
pf(f,p-1,n-p,lower.tail=FALSE)
# Both agree with summary(fit) (bottom row)

## Studentized residuals - 3 versions in Fahrmeir et al. p. 126 are equal
# (only for the especially interested)
r<-studres1<-studres2<-studres3<-rep(NA,n)
for(i in 1:n){
  x_i<-x[-i,]; y_i<-y[-i]
  fit_i<-lm(y_i~x_i-1) # also possible to specify model by design matrix
  # must remove the intercept that R adds automatically, though
  # Dangerous! See https://www.r-bloggers.com/2014/06/be-careful-with-using-model-design-in-r/
  res_i<-y[i]-x[i,]%*%fit_i$coefficients
  s_i<-summary(fit_i)$sigma
  studres1[i]<-res_i/(s_i*sqrt(1+x[i,]%*%solve(t(x_i)%*%x_i)%*%x[i,]))
  studres2[i]<-fit$residuals[i]/(s_i*sqrt(1-h[i,i]))
  r[i]<-fit$residuals[i]/(summary(fit)$sigma*sqrt(1-h[i,i])) # standardized residual
  studres3[i]<-r[i]*sqrt((n-p-1)/(n-p-r[i]^2))
}
cbind(r,studres1,studres2,studres3)

## Standardized and studentized residuals by built-in R functions
rstandard(fit)
rstudent(fit)

## 26 February 2021

## Check of model assumptions

# Scatter plots
pairs(acidrain) # scatter plots - note esp. first row

# Residual plot
plot(fit$fitted.values,fit$residuals) # residual plot using raw residuals
rres <- rstudent(fit) # studentized residuals
plot(fit$fitted.values,rres) # residual plot - seems OK

# If not OK (but here it was), plot each x vs. studentized residuals, e.g.,
plot(acidrain$x5,rres)

# Normality of residuals
qqnorm(rres)
qqline(rres)
# They really are t-distributed, but when df is high, t is close to normal
qqplot(qt(1:n/(n+1),n-p-1),rres)
# install.packages("nortest")
library(nortest)
ad.test(rres) # Anderson-Darling test
# normality test not used in general - low power for small samples, and small
# deviations give rejections for large samples

# Kolmogorov-Smirnov tests
ks.test(rres,"pt",26-8-1) # residuals from t(17)?
ks.test(rres,"pnorm") # residuals from N(0,1)?

# built-in plots
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

## 4 March 2021

## We can start from here (no need to read previous lines). We first read the data again:
acidrain <- read.table("https://www.math.ntnu.no/emner/TMA4267/2021v/acidrain.txt",header=TRUE)

## Model selection with Cp and R^2adj

# install.packages("leaps")
library(leaps)

# Best subset

best <- regsubsets(y~.,data=acidrain)
sumbest <- summary(best)
sumbest
# (stars in each line indicate M_k)
# Note that x4 is in M1 but drops out of M2, ..., M5.
sumbest$outmat # only the table
sumbest$which # table in another format
# Or use regsubsets' plot method:
plot(best,scale="Cp")

names(sumbest) # choices when proceeding
sumbest$cp # the C_p's of the 7 models
# Note that the model having no covariates except intercept is not evaluated by regsubsets.

# RSS (SSE) or R^2 no good for selecting model due to monotonical improvement
plot(sumbest$rss,type="l")
plot(sumbest$rsq,type="l")

# Try adjusted R^2 and Mallows' C_p:
plot(sumbest$adjr2)
lines(sumbest$adjr2) # add lines to see better
plot(sumbest$cp)
lines(sumbest$cp)
which.max(sumbest$adjr2) # 5
which.min(sumbest$cp) # 3
# look at sumbest again to see models 3 and 5
# compare p-values from summary of lm object:
fit <- lm(y~.,data=acidrain) # full model
summary(fit)

## To compute C_p and adjusted R^2 manually, e.g. for model 2 - just to see that regsubsets and we use the same definitions:

fit13 <- lm(y~x1+x3,data=acidrain)
y <- acidrain$y
sse <- sum((y-fit13$fitted.values)^2)
sigmahat<-summary(fit)$sigma
n <- length(y)
p <- 3 # intercept and two predictors
sse/sigmahat^2-n+2*p # C_p
sumbest$cp # the two agree

1-sse/(n-p)/(sum((y-mean(y))^2)/(n-1)) # adjusted R^2
sumbest$adjr2 # agreement again
summary(fit) # and agreement (last entry of sumbest$adjr2) with "Adjusted R-squared" in lm object

## transformations

library(MASS)
boxcox(fit)
boxcox(fit,lambda=seq(-5,5,.1))

## 5 March 2021

## Start from here (no need to read previous lines).
acidrain <- read.table("https://www.math.ntnu.no/emner/TMA4267/2019v/acidrain.txt",header=TRUE)
fit <- lm(y~.,data=acidrain)

## Hypothesis test, H_0: c %*% beta = 0. 3.3 in Fahrmeir et al.

# Method 1:
summary(fit)
c<-matrix(c(0,0,1,0,0,0,0,0,
            0,0,0,0,1,0,0,0,
            0,0,0,0,0,1,0,0,
            0,0,0,0,0,0,1,0,
            0,0,0,0,0,0,0,1),byrow=TRUE,nrow=5)
r<-5 # rank of c
p<-8
n<-26
f<-t(c%*%fit$coefficients)%*%solve(c%*%solve(t(model.matrix(fit))%*%model.matrix(fit))%*%t(c))%*%
  c%*%fit$coefficients/r/
  summary(fit)$sigma^2
f
pf(f,r,n-p,lower.tail=FALSE)

# Method 2, using SSE og full and restricted model
restricted<-lm(y~x1+x3,data=acidrain)
sse_res<-(n-(p-r))*summary(restricted)$sigma^2 # n - (p - r) = n - 3
sse_full<-(n-p)*summary(fit)$sigma^2
f_restr<-(sse_res-sse_full)/r/(sse_full/(n-p))
f_restr # same as f above

# Easy way in R:
anova(restricted,fit)  
