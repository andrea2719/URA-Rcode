
## Figure 12.1 (need to run add.loess function first)
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth)
fit = lm(P.assets ~ Age)
abs.resid = abs(fit$residuals)
plot(Age, abs.resid); add.loess(Age, abs.resid)
add.loess(Age, abs.resid, span=2, lty=2)

## Section 12.1
fit.0 = lm(P.assets ~ Age, weights=1/Age^0)
summary(fit.0); logLik(fit.0)
fit.1 = lm(P.assets ~ Age, weights=1/Age^1)
summary(fit.1); logLik(fit.1)
fit.2 = lm(P.assets ~ Age, weights=1/Age^2)
summary(fit.2); logLik(fit.2)

## Figure 12.2
par(mfrow=c(2,2))
par(mar=c(4, 4, 1, 4))
plot(Age, P.assets, xlab=""); abline(-6.25592, 0.27928, lty=1)
abline(-5.87593, 0.26825, lty=2); abline(-5.65151, 0.26137, lty=3)

xlist = seq(20, 70, .1)
Low0 = -6.2559+0.27928*xlist - 2*1.651
Upp0 = -6.2559+0.27928*xlist + 2*1.651
plot(Age, P.assets, ylab="", xlab="", ylim = c(-5, 20))
points(xlist, Low0, type="l", lty=1); points(xlist, Upp0, type="l",
lty=1)

Low1 =-5.87593+0.268255*xlist - 2*.2691*xlist^(1/2)
Upp1 =-5.87593+0.268255*xlist + 2*.2691*xlist^(1/2)
plot(Age, P.assets,ylim = c(-5, 20))
points(xlist, Low1, type="l"); points(xlist, Upp1, type="l")
Low2 = -5.65151+0.26137*xlist - 2*.0448*xlist
Upp2 = -5.65151+0.26137*xlist + 2*.0448*xlist
plot(Age, P.assets,ylab="",ylim = c(-5, 20))
points(xlist, Low2, type="l"); points(xlist, Upp2, type="l")

## Figure 12.3
par(mfrow=c(1,2))
n=500
set.seed(123)
x = 5*rexp(n)
y = 2*x + x*rnorm(n)
## The weight is inversely proportional to the conditional variance
w = 1/x^2 

fit.ols = lm(y ~ x)
ols.pred = fit.ols$fitted.values
fit.wls = lm(y ~ x, weights = w)
wls.pred = fit.wls$fitted.values
## Scatterplot and fitted functions
plot(x, y, pch=".", cex=1.5, ylim=c(-5,120))
points(sort(x), sort(ols.pred), type="l", lty=2)
points(sort(x), sort(wls.pred), type="l", lty=1)
legend("topleft", c("WLS", "OLS"), lty = c(1,2))
## Fitted OLS and WLS lines and data where variance is small
plot(x[x<1], y[x<1], ylim = c(-1,4))
points(sort(x[x<1]), sort(ols.pred[x<1]), type="l", lty=2)
points(sort(x[x<1]), sort(wls.pred[x<1]), type="l", lty=1)
legend("topleft", c("WLS", "OLS"), lty = c(1,2))

## Section 12.2.1 (Figure 12.4)
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth); n = nrow(Worth); X = Age

## Conditional-x heteroscedastic simulation model parameters
gamma = 0.05; beta0 = -6; beta1 = 0.25

## Simulation study to understand efficiency of WLS versus OLS
NSIM = 20000
b1.ols = numeric(NSIM); b1.wls = numeric(NSIM)
for (i in 1:NSIM) {
Y = beta0 + beta1*X + gamma*X*rnorm(n)
b1.ols[i] = lm(Y ~ X)$coefficients[2]
b1.wls[i] = lm(Y ~ X, weights=1/X^2)$coefficients[2]
}

par(mfrow=c(2,1))
hist(b1.ols, xlim = c(.10,.40), main = "Distribution of OLS Estimates")
hist(b1.wls, xlim = c(.10,.40), main = "Distribution of WLS Estimates")
sd(b1.ols); sd(b1.wls)

## Section 12.3
ba = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
attach(ba)
y = gpa; x1 = gmat
x2 = ifelse(degree=="P", 1,0)

library(maxLik)
loglik <- function(param) {
b0 = param[1]
b1 = param[2]
b2 = param[3]
g0 = param[4]
g1 = param[5]
g2 = param[6]
mean = b0 + b1*x1 + b2*x2
ln.sd = g0 + g1*x1 + g2*x2 ; sd = exp(ln.sd)
z = (y - mean)/sd
ll = sum(dnorm(z,log=T) - ln.sd)
ll
}

fit = maxLik(loglik, start=c(lm(y~x1+x2)$coefficients,0,0,0))
summary(fit)

## assuming constant variance
library(maxLik)
loglik0 <- function(param) {
b0 = param[1]
b1 = param[2]
b2 = param[3]
g0 = param[4]
mean = b0 + b1*x1 + b2*x2
ln.sd = g0; sd = exp(ln.sd)
z = (y - mean)/sd
ll = sum(dnorm(z,log=T) - ln.sd)
ll
}
fit0 = maxLik(loglik0, start=c(lm(y~x1+x2)$coefficients,0))
summary(fit0)

## Figure 12.5
g = fit$estimate; g0 = g[4]; g1 = g[5]; g2 = g[6]
gmat.g = seq(400,800,10)
s.phd = exp(g0 + g1*gmat.g + g2)
s.masters = exp(g0 + g1*gmat.g)

plot(gmat.g,s.phd, type="l", ylim = c(0,.4), 
 ylab="Estimated Standard Dev. of GPA", xlab="GMAT Score")
points(gmat.g,s.masters, type="l", lty=2)
abline(h = exp(fit0$estimate[4]), lwd=2)
legend("bottomright", c("Ph.D.", "Masters"), lty = c(1,2))
lm(y ~ x1 + x2)$ coefficients
summary(lm(y ~ x1 + x2))$sigma
AIC(fit0)
AIC(fit)

## Likelihood ratio test
LL = fit$maximum
LL0 = fit0$maximum
Chi.sq = 2*(LL - LL0)
Chi.sq
pval = 1- pchisq(Chi.sq, 6-4)
pval

## using gamlss library
library(gamlss)
mod=gamlss(y~x1+x2,sigma.fo=~x1+x2, data=ba) 
summary(mod)

## Section 12.5.1 (Figure 12.6)
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth); n = nrow(Worth); X = Age;

## Conditional-x heteroscedastic simulation model parameters:
gamma = 0.05; beta0 = -6; beta1 = 0.25

## Simulation study to understand how well e2^2 estimates sigma2^2
NSIM = 20000
e2 = numeric(NSIM)
for (i in 1:NSIM) {
eps = gamma*X*rnorm(n)
Y = beta0 + beta1*X + eps
e2[i] = lm(Y ~ X)$residuals[2]
}
hist(e2^2, main="", breaks=50, xlab="Squared Second Residual")
abline(v=(0.05*43)^2, lty = 1, lwd=2)
abline(v=mean(e2^2), col="gray", lty=2, lwd=2)

## HC standard errors
ba = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
attach(ba)
y = gpa
x1 = gmat
x2 = ifelse(degree=="P", 1,0)
library(car)
fit = lm(y ~ x1 + x2)

## Homoscedastic analysis
summary(fit)$coefficients[,1:3]

## Heteroscedasticity-consistent analysis using method HC3
se.hc3 = sqrt(diag(hccm(fit, type = "hc3")))
b = fit$coefficients
t = b/se.hc3
cbind(b, se.hc3, t)

## Investigate the performance of HC
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth); n = nrow(Worth); X = Age;

## Conditional-x heteroscedastic simulation model parameters:
gamma = 0.05; beta0 = -6; beta1 = 0.25

## Conditional-x heteroscedastic simulation model
## Simulation study to estimate the true confidence level of ordinary
## and HC intervals
library(car)

NSIM = 10000
b1 = numeric(NSIM)
se.b1 = numeric(NSIM)
se.b1.robust = numeric(NSIM)

for (i in 1:NSIM) {
 eps = gamma*X*rnorm(n)
 Y = beta0 + beta1*X + eps
 fit = lm(Y~X)
 fit1 = summary(lm(Y~X))
 b1[i] = fit1$coefficients[2,1]
 se.b1[i] = fit1$coefficients[2,2]
 se.b1.robust[i] = sqrt(diag(hccm(fit, type = "hc3")))[2]
}
chk1 = (b1 - 2*se.b1 < beta1) *(b1 + 2*se.b1 > beta1)
mean(chk1)
chk2 = (b1 - 2*se.b1.robust < beta1) *(b1 + 2*se.b1.robust > beta1)
mean(chk2)

## Section 12.6
X = c(3.1, 4.3, 0.7); Y = c(1.2, 2.0, 1.3)
# Analysis based on n=3
XY.1 = data.frame(X,Y); summary(lm(Y~X, data=XY.1))
# Analysis based on n=6
XY.2 = rbind(XY.1,XY.1); summary(lm(Y~X, data=XY.2)) 
# Analysis based on n=12
XY.3 = rbind(XY.2,XY.2); summary(lm(Y~X, data=XY.3))
# Analysis based on n=24
XY.4 = rbind(XY.3,XY.3); summary(lm(Y~X, data=XY.4))
# Analysis based on n=48
XY.5 = rbind(XY.4,XY.4); summary(lm(Y~X, data=XY.5))

## Section 12.6.1
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(charity)
sub.f = as.factor(SUBJECT)
library(nlme)

# ML/GLS fit assuming classical sig^2*I covariance structure
fit.ols = gls(CHARITY ~ INCOME + DEPS + sub.f, method="ML")

# ML/GLS fit assuming block-diagonal AR1 covariances
fit.gls = gls(CHARITY ~ INCOME + DEPS + sub.f,
 corr=corAR1(form = ~1 | SUBJECT), method="ML")

summary(fit.ols)$tTable
summary(fit.ols)$sigma
summary(fit.gls)$tTable
summary(fit.gls)$sigma
fit.gls$modelStruct$corStruct
AIC(fit.ols)
AIC(fit.gls)

## Exercise 4
ge = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ge_ret_vol.csv")
