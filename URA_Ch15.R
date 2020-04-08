
## Call center data
time = c(4.5, 3.9, 1.0, 2.3, 4.8, 0.1, 0.3, 2.0, 0.8, 3.0, 0.3, 2.1)
answer = c( 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0)
mean(time)
mean(time[answer==1])

## Figure 15.1
time1 = seq(0,15,.01)
pdf.time = dexp(time1, 1/4) # The parameter is lambda = 1/mu
plot(time1, pdf.time, type="l", yaxs="i", ylim = c(0, 1.2*max(pdf.time)), 
 xaxs="i", xlim = c(0,15.5), xlab="time")
points(c(4.5,4.5), c(0, dexp(4.5,1/4)), type="l")
points(c(3.9,3.9), c(0, dexp(3.9,1/4)), type="l", lty=2)
x = seq(4.5, 15,.1); y = dexp(x,1/4)
col1 <- rgb(.2,.2,.2,.5)
polygon(c(x,rev(x)), c(rep(0,length(y)), rev(y)),
 col=col1, border=NA)

loglik <- function(lambda) {
answer*log(dexp(time, lambda)) + (1-answer)*log(1-pexp(time, lambda))
}
library(maxLik)
## The starting value, 0.5 here, is a "guess" of lambda.
results <- maxLik(loglik, start=c(0.5))
summary(results)

library(survival)
cens.fit = survreg(Surv(time, answer) ~ 1, dist = "exponential")
summary(cens.fit)
exp(cens.fit$coefficients)

## Repeat on analysis on Section 5.2.3
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(charity)
plot(DEPS, Income.AGI)
ln.char = log(Income.AGI)
fit.orig = lm(Income.AGI ~ DEPS)
fit.trans = lm(ln.char ~ DEPS)
LL.orig = logLik(fit.orig)
LL.trans = logLik(fit.trans) - sum(log(Income.AGI))
LL.orig; LL.trans; LL.trans - LL.orig

## using survreg
n = nrow(charity)
observed = rep(1,n)
fit.normal =survreg(Surv(Income.AGI, observed) ~ DEPS,
dist = "gaussian")
fit.lognormal =survreg(Surv(Income.AGI, observed) ~ DEPS,
dist = "lognormal")

## Section 15.1.1
divorce = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/divorce.txt")
attach(divorce)
head(divorce)
table(div)

## Figure 15.2
par(mfrow=c(2,1)); par(mar=c(4, 4, 2, 1))
hist(years[div=="Yes"], xlim = c(0,80), breaks=seq(0,80,2),
 xlab = "Years Until Divorce", main = "", ylim = c(0,150))
hist(years[div=="No"], xlim = c(0,80), breaks=seq(0,80,2),
 xlab = "Years Until Censoring", main = "")

library(survival)
div.ind = ifelse(div=="Yes",1,0)
educ = as.factor(heduc)
fit.lognormal =survreg(Surv(years, div.ind) ~ educ, dist = "lognormal")
summary(fit.lognormal)
table(educ)

## Figure 15.3
b = fit.lognormal$coefficients
b0 = b[1]; b1 = b[2]; b2 = b[3]
m1 = b0
m2 = b0+b1
m3 = b0+b2
s = fit.lognormal$scale
years.graph = seq(0,70,.1)
sdf.y1 = 1 - plnorm(years.graph, m1, s)
sdf.y2 = 1 - plnorm(years.graph, m2, s)
sdf.y3 = 1 - plnorm(years.graph, m3, s)
plot(years.graph, sdf.y1, type="l", ylim = c(0,1),
xlab="Years Married", ylab="Marriage Survival Probability")
points(years.graph, sdf.y2, type="l", lty=2)
points(years.graph, sdf.y3, type="l", lty=3)
abline(v=20, col="gray") 
legend("bottomright", c("< 12 Yrs Educ", 
 "12-15 Yrs Educ", ">15 Yrs Educ"), lty=c(1,2,3))

1-plnorm(20, m1, s)

div.Low.Ed = div[heduc=="< 12 years"]
GT.20.Low.Ed = years[heduc=="< 12 years"] > 20
table(div.Low.Ed, GT.20.Low.Ed)

## Section 15.2
divorce = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/divorce.txt")
library(survival)
divorce$SurvObj <- with(divorce, Surv(years, div == "Yes"))
cox.phrm = coxph(SurvObj ~ heduc, data = divorce)
cox.phrm

## Figure 15.4
divorce = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/divorce.txt")
library(survival)
divorce$SurvObj <- with(divorce, Surv(years, div == "Yes"))
cox.phrm = coxph(SurvObj ~ heduc, data = divorce)
plot(survfit(cox.phrm, newdata=data.frame(heduc="< 12 years"), 
 conf.int=0), main="Cox Survival Model", xlab = "Years Married",
 ylab="Survival Probability",ylim=c(0,1.0))
lines(survfit(cox.phrm, newdata=data.frame(heduc="12-15 years")
 ,conf.int=0), lty=2)
lines(survfit(cox.phrm, newdata=data.frame(heduc="16+ years")
 ,conf.int=0), lty=3)
legend("bottomright", c("< 12 Yrs Educ", "12-15 Yrs Educ", 
 ">15 Yrs Educ"), lty=1:3)

## Figure 15.5
Y.list = seq(0,10, .01)
d1 = dnorm(Y.list, -2, 3)
d2 = dnorm(Y.list, 1, 3)
par(mfrow=c(1,2))
plot(Y.list, d1, type="l", yaxs="i", ylim = c(0,.8),
 xlab="Lawsuit Payout (in Millions of $),y", 
 ylab = "p(y | X = Low)", xlim = c(-.5,10))
points(c(0,0), c(0, pnorm(0, -2,3)), type="l", lwd=2, col="gray")
plot(Y.list, d2, type="l", yaxs="i", ylim = c(0,.8),
 xlab="Lawsuit Payout (in Millions of $),y",
 ylab = "p(y | X = High)", xlim = c(-.5,10))
points(c(0,0), c(0, pnorm(0, 1,3)), type="l", lwd=2, col="gray")

## Figure 15.6
X = rnorm(2000, 2.5,1)
Z = -3 + 1*X + rnorm(1000, 0, 3)
plot(X,Z, col="gray", xlab = "Net Profits", 
 ylab="Litigation Award (Millions of $)")
Y = ifelse(Z<0,0,Z)
points(X,Y)
abline(v=c(1,4), lwd=2)
abline(-3,1, lty=2)

## Section 15.3.1 (Figure 15.7)
back = read.csv ("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/liles.csv")
Y = back$dayslost
X = back$alr
plot(jitter(X), Y, xlab="Index of Lower Back Stress", ylab="Days Lost")

fit.normal <- survreg(Surv(Y, Y>0, type='left') ~ X, dist='gaussian')
summary(fit.normal)

## Figure 15.8
ylist = seq(-300, 200, .1)
d1 = dnorm(ylist, -112.25 + 14.71*.5, 59)
d2 = dnorm(ylist, -112.25 + 14.71*3, 59)
par(mfrow=c(1,2))
plot(ylist, d1, type="l", yaxs="i", ylim = c(0,.008), xaxt="n",
 xlab = "Days Lost, y", ylab = "p(y | Stress Index = 0.5)")
axis(1, at = c(0,50,100,150,200), lab=c(0,50,100,150,200))
x1 = seq(-300, 0,.1); y1 = dnorm(x1,-112.25 + 14.71*.5, 59)
col1 <- rgb(.2,.2,.2,.5)
polygon(c(x1,rev(x1)), c(rep(0,length(y1)), rev(y1)), col=col1,
 border=NA)
plot(ylist, d2, type="l",yaxs="i", ylim = c(0,.008), xaxt="n",
 xlab = "Days Lost, y", ylab = "p(y | Stress Index = 3.0)")
axis(1, at = c(0,50,100,150,200), lab=c(0,50,100,150,200))
x2 = seq(-300, 0,.1); y2 = dnorm(x1,-112.25 + 14.71*3, 59)
col1 <- rgb(.2,.2,.2,.5)
polygon(c(x2,rev(x2)), c(rep(0,length(y2)), rev(y2)), col=col1,
 border=NA)

## comparing fits
AIC(fit.normal)
fit.t <- survreg(Surv(Y, Y>0, type='left') ~ X, dist='t')
AIC(fit.t)
fit.log <- survreg(Surv(Y, Y>0, type='left') ~ X, dist='logistic')
AIC(fit.log)



