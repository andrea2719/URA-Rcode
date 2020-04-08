
## Figure 5.1
x = seq(0.2, 10, .1)
EY1 = 0 + 1*log(x); EY2 = 2 - .5*log(x); EY3 = 1 + 2*log(x)
plot(x, EY1, type="l", ylim = c(-2,5), lty=1, ylab = "E(Y | X=x)")
points(x, EY2, type="l", lty=2); points(x, EY3, type="l", lty=3)
abline(h=0, col="gray"); abline(v=1, lty=1, col="gray")
legend("bottomright", c("b0= 0, b1= 1", "b0= 2, b1=-.5",
"b0= 1, b1= 2"), lty = c(1,2,3), cex=0.8)

## Figure 5.2
x = seq(0.2, 10, .1)
EY1 = 0 + 1*x^-1
EY2 = 2 - .5*x^-1
EY3 = 1 + 2*x^-1
plot(x, EY1, type="l", lty=1, ylab = "E(Y | X=x)", ylim = c(-1,6))
points(x, EY2, type="l", lty=2)
points(x, EY3, type="l", lty=3)
legend("topright", c("b0= 0, b1= 1", "b0= 2, b1=-.5",
"b0= 1, b1= 2"), lty = c(1,2,3), cex = 0.8)

## Section 5.1.2 (Figure 5.3)
CarS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(CarS)
fit.linear.x = lm(NSOLD ~ INTRATE)
ln.INTRATE = log(INTRATE) ; fit.ln.x = lm(NSOLD ~ ln.INTRATE)
inv.INTRATE = INTRATE^-1 ; fit.inv.x = lm(NSOLD ~ inv.INTRATE)
logLik(fit.linear.x); logLik(fit.ln.x); logLik(fit.inv.x)
summary(fit.inv.x)
b0 = fit.inv.x$coefficients[1]; b1 = fit.inv.x$coefficients[2]
x = seq(5, 20, .01); y.hat = b0 + b1*x^-1
plot(INTRATE, NSOLD); points(x, y.hat, type="l")

## Figure 5.4
## Graphically assessing the inverse interest rate transformed model
par(mfrow=c(1,2))
plot(inv.INTRATE, NSOLD); abline(lsfit(inv.INTRATE, NSOLD))
add.loess(inv.INTRATE, NSOLD, col = "gray", lty=2)
fit = lm(NSOLD~inv.INTRATE)
y.hat = fit$fitted.values; resid = fit$residuals
plot(y.hat, resid); add.loess(y.hat, resid, col = "gray", lty=2)
abline(h=0)

## Testing curvature in the transformed model
inv.INT.squared = inv.INTRATE^2
fit.quad = lm(NSOLD~inv.INTRATE+inv.INT.squared)
summary(fit.quad)

## Figure 5.5
x = seq(-1, 4, .1)
E.lnY1 = 0+.5*x; MY1 = exp(E.lnY1)
E.lnY2 = 2 - .5*x; MY2 = exp(E.lnY2)
E.lnY3 = -1 + .9*x; MY3 = exp(E.lnY3)
plot(x, MY1, type="l", lty=1, ylab = "Median of Y|X=x", ylim=c(0,15))
points(x, MY2, type="l", lty=2)
points(x, MY3, type="l", lty=3)
legend(1,14, c("b0= 0, b1= .5", "b0= 2, b1= -.5", "b0= -1,
  b1= 0.9"), lty = c(1,2,3))

## Section 5.2.1
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(charity)
ln.AGI = log(Income.AGI)
fit = lm(ln.AGI ~ DEPS)
summary(fit)
confint(fit)

## Figure 5.6
par(mfrow=c(2,2))
plot(DEPS, Income.AGI)
add.loess(DEPS, Income.AGI)
qqnorm(lsfit(DEPS, Income.AGI)$residuals)
qqline(lsfit(DEPS, Income.AGI)$residuals)
ln.AGI = log(Income.AGI)
plot(DEPS, ln.AGI)
add.loess(DEPS, ln.AGI)
qqnorm(lsfit(DEPS, ln.AGI)$residuals)
qqline(lsfit(DEPS, ln.AGI)$residuals)

## Section 5.2.2 (Figure 5.7)
n = 100; set.seed(12345)
X = runif(n); Y = 20000 + 2000*X + rnorm(n, 0, 500)
par(mfrow=c(1,2)); plot(X,Y)
## W = Y in thousands
W = Y/1000; plot(X,W)
fit.orig = lm(Y~X); fit.trans = lm(W~X)
LL.orig = logLik(fit.orig); LL.trans = logLik(fit.trans)
LL.orig; LL.trans
LL.trans - LL.orig
-(n/2)*log(1/1000^2)

## Section 5.2.3
fit.orig = lm(Income.AGI ~ DEPS); fit.trans = lm(ln.AGI ~ DEPS)
LL.orig = logLik(fit.orig)
LL.trans = logLik(fit.trans) - sum(log(Income.AGI))
LL.orig; LL.trans; LL.trans - LL.orig

## Figures 5.9 and 5.10
n = 100; beta0 = -2.00; beta1 = 0.05; sigma = 0.30
set.seed(12345); X = rnorm(n, 70, 10)
lnY = beta0 + beta1*X + rnorm(n, 0, sigma); Y= exp(lnY)

par(mfrow=c(1,2)); plot(X,Y); abline(v=c(60,80), col="gray")
plot(X, lnY); abline(v=c(60,80), col="gray")
y.seq = seq(0.01,30,.01)
dy1 = dlnorm(y.seq, beta0+beta1*60, sigma)
dy2 = dlnorm(y.seq, beta0+beta1*80, sigma)

par(mfrow=c(1,2))
plot(y.seq, dy1, type="l", xlim = c(0,20), yaxs="i", ylim = c(0, .6),
ylab="lognormal density", xlab = "Untransformed y")
points(y.seq, dy2, type="l", lty =2)
legend("topright", c("X=60", "X=80"), lty=c(1,2), cex=0.8)
ly.seq = log(y.seq)
dly1 = dnorm(ly.seq, beta0+beta1*60, sigma)
dly2 = dnorm(ly.seq, beta0+beta1*80, sigma)
plot(ly.seq, dly1, type="l", xlim = c(-.2,4), yaxs="i", ylim = c(0,1.6),
ylab="normal density", xlab = "Log Transformed y")
points(ly.seq, dly2, type="l", lty=2)
legend("topright", c("X=60", "X=80"), lty=c(1,2), cex=0.8)

## Figure 5.11
comp = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/compspeed.txt")
attach(comp)
reg.orig = lm(time ~ GB); summary(reg.orig)
par(mfrow=c(2,2)); plot(GB, time); add.loess(GB, time)
qqnorm(reg.orig$residuals); qqline(reg.orig$residuals)
speed = 1/time; reg.trans = lm(speed ~ GB)
summary(reg.trans)
plot(GB, speed); add.loess(GB, speed)
qqnorm(reg.trans$residuals); qqline(reg.trans$residuals)

## Section 5.5 Box-Cox analysis (Figure 5.12)
library(MASS)
par(mfrow=c(2,2))

## Production Cost Data
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
fit = lm(Cost ~ Widgets, data=ProdC); boxcox(fit)

## Charity Data: Income versus dependents
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
fit = lm(Income.AGI ~ DEPS, data=charity); boxcox(fit)

## Computer time versus RAM
comp = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/compspeed.txt")
fit = lm(time ~ GB, data=comp); boxcox(fit)

## Simulated: lambda = 0 (log transform) is known to be correct.
n = 100; beta0 = -2.00; beta1 = 0.05; sigma = 0.30
set.seed(12345)
X = rnorm(n, 70, 10)
lnY = beta0 + beta1*X + rnorm(n, 0, sigma); Y = exp(lnY)
fit = lm(Y ~ X); boxcox(fit)

## Figure 5.13
Cr=read.table("https://raw.githubusercontent.com/andrea2719/URA-DataSets/
master/crimes_2012.txt")
attach(Cr); par(mfrow=c(2,2))
plot(Tot_Crimes ~ Pop_Cov)
lcrimes = log(Tot_Crimes); lpop = log(Pop_Cov)
plot(Tot_Crimes~lpop); plot(lcrimes~Pop_Cov); plot(lcrimes~lpop)
summary(lm(lcrimes~lpop))

## Figure 5.14
par(mfrow=c(1,1))
plot(Tot_Crimes ~ Pop_Cov)
pop.seq = 0:10000000
median.estimate = exp(-4.85116) * pop.seq^1.03531
points(pop.seq, median.estimate, type="l")

## Exercises
Firms = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Firms.csv")