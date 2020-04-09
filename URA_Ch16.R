
## Figure 16.1
par(mfrow=c(1,2))
v = c(2.31, 1.28, 2.15, 23.02, 3.02, 1.10)
plot(v, rep(0,6), ylab="", yaxt="n")
points(23.02, 0, pch=16)
w = c(231, 128, 215, 322, 302, 110)
plot(w, rep(0,6), ylab="", yaxt="n")
points(322, 0, pch=16)

scale(v)
scale(w)

##
set.seed(12345)
Most.data = runif(10000)
Outlier.data.value = 1000
z.all = scale(c(Most.data, Outlier.data.value))
z.all[10001]

## Figure 16.2
set.seed(12345)
W = rnorm(100, 70,10)
U = .05*W + rnorm(100,0,.1)
W = c(W,80); U = c(U,2.8); plot(W,U)
points(80,2.8, pch=19)
scale(W)[101]; scale(U)[101]

## Figure 16.3
set.seed(12345); X = runif(30, .5, 3.5)
beta0 = 1.0; beta1 = 1.5; sigma = 0.7
Y = beta0 + beta1*X + sigma*rnorm(30) # The regular process

# Suspicious data: Four cases
X.suspect1 = 1.5; Y.suspect1 = 3.3
X.suspect2 = 1.5; Y.suspect2 = 9.7
X.suspect3 = 9.0; Y.suspect3 = 14.5
X.suspect4 = 9.3; Y.suspect4 = 0.6

Y.all1 = c(Y, Y.suspect1); X.all1 = c(X, X.suspect1)
Y.all2 = c(Y, Y.suspect2); X.all2 = c(X, X.suspect2)
Y.all3 = c(Y, Y.suspect3); X.all3 = c(X, X.suspect3)
Y.all4 = c(Y, Y.suspect4); X.all4 = c(X, X.suspect4)

par(mfrow=c(2,2),mar = c(4,4,2,1))
plot(X.all1, Y.all1)
points(X.suspect1, Y.suspect1, pch=19)
plot(X.all2, Y.all2)
points(X.suspect2, Y.suspect2, pch=19)
plot(X.all3, Y.all3)
points(X.suspect3, Y.suspect3, pch=19)
plot(X.all4, Y.all4)
points(X.suspect4, Y.suspect4, pch=19)

## Figure 16.4
# method 1: "By hand" calculation of hii. 
set.seed(12345)
W = rnorm(100, 70,10)
U = .05*W + rnorm(100,0,.1)
W = c(W,80); U = c(U,2.8)
X = cbind(rep(1,101), W,U)
H = X %*% solve(t(X) %*% X) %*% t(X)
hii = diag(H)
# method 2: Requires a fitted lm object. So make up fake Y data.
Y = rnorm(101)
hii = hat(model.matrix(lm(Y~U+W)))
hii
plot(W,U, xlim=c(40,105), ylim=c(2.0,5.0))
identify(W,U, round(hii,3)) # now point and click on the graph

## Figure 16.5
fit=lm(Y.all4~X.all4)
rstudent(fit)
plot(X.all4,Y.all4, xlim = c(0,10), ylim = c(0,8))
# now, point and click on the graph!
identify(X.all4,Y.all4, round(rstudent(fit),2))
abline(lsfit(X.all4,Y.all4), lty=2)
hat(model.matrix(fit))[31]

## Figure 16.6
fit=lm(Y.all4~X.all4)
cooks.distance(fit)
plot(X.all4,Y.all4, xlim = c(0,10), ylim = c(0,8))
identify(X.all4,Y.all4, round(cooks.distance(fit),2)) # point and click!
abline(lsfit(X.all4,Y.all4), lty=2)

## Section 16.6.1
library(MASS)
data("Boston"); attach(Boston)
fit = lm(crim ~ zn + indus + rm + age + dis + rad + tax + lstat + medv)
plot(fit)
tail(order(cooks.distance(fit)))
cooks.distance(fit)[c(405, 415, 411, 419, 406, 381)]
tail(order(hat(model.matrix(fit))))
round(scale(data.frame(zn,indus,rm,age,dis,rad,tax,lstat,medv))[369,],2)
round(scale(data.frame(zn,indus,rm,age,dis,rad,tax,lstat,medv))[366,],2)
cooks.distance(fit)[369]
cooks.distance(fit)[366]
summary(crim)
rstudent(fit)[381]

fit1 = lm(crim ~ zn + indus + rm + age + dis + rad + tax + lstat + medv,
data=Boston[-381,])

fit.log = lm(log(crim) ~ zn + indus + rm + age + dis + rad + tax + lstat + medv)
plot(fit.log)

## Section 16.7.1 (Figure 16.10)
library(survival)
one = rep(1,length(Y.all4))
fit.T = survreg(Surv(Y.all4, one) ~ X.all4, dist = "t")
summary(fit.T)
plot(X.all4, Y.all4)
abline(lsfit(X.all4, Y.all4), lwd=2)
abline(fit.T$coefficients[1:2], lty=2, lwd=2)
logLik(lm(Y.all4~X.all4))
logLik(fit.T)

## Figure 16.11
Year = 2002:2014
Weekly.10 = c(295, 301, 305, 310, 319, 330, 346, 350,
 353, 358, 359, 372, 379)
Weekly.90 = c(1376, 1419, 1460, 1506, 1545, 1602, 1693,
 1744, 1769, 1802, 1875, 1891, 1898)
plot(Year, Weekly.10, ylim = c(0,2000), pch="+",
 ylab = "Weekly U.S. Salary")
points(Year, Weekly.10, type="l")
points(Year, Weekly.90, pch="+")
points(Year, Weekly.90, type="l", lty=2)
legend(2009, 1300, c("90th percentile", "10th percentile"),
 pch=c("+","+"), lty= c(2,1))

## Figure 16.12
par(mfrow=c(1,2))
y1 = c(2.3, 4.5, 6.7, 13.4, 40.0) # Median is 6.7
y2 = c(2.3, 4.5, 6.7, 13.4) # For second graph
b0 = seq(2, 15,.001)
mat1 = outer(y1,b0,"-"); mat2 = outer(y2,b0,"-")
SAE1 = colSums(abs(mat1)); SAE2 = colSums(abs(mat2));
plot(b0, SAE1, type="l", ylab = "SAE"); abline(v = 6.7, lty=2)
plot(b0, SAE2, type="l", ylab = "SAE"); abline(v = (4.5+6.7)/2, lty=2)

## Figure 16.14
tau = .10 ## A quantile of interest, here .10
y = c(1:99, 1000) ## The .10 quantile should be around 10.
b0 = seq(5, 15,.001)
mat = outer(y,b0,"-")
mat = ifelse(mat < 0, (1-tau)*abs(mat), tau*abs(mat))
SAE = colSums(mat)
plot(b0, SAE, type="l")

## Section 16.8.1
n = 1000
beta0 = 2.1; beta1 = 0.7
set.seed(12345)
X = runif(n, 10, 20)
Y = beta0 + beta1*X + .4*X*rnorm(n)
# True slope of the .90 quantile function
beta1.90 = beta1 + qnorm(.90)*.4 
beta1.90
library(quantreg)
fit.90 = rq(Y ~ X, tau = .90)
summary(fit.90)

n = 100000
beta0 = 2.1; beta1 = 0.7
set.seed(12345)
X = runif(n, 10, 20)
Y = beta0 + beta1*X + .4*X*rnorm(n)
# True slope of the .90 quantile function
beta1.90 = beta1 + qnorm(.90)*.4 
beta1.90
library(quantreg)
fit.90 = rq(Y ~ X, tau = .90)
summary(fit.90)

## Section 16.8.2 (Figure 16.15)
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth)
library(quantreg)
fit.25 = rq(P.assets ~ Age, tau = .25) # .25 quantile
b = coefficients(fit.25); b0 = b[1]; b1 = b[2]
x = seq(20, 70)
y.25 = b0 + b1*x
fit.50 = rq(P.assets ~ Age, tau = .50) # .5 quantile (median)
b = coefficients(fit.50); b0 = b[1]; b1 = b[2]
y.50 = b0 + b1*x
fit.75 = rq(P.assets ~ Age, tau = .75) # .75 quantile
b = coefficients(fit.75); b0 = b[1]; b1 = b[2]
y.75 = b0 + b1*x

plot(Age, P.assets, xlim = c(20,70))
points(x, y.25, type="l"); points(x, y.50, type="l", lty=2);
points(x, y.75, type="l", lty=3)
abline(lsfit(Age, P.assets), col = "gray", lwd=2)
legend("topleft", c("tau=.25", "tau=.50", "tau=.75", "OLS"),
 lty=c(1,2,3,1), lwd=c(1,1,1,2), col=c(1,1,1,"gray"))

## Section 16.9
WD = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Testdata.txt")
summary(lm(Y ~ X, data=WD))

library(DescTools)
Yw = Winsorize(WD$Y); Xw = Winsorize(WD$X)
summary(lm(Yw ~ Xw))

## Figure 16.16
set.seed(123)
X = exp(rnorm(1000, 1, 1))
Y = 2 + .10*X + 1.5*rt(1000,3)
library(DescTools)
X.w = Winsorize(X)
Y.w = Winsorize(Y)
par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(X, Y, cex=.5, xlim = c(0,80), ylim = c(-10,20)); abline(v=10, lwd=2,
 col="gray"); plot(X.w, Y.w, cex=.5, xlim = c(0,80), ylim = c(-10,20));
abline(v=10, lwd=2, col="gray")

## Figure 16.18
n = 200000; beta0 = 2; beta1 = .10; scale = 1.5;
set.seed(12345)
library(DescTools); library(quantreg)
X = runif(n, 0, 20); epsilon = scale*rt(n,1)
Y = beta0 + beta1*X + epsilon
X.w = Winsorize(X); Y.w = Winsorize(Y)
fit.wins = lm(Y.w~X.w)
L = quantile(Y, .05); U = quantile(Y, .95)
Y.t = Y[Y>L & Y<U]; X.t = X[Y>L & Y<U]
fit.trunc = lm(Y.t~X.t)

X.new = seq(0, 20, .1)
X.new = data.frame(X.new); names(X.new) = c("X")
Xw.new = data.frame(X.new); names(Xw.new) = c("X.w")
Xt.new = data.frame(X.new); names(Xt.new) = c("X.t")
pred.w = predict(fit.wins, Xw.new, interval="prediction", level=.90)
pred.t = predict(fit.trunc, Xt.new, interval="prediction", level=.90)
fit.quant.05 = rq(Y~X,tau=.05); fit.quant.95 = rq(Y~X,tau=.95)
lower.W = predict(fit.quant.05, X.new)
upper.W = predict(fit.quant.95, X.new)

x.plot = X.new[,1]
true.quant.05 = beta0 + beta1*x.plot + scale*qt(.05,1)
true.quant.95 = beta0 + beta1*x.plot + scale*qt(.95,1)
plot(x.plot, lower.W, type="l", lty=2, ylim = c(-10,15),
xlab="X", ylab="Prediction Limits")
points(x.plot, upper.W, type="l", lty=2)
points(x.plot, true.quant.05, type="l")
points(x.plot, true.quant.95, type="l")
points(x.plot, beta0 + beta1*x.plot, type="l", lty=2, lwd=3)
points(x.plot, pred.w[,2], type="l", lty=3)
points(x.plot, pred.w[,3], type="l", lty=3)
points(x.plot, pred.t[,2], type="l", lty=4)
points(x.plot, pred.t[,3], type="l", lty=4)

## Appendix A
n = 100 # Sample size per data set
beta0 = 2; beta1 = .10; scale = 1.5 # true parameter values
set.seed(12345)
nsim = 20000 ## Simulations. WARNING!! 20000 takes about an hour.
             ## Change nsim to 500 first.

# Initializations
b1.ols = numeric(nsim); pv.ols = numeric(nsim); pv0.ols = numeric(nsim)
b1.wins = numeric(nsim); pv.wins = numeric(nsim); pv0.wins = numeric(nsim)
b1.trunc = numeric(nsim); pv.trunc = numeric(nsim); pv0.trunc = numeric(nsim)
b1.quant = numeric(nsim); pv.quant = numeric(nsim); pv0.quant = numeric(nsim)
b1.ml = numeric(nsim); pv.ml = numeric(nsim); pv0.ml = numeric(nsim)

# libraries
library(DescTools); library(maxLik); library(quantreg)
# ML estimation using Cauchy errors
loglik <- function(param) {
 b0 = param[1]; b1 = param[2]; g0 = param[3]
 ln.scale = g0
 z = (Y - b0 - b1*X)/exp(ln.scale)
 ll = sum(dt(z,log=T,1) - ln.scale)
 ll
}

# Main simulation loop
for (i in 1:nsim) {
 X = runif(n, 0, 20)
 epsilon = scale*rt(n,1)
 Y = beta0 + beta1*X + epsilon
 Y0 = beta0 + (0)*X + epsilon
 fit.ols = summary(lm(Y~X))$coefficients
 b1.ols[i] = fit.ols[2,1]
 pv.ols[i] = fit.ols[2,4]
 pv0.ols[i] = summary(lm(Y0~X))$coefficients[2,4]
 
 X.w = Winsorize(X); Y.w = Winsorize(Y)
 Y0.w = Winsorize(Y0)
 fit.wins = summary(lm(Y.w~X.w))$coefficients
 b1.wins[i] = fit.wins[2,1]
 pv.wins[i] = fit.wins[2,4]
 pv0.wins[i] = summary(lm(Y0.w~X.w))$coefficients[2,4]
 
 L = quantile(Y, .05); U = quantile(Y, 1-.05)
 Y.t = Y[Y>L & Y<U]; X.t = X[Y>L & Y<U]
 L0 = quantile(Y0, .05); U0 = quantile(Y0, 1-.05)
 Y0.t = Y0[Y0>L0 & Y0<U0]; X0.t = X[Y0>L0 & Y0<U0]
 fit.trunc = summary(lm(Y.t~X.t))$coefficients
 b1.trunc[i] = fit.trunc[2,1]
 pv.trunc[i] = fit.trunc[2,4]
 pv0.trunc[i] = summary(lm(Y0.t~X0.t))$coefficients[2,4]
 
 fit.quant = summary(rq(Y~X), se="iid")$coefficients
 b1.quant[i] = fit.quant[2,1]
 pv.quant[i] = fit.quant[2,4]
 fit0.quant = summary(rq(Y0~X), se="iid")$coefficients
 pv0.quant[i] = fit0.quant[2,4]
 
 fit.ml = summary(maxLik(loglik, start=c(0,0,0)))$estimate
 b1.ml[i] = fit.ml[2,1]; pv.ml[i] = fit.ml[2,4]
 Y = Y0
 pv0.ml[i] = summary(maxLik(loglik, start=c(0,0,0)))$estimate[2,4]
}

## OLS results: mean, sdev, rmse, and power
mean(b1.ols); sd(b1.ols); sqrt(mean ((b1.ols - .10)^2) );
mean(pv.ols <.05)

## Winsorized results: mean, sdev, rmse, and power
mean(b1.wins); sd(b1.wins); sqrt(mean ((b1.wins - .10)^2) );
mean(pv.wins <.05)

## Truncation results: mean, sdev, rmse, and power
mean(b1.trunc); sd(b1.trunc); sqrt(mean ((b1.trunc - .10)^2) );
mean(pv.trunc <.05)

## Quantile regression results: mean, sdev, rmse, and power
mean(b1.quant); sd(b1.quant); sqrt(mean ((b1.quant - .10)^2) );
mean(pv.quant < 0.05)

## ML results: mean, sdev, rmse, and power
mean(b1.ml); sd(b1.ml); sqrt(mean ((b1.ml - .10)^2) ); mean(pv.ml <.05)

## Type I error rates:
mean(pv0.ols <.05) #OLS
mean(pv0.wins <.05) # Winsorized
mean(pv0.trunc <.05) # Truncated
mean(pv0.quant <.05) # Quantile
mean(pv0.ml <.05) # ML

### Exercise 1
set.seed(12345)
X.0 = runif(30, .5, 3.5)
beta0 = 1.0
beta1 = 1.5
sigma = 0.7
Y.0 = beta0 + beta1*X.0 + sigma*rnorm(30) # The regular process
# Suspicious data value
X.suspect = 9.3
Y.suspect = 0.6
# The complete data
Y = c(Y.0, Y.suspect)
X = c(X.0, X.suspect)

## Exercise 2
Firms = read.csv("https://raw.githubusercontent.com/
andrea2719/URA-DataSets/master/Firms.csv")

## Exercise 4
fees=read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/auditfees.csv", header=T)





