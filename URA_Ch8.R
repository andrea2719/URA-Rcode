
## Section 8.2
CarS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(CarS); Y = NSOLD; X1 = INTRATE; X2 = X1^2
fit = lm(Y ~ X1 + X2); summary(fit)
## By hand calculations of R-squared statistics
SST = sum( (Y-mean(Y))^2 )
SSE = sum(fit$residuals^2)
R.squared = 1 - SSE/SST
n = length(NSOLD) 
R.squared.adj = 1 - (SSE/(n-3))/(SST/(n-1))
R.squared; R.squared.adj
((SST-SSE)/2)/(SSE/(n-3))

## Section 8.3.1
CarS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(CarS); X1 = INTRATE; X2 = X1^2
n = length(X1)
Y.sim =15 +0*X1 +0*X2 +rnorm(n,0,2) ## Notice the 0’s: Null model is true
fit = lm(Y.sim ~ X1 + X2); summary(fit)

## Figure 8.2
x = c(1,2,4)
y = c(3.1, 5.8, 5.7)
plot(x,y, ylim = c(3, 6.2), pch="*", cex=1.6, xlab="X", ylab = "Y")
abline(lsfit(x,y), lwd=1.5)
abline(h = mean(y), lwd=1.5)
b0 = 3.1500000; b1 =  0.7357143
arrows(x[1]+.01,y[1], x[1]+.01, mean(y), lty=3, lwd=1.7)
arrows(x[1]-.01,y[1], x[1]-.01, b0+b1*x[1], lty=1, lwd=1.7)
arrows(x[2]+.01,y[2], x[2]+.01, mean(y), lty=3, lwd=1.7)
arrows(x[2]-.01,y[2], x[2]-.01, b0+b1*x[2], lty=1, lwd=1.7)
arrows(x[3]+.01,y[3], x[3]+.01, mean(y), lty=3, lwd=1.7)
arrows(x[3]-.01,y[3], x[3]-.01, b0+b1*x[3], lty=1, lwd=1.7)

## Figure 8.3
Nsim = 10000
Fsim.null = numeric(Nsim)
Fsim.alt = numeric(Nsim)
for (i in 1:Nsim) {
  Y0.sim =15 +0*X1 +0*X2 +rnorm(n,0,2) ## Null model is true
  Y1.sim = 15 - 0.15*X1 + .003*X2 + rnorm(n,0,2) ##Non-null model
  fit.null = lm(Y0.sim ~ X1 + X2)
  fit.alt = lm(Y1.sim ~ X1 + X2)
  Fsim.null[i] = summary(fit.null)$fstatistic[1]
  Fsim.alt[i] = summary(fit.alt)$fstatistic[1]
}
par(mfrow=c(3,1))
hist(Fsim.null, breaks=100, freq=F, main="", xlab="F value")
hist(Fsim.null, breaks=100, freq=F, main="", xlab="F value")
flist = seq(0,15,.01)
fdist = df(flist,2,117)
crit = qf(.95,2,117)
points(flist, fdist, type="l")
abline(v=crit, col="gray")
hist(Fsim.null, breaks=100, freq=F, main="", xlab="F value")
points(flist, fdist, type="l")
abline(v=crit, col="gray")
hist(Fsim.alt,breaks=100, freq=F, add=T, lty=2, border="gray")

## Section 8.4
set.seed(12345)
X1 = rnorm(100)
X2 = 2*X1 -1 # Perfect collinearity
Y = 1 + 2*X1 + 3*X2 + rnorm(100,0,1)
summary(lm(Y~X1+X2))

## Section 8.4.1
set.seed(12345)
x1 = rep(1:10, each=10)
x2 = x1 + rnorm(100, 0, .05) # X2 differs only slightly from X1
## You can see the collinearity in the graph:
plot(x1,x2)
## The true model has beta0 = 7, beta1=1, and beta2 = 1,
## with all assumptions satisfied.
y = 7 + x1 + x2 + rnorm(100,0,10)
dat.high.mc = data.frame(y,x1,x2)
high.mc = lm( y ~ x1 + x2, data = dat.high.mc)
summary(high.mc)
summary(lm(X1~X2))
summary(lm(X2 ~X1))

set.seed(12345)
x1 = rep(1:10, each=10)
x2 = rep(1:10, 10)
## You can see the lack of collinearity in the graph:
plot(x1,x2)
## The true model has beta0 = 7, beta1=1, and beta2 = 1,
## with all assumptions satisfied.
y = 7 + x1 + x2 + rnorm(100,0,10)
dat.no.mc = data.frame(y,x1,x2)
no.mc = lm( y ~ x1 + x2, data = dat.no.mc)
summary(no.mc)

## Exercise 1
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")

## Exercise 4
census = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/census.csv")
attach(census)
crime.rate = nCrimes/pop

# Original analysis
fit1= lm(crime.rate ~ pop + nDoctors + nBeds + civil + income + area)
library(car)
vif(fit1)

# Revised analysis
pop.log = log(pop)
docs.per = nDoctors/pop
beds.per = nBeds/pop
civ.per = civil/pop
inc.per = income/pop
area.log = log(area)
fit2 = lm(crime.rate ~ pop.log + docs.per + beds.per + 
  civ.per + inc.per + area.log)
vif(fit2)

