## Section 2.2.1

x = rnorm(10,5,1)
#True beta0=2.0; true beta1=0.7; true sigma=1
y = 2.0 + 0.7*x + rnorm(10,0,1) 
plot(x,y)
sseplot = function(a,b) {
  abline(a,b, col="gray")
  sse = sum ((y - (a+b*x))^2)
  sse
}

sseplot(1.5,.8)   # intercept = 1.5, slope = .8
sseplot(1.6,.5)
sseplot(3.0,.2)
sseplot(5.0,.2)
sseplot(1.2,.5)
sseplot(2.0,.7)

b0.hat = lsfit(x,y)$coefficients[1]
b1.hat = lsfit(x,y)$coefficients[2]
sseplot(b0.hat,b1.hat)

b1.ols = cov(x,y)/var(x); b1.ols
b0.ols = mean(y) - b1.ols*mean(x); b0.ols

b1.hat; b0.hat

## Figure 2.2
par(mfrow=c(1,2))
z = seq(-5,5,.01)
p.normal = dnorm(z)
p.laplace = (1/sqrt(2))*exp(-sqrt(2)*abs(z))
plot(z, p.laplace, type="l", lty=2, yaxs="i",
  ylim=c(0,1.1*max(p.laplace)), ylab="density")
points(z, p.normal,type="l")
plot(z, p.laplace, type="l", lty=2, yaxs="i", ylim=c(0,.03),
  xlim = c(2.5,5), ylab="density")
points(z, p.normal,type="l")

## Figure 2.3
set.seed(123) # To fix the random numbers.
x = rnorm(10,5,1)
y = 2.0 + 0.7*x + rnorm(10,0,1)
x[11]=7; y[11]=-1.0 # An outlier
plot(x,y); points(x[11], y[11], pch=20)
abline(lsfit(x,y), lty=2)
library(quantreg)
abline(rq(y~x)$coefficients)

