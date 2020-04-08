
## Figure 9.1
x = seq(0.2, 10, .1)
EY1 = -3 + 0.3*x + 0.1*x^2; EY2 = 2 - 0.9*x + 0.3*x^2
EY3 = -1 + 3.0*x - 0.4*x^2; EY4 = 1 + 1.2*x - 0.1*x^2
plot(x, EY1, type="l", lty=1, ylab = "E(Y | X=x)")
points(x, EY2, type="l", lty=2); points(x, EY3, type="l", lty=3)
points(x, EY4, type="l", lty=4)
legend(0,10, c("b0 b1 b2 ", "-3.0 0.3 0.1","
  2.0 -.9 0.3", "-1.0 3.0 -.4", " 1.0 1.2 -.1"), lty =
  c(0,1,2, 3,4))

## Figure 9.2
cp = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/complex.txt")
attach(cp)
plot(Complex, jitter(Pref), ylab="Consumer Preference",
  xlab="Product Complexity")

Comp2 = Complex^2
fit = lm(Pref ~ Complex + Comp2)
b0 = fit$coefficients[1]; b1 = fit$coefficients[2]; 
b2 = fit$coefficients[3]

comp.seq = seq(0,40,.1)
yhat = b0 + b1*comp.seq + b2*comp.seq^2
points(comp.seq, yhat, type="l", col= "gray", lwd=2)
Comp.opt = -b1/(2*b2); Comp.opt
abline(v = Comp.opt, col="gray", lwd=2)

## Figure 9.3
x = seq(0,pi,pi/40)
x1 = rep(x, each = 41); x2 = rep(x,41)
Y  = -1 + sin(x2) + 3*sin(x2) * cos(x1) 

library(lattice); library(grid)
newcols <- colorRampPalette(c("grey90", "grey10"))

poly2 = lm(Y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2))
xpred = data.frame(x1,x2)
yhat2 = predict(poly2,xpred)

wireframe(yhat2 ~ x1*x2, 
  xlab = "X1", ylab = "X2", zlab= "EY",
  main = "Quadratic Response Surface in X1, X2",
  xlim = c(0,pi), ylim = c(0,pi),
  drape = TRUE,
  colorkey = FALSE,
  scales = list(arrows=FALSE,cex=.8,tick.number = 5),
  screen = list(z = -70, x = -70),
  col.regions = newcols(100)) 

## Figure 9.4
Design = sample(1:10, 500, replace = T)
Income = sample(1:5, 500, replace = T)
Intent = 5 - 0.6*Design + Income + 0.7*Design*Income

wireframe(Intent ~ Design*Income, 
  #xlab = "X1", ylab = "X2", zlab= "EY",
  main = "Multiple Regression Function with Interaction",
  #xlim = c(0,pi), ylim = c(0,pi),
  drape = TRUE,
  colorkey = FALSE,
  scales = list(arrows=FALSE,cex=.8,tick.number = 5),
  screen = list(z = -70, x = -70),
  col.regions = newcols(100))

## Section 9.3.3 (Figure 9.5)
AnR = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/AnimalRights.csv")
attach(AnR)
Y = AnRights; X1 = Misanthropy; X2 = Idealism
X1X2 = X1*X2 # The interaction term
fit = lm(Y ~ X1 + X2 + X1X2)
summary(fit)
yhat = fit$fitted.values; resid = fit$residuals

par(mfrow=c(2,2)); par(mar=c(4, 4, 1, 1))
plot(yhat,Y) # Graph 1: Correct functional spec
abline(lsfit(yhat,Y))
add.loess(yhat,Y, col="gray")
plot(yhat, resid) # Graph 2: Correct functional spec
abline(h=0)
add.loess(yhat, resid, col="gray")
plot(yhat, abs(resid)) # Graph 3: Constant variance
add.loess(yhat, abs(resid), col="gray")
qqnorm(resid, main="") # Graph 4: Normality
qqline(resid, col="gray")

quantile(X2, c(.10,.90))

## Figure 9.6
X1.seq = seq(1,4,1)
X2.low = quantile(X2,.10)
X2.high = quantile(X2,.90)
b = fit$coefficients; b0 = b[1]; b1 = b[2]; b2 = b[3]; b3 = b[4]
Y.low.X2 = b0 + b1*X1.seq + b2*X2.low + b3*X1.seq*X2.low
Y.high.X2 = b0 + b1*X1.seq + b2*X2.high + b3*X1.seq*X2.high
plot(X1.seq, Y.low.X2, type="l", xlab = "Misanthropy", ylab =
  "Predicted Animal Rights Support")
points(X1.seq, Y.high.X2, type="l", lty=2)
legend(2.7, 2.0, c("Low Idealism", "High Idealism"), lty=c(1,2))

## Section 9.4.1 (Figure 9.7)
x = c(72, 72, 70, 69, 72, 64, 72, 69, 69, 67, 70, 76, 71, 72) # and
y = c(6, 11, 7, 10, 12, 17, 10, 16, 9, 8, 5, 10, 7, 10)
fit1 = lm(y~x); summary(fit1)
fit2 = lm(y ~ x-1)
summary(fit2)

par(mfrow=c(1,2)) 
plot(y~x)
abline(fit1$coefficients[1],fit1$coefficients[2])

plot(y~x, ylim=c(0,20), xlim=c(0,80))
abline(0,fit2$coefficients[1])
abline(fit1$coefficients[1],fit1$coefficients[2], lty=2)

## Section 9.4.2
## Code to illustrate the danger of violating the inclusion principle
## Note that Y does not depend on X squared.
set.seed(12345)
x = rnorm(100, 10, 1)
y = 5 + 2*x + rnorm(100,0,2)
x.sq = x**2
# In the model where the inclusion principle is not followed, the 
# quadratic term is “significant,” incorrectly indicating curvature.
inclusion.model = lm(y ~ x + x.sq)
summary(inclusion.model)
no.inclusion.model = lm(y ~ x.sq)
summary(no.inclusion.model)

## Section 9.4.3
## Note that there is no interaction.
set.seed(12345)
x1 = rnorm(100, 10, 1)
x2 = 15 + 2*rnorm(100)
y = 5 + 2*x1 + x2 + rnorm(100)
x1.x2 = x1*x2
## In the model where the inclusion principle is not followed,
## the interaction term is “significant,” incorrectly indicating
## that the effect of X1 on Y depends on X2.
inclusion.model = lm(y ~ x1 + x2 + x1.x2)
summary(inclusion.model)
no.inclusion.model = lm(y ~ x1 + x1.x2)
summary(no.inclusion.model)

## Exercise 1
sales = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/sales.csv")

## Exercise 2
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")




