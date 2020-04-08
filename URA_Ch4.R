
## Figure 4.1 (need to run add.loess function first)
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
attach(ProdC)
par(mfrow=c(1,2))
plot(Widgets, Cost); abline(lsfit(Widgets, Cost))
add.loess(Widgets, Cost, col = "gray", lty=2)
fit = lm(Cost ~ Widgets)
y.hat = fit$fitted.values; resid = fit$residuals
plot(y.hat, resid); add.loess(y.hat, resid, col = "gray", lty=2)
abline(h=0)

## Figure 4.2
CarS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(CarS)
par(mfrow=c(1,2))
plot(INTRATE, NSOLD); abline(lsfit(INTRATE, NSOLD))
add.loess(INTRATE, NSOLD, col = "gray", lty=2)
fit = lm(NSOLD~INTRATE)
y.hat = fit$fitted.values; resid = fit$residuals
plot(y.hat, resid); add.loess(y.hat, resid, col = "gray", lty=2)
abline(h=0)

## Section 4.4.1 (Figure 4.3)
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
attach(ProdC)
plot(Widgets, Cost); abline(lsfit(Widgets, Cost))
Widgets.squared = Widgets^2
fit.quad = lm(Cost ~ Widgets + Widgets.squared); summary(fit.quad)
lines(spline(Widgets, predict(fit.quad)), col = "gray", lty=2)

## Section 4.4.2 (Figure 4.4)
sales = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(sales)
plot(INTRATE, NSOLD); abline(lsfit(INTRATE, NSOLD))
INTRATE.squared = INTRATE^2
fit.quad = lm(NSOLD ~ INTRATE + INTRATE.squared); summary(fit.quad)
lines(spline(INTRATE, predict(fit.quad)), col = "gray", lty=2)

## Section 4.5.1 (Figure 4.5)
set.seed(54321) # For perfect replicability of the random simulation.
x = 10 + 2*rnorm(1000000); xsq = x^2
y = 2 + .6*x + .003*xsq + 4*rnorm(1000000) #beta2=.003 does not equal 0!
fit.quad = lm(y ~ x + xsq)
summary(fit.quad) # Significant curvature: p-value = 0.000166
## A .1% random sample from the data set is selected to make the scatterplot
## more legible. Otherwise, the points are too dense to view.
select = runif(1000000); x1 = x[select<.001] ; y1 = y[select<.001]
plot(x1, y1, main = "Scatterplot of a 0.1% Subsample" )
abline(lsfit(x, y), col="gray")
lines(spline(x, predict(fit.quad)), lty=3)

## Section 4.6.1 (Figure 4.6)
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
attach(ProdC); par(mfrow=c(1,2))
fit = lm(Cost ~ Widgets)
y.hat = fit$fitted.values; resid = fit$residuals
plot(y.hat, resid); abline(h=0)
abs.resid = abs(resid)
plot(y.hat, abs.resid); add.loess(y.hat, abs.resid, col = "gray", lty=2)

## Section 4.6.2 (Figure 4.7)
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth); par(mfrow=c(1,2))
fit = lm(P.assets ~ Age)
y.hat = fit$fitted.values; resid = fit$residuals
plot(y.hat, resid); abline(h=0)
abs.resid = abs(resid); plot(y.hat, abs.resid)
add.loess(y.hat, abs.resid, col = "gray", lty=2)

## Section 4.7.1
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
fit = lm(Cost ~ Widgets, data=ProdC)
y.hat = fit$fitted.values; resid = fit$residuals; abs.resid = abs(resid)
fit.glejser = lm(abs.resid ~ y.hat)
summary(fit.glejser)

## Section 4.7.2
Pass = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Pass) ; fit = lm(P.assets ~ Age)
y.hat = fit$fitted.values; resid = fit$residuals; abs.resid = abs(resid)
fit.glejser = lm(abs.resid~y.hat)
summary(fit.glejser)

## Section 4.8.1 (Figure 4.8)
CarS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(CarS); n = nrow(CarS)
fit = lm(NSOLD ~ INTRATE)
resid = fit$residuals
par(mfrow=c(1,2))
plot(1:n, resid, xlab="month", ylab="residual")
points(1:n, resid, type="l"); abline(h=0)
lag.resid = c(NA, resid[1:n-1])
plot(lag.resid, resid, xlab="lagged residual", ylab = "residual")
abline(lsfit(lag.resid, resid))
cor.test(resid, lag.resid)

## Figure 4.9
# Normal conditional distributions, 
# but non-normal marginal distribution
X = rexp(200,1)
# Y is conditionally normal with mean=10+2x and variance=1
Y = 10 + 2*X + rnorm(200)
qqnorm(Y); qqline(Y)

## Section 4.10.1 (Figure 4.10)
CarS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/CarS.txt")
attach(CarS)
fit = lm(NSOLD ~ INTRATE)
Residuals = fit$residuals
par(mfrow=c(1,2))
hist(Residuals)
qqnorm(Residuals); qqline(Residuals)

## Section 4.11
shapiro.test(Residuals)

## Section 4.12 (Figure 4.11)
par(mfrow=c(1,2)); set.seed(12345)
X = rnorm(1000,3,1); Y = round(X + rnorm(1000,0,1))
table(Y) # Y is highly discrete and obviously non-normal
model = lm(Y ~ X)
## But the diagnostic plots and tests suggest normality is reasonable
qqnorm(model$residuals); qqline(model$residuals)
shapiro.test(model$residuals); plot(X,Y)

## Exercise 1
charity = read.csv(“https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv”)


