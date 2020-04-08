
## Figure 7.1
X1 = runif(20,1,2); X2 = runif(20,1,2)
Y = -1 + 2*X1 + 4*X2 + rnorm(20,0,2)
fit <- lm(Y ~ X1 + X2)
pch = ifelse(fit$residuals < 0, "-", "+")
library(scatterplot3d)
s3d <-scatterplot3d(X1,X2,Y, pch=pch, cex.symbols=1.6, highlight.3d= FALSE,
  type="h", main="3-D Scatter and Fitted Regression Plane")
s3d$plane3d(fit, draw_polygon = TRUE)

## Section 7.3.1
Widgets = c(1500,800,1500,1400,900,800,1400,1400,1300,1400,700,1000,1200,
1200,900,1200,1700,1600,1200,1400,1400,1000,1200,800,1000,1400,1400,1500,
1500,1600,1700,900,800,1300,1000,1600,900,1300,1600,1000)

## Figure 7.2
Nsim = 100000; b1.ols = numeric(Nsim)
for (i in 1:Nsim) {
  Widgets.sim = round(rnorm(40, 1200, 300), -2)
  Cost.sim = 55 + 1.5*Widgets.sim + rnorm(40,0,250)
  b1.ols[i] = lm(Cost.sim ~ Widgets.sim)$coefficients[2] }
hist(b1.ols, freq=F, breaks=100, main="",xlab = expression("OLS
estimate of" ~beta[1]))
abline(v=1.5, lwd=2.5)

## Figure 7.3
Nsim = 100000; b1.ols.XG = numeric(Nsim); b1.ols.XA = numeric(Nsim)
for (i in 1:Nsim) {
  Widgets.A = round(rnorm(40, 1200, 300), -2) #Actual Widgets
  delta = rnorm(40, 0 , 100) # Measurement error
  Widgets.G = Widgets.A + delta # Guess of Widgets
  Cost = 55 + 1.5*Widgets.A + rnorm(40,0,250)
  b1.ols.XA[i] = lm(Cost ~ Widgets.A)$coefficients[2]
  b1.ols.XG[i] = lm(Cost ~ Widgets.G)$coefficients[2] }

par(mfrow=c(2,1))
hist(b1.ols.XA, freq=F, breaks=100, xlim=c(1,2), 
  xlab="OLS estimate with true X", main="")
abline(v=1.5, lwd=2, lty=2)

hist(b1.ols.XG, freq=F, breaks=100, xlim = c(1,2), 
  xlab="OLS estimate with measurement error in X", main="")
abline(v=1.5, lwd=2, lty=2)

## Section 7.5
compS = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/compspeed.txt")
attach(compS); Y = 1/time; X1 = GHz; X2 = GB
fit = lm(Y ~ X1 + X2); summary(fit)
n = length(Y); Intcpt = rep(1, n)
X = as.matrix(cbind(Intcpt, X1, X2))
XTX = t(X) %*% X; XTX
XTX.inv = solve(XTX); XTX.inv
## This XTX inverse matrix is called "cov.unscaled" 
## in the fitted lm object:
summary(fit)$cov.unscaled

XTY = t(X) %*% as.matrix(Y)
beta.hat = XTX.inv %*% XTY; beta.hat
## Compare with the lm results:
fit$coefficients
## Sum of squared residuals
Resid = Y - X %*% beta.hat
SS.Resid = t(Resid) %*% Resid
sigma.hat = sqrt(SS.Resid/(n-3))
sigma.hat
## Compare with the lm result:
summary(fit)$sigma

## Standard Errors
Cov.beta.hat = as.numeric(sigma.hat^2) * XTX.inv
Cov.beta.hat
standard.errors = sqrt(diag(Cov.beta.hat))
standard.errors
## Compare with the lm result:
summary(fit)$coefficients[,2]

## Figure 7.4
gpa.data = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
gpa.data$PhD = ifelse(gpa.data$degree=="P",1,0)
attach(gpa.data)

fit <- lm(gpa ~ gmat + PhD, data=gpa.data)
summary(fit) 
pch = ifelse(fit$residuals < 0, "-", "+")

library(scatterplot3d) 
s3d <-scatterplot3d(gmat,PhD,gpa, pch=pch, cex.symbols=1.6, highlight.3d= FALSE,
   type="h", main="3-D Scatter and Fitted Regression Plane")
s3d$plane3d(fit, draw_polygon = TRUE)

## Exercise 1
sales = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/sales.csv")




