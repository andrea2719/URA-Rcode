
## Section 6.1.2 Law of Total Expectation
nsim = 10000000
X = runif(nsim, 40,120)
Y = 0.3*X + rnorm(nsim, 0, 0.04*X)
Cond.Mean = 0.3*X
Cond.Var = (0.04*X)^2
## Estimated unconditional expected value of Y
mean(Y)
## Estimated expected value of the conditional mean
mean(Cond.Mean)

nsim = 10000000
X = runif(nsim, 40,120); U = rpois(nsim, 1.2)
Y = 0.3*X + 2*U + rnorm(nsim, 0, 0.04*X)
## Predictors of Y
Cond.Mean.X = 0.3*X + 2.4; Cond.Mean.X.U = 0.3*X + 2*U
## Mean squared difference from predictor to Y
mean( (Y - Cond.Mean.X)^2 ); mean( (Y - Cond.Mean.X.U)^2)

nsim = 10000000
X = runif(nsim, 40,120); Y = 0.3*X + rnorm(nsim, 0, 0.04*X)
Cond.Mean = 0.3*X; Cond.Var = (0.04*X)^2
## Estimated unconditional variance of Y
var(Y)
## Estimated variance of the conditional mean
var(Cond.Mean)
## Estimated expected value of the conditional variance
mean(Cond.Var)
## Verifying the Law of Total Variance
var(Cond.Mean) + mean(Cond.Var)

## Figure 6.3
x = seq(1,2,.05)
x1 = rep(x, each = 21)
x2 = rep(x,21)
EY = -1 + 6*x1 + 10*x2
library(lattice)
newcols <- colorRampPalette(c("grey90", "grey10"))
wireframe(EY ~ x1*x2,
  xlab = "X1", ylab = "X2",
  main = "Multiple Regression Function", xlim = c(1,2), ylim = c(1,2),
  drape = TRUE,
  colorkey = FALSE,
  col.regions=newcols(100),
  scales = list(arrows=FALSE,cex=.8,tick.number = 5),
  screen = list(z = -60, x = -60))

## Section 6.3
grades = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
Y = grades$gpa
X1 = grades$gmat
# A 1/0 variable; 1 = PhD, 0 = Masters
X2 = ifelse(grades$degree =="P",1,0) 
head(data.frame(X1, X2, Y))
summary(lm(Y~X1+X2))

## Exercise 3
comp = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/compspeed.txt") 

## Exercise 5
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")