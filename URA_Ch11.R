
## Section 11.2.1

## With this code, you can simulate data from a model where a student's
## grad GPA truly depends on their GRE verbal (X1), GRE quant (X2),
## and Undergrad GPA (X3). Yet the estimates from the model using X1 only
## are better, because of the variance/bias trade-off.

nsimu = 10000 ## Number of simulated data sets
n = 20 ## Sample size of students within a particular data set

## The following code simulates the X's from a multivariate normal
## distribution, with variances that are sensible, and where the X's are
## correlated at .5 with one another.

Sig.X = diag(c(5,5,.3))%*% matrix(c(1,.5,.5,.5,1,.5,.5,.5,1)
 , nrow=3) %*%diag(c(5,5,.3))
Mean.X = c(150, 150, 3)
library("MASS")
X = mvrnorm(n, Mean.X, Sig.X)
X1 = round(X[,1],0) # Simulated GRE verbal
X2 = round(X[,2],0) # Simulated GRE quant
X3 = round(X[,3],2) # simulated undergrad GPA
head(cbind(X1,X2,X3)) ## Stop and look at the simulated X data!

b0 = -.50 ## The true intercept
b1 = .02 ## The true effect of verbal GRE
b2 = .005 ## The true effect of quant GRE
b3 = .02 ## The true effect of undergrad GPA
sg = .20 ## The true conditional standard deviation of GPA
EY = b0 + b1*X1 + b2*X2 + b3*X3 ## The true conditional means of GPA

## Suppose Hans has X1=140, X2=160, X3=2.7. How well does the estimated
## regression model predict Hans?
X.H = c(1, 140, 160, 2.7) # Hans' X values, including intercept
X.Hr = c(1, 140) # Hans' X values for the reduced model

## Here are nsimu *predictions* of Hans’ GPA using two regression models,
## (i) the correct full model (p.F), and (ii) the incorrect reduced
## model (p.R).
p.F = replicate(nsimu, lm(EY +rnorm(n,0,sg) ~ X1+X2+X3)$coefficients%*%X.H)
p.R = replicate(nsimu, lm(EY +rnorm(n,0,sg) ~ X1)$coefficients%*%X.Hr)
head(p.F); head(p.R)
## Stop and look at the predictions of Hans’ GPA!

## Hans' GPA comes from a distribution whose true mean is
## b0 + b1*140 + b2*160 + b3*2.7, and whose standard deviation is sg =.20.
Hans.M = b0 + b1*140 + b2*160 + b3*2.7; Hans.M
## Stop and notice the mean GPA for people like Hans is Hans.M = 3.154.
## Here are nsimu simulated *actual values* of Hans’ ultimate GPA:
Hans = rnorm(nsimu, Hans.M, sg); head(Hans)
## stop and compare Hans’ actual GPA with the predictions above!

## Here are the errors of prediction using the two different models.
Err.F = Hans - p.F ; Err.R = Hans - p.R

## Here is the error of prediction if you were mega-Hans, and knew the
## true conditional mean:
Err.T = Hans - Hans.M

head(Err.F); head(Err.R); head(Err.T)
# Stop and compare the errors of prediction using the three methods!
## Here are the root mean squared prediction errors for the estimated
## full model, the estimated reduced model, and the mega-Hans model:
sqrt(mean(Err.F^2)); sqrt(mean(Err.R^2)); sqrt(mean(Err.T^2))

## Stop and read this: The root mean square prediction errors measure
## how close the predictions using the various methods are to Hans’ GPA.
## The mega-Hans model is best, but unusable in practice. You could use
## either the full or reduced models, as estimated from the data.
## Why is the full (correct) model worse? Because the predicted values have
## higher variance (less accuracy) when you use the full model, even though
## it is correct. This happens because all of the coefficient estimates are
## random, i.e., data-set specific: Different data sets imply different
## coefficient estimates. Although unbiased, the coefficient estimates from
## the full model are not equal to the true parameter values.

sd(p.F); sd(p.R)
## Stop and notice that there is more variability in the full model
## predictions.

mean(p.F) - Hans.M; mean(p.R) - Hans.M
## Stop and notice that there is no bias when the full model is used to
## predict Hans, but there is bias with the reduced model, which tends
## to give predictions that are too low:

## Figure 11.2
par(mfrow=c(2,1))
hist(p.F, xlim = c(2.5,4), main="Predicted GPA Using Full Model")
abline(v=Hans.M, lwd=2, col="gray")
hist(p.R, xlim = c(2.5,4), main = "Predicted GPA Using Reduced Model")
abline(v=Hans.M, lwd=2, col="gray")

## Stop and look at the histograms to see (i) the unbiasedness of the full
## model predictions, (ii) the higher variability of the full model
## predictions, and (iii) the biasedness of the reduced model predictions,
## and (iv) the lower variability of the reduced model predictions.

## Figure 11.3
par(mfrow=c(3,1))
hist(Hans - p.F, xlim = c(-1,1), main= "Full Model Prediction Errors")
hist(Hans - p.R, xlim = c(-1,1), main = "Reduced Model Prediction Errors")
hist(Hans - Hans.M, xlim=c(-1,1), main = "Mega-Hans Model Prediction Errors")

## Figure 11.4
library(MASS)
data("Boston")
library(leaps)
fits = regsubsets(crim ~ zn + indus + rm + age + dis + rad + tax +
 lstat + medv, data=Boston, nbest=10)
library(car)
par(mfrow=c(1,2))
subsets(fits, statistic="bic")
subsets(fits, statistic="bic", ylim = c(-260,-250))

fit = lm(crim ~ rad + lstat, data=Boston)
summary(fit)

## Figure 11.5
x = c(2.0, 3.2, 2.1, 2.00, 3.1, 8.0, 8.2, 7.0)
y = c(2.2, 2.1, 2.7, 2.36, 2.8, 3.6, 3.0, 3.2)
x1=x; x2 = x^2; x3 = x^3; x4 = x^4; x5 = x^5
fit1 = lm(y ~ x1) 
fit2 = lm(y ~ x1 + x2) 
fit3 = lm(y ~ x1 + x2 + x3)
fit4 = lm(y ~ x1 + x2 + x3 + x4) 
fit5 = lm(y ~ x1 + x2 + x3 + x4 + x5)

## Now, graph the models and the data:
xg = seq(1.8, 8.4, .001)
pred = data.frame(xg, xg^2, xg^3, xg^4, xg^5)
colnames(pred) = c("x1","x2","x3","x4","x5")
plot(xg,predict(fit1, pred), type="l", ylim = c(0, 5),
 xlab="x", ylab="y-hat")
points(x,y, cex=2, lwd=2) 
points(xg,predict(fit2, pred), type="l", lty=2)
points(xg,predict(fit3, pred), type="l", lty=3)
points(xg,predict(fit4, pred), type="l", lty=1, lwd=2)
points(xg,predict(fit5, pred), type="l", lty=2, lwd=2)

## Section 11.4.1
library(DAAG)
x = c(2.0, 3.2, 2.1, 2.00, 3.1, 8.0, 8.2, 7.0)
y = c(2.2, 2.1, 2.7, 2.36, 2.8, 3.6, 3.0, 3.2)
x1=x; x2 = x^2; x3 = x^3; x4 = x^4; x5 = x^5

n = length(y)
data.f = data.frame(x1,x2,x3,x4,x5,y)
pred.1 = CVlm(data = data.f, form.lm=formula(y ~ x1), m=n)
pred.2 = CVlm(data = data.f, form.lm=formula(y ~ x1+x2), m=n)
pred.3 = CVlm(data = data.f, form.lm=formula(y ~ x1+x2+x3), m=n)
pred.4 = CVlm(data = data.f, form.lm=formula(y ~ x1+x2+x3+x4), m=n)
pred.5 = CVlm(data = data.f, 
 form.lm=formula(y ~ x1+x2+x3+x4+x5), m=n)
## In-sample root mean square errors (RMSE)
sqrt(mean((y-pred.1$Predicted)^2))
sqrt(mean((y-pred.2$Predicted)^2))
sqrt(mean((y-pred.3$Predicted)^2))
sqrt(mean((y-pred.4$Predicted)^2))
sqrt(mean((y-pred.5$Predicted)^2))
## Out-of-sample root mean square prediction errors (RMSPE)
sqrt(mean((y-pred.1$cvpred)^2))
sqrt(mean((y-pred.2$cvpred)^2))
sqrt(mean((y-pred.3$cvpred)^2))
sqrt(mean((y-pred.4$cvpred)^2))
sqrt(mean((y-pred.5$cvpred)^2))

## need to run code for Figure 11.4 first
results = cbind(summary(fits)$bic, summary(fits)$which)
results = results[order(summary(fits)$bic),]
head(results)

## calculate RMSPE
library(MASS)
data("Boston")
n = nrow(Boston)
library(DAAG)
pred1=CVlm(data=Boston,form.lm=formula(crim ~ rad+lstat), m=n, printit=FALSE)
pred2=CVlm(data=Boston,form.lm=formula(crim ~ zn+dis+rad+medv), m=n, printit=FALSE)
pred3=CVlm(data=Boston,form.lm=formula(crim ~ rad+medv), m=n, printit=FALSE)
pred4=CVlm(data=Boston,form.lm=formula(crim ~ rad+lstat+medv), m=n, printit=FALSE)
pred5=CVlm(data=Boston,form.lm=formula(crim ~ zn+indus+dis+rad+medv), m=n, printit=FALSE)
pred6=CVlm(data=Boston,form.lm=formula(crim ~ zn+rad+lstat), m=n, printit=FALSE)
y=Boston$crim

## In-sample root mean square prediction errors
sqrt(mean((y-pred1$Predicted)^2))
sqrt(mean((y-pred2$Predicted)^2))
sqrt(mean((y-pred3$Predicted)^2))
sqrt(mean((y-pred4$Predicted)^2))
sqrt(mean((y-pred5$Predicted)^2))
sqrt(mean((y-pred6$Predicted)^2))

## Out-of-sample root mean square prediction errors
sqrt(mean((y-pred1$cvpred)^2))
sqrt(mean((y-pred2$cvpred)^2))
sqrt(mean((y-pred3$cvpred)^2))
sqrt(mean((y-pred4$cvpred)^2))
sqrt(mean((y-pred5$cvpred)^2))
sqrt(mean((y-pred6$cvpred)^2))

