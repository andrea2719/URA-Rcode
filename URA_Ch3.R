
## Simulation from Section 3.2
beta0 = 55; beta1 = 1.5; sigma = 250 # Nature’s parameters
Widgets = c(1500,800,1500,1400,900,800,1400,1400,1300,1400,700,
  1000,1200,1200,900,1200,1700,1600,1200,1400,1400,1000,
  1200,800,1000,1400,1400,1500,1500,1600,1700,900,800,1300,
  1000,1600,900,1300,1600,1000)
n = length(Widgets) # Examples of potentially observable data sets:
Sim.Cost = beta0 + beta1*Widgets + rnorm(n, 0, sigma)
head(cbind(Widgets, Sim.Cost))
lm(Sim.Cost ~ Widgets)$coefficients[2]

## Figure 3.1
Nsim = 100000; b1.ols = numeric(Nsim)
for (i in 1:Nsim) {Sim.Cost = beta0 + beta1*Widgets + sigma*rnorm(n)
  b1.ols[i] = lm(Sim.Cost ~ Widgets)$coefficients[2] }
hist(b1.ols, freq=F, breaks=100, main="",
  xlab = expression("OLS estimate of" ~beta[1]))
abline(v=1.5, lwd=2.5)

## Figure 3.2 (need to run add.loess function first)
par(mfrow=c(1,2))
n = 1000 ;X = rnorm(n, 15,5); Xsq = X^2
Y = 7 + X -.03*Xsq + rnorm(n,0,10)
plot (X,Y, pch="."); abline(lsfit(X,Y)); add.loess(X,Y, lty=2)
Nsim = 10000; mu.hat = numeric(Nsim)
for (i in 1:Nsim) {
  X = rnorm(n, 15,5); Xsq = X^2
  Y = 7 + X -.03*Xsq + rnorm(n,0,10)
  fit = lm(Y ~ X)
  mu.hat[i] = fit$coefficients[1] + fit$coefficients[2] * 15
}
hist(mu.hat, breaks=20, main="", cex=0.8,
  xlab = "Estimated Mean of Y | X = 15")
abline(v = 7 + 15 -.03*15^2, lwd=2)

## Figure 3.3
n=40
Nsim = 100000
sig2.hat = numeric(Nsim) 
sig2.biased = numeric(Nsim)
for (i in 1:Nsim) {
  Sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,sigma)
  SSE = sum(lm(Sim.Cost~Widgets)$residuals^2)
  sig2.hat[i] = SSE/(n-2); sig2.biased[i] = SSE/n }
par(mfrow=c(1,2))
hist(sig2.hat, freq=F, breaks=100, main="", 
  xlab="Unbiased estimates of sig.squared")
abline(v=250^2, lwd=2, col="gray")
abline(v=mean(sig2.hat), lwd=2, lty=2)
hist(sig2.biased, freq=F, breaks=100, main = "", 
  xlab="Biased estimates of sig.squared")
abline(v=250^2, lwd=2, col="gray")
abline(v=mean(sig2.biased), lwd=2, lty=2)

## Figure 3.4
n=40
Nsim = 100000
sig2.hat = numeric(Nsim)
sig2.biased = numeric(Nsim)
for (i in 1:Nsim) {
  Sim.Cost = beta0 + beta1*Widgets + arima.sim(n = n, 
             list(ar = c(0.8)),sd = sigma*sqrt(1-.8^2))
  SSE = sum(lm(Sim.Cost~Widgets)$residuals^2)
  sig2.hat[i] = SSE/(n-2); sig2.biased[i] = SSE/n }

par(mfrow=c(1,2))
hist(sig2.hat, freq=F, breaks=100, main="", 
  xlab="Unbiased estimates of sig.squared") 
abline(v=250^2, lwd=2, col="gray")
abline(v=mean(sig2.hat), lwd=2, lty=2)
hist(sig2.biased, freq=F, breaks=100, main="", 
  xlab="Biased estimates of sig.squared") 
abline(v=250^2, lwd=2, col="gray")
abline(v=mean(sig2.biased), lwd=2, lty=2)

## Figure 3.5
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
attach(ProdC)
summary(lm(Cost~Widgets, data = ProdC))

plot(Cost ~ Widgets, data=ProdC)
fit = lm(Cost~Widgets, data = ProdC)
beta0.hat = fit$coefficients[1]
beta1.hat = fit$coefficients[2]
sigma.hat = sqrt(sum(fit$residuals^2)/(n-2))
lb = beta0.hat + beta1.hat*1000 - 1.96*sigma.hat
ub = beta0.hat + beta1.hat*1000 + 1.96*sigma.hat
lines(x = c(1000,1000), y = c(lb,ub), type = "l", lwd = 2)

## R code to verify the mathematical variance formula
b1.ols = numeric(100000)
for (i in 1:100000) {Sim.Cost = beta0 + beta1*Widgets + sigma*rnorm(n)
  b1.ols[i] = lm(Sim.Cost ~ Widgets)$coefficients[2] }
var(b1.ols) # empirical
sigma^2/((n-1)*var(Widgets)) # theoretical

## Figure 3.6
par(mar=c(5,4.5,4,2)+0.1)
b1.list = seq(1.0, 2.0,.001) 
pdf.b1 = dnorm(b1.list, 1.5, 0.1387)
plot(b1.list, pdf.b1, type="l", yaxs="i", 
  ylim=c(0,1.2*max(pdf.b1)), xlab=bquote(hat(beta)[1]), 
  ylab = bquote("density of " ~ hat(beta)[1]))
points(c(1.228,1.228),c(0, dnorm(1.228,1.5,.1387)), 
  type="l", col="gray")
points(c(1.772,1.772),c(0, dnorm(1.772,1.5,.1387)), 
  type="l", col="gray")

## Section 3.6.1 - Understanding exactness and non-exactness
Nsim = 100000; Exact.LCL = numeric(Nsim); Exact.UCL = numeric(Nsim)
Nonexact.LCL = numeric(Nsim); Nonexact.UCL = numeric(Nsim)
for (i in 1:Nsim) {
  Sim.Cost.1 = 55 + 1.5*Widgets + 250*rnorm(40)
  Sim.Cost.2 = 55 + 1.5*Widgets + 250*(rt(40,4)^2 -2) #non-normal
  CL.1 = confint(lm(Sim.Cost.1 ~ Widgets))
  CL.2 = confint(lm(Sim.Cost.2 ~ Widgets))
  Exact.LCL[i] = CL.1[2,1]; Exact.UCL[i] = CL.1[2,2]
  Nonexact.LCL[i] = CL.2[2,1]; Nonexact.UCL[i] = CL.2[2,2] }
# Exact case
mean((Exact.LCL < 1.5) & (Exact.UCL > 1.5))
# non-exact case
mean((Nonexact.LCL < 1.5) & (Nonexact.UCL > 1.5))

## Section 3.7
fit = lm(Cost ~ Widgets, data=ProdC)
x = data.frame(c(850))
colnames(x) = c("Widgets")
predict(fit,x, interval="confidence")

## Section 3.8
predict(fit,x, interval="prediction")

## Section 3.9.1
## Simulation of data relating Height to SSN
n = 100
set.seed(12345) # so that the results will replicate perfectly
height = round(rnorm(100, 70, 4))
ssn = sample(0:9, 100, replace=T)
ssn.data = data.frame(ssn, height)
head(ssn.data)
summary(lm(height~ssn))

## Section 3.9.2
n = 100
beta0 = 70; beta1 = 0 # The null model; true beta1 = 0
ssn = sample(0:9, 100, replace=T)
height = beta0 + beta1*ssn + rnorm(100,0,4)
ssn.data = data.frame(ssn, height)
fit.1 = lm(height~ssn, data=ssn.data)
summary(fit.1)

## Figure 3.7
n = 100; beta0 = 70; beta1 = 0 # The null model; true beta1 = 0
nsim = 10000; t.stat = numeric(nsim)
for (i in 1:nsim) {ssn = sample(0:9, 100, replace=T)
  height = beta0 + beta1*ssn + rnorm(100,0,4)
  t.stat[i] = summary(lm(height~ssn))$coefficients[2,3] }
hist(t.stat, freq=F, breaks=50)
curve(dt(x,98), from=-3, to=3, add=T)
abline(v=c(qt(.025,98), qt(.975,98)), col="gray")

## Section 3.9.3
Lower.tail = pt(-1.459, 98) ; Upper.tail = 1 - pt(1.459, 98)
Lower.tail + Upper.tail

## Exercise 1
inc = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/t11_1.csv")