
## Figure 17.1
x = seq(0,pi,pi/40)
x1 = rep(x, each = 41); x2 = rep(x,41)
Y = -1 + sin(x1) + 3*sin(x1) * cos(x2)
library(lattice)
wireframe(Y ~ x1*x2,
 xlab = "x1", ylab = "x2",
 main = "True Function", xlim = c(0,pi), ylim = c(0,pi),
 drape = TRUE,
 colorkey = FALSE,
 scales = list(arrows=FALSE,cex=.8,tick.number = 5),
 screen = list(z = -70, x = -70))

## Figure 17.2
x = seq(0,pi,pi/40)
x1 = rep(x, each = 41); x2 = rep(x,41)
Y = -1 + sin(x1) + 3*sin(x1) * cos(x2)
library(lattice); library(grid)
par(mfrow=c(2,2))
poly1 = lm(Y ~ x1 + x2)
poly2 = lm(Y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2))
poly3 = lm(Y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2) + I(x1^3)
 + I(x2^3) + I(x1^2*x2) + I(x1*x2^2) )
xpred = data.frame(x1,x2)
yhat1 = predict(poly1,xpred)
yhat2 = predict(poly2,xpred)
yhat3 = predict(poly3,xpred)

wf <- function(y) {
 wireframe(y ~ x1*x2,
 xlab = "x1", ylab = "x2", zlab= " ",
 main = "", xlim = c(0,pi), ylim = c(0,pi),
 drape = TRUE,
 colorkey = FALSE,
 scales = list(arrows=FALSE,cex=.8,tick.number = 5),
 screen = list(z = -70, x = -70))
}

g1 = wf(Y); g2 = wf(yhat1); g3 = wf(yhat2); g4 = wf(yhat3)
print(g1, split = c(1, 1, 2, 2), more = TRUE)
print(g2, split = c(2, 1, 2, 2), more = TRUE)
print(g3, split = c(1, 2, 2, 2), more = TRUE)
print(g4, split = c(2, 2, 2, 2))

## Figure 17.3
library(neuralnet)
df = data.frame(Y, x1,x2); set.seed(123)
nn.fit1 = neuralnet(Y ~ x1+x2, data=df, hidden = 1)
nn.fit2 = neuralnet(Y ~ x1+x2, data=df, hidden = 2)
nn.fit3 = neuralnet(Y ~ x1+x2, data=df, hidden = 3)
yhat.1 = compute(nn.fit1, xpred)$net.result
yhat.2 = compute(nn.fit2, xpred)$net.result
yhat.3 = compute(nn.fit3, xpred)$net.result
g1 = wf(Y); g2 = wf(yhat.1); g3 = wf(yhat.2); g4 = wf(yhat.3)
print(g1, split = c(1, 1, 2, 2), more = TRUE)
print(g2, split = c(2, 1, 2, 2), more = TRUE)
print(g3, split = c(1, 2, 2, 2), more = TRUE)
print(g4, split = c(2, 2, 2, 2))
plot(nn.fit3)
sum((yhat.3 - Y)^2)/2

# verification
b10 = nn.fit3$weights[[1]][[1]][1,1]; b10
b11 = nn.fit3$weights[[1]][[1]][2,1]; b11
b12 = nn.fit3$weights[[1]][[1]][3,1]; b12

b20 = nn.fit3$weights[[1]][[1]][1,2]; b20
b21 = nn.fit3$weights[[1]][[1]][2,2]; b21
b22 = nn.fit3$weights[[1]][[1]][3,2]; b22

b30 = nn.fit3$weights[[1]][[1]][1,3]; b30
b31 = nn.fit3$weights[[1]][[1]][2,3]; b31
b32 = nn.fit3$weights[[1]][[1]][3,3]; b32

# Linear functions
L1 = b10 + b11*x1 + b12*x2
L2 = b20 + b21*x1 + b22*x2
L3 = b30 + b31*x1 + b32*x2

#Nodes
N1 = 1/(1 + exp(-L1))
N2 = 1/(1 + exp(-L2))
N3 = 1/(1 + exp(-L3))

# Node weights
g0 = nn.fit3$weights[[1]][[2]][1,1]; g0
g1 = nn.fit3$weights[[1]][[2]][2,1]; g1
g2 = nn.fit3$weights[[1]][[2]][3,1]; g2
g3 = nn.fit3$weights[[1]][[2]][4,1]; g3

#Final prediction
yhat.3.check = g0 + g1*N1 + g2*N2 + g3*N3

#The hand calculation agrees with the software within machine error:
sum ( (yhat.3 - yhat.3.check)^2 )

## Section 17.3
char = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(char)
INC.S = scale(INCOME); D.S = scale(DEPS)
f = as.formula(paste("CHARITY ~ INC.S + D.S"))
dat = data.frame(CHARITY, INC.S, D.S)

library(neuralnet)
##Fit various NN models
# Single hidden layer models
fit1 = neuralnet(f, data=dat, hidden=c(1), stepmax=1e6)
fit2 = neuralnet(f, data=dat, hidden=c(2), stepmax=1e6)
fit3 = neuralnet(f, data=dat, hidden=c(3), stepmax=1e6)
fit4 = neuralnet(f, data=dat, hidden=c(4), stepmax=1e6)
# ... continue through fit12...
fit12 = neuralnet(f, data=dat, hidden=c(12), stepmax=1e6)

rmse = sqrt(mean( (CHARITY - fit4$net.result[[1]])^2 ))
rmse

## Figure 17.5
inc.pred = seq(min(INCOME), max(INCOME), (max(INCOME)-min(INCOME))/20 )
inc.pred.s = (inc.pred - mean(INCOME))/sd(INCOME)
inc.pred.s = rep(inc.pred.s,7)

deps.pred = seq(0,6,1)
deps.pred.s = (deps.pred - mean(DEPS))/sd(DEPS)
deps.pred.s = rep(deps.pred.s, each=21)

all.pred = data.frame(inc.pred.s, deps.pred.s)
names(all.pred) = c("INC.S", "D.S")
yhat.plot = compute(fit12, all.pred)$net.result

INC.plot = all.pred[,1]*sd(INCOME) + mean(INCOME)
DEPS.plot = all.pred[,2]*sd(DEPS) + mean(DEPS)

library(lattice)
wireframe(yhat.plot ~ INC.plot*DEPS.plot,
 xlab = "ln(INC)", ylab = "DEPS", zlab = "ln(CHAR)",
 main = "NN(12) Regression Function",
 drape = TRUE,
 colorkey = FALSE,
 scales = list(arrows=FALSE,cex=.8,tick.number = 5),
 screen = list(z = -70, x = -70))

## Figure 17.6
dep.p = (0:6 - mean(DEPS))/sd(DEPS)
p10 = cbind(quantile(INC.S, .10), dep.p); p10 = data.frame(p10)
colnames(p10) = c("INC.S", "D.S")
yhat.10 = compute(fit12, p10)$net.result

p50 = cbind(quantile(INC.S, .50), dep.p); p50 = data.frame(p50)
colnames(p50) = c("INC.S", "D.S")
yhat.50 = compute(fit12, p50)$net.result

p90 = cbind(quantile(INC.S, .90), dep.p); p90 = data.frame(p90)
colnames(p90) = c("INC.S", "D.S")
yhat.90 = compute(fit12, p90)$net.result

plot(0:6,yhat.10, ylim = c(5,8.5), xlab="Number of Dependents",
 ylab="Predicted ln(Charity)")
points(0:6,yhat.10, type="l",lty=1)
points(0:6,yhat.50, pch=2); points(0:6,yhat.50, type="l",lty=2)
points(0:6,yhat.90, pch=3); points(0:6,yhat.90, type="l",lty=3)
legend("topright", c(".9 Income Quantile", ".5 Income Quantile",
 ".1 Income Quantile"), lty=c(3,2,1), pch=c(3,2,1))

## R Code to Find Ten-Fold Cross-Validated RMSPE for a NN model
dat.r = dat[sample(nrow(dat)),]
folds = cut(seq(1,nrow(dat.r)),breaks=10,labels=FALSE)
# Change neural net model and then run the following code block
sspe = 0
for(i in 1:10){
 testIndexes = which(folds==i,arr.ind=TRUE)
 testData = dat.r[testIndexes, ]
 trainData = dat.r[-testIndexes, ]
 fit.tr = neuralnet(f, data=trainData, hidden=c(1), stepmax=1e6)
 pred.te = compute(fit.tr, testData[,2:3])$net.result
 sspe = sspe + sum( (testData[,1] - pred.te)^2 )
}
rmspe = sqrt(sspe/nrow(dat))
rmspe

## Figure 17.7
dep.p = (0:6 - mean(DEPS))/sd(DEPS)
p10 = cbind(quantile(INC.S, .10), dep.p); p10 = data.frame(p10)
colnames(p10) = c("INC.S", "D.S")
yhat.10 = compute(fit3, p10)$net.result

p50 = cbind(quantile(INC.S, .50), dep.p); p50 = data.frame(p50)
colnames(p50) = c("INC.S", "D.S")
yhat.50 = compute(fit3, p50)$net.result

p90 = cbind(quantile(INC.S, .90), dep.p); p90 = data.frame(p90)
colnames(p90) = c("INC.S", "D.S")
yhat.90 = compute(fit3, p90)$net.result

plot(0:6,yhat.10, ylim = c(5,8.5), xlab="Number of Dependents",
 ylab="Predicted ln(Charity)")
points(0:6,yhat.10, type="l",lty=1)
points(0:6,yhat.50, pch=2); points(0:6,yhat.50, type="l",lty=2)
points(0:6,yhat.90, pch=3); points(0:6,yhat.90, type="l",lty=3)
legend("topright", c(".9 Income Quantile", ".5 Income Quantile",
 ".1 Income Quantile"), lty=c(3,2,1), pch=c(3,2,1))

## backward elimination
C = CHARITY; I = INCOME; D = DEPS
I2 = I^2; D2 = D^2; ID = I*D
I3 = I^3; D3 = D^3; ID2 = I *D^2; I2D = I^2 *D
library(leaps)
dat = data.frame(C,I,D,I2,D2,ID,I3,D3,ID2,I2D)
back = regsubsets(C~I+D+I2+D2+ID+I3+D3+ID2+
 I2D, data = dat, method = "backward")
cbind(summary(back)$bic, summary(back)$which)


## R Code to find n-Fold Cross-Validated RMSPE for an lm model
f = as.formula(paste("C~I+D+I2+D2+ID+I3+D3+ID2+I2D")) # The specific model
n = nrow(dat)
folds = cut(seq(1,n),breaks=n,labels=FALSE)
# Change lm model and then run the following code block
sspe = 0
for(i in 1:n){
 testIndexes = which(folds==i,arr.ind=TRUE)
 testData = dat[testIndexes, ]
 trainData = dat[-testIndexes, ]
 fit.tr = lm(f, data=trainData)
 pred.te = predict(fit.tr, testData)
 sspe = sspe + sum( (testData[,1] - pred.te)^2 )
}
rmspe = sqrt(sspe/nrow(dat)); rmspe
rmse = mean(lm(f)$residuals^2); rmse

## Figure 17.9
C = CHARITY; I = INCOME; D = DEPS
I2 = I^2; D2 = D^2; ID = I*D
I3 = I^3; D3 = D^3; ID2 = I *D^2; I2D = I^2 *D
fit6 = lm(C ~ D+I2+D2+ID+I3+ID2)

I.10 = quantile(I,.10); Dg = 0:6
I.10.2 = I.10^2; Dg2 = Dg^2; I.10Dg = I.10*Dg
I.10.3 = I.10^3; I.10Dg2 = I.10*Dg^2
p10 = cbind(Dg,I.10.2,Dg2,I.10Dg,I.10.3,I.10Dg2); p10 = data.frame(p10)
colnames(p10) = c("D","I2","D2","ID","I3","ID2")

I.50 = quantile(I,.50)
I.50.2 = I.50^2; I.50Dg = I.50*Dg
I.50.3 = I.50^3; I.50Dg2 = I.50*Dg^2
p50 = cbind(Dg,I.50.2,Dg2,I.50Dg,I.50.3,I.50Dg2); p50 = data.frame(p50)
colnames(p50) = c("D","I2","D2","ID","I3","ID2")

I.90 = quantile(I,.90)
I.90.2 = I.90^2; I.90Dg = I.90*Dg
I.90.3 = I.90^3; I.90Dg2 = I.90*Dg^2
p90 = cbind(Dg,I.90.2,Dg2,I.90Dg,I.90.3,I.90Dg2); p90 = data.frame(p90)
colnames(p90) = c("D","I2","D2","ID","I3","ID2")

yhat.10 = predict(fit6, p10) 
yhat.50 = predict(fit6, p50)
yhat.90 = predict(fit6, p90)

plot(0:6,yhat.10, ylim = c(5,8.5), xlab="Number of Dependents", 
 ylab="Predicted ln(Charity)")
points(0:6,yhat.10, type="l",lty=1)
points(0:6,yhat.50, pch=2); points(0:6,yhat.50, type="l",lty=2)
points(0:6,yhat.90, pch=3); points(0:6,yhat.90, type="l",lty=3)
legend("topright", c(".9 Income Quantile", ".5 Income Quantile", 
 ".1 Income Quantile"), lty=c(3,2,1), pch=c(3,2,1))


