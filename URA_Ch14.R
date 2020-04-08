
## Figure 14.1
par(mfrow=c(2,2), mar=c(4,4,1,1))
Y = 0:10
d1=dpois(Y, .1); d2=dpois(Y, .6); d3=dpois(Y, 1.1); d4=dpois(Y, 2.0)
plot(Y, d1, type="h", yaxs="i",ylim=c(0,1), lwd=2, ylab="Poisson Probability", xlab="")
legend(6,.8, expression(paste(lambda," = 0.1")))
plot(Y, d2, type="h", yaxs="i",ylim=c(0,1), lwd=2, ylab="", xlab="")
legend(6,.8, expression(paste(lambda," = 0.6")))
plot(Y, d3, type="h", yaxs="i",ylim=c(0,1), lwd=2, ylab="Poisson Probability", xlab="y")
legend(6,.8, expression(paste(lambda," = 1.1")))
plot(Y, d4, type="h", yaxs="i",ylim=c(0,1), lwd=2, ylab="", xlab="y")
legend(6,.8, expression(paste(lambda," = 2.0")))

## Section 14.1.1
planners = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/planners.csv", header=T); head(planners, n=3)
attach(planners)
plot(prop.table(table(nplanners)), ylab="p(y)", 
 xlab="Number of Planners, y", yaxs="i", ylim = c(0,1), lwd=3)
mean(nplanners)
var(nplanners)

fit.pois = glm(nplanners~age+gender, family = poisson)
summary(fit.pois)
-2*logLik(fit.pois)[1] + 2*3

## Figure 14.4
X.pred = data.frame(c(45,65,45,65), c(0,0,1,1))
colnames(X.pred) = c("age", "gender")
pred.mu = exp(predict(fit.pois, X.pred))
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(0:6, dpois(0:6, pred.mu[1]), type="h", lwd=3, ylim = c(0,1),
 yaxs="i", ylab="Poisson Probability", xlab="")
legend(1.8,.9, c("45-Year old Males"))
plot(0:6, dpois(0:6, pred.mu[2]), type="h", lwd=3,ylim = c(0,1),
 yaxs="i", ylab="", xlab="")
legend(1.8,.9, c("65-Year old Males"))
plot(0:6, dpois(0:6, pred.mu[3]), type="h", lwd=3, ylim = c(0,1),
 yaxs="i", ylab="Poisson Probability", xlab="Financial Planners Used")
legend(1.4,.9, c("45-Year old Females"))
plot(0:6, dpois(0:6, pred.mu[4]), type="h", lwd=3, ylim = c(0,1),
 yaxs="i", ylab="", xlab="Financial Planners Used")
legend(1.4,.9, c("65-Year old Females"))

## Figure 14.5
par(mfrow=c(2,2), mar=c(4,4,1,1))
y = 0:15
plot(y, dnbinom(y, mu=1, size=.1), type="h", ylim=c(0,.8),
 ylab="Probability", xlab="", yaxs="i", lwd=2)
plot(y, dnbinom(y, mu=1, size=1), type="h", ylim=c(0,.8), ylab="",
 xlab="", yaxs="i", lwd=2)
plot(y, dnbinom(y, mu=1, size=2), type="h", ylim=c(0,.8),
 ylab="Probability", xlab="y", yaxs="i", lwd=2)
plot(y, dnbinom(y, mu=1, size=10000), type="h", ylim=c(0,.8),ylab="",
 xlab="y", yaxs="i", lwd=2)

## Section 14.2.1
library(MASS)
fit.nb = glm.nb(nplanners~age+gender)
summary(fit.nb)
-2*logLik(fit.nb)[1] + 2*4

## Figure 14.6
X.pred = data.frame(c(45,65,45,65), c(0,0,1,1))
colnames(X.pred) = c("age", "gender")
pred.mu = exp(predict(fit.nb, X.pred))
theta = fit.nb$theta
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(0:6, dnbinom(0:6, mu=pred.mu[1], size=theta), type="h", lwd=3,
 ylim = c(0,1), yaxs="i", ylab="NB Probability", xlab="")
legend(1.6,.9, c("45-Year old Males"))
plot(0:6, dnbinom(0:6, mu=pred.mu[2], size=theta), type="h",
 lwd=3,ylim = c(0,1), yaxs="i", ylab="", xlab="")
legend(1.6,.9, c("65-Year old Males"))
plot(0:6, dnbinom(0:6, mu=pred.mu[3], size=theta), type="h", lwd=3,
 ylim = c(0,1), yaxs="i", ylab="NB Probability", xlab="Financial Planners Used")
legend(1.2,.9, c("45-Year old Females"))
plot(0:6, dnbinom(0:6, mu=pred.mu[4], size=theta), type="h", lwd=3,
 ylim = c(0,1), yaxs="i", ylab="", xlab="Financial Planners Used")
legend(1.2,.9, c("65-Year old Females"))

## Figure 14.7
X.male = data.frame(40:70, rep(0,length(40:70)))
colnames(X.male) = c("age", "gender")
pred.male.NB = exp(predict(fit.nb, X.male))
X.female = data.frame(40:70, rep(1,length(40:70)))
colnames(X.female) = c("age", "gender")
pred.female.NB = exp(predict(fit.nb, X.female))
pred.male.pois = exp(predict(fit.pois, X.male))
pred.female.pois = exp(predict(fit.pois, X.female))
par(mfrow=c(1,1))
plot(40:70, pred.male.NB, type="l", ylim = c(0,1.5), 
 ylab="Mean # of Financial Planners", 
 xlab = "Age", main="Estimated Mean Functions")
points(40:70, pred.female.NB, type="l", lty=2)
points(40:70, pred.male.pois, type="l", col="gray")
points(40:70, pred.female.pois, type="l", lty=2, col="gray")
legend(40,1.3, c("Males, Poisson", "Males, NB", "Females, Poisson",
 "Females, NB"), lty=c(1,1,2,2), col =c("gray", "black", "gray", "black" ))
