
## Figure 13.1
x = seq(0,20,.1)
p1 = 1/(1 + exp(-(-1 + .2*x))) # Mathematically equivalent form
p2 = 1/(1 + exp(-(-4 + .4*x)))
p3 = 1/(1 + exp(-(3 - .4*x)))
plot(x,p1,type="l", ylim=c(0,1),ylab="Pr( Success | x )",yaxs="i")
points(x,p2,type="l",lty=2)
points(x,p3,type="l",lty=3)
legend(14.5,.5, c("b0= -1, b1= .2", "b0= -4, b1= .4", "b0= 3, b1= -.4"), 
 lty=c(1,2,3))

## Figure 13.2
pi.seq = seq(0.0005, .9995, .0005)
logit = log(pi.seq/(1-pi.seq))
plot(pi.seq, logit, type= "l", xaxs="i", xlim = c(0,1),
 xlab="Success Probability, p", ylab="logit(p)")
abline(v=.5, lty=2)
abline(h=0, lty=3)

## Section 13.1.1
dist = rep(c(3,6,9,12,15,18,21), each=3)
success = c("Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y", "N", "N", "N",
 "Y", "N", "N", "N", "N", "N", "N", "Y", "N")
Y = ifelse(success=="Y",1,0)
par(mar=c(4,4,2,2))
par(mfrow=c(2,1))
plot(dist,Y)
set.seed(12345)
plot(jitter(dist,.3), jitter(Y,.2))

fit <- glm(Y ~ dist, family = "binomial")
summary(fit)

## Figure 13.4
par(mfrow=c(1,1))
b = fit$coefficients; b0 = b[1]; b1 = b[2]
dist.plot = seq(0,24,.01)
prob.est = exp(b0+b1*dist.plot)/(1+exp(b0+b1*dist.plot))
plot(dist.plot, prob.est, type="l", ylim=c(0,1), xlab="Distance",
 ylab="Probability")
set.seed(12345)
points(jitter(dist, .3), jitter(Y,.2))
abline(v=12, lty=2); abline(h=0.464, lty=2)

## Assess degree of curvature in the true logit function
dist2 = dist^2
fit2 <- glm(Y ~ dist + dist2, family = "binomial")
summary(fit2)
c = fit2$coefficients; c0 = c[1]; c1 = c[2]; c2 = c[3]
dist.plot = seq(0,24,.01)
logit.lin = b0 + b1*dist.plot
logit.quad = c0 + c1*dist.plot + c2*dist.plot^2

par(mfrow=c(1,2))
plot(dist.plot, logit.lin, type="l", ylab="logit", xlab="Distance")
points(dist.plot, logit.quad, type="l", lty=2)
legend(15,2.5, c("Linear", "Quad"), lty = c(1,2))
prob1.est = exp(logit.lin)/(1+exp(logit.lin))
prob2.est = exp(logit.quad)/(1+exp(logit.quad))

plot(dist.plot, prob1.est, type="l", ylim = c(0,1), xlab="Distance", 
 ylab="Probability")
points(dist.plot, prob2.est, type="l", lty=2)
legend(15,.8, c("Linear", "Quad"), lty = c(1,2))
points(jitter(dist), Y)

anova(fit,fit2, test="LRT")
var(fit$fitted.values)/var(Y)
var(fit2$fitted.values)/var(Y)

## Section 13.3 (Figure 13.7)
Rating = c( 1, 1, 2, 2, 1, 3, 3, 1, 1, 2, 3, 2, 1, 2)
Salary = c(15, 30, 20, 30, 40, 45, 49, 16, 20, 40, 55, 56, 24, 31)
plot(Salary, Rating)

## Figure 13.8
Rating = c( 1,  1,  2,  2,  1,  3,  3,  1,  1,  2,  3,  2,  1,  2)
Salary = c(15, 30, 20, 30, 40, 45, 49, 16, 20, 40, 55, 56, 24, 31)
library(MASS)
fit = polr(as.factor(Rating) ~ Salary, method="probit")
summary(fit)

# hipotetical data for 200 employees
Salary.h = round(rnorm(200,40,10))
Z = 0.08423*Salary.h + rnorm(200,0,1)
plot(Salary.h, Z, xlab="Salary", ylab="Boss.liking",xlim=c(10,70))
abline(h=c(fit$zeta[1],fit$zeta[2]))
text(12,6, c("Y=3"),cex=1.2)
text(12,3, c("Y=2"),cex=1.2)
text(12,0, c("Y=1"),cex=1.2)

## fitting the ordinal model
library(MASS); fit = polr(as.factor(Rating) ~ Salary, method="probit")
summary(fit)
fit$fitted.values
cbind(Salary, fit$fitted.values)

## Figure 13.9
Plot.data = data.frame(10:60)
colnames(Plot.data) = c("Salary")
pred = predict(fit, Plot.data, "probs")
par(mfrow=c(1,2))
plot(10:60, pred[,1], type="l", ylim = c(0,1.0), ylab =
 "Probability", xlab="Salary")
points(10:60, pred[,2], type="l", ylim = c(0,1), lty=2)
points(10:60, pred[,3], type="l", ylim = c(0,1), lty=3)
legend(22,1.05, c("1=Lowest", "2=Middle", "3=Highest"), lty=1:3)
EY = 1*pred[,1] + 2*pred[,2] + 3*pred[,3]
plot(10:60, EY, type="l", ylim = c(1,3), ylab="Expected Rating",
 xlab = "Salary")
2*pnorm(-2.643)

## Exercise 2
pron = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/pron.csv")
attach(pron)
X = Age
Y = ifelse(Q4==1, 1,0)
