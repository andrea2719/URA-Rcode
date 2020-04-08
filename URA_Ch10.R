
## Figure 10.1
ba = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
attach(ba)
PHD = ifelse(degree=="P",1,0)
plot(PHD, gpa); abline(lsfit(PHD, gpa))

mean(gpa[PHD==1])
mean(gpa[PHD==0])
fit = lm(gpa ~ PHD); summary(fit)
confint(fit)
t.test(gpa ~ degree, var.equal=T)

## Section 10.2
house = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/house.csv", header=T); attach(house)
table(location)

XA = ifelse(location=="A",1,0)
XB = ifelse(location=="B",1,0)
XC = ifelse(location=="C",1,0)
XD = ifelse(location=="D",1,0)

# Ordinary averages of the observed data:
mean(house$sell[location=="A"])
mean(house$sell[location=="B"])
mean(house$sell[location=="C"])
mean(house$sell[location=="D"])
mean(house$sell[location=="E"])

Fit.ind = lm(sell ~ XA + XB + XC + XD, data=house)
b = Fit.ind$coefficients
b0 = b[1]; b1 = b[2]; b2 = b[3]; b3 = b[4]; b4 = b[5]
b0+b1; b0+b2; b0+b3; b0+b4; b0
summary(Fit.ind)

## Figure 10.3
sell = c(b0+b1, b0+b2, b0+b3, b0+b4, b0)
X = 1:5
plot(X, sell, xaxt="n", xlab = "Location", ylim = c(80,105),
ylab="Mean Selling Price")
axis(1, at = 1:5, lab=c("A", "B", "C", "D", "E"))
points(X, sell, type="l")

# without you creating the indicator variables
Fit.ind1 = lm(sell ~ location, data=house)
summary(Fit.ind1)

summary(aov(sell ~ location, data=house))

## Figure 10.4
grades = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
attach(grades)
fit = lm(gpa ~ degree + gmat)
b = fit$coefficients; b0 = b[1];b1 = b[2]; b2 = b[3]
pch = ifelse(degree == "P", 17, 1)
plot(gpa ~ gmat, data=grades, pch = pch, xlim = c(400,800),
xlab = "GMAT Score", ylab = "GPA", cex=.8)
gmat.list = seq(400,800,10)
PhD.line = (b0 + b1) + b2*gmat.list
Masters.line = b0 + b2*gmat.list
points(gmat.list, PhD.line, type="l", lwd=2)
points(gmat.list, Masters.line, type="l", lty=2, lwd=2)
legend(692, 3.1, c("PhD", "Masters"), lty = c(1,2), lwd=c(2,2))
summary(fit)

## Figure 10.5 (need to run code for Figure 10.4 first)
PHD = grades[degree=="P",]; MAS = grades[degree=="M",]
abline(lsfit(PHD$gmat, PHD$gpa), lty=2, col="gray")
abline(lsfit(MAS$gmat, MAS$gpa), lty=2, col="gray")

## Section 10.4
fit.int = lm(gpa ~ degree + gmat + degree*gmat)
summary(fit.int)

## Estimated regression lines using data subsets.
# Master’s students
lsfit(gmat[degree=="M"], gpa[degree=="M"])$coefficients
#Ph.D. students
lsfit(gmat[degree=="P"], gpa[degree=="P"])$coefficients
## Estimated regression lines using the combined
## data and interaction model.
PHD = ifelse(degree=="P",1,0)
fit.int = lm(gpa ~ PHD + gmat + gmat*PHD)
b = fit.int$coefficients
b0 = b[1]
b1 = b[2]
b2 = b[3]
b3 = b[4]
# Master’s students
c(b0,b2)
# Ph.D. students
c(b0+b1, b2+b3)

## Section 10.4.1 
house = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/house.csv", header=T)
attach(house)
fit.main = lm(sell ~ location + sqfeet, data=house)
summary(fit.main)

## Figure 10.6
b = fit.main$coefficients
b0 = b[1];b1 = b[2]; b2 = b[3]; b3 = b[4]; b4 = b[5]; b5 = b[6]
pch = as.numeric(location)
plot(sell ~ sqfeet, data=house, pch=pch)
sqf = seq(1000,3000,10)
A.line = b0 + b5*sqf
B.line = b0 + b1 + b5*sqf
C.line = b0 + b2 + b5*sqf
D.line = b0 + b3 + b5*sqf
E.line = b0 + b4 + b5*sqf
points(sqf, A.line, type="l", lty=1)
points(sqf, B.line, type="l", lty=2)
points(sqf, C.line, type="l", lty=3)
points(sqf, D.line, type="l", lty=4)
points(sqf, E.line, type="l", lty=5)
legend("bottomright",c("Region A","Region B","Region C","Region D",
"Region E"), lty = 1:5, pch=1:5)

## Section 10.4.1 including interaction
fit.int = lm(sell ~ location + sqfeet + location*sqfeet , data=house)
summary(fit.int)

## Figure 10.7
b = fit.int$coefficients
b0 = b[1];b1 = b[2]; b2 = b[3]; b3 = b[4]; b4 = b[5]; b5 = b[6]
b6 = b[7]; b7 = b[8]; b8 = b[9]; b9 = b[10]
pch = as.numeric(location)
plot(sell ~ sqfeet, pch=pch, data=house)
sqf = seq(1000,3000,10)
A.line = b0 + b5*sqf
B.line = b0 + b1 + (b5+b6)*sqf
C.line = b0 + b2 + (b5+b7)*sqf
D.line = b0 + b3 + (b5+b8)*sqf
E.line = b0 + b4 + (b5+b9)*sqf
points(sqf, A.line, type="l", lty=1)
points(sqf, B.line, type="l", lty=2)
points(sqf, C.line, type="l", lty=3)
points(sqf, D.line, type="l", lty=4)
points(sqf, E.line, type="l", lty=5)
legend("bottomright", c("Location A","Location B","Location C",
"Location D","Location E"), lty = 1:5, pch=1:5)

## Section 10.5
SSE.F = sum(fit.int$residuals^2); SSE.F
SSE.R = sum(fit.main$residuals^2); SSE.R
F = ((SSE.R - SSE.F)/(58-54))/(SSE.F/54); F
1-pf(F, 4,54)

## Section 10.5.1
anova(fit.main, fit.int)

## Section 10.5.2
house = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/house.csv", header=T)
attach(house)
n = nrow(house)
locB = (location=="B"); locC = (location=="C")
locD = (location=="D"); locE = (location=="E")
sell.sim = 26 -21*locB - 21*locC - 25*locD -27*locE +
.04*sqfeet + rnorm(n,0,6.6)
fit.Full = lm(sell.sim ~ location + sqfeet + location*sqfeet)
fit.Rest = lm(sell.sim ~ location + sqfeet)
anova(fit.Rest, fit.Full)

## Figure 10.8
NSIM = 10000; F.sim = numeric(NSIM)
for (i in 1:NSIM) {
 sell.sim = 26 -21*locB - 21*locC - 25*locD -27*locE +
 .04*sqfeet + rnorm(n,0,6.6)
 fit.Full = lm(sell.sim ~ location + sqfeet + location*sqfeet)
 fit.Rest = lm(sell.sim ~ location + sqfeet)
 F.sim[i] = anova(fit.Rest, fit.Full)$F[2]
}

F.list = seq(0,5,.001); F.dist = df(F.list,4,54)
plot(F.list, F.dist, type="l", xlab = "Null F Value", 
 ylab = "F(4,54) density", ylim = c(0,.9), yaxs="i")
hist(F.sim, breaks=50, add=T, freq=FALSE, lty=2)
abline(v = qf(.95, 4,54), lwd=2)

## ANOVA test
model1 = lm(sell ~ sqfeet, data=house)
model2 = lm(sell ~ sqfeet + location, data=house)
model3 = lm(sell ~ sqfeet + location + sqfeet*location, data=house)
anova(model1, model2, model3)

## Section 10.6
grades = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
names(grades)
table(grades$sex, grades$major)
grades1 = subset(grades, major %in% c(2,70,203))
attach(grades1)

Fit.n = lm(gpa ~ major)
summary(Fit.n)

## Figure 10.9
par(mfrow=c(1,2))
plot(major, gpa, xlab = "Major", ylab="GPA")
abline(lsfit(major,gpa))
major.123 = ifelse(major == 2,1, ifelse(major==70,2,3))
plot(major.123, gpa, xaxt="n", xlab = "Major", ylab="GPA")
axis(1, at = 1:3, lab=c("002", "070", "203"))
Fit.f = lm(gpa ~ as.factor(major))
b = Fit.f$coefficients
b0 = b[1]; b1 = b[2]; b2 = b[3]
ybars = c(b0, b0+b1, b0+b2)
points(1:3, ybars, type="l")
points(1:3, ybars, pch=4, cex=2)

summary(Fit.f)

major = as.factor(major)
fit1 = lm(gpa ~ sex + major)
summary(fit1)

## Figure 10.10
Major = rep(c("2", "70", "203") , each = 2)
Sex = rep(c("M", "F"), 3)
plot.dat = data.frame(Major, Sex)
names(plot.dat) = c("major", "sex")
pred.fit1 = predict(fit1, plot.dat)
interaction.plot(Major, Sex, pred.fit1, type="b", 
 ylab = "GPA", ylim = c(3.4,3.8))

mean(gpa[major=="2" & sex =="F"])

## interaction model
fit2 = lm(gpa ~ sex + major + sex*major)
summary(fit2)

## Figure 10.11
pred.fit2 = predict(fit2, plot.dat)
interaction.plot(Major, Sex, pred.fit2, type="b", ylab = "GPA",
 ylim = c(3.4,3.8))

anova(fit1,fit2)

## Section 10.7.1
Ind = ifelse(Income < 250, 0, 1)

## Section 10.7.2
Comm = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Comm_Price.txt")
attach(Comm)

## Figure 10.14
pch = ifelse(Year < 2002, 1, 2)
par(mfrow=c(1,2))
plot(Stocks, Price, pch=pch)
abline(lsfit(Stocks, Price))
plot(Stocks, Price, pch=pch)
abline(lsfit(Stocks[Year<2002], Price[Year<2002]), lty=1)
abline(lsfit(Stocks[Year>=2002], Price[Year>=2002]), lty=2)

Ind = (Year < 2002)
fit1 = lm(Price ~ Stocks + Ind + Stocks*Ind)
summary(fit1)

fit0 = lm(Price ~ Stocks)
anova(fit0, fit1)

## Section 10.7.3
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(charity)
fit1 = lm(CHARITY ~ INCOME + DEPS)
summary(fit1)

table(DEPS)
DEPS.F = as.factor(DEPS)
fit2 = lm(CHARITY ~ INCOME + DEPS.F)
summary(fit2)

## Figure 10.15
b = fit1$coefficients
b0 = b[1]; b1 = b[2]; b2 = b[3]
yhat.lin = b0 + b1*mean(INCOME) + b2*(0:6)
b = fit2$coefficients
b0 = b[1]; b1 = b[2]; b2 = b[3:8]
yhat.ord = b0 + b1*mean(INCOME) + c(0,b2*c(1,1,1,1,1,1))
plot(0:6, yhat.lin, ylim = c(6.2, 7.1), xlab="DEPS",
ylab = "CHARITY")
points(0:6, yhat.lin, type="l")
points(0:6, yhat.ord, pch = 2)
points(0:6, yhat.ord, type="l", lty=2)
legend(2.7, 6.45, c("Linear Model", "Indicator Variable Model"),
lty=c(1,2), pch = c(1,2))

anova(fit1,fit2)

## Section 10.7.4
sub.f = as.factor(SUBJECT)
fit3 = lm(CHARITY ~ INCOME + DEPS + sub.f)
summary(fit3)
confint(fit3)

## Section 10.7.5
s = 3 # subjects
r = 100 # replications within subject
X = rnorm(s); X = rep(X, each=r) +rnorm(r*s,0,.001)
a = rnorm(s); a = rep(a, each=r)
e = rnorm(s*r,0,.001)
epsilon = a+e
Y = 0 + 0*X + rnorm(s*r) +epsilon # Y unrelated to X
sub = rep(1:s, each = r)
summary(lm(Y ~ X)) # Highly significant X effect
summary(lm(Y ~ X + as.factor(sub))) # Insignificant X effect

## Exercise 1
clinical = read.table("https://raw.githubusercontent.com/
andrea2719/URA-DataSets/master/clinical.txt")
attach(clinical)
t.test(T4 ~ DRUG, var.equal = T)

## Exercise 2
grad = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/pgs.csv", header=T)
attach(grad)
Fac.Gen = FacKnowledge + FacTeaching + FacResInClass +
FacOutsideClass + FacIntSuccess
summary(lm(Fac.Gen ~ COL))

## Exercise 4
clinical = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/clinical.txt")

## Exercise 7
firms = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/firms10.csv")
