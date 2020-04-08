
## Figure 18.1
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(charity)
Y2 = subset(CHARITY, DEPS ==2)
hist(Y2, breaks=10, freq=F, main="", xlab="Log Charitable Contributions When DEPS = 2")
rug(Y2)
legend("topleft", c(paste("mean =", round(mean(Y2),2)), paste("sd =", round(sd(Y2),2))))

## Figure 18.2
Y.LO.I = subset(CHARITY, INCOME <= 10.533)
Y.HI.I = subset(CHARITY, INCOME > 10.533) 
hist(Y.LO.I, breaks=10, freq=F, main="", lty="blank", 
 col="gray", xlab="Log Charitable Contributions", xlim = c(0,12))
hist(Y.HI.I, breaks=seq(1.9, 10.9,.5), freq=F, add=TRUE)

## Section 18.1
library(rpart)
fit1 = rpart(CHARITY ~ INCOME, maxdepth=1)
summary(fit1)

## Figure 18.3
Y.Lower = subset(CHARITY, INCOME < 10.90845)
Y.Upper = subset(CHARITY, INCOME >= 10.90845)
hist(Y.Lower, freq=F, lty="blank", xlim = c(2, 10.2), breaks=10,
 ylim = c(0, .5), xlab = "Logged Charitable Contributions",
 main = "Estimated Conditional Distributions", col="gray")
hist(Y.Upper, freq=F, lty=1, add=T, breaks=seq(1.9, 10.9,.5))

## Figure 18.4
library(rpart.plot)
rpart.plot(fit1, extra=1, digits=4)

## Figure 18.5 (requires add.loess function)
plot(INCOME, CHARITY, col="gray", pch="+")
points(c(8.5,10.9), c(6.34, 6.34), type="l", lwd=2)
points(c(10.9,12.0), c(7.35, 7.35), type="l", lwd=2)
points(c(10.9, 10.9), c(6.34,7.35), type = "l", col="gray", lwd=2)
abline(lsfit(INCOME, CHARITY), lty=2, lwd=2)
add.loess(INCOME, CHARITY, lty=3, lwd=2)
legend("topleft", c("Tree", "OLS", "LOESS"), lty=1:3, lwd=c(2,2,2))

## Figure 18.6
unique = sort(unique(INCOME))
n.unique = length(unique)
midpoints = (unique[1:(n.unique-1)] + unique[2:n.unique])/2
n.midpoints = n.unique - 1
rsq = numeric(n.midpoints)
for (i in 1:n.midpoints) {
 Indicator = INCOME > midpoints[i]
 rsq[i] = summary(lm(CHARITY~Indicator))$r.squared
}
plot(midpoints, rsq, xlab="INCOME Split Point", ylab="R Squared")
abline(v=10.90845)

## Figure 18.7
charity = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/charitytax.csv")
attach(charity)
fit2 = rpart(CHARITY ~ INCOME)
summary(fit2)
rpart.plot(fit2, extra=1, digits=3)
mean(CHARITY[INCOME >= 10.83595 & INCOME < 10.90845])
length(CHARITY[INCOME >= 10.83595 & INCOME < 10.90845])

## Figure 18.8
Income.plot = seq(8.5, 12, .001)
Income.pred = data.frame(Income.plot)
names(Income.pred) = c("INCOME")
Yhat2 = predict(fit2, Income.pred)
plot(INCOME, CHARITY, pch = "+", col="gray")
abline(lsfit(INCOME, CHARITY), lwd=2, lty=2)
points(Income.plot, Yhat2, type="l", lwd=2)
add.loess(INCOME, CHARITY, lwd=2, lty=3)
legend("topleft", c("Tree", "OLS", "LOESS"), lty=1:3, lwd=c(2,2,2))

## Figure 18.9
fit3 = rpart(CHARITY ~ INCOME + DEPS, maxdepth=2)
rpart.plot(fit3, extra=1, digits=3)

## Figure 18.10
inc.plot = seq(min(INCOME), max(INCOME), (max(INCOME)-min(INCOME))/20 )
inc.plot = rep(inc.plot,7)
deps.plot = seq(0,6,1)
deps.plot = rep(deps.plot, each=21)
all.pred = data.frame(inc.plot, deps.plot)
names(all.pred) = c("INCOME", "DEPS")

fit2 = rpart(CHARITY ~ INCOME + DEPS, maxdepth=2)
yhat.plot = predict(fit2, all.pred)

library(lattice)
wireframe(yhat.plot ~ inc.plot*deps.plot,
 xlab = "INC", ylab = "DEPS", zlab = "CHAR",
 main = "Tree Regression Function",
 drape = TRUE,
 colorkey = FALSE,
 scales = list(arrows=FALSE,cex=.8,tick.number = 5),
 screen = list(z = -20, x = -60))

## Figure 18.11
fit3 = rpart(CHARITY ~ INCOME + DEPS + AGE + MS)
rpart.plot(fit3, extra=1, digits=3)


