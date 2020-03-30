## Figure 1.1
area = seq(240, 380,.001)
pdf.9 = dnorm(area, pi*9^2, 1)
pdf.10 = dnorm(area, pi*10^2, 1)
plot(area, pdf.9, type="l", yaxs="i", ylim = c(0, 1.2*max(pdf.9)),
  ylab = "p(y|x)", xlab = "area, y", cex.axis=0.8, cex.lab=0.8)
points(area, pdf.10, type="l", lty=2)
legend("topright", inset=0.05, c("radius=9", "radius=10"),
  lty = c(1,2), cex=0.8)

## Figure 1.2
assets = seq(0, 1200,.1)
pdf.27 = dlnorm(assets, 4.2 , .8)
pdf.35 = dlnorm(assets, 5.7, .8)
plot(assets, pdf.27, type="l", yaxs="i", ylim = c(0, 1.2*max(pdf.27)),
  ylab = "p(y|x)", xlab = "assets, y (thousands)", cex.axis=0.8,
cex.lab=0.8)
points(assets, pdf.35, type="l", lty=3)
legend("topright", inset=0.05, c("age=27", "age=35"), lty = c(1,2),
  cex = 0.9)

## Figure 1.4
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
attach(ProdC)
plot(Widgets, Cost)
abline(lsfit(Widgets, Cost), lwd=2)
fit = lm(Cost ~ Widgets)
summary(fit)

## Figure 1.5
Worth = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/Pass.txt")
attach(Worth)
plot(Age, P.assets, ylab = "Accumulated Personal Assets",
  cex.lab = 0.8)
abline(lsfit(Age, P.assets), lwd=2)

## Figure 1.6
G.P.A. = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/gpa_gmat.txt")
attach(G.P.A.)
## Data for Ph.D. students.
gpa.phd = gpa[degree=='P']
gmat.phd = gmat[degree=='P']
## Data for Masters students.
gpa.mas = gpa[degree=='M']
gmat.mas = gmat[degree=='M']
par(mfrow=c(1,2))
plot(gmat.phd, gpa.phd, ylim = c(2.4,4.0), xlim = c(400,800),
  ylab= 'Grade Point Average ', xlab = 'GMAT Score ',
  main = 'Ph.D. Students ')
abline(lsfit(gmat.phd, gpa.phd), lwd=2)
plot(gmat.mas, gpa.mas, ylim = c(2.4,4.0), xlim = c(400,800),
  ylab= 'Grade Point Average ', xlab = 'GMAT Score ',
  main = 'Masters Students ')
abline(lsfit(gmat.mas, gpa.mas), lwd=2)

## Fig 1.7
library(scatterplot3d)

Y = seq(0,100,.1)
X = rep(0, 1001)
Z = dnorm(Y, 50+2*X, 5)
reg = scatterplot3d(X,Y,Z, pch=".", xlab = "Hours Studying, 
  X", ylab = "Exam Score, Y", zlab = "Conditional Density", 
  type="l", box=F, angle=120, xlim= c(0,20))

X = rep(5, 1001)
Z = dnorm(Y, 50+2*X, 5)
reg$points3d(X,Y,Z,type="l")

X = rep(10, 1001)
Z = dnorm(Y, 50+2*X, 5)
reg$points3d(X,Y,Z,type="l")

X = rep(15, 1001)
Z = dnorm(Y, 50+2*X, 5)
reg$points3d(X,Y,Z,type="l")

floorx = seq(0,20,.1)
floory = (50+2*floorx)
floorz = rep(0, length(floorx))
reg$points3d(floorx, floory, floorz,type="l", lty=2, lwd=2)

x = c(0,0); y = c(50,50); z = c(0, dnorm(50,50,5))
reg$points3d(x, y, z,type="l", col="gray")

x = c(5,5); y = c(60,60); z = c(0, dnorm(60,60,5))
reg$points3d(x, y, z,type="l", col="gray")

x = c(10,10); y = c(70,70); z = c(0, dnorm(70,70,5))
reg$points3d(x, y, z,type="l", col="gray")

x = c(15,15); y = c(80,80); z = c(0, dnorm(80,80,5))
reg$points3d(x, y, z,type="l", col="gray")

Hours = runif(30,0,20)
Exam = rnorm(30, 50+2*Hours, 5)
z = rep(0,30)
reg$points3d(Hours, Exam, z)

## Section 1.8
ProdC = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/ProdC.txt")
attach(ProdC)
plot(Widgets, Cost)
## Plausible true parameters of your simulation model.
beta0 = 55 # A plausible value of the intercept
beta1 = 1.5 # A plausible value of the slope
sigma = 250 # A plausible value of the conditional standard deviation
n = length(Cost)

## Figure 1.8
par(mfrow=c(2,2))
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,sigma)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,sigma)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,sigma)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,sigma)
plot(Widgets, sim.Cost)

## Figure 1.9
par(mfrow=c(2,2))
sim.Cost = 1000*(sin(Widgets/120) + rnorm(n,0,.2)) + 2000
plot(Widgets, sim.Cost)
sim.Cost = 1000*(sin(Widgets/120) + rnorm(n,0,.2)) + 2000
plot(Widgets, sim.Cost)
sim.Cost = 1000*(sin(Widgets/120) + rnorm(n,0,.2)) + 2000
plot(Widgets, sim.Cost)
sim.Cost = 1000*(sin(Widgets/120) + rnorm(n,0,.2)) + 2000
plot(Widgets, sim.Cost)

## Figure 1.10
par(mfrow=c(2,2))
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,1)*(Widgets - 600)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,1)*(Widgets - 600)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,1)*(Widgets - 600)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + rnorm(n,0,1)*(Widgets - 600)
plot(Widgets, sim.Cost)

## Figure 1.11
par(mfrow=c(2,2))
sim.Cost = beta0 + beta1*Widgets + sigma *(rt(n,4)^2 -2)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + sigma *(rt(n,4)^2 -2)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + sigma *(rt(n,4)^2 -2)
plot(Widgets, sim.Cost)
sim.Cost = beta0 + beta1*Widgets + sigma *(rt(n,4)^2 -2)
plot(Widgets, sim.Cost)

## Figure 1.12
library(shape)
x = seq(0,4,1); y = 2+3*x
plot(x,y, type="l", ylim = c(0,15), xlim = c(-2,4),
  ylab="E(Y|X=x)" , cex.axis=0.8, cex.lab=0.8)
x1 = seq(-1,0,1); y1 = 2+3*x1
points(x1,y1,type="l", lty=2); abline(v=0)
text(-1.2,2, bquote(paste(beta[0])), adj=c(.5,.5), cex=1.2)
Arrows(-1. ,2., 0, 2, arr.type="triangle",arr.adj=1)
points(c(1,2), c(5,5), type="l",lty=3)
points(c(2,2), c(5,8), type="l",lty=3)
text(3.2,6.5, bquote(paste(beta[1])), adj=c(.5,.5), cex=1.2)
Arrows(3.0 ,6.5, 2, 6.5, arr.type="triangle",arr.adj=1)

## Figure 1.13
par(mfrow=c(1,2))
x = seq(0, 40, 0.01)
mu1 = 30 - 3*x +.1*x^2
plot(x,mu1, type="l", cex.axis=0.8, cex.lab=0.8,
  ylab = bquote(paste(mu[1])))
abline(lsfit(x,mu1), lty=2)
mu2 = 30 - .3*x +.002*x^2
plot(x,mu2, type="l", cex.axis=0.8, cex.lab=0.8,
  ylab = bquote(paste(mu[2])))
abline(lsfit(x,mu2), lty=2)

## Figure 1.14
x = c(2,2,3,3,3,3,4,4,4,4)
y = c(2,3,5,2,4,4,5,4,4,5)
x1 = c(2,3,4)
f.hat = c(2.5,3.75,4.5)
plot(x, jitter(y,.5), ylab="Rating of Instructor (jittered)",
  xlab="Expected Grade" , cex.axis=0.8, cex.lab=0.8)
points(x1, f.hat, pch = "X")
points(x1,f.hat, type="l", lty=2)

## add.loess function
add.loess <- function(X,Y, span = .75,
  col="black", lty=1, lwd=1){
  lines(spline(X,predict(loess(Y~X, span=span,
  family="gaussian"))), col=col,lty=lty, lwd=lwd)}

## Figure 1.15
djia = read.csv("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/DJIA.csv", header=T)
par(mfrow=c(1,2))
attach(djia)
Trading.Day = 1:length(DJIA)
plot(Trading.Day, DJIA, xlab="2/13/2017 - 2/13/2018 Trading Day")
add.loess(Trading.Day, DJIA, lty=2, lwd=2)
add.loess(Trading.Day, DJIA, span=.1, lwd=2)
set.seed(12346)
X = rnorm(253)
Y = X + rnorm(253)
plot(X,Y)
abline(0,1, lwd=3)
add.loess(X,Y, lty=2, lwd=2)
add.loess(X,Y, span=.1, lwd=2)

## Figure 1.16
cp = read.table("https://raw.githubusercontent.com/andrea2719/
URA-DataSets/master/complex.txt")
attach(cp)
plot(Complex, jitter(Pref), xlab="Complexity",
  ylab="Preference (jittered)")
abline(lsfit(Complex, Pref))
add.loess(Complex, Pref, lty=2)

## Figure A.1
mux = 175; muy=mux; sx = 8; sy=sx; r = .4;
sxy = r*sx*sy; x = seq(150,200,length=41); y=x;
pxy = function(x,y) {
  zx = (x-mux)/sx; zy = (y-muy)/sy;
  volume = 2*pi*sx*sy*sqrt(1-r^2)
  kernel = exp(-.5*(zx^2+zy^2-2*r*zx*zy)/(1-r^2))
  kernel/volume }
density = outer(x,y,pxy)
persp(x, y, density, theta=10, phi=20, r=50, d=0.1, expand=0.5,
  ltheta=90, lphi=180, shade=0.75, ticktype="detailed", nticks=5,
  xlab="\nX=Father's Height (cm)", ylab= "\nY=Son's Height (cm)",
  zlab="\n\np(x,y)", cex.axis=0.8, cex.lab=0.9)

## Figure A.2
library(MASS)
Mu = c(175, 175)
Sigma = matrix(c( 8^2, .4*8*8,
               .4*8*8, 8^2) , nrow = 2)
XY.sim = mvrnorm(n = 10000, Mu, Sigma)
plot(XY.sim, pch=".", xlab="Father's Height (cm)",
  ylab = "Son's Height (cm)")
contour(x = x, y = y, z = density,
  nlevels=5, lwd=1.6, add = TRUE, drawlabels=FALSE)

## Figure A.3
Father = 150:200
Naive.Guess = Father
Cond.Mean = 105 + .4*Father
plot(Father, Naive.Guess, type="l", xlab = "Father's Height",
  ylab = "Son's Height")
points(Father, Cond.Mean, type="l", lty=2)
abline(h = 175, lty=3)
arrows(190,189.5, 190, 182)
arrows(160,160.5, 160, 168)



