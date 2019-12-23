## Simulation study to compare OLS (which minimize the SSE)
 # with the LAD (which minimize the SAE) estimates.
 # https://en.wikipedia.org/wiki/Least_absolute_deviations

beta0 = 20
beta1 = 5
sigma = 4
n = 100

## Function to generate Laplace random variables
rLaplace <- function(n, m, s) {
   U = runif(n)
   Laplace <<- m + ifelse(U<.5, s*log(2*U)/sqrt(2), -s*log(2-2*U)/sqrt(2))     
   }

## Simulation study. Takes a minute or two.
Nsim = 10000
b1.OLS.normal    = numeric(Nsim)
b1.LAD.normal    = numeric(Nsim)
b1.OLS.Laplace   = numeric(Nsim)
b1.LAD.Laplace   = numeric(Nsim)
X = rnorm(n, 10, 2) 

library(quantreg)  ## For computing LAD (min SAE) estimates
for (i in 1:Nsim) {
#    X = rnorm(n, 10, 2)   # uncomment to make it random-X
    Y.normal  = beta0 + beta1*X +    rnorm(n, 0 , sigma)
    Y.Laplace = beta0 + beta1*X + rLaplace(n, 0 , sigma)    
 b1.OLS.normal[i]  = lm(Y.normal  ~ X)$coefficients[2]    
 b1.LAD.normal[i]  = rq(Y.normal  ~ X)$coefficients[2]  
 b1.OLS.Laplace[i] = lm(Y.Laplace ~ X)$coefficients[2] 
 b1.LAD.Laplace[i] = rq(Y.Laplace ~ X)$coefficients[2]
}

par(mfrow=c(2,1))
hist(b1.OLS.normal, freq=F, breaks=100, xlab="min SSE estimate of beta1, Normal errors", xlim = c(4,6)) 
abline(v=beta1, lwd=2, col="red")
hist(b1.LAD.normal, freq=F, breaks=100, xlab="min SAE estimate of beta1, Normal errors", xlim = c(4,6)) 
abline(v=beta1, lwd=2, col="red")

windows()
par(mfrow=c(2,1))
hist(b1.OLS.Laplace, freq=F, breaks=100, xlab="min SSE estimate of beta1, Laplace errors", xlim = c(4,6)) 
abline(v=beta1, lwd=2, col="red")
hist(b1.LAD.Laplace, freq=F, breaks=100, xlab="min SAE estimate of beta1, Laplace errors", xlim = c(4,6)) 
abline(v=beta1, lwd=2, col="red")

## Checking for unbiasedness - target is beta1
mean(b1.OLS.normal)
mean(b1.LAD.normal)
mean(b1.OLS.Laplace)
mean(b1.LAD.Laplace)


## Checking for accuracy - smaller sd is better because it means (assuming unbiasedness)
 # that the estimate tends to be closer to the target.

sd(b1.OLS.normal)
sd(b1.LAD.normal)

sd(b1.OLS.Laplace)
sd(b1.LAD.Laplace)

## Comment: The difference between the two OLS sd's is essentially explained by chance alone
 # since the theoretical sd values are exactly the same in either case.  

## But the main point is to compare the estimates for a particular type of distribution.
 # The reason is that when you apply this result in real life, you are stuck with 
 # whatever distribution Nature throws at you. So you want to know which estimate is best,
 # for a given (natural) distribution that produced your particular data set.

