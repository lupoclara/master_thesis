set.seed(1234)
n <- 1000  
p <- rbeta(n, 10, 50)  
y <- rbinom(n, size=1, prob=p)
mu <- c(100, 104)
sigma <- c(1, 1)
x <- rnorm(n, mu[y+1], sigma[y+1])

alpha0 <- 2
beta0 <- 4
gibbs <- function(j,  mcmc=10000) {
  k <- x[1:j]
  n <- length(k)
  p <- rbeta(1, alpha0, beta0)
  ymcmc <- matrix(NA, n, mcmc)
  pmcmc <- xmcmc <- rep(NA, mcmc)
  for (i in 1:mcmc) {
    theta <- (dnorm(k, mu[1], sigma[1])*p/
                (dnorm(k, mu[1], sigma[1])*p 
                 + dnorm(k, mu[2], sigma[2])*(1-p)))
    y <- rbinom(n, size=1, prob=theta)
    ymcmc[,i] <- y
    pmcmc[i] <- p <- rbeta(1, sum(y)+alpha0, n-sum(y)+beta0)
    z <- rbinom(1, size=1, prob=1-p)
    xmcmc[i] <- rnorm(1, mu[z+1], sigma[z+1])
  }
  result <- list(y=ymcmc, p=pmcmc, x=xmcmc)
  return(result)
}
mcmc <- 10000
postmcmc <- xpred <- matrix(0, nrow=mcmc, ncol=10)
for (j in 1:10) {
  g <- gibbs(j*100, mcmc=mcmc)
  postmcmc[,j] <- g$p
  xpred[,j] <- g$x  
}
postmcmc <- postmcmc[-(1:3000),]
xpred <- xpred[-(1:3000),]  
par(mfrow=c(2,5))
for (j in 1:10) {
  plot(density(1-postmcmc[,j]), main=paste("Size=", i*100, sep=""))
}
par(mfrow=c(2,5))
for (j in 1:10) {
  plot(density(xpred[,j]), main=paste("Size=", i*100, sep=""))
  lines(density(x), col=2)
}
