# One dimensional mixture
n <- 1000
mcmc <- 5000
mu <- c(100, 104)
sigma <- c(1, 1)
alpha0 <- 1
beta0 <- 3
ptrue <- rbeta(1, alpha0, beta0)
ytrue <- rbinom(1, size=1, prob=ptrue)
x <- rnorm(n, mean=mu[ytrue+1], sd=sigma[ytrue+1])

p <- rbeta(1, alpha0, beta0)
pmcmc <- ymcmc <- rep(NA, mcmc)
for (i in 1:mcmc) {
  theta <- (1 + exp(sum(log(1-p) + dnorm(x, mean=mu[2], sd=sigma[2], log=TRUE) - log(p) - dnorm(x, mean=mu[1], sd=sigma[1], log=TRUE))))^(-1)
  ymcmc[i] <- y <- rbinom(1, size=1, prob=1-theta)
  pmcmc[i] <- p <- rbeta(1, y+alpha0, 1-y+beta0)
}

plot(density(pmcmc[-(1:1000)]))
