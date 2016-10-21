rm(list = ls())
cat('/014')
#M7 Asymptotic
#example LLN
library(gridExtra)
library(ggplot2)
n <- 10000
#normal distribution
means <- cumsum(rnorm(n))/(1:n)
df <- data.frame(x = 1:n, y = means)
g <- ggplot(df, aes(x = x, y = y)) + geom_hline(yintercept = 0) +
        geom_line(size = 2) + labs(x = "# of obs.", y = "Cumulative mean") +
        ggtitle("Normal Distribution")
#bernoulii distribution
means <- cumsum(sample(0:1, n, replace = T))/(1:n)
df1 <- data.frame(x = 1:n, y = means)
p <- ggplot(df1, aes(x = x, y = y)) + geom_hline(yintercept = 0.5) +
        geom_line(size = 2) + labs(x = "# of obs.", y = "Cumulative mean") +
        ggtitle("Bernoulli Distribution")
grid.arrange(g,p,ncol = 2)

#exmple - CLT with Bernoulli Trials
# with fix p = 0.5
nosim <- 1000
cfunc <- function(x, n) 2 * sqrt(n) * (mean(x) - 0.5) 
dat <- data.frame(
        x = c(apply(matrix(sample(0:1, nosim * 10, replace = TRUE), 
                           nosim), 1, cfunc, 10),
              apply(matrix(sample(0:1, nosim * 20, replace = TRUE), 
                           nosim), 1, cfunc, 20),
              apply(matrix(sample(0:1, nosim * 30, replace = TRUE), 
                           nosim), 1, cfunc, 30)
        ),
        size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)

# with biased coin with p
nosim <- 100000
p = 0.35
cfunc <- function(x, n) sqrt(n) * (mean(x) - p) / sqrt((1-p)* p)
dat <- data.frame(
        x = c(apply(matrix(sample(0:1, prob = c((1-p),p), nosim * 10, replace = TRUE), 
                           nosim), 1, cfunc, 10),
              apply(matrix(sample(0:1, prob = c((1-p),p), nosim * 20, replace = TRUE), 
                           nosim), 1, cfunc, 20),
              apply(matrix(sample(0:1, prob = c((1-p),p), nosim * 30, replace = TRUE), 
                           nosim), 1, cfunc, 30)
        ),
        size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)

#example : CI for son height data
library(UsingR);data(father.son); x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x))) / 12

## Example CI - good vs. bad coverage Binomial Distribution

n <- 20; pvals <- seq(.1, .9, by = .05); nosim <- 1000

coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)

# increased samples smoothes the output
n <- 200; pvals <- seq(.1, .9, by = .05); nosim <- 1000

coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)


# time for Agresti-Coull Interval
n <- 20; pvals <- seq(.1, .9, by = .05); nosim <- 1000

coverage <- sapply(pvals, function(p){
        phats <- (rbinom(nosim, prob = p, size = n) + 2) / (n + 4)
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)


#Example of CI : Poisson Interval
x <- 5; t <- 94.32; lambda <- x / t
round(lambda + c(-1, 1) * qnorm(.975) * sqrt(lambda / t), 3)
poisson.test(x, T = 94.32)$conf


#bad coverage
lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t) / t
        ll <- lhats - qnorm(.975) * sqrt(lhats / t)
        ul <- lhats + qnorm(.975) * sqrt(lhats / t)
        mean(ll < lambda & ul > lambda)
})
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)

# good coverage with increased time
lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 1000
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t) / t
        ll <- lhats - qnorm(.975) * sqrt(lhats / t)
        ul <- lhats + qnorm(.975) * sqrt(lhats / t)
        mean(ll < lambda & ul > lambda)
})

ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)
