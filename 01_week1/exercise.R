rm(list = ls())
cat('\014')
#Week1
#M2 - Probability
#L0203 - probability density functions
x <- c(-0.5, 0, 1, 1, 1.5)
y <- c(0, 0, 2, 0, 0)
plot(x, y, lwd = 3, frame = F, type = 'l')
#q1 : What is the probability that 75% or fewwer of calls get adressed?
pbeta(0.75,2,1)
#M4 Expected values
#L0401 expected values
# example 1
library(UsingR); data(galton); library(ggplot2)
# plot galton data
g <- ggplot(galton, aes(x = child))
# add histogram for children data
g <- g + geom_histogram(fill = "salmon", binwidth = 1, aes(y = ..density..), colour = 'black')
# add density smooth
g <- g + geom_density(size = 2)
# add vertical line
g <- g +geom_vline(xintercept = mean(galton$child), size = 2)
# print graph
g
#example 2
library(manipulate)
myHist <- function (mu){
        data(galton)
        # plot galton data
        g <- ggplot(galton, aes(x = child))
        # add histogram for children data
        g <- g + geom_histogram(fill = "salmon", binwidth = 1, aes(y = ..density..), colour = 'black')
        # add density smooth
        g <- g + geom_density(size = 2)
        # add vertical line
        g <- g + geom_vline(xintercept = mu, size = 2)
        # calculate mse
        mse <- round(mean((galton$child - mu)^2),3)
        g <- g + labs(title=paste('mu = ',mu, " MSE = ", mse))
        g
}
manipulate(myHist(mu), mu = slider(62,74,step = 0.5))

# example 3
# related to sample mean
nosim <- 1000
# average of x die rolls
# simulate data for sample size  1 to 4
dat <- data.frame(
        x = c(sample(1:6,nosim, replace = T),
        apply(matrix(sample(1:6, nosim * 2, replace = T),nosim),1,mean),
        apply(matrix(sample(1:6, nosim * 3, replace = T),nosim),1,mean),
        apply(matrix(sample(1:6, nosim * 4, replace = T),nosim),1,mean)),
        size = factor(rep(1:4,rep(nosim,4)))
)
head(dat)
names(dat)
g <- ggplot(dat, aes(x = x, fill = size)) 
g <- g + geom_histogram(alpha = 0.2, binwidth = .25, colour = 'black')
g + facet_grid(. ~ size)

# average of x coin flips
# simulate data for sample size  1 to 4
dat <- data.frame(
        x = c(sample(1:2,nosim, replace = T),
              apply(matrix(sample(1:2, nosim * 10, replace = T),nosim),1,mean),
              apply(matrix(sample(1:2, nosim * 20, replace = T),nosim),1,mean),
              apply(matrix(sample(1:2, nosim * 30, replace = T),nosim),1,mean)),
        size = factor(rep(c(1,10,20,30),rep(nosim,4)))
)
head(dat)
names(dat)
g <- ggplot(dat, aes(x = x, fill = size)) 
g <- g + geom_histogram(alpha = 0.2, binwidth = .25, colour = 'black')
g + facet_grid(. ~ size)
