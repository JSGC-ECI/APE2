library(tidyverse)
n <- 50
p <- 0.7
x <- 0:n
mu <- n*p
q <- 1-p
sgm <- sqrt(n*p*q)
f.binom <- dbinom(x,n,p)
f.norm <- dnorm(x, mu, sgm)
df <- data_frame(x,f.binom, f.norm)
ggplot(df, aes(x, f.binom, fill = x <= 6)) + geom_col() + geom_line(aes(x,f.norm))
a <- pbinom(33,n,p)
b <- pbinom(37,n,p )
t <- b-a 
t
c <- pnorm(33.5,mu,sgm)
d <- pnorm(37.5,mu,sgm)
h <- d-a
h




x1.1 <- rnorm(500,80,5)
x2.1 <- rnorm(400,65,3)
x1 <- sample(c(x1.1, x2.1))
df <- data.frame(x1)
ggplot(df , aes(x1)) + geom_histogram()
mean(x1)
sd(x1)


x1.2 <- rnorm(500,80,5)
x2.2 <- rnorm(400,65,3)
x2 <- sample(c(x1.2, x2.2))
df <- data.frame(x2)
mean(x2)
sd(x2)

sum.x <- x1 + x2
mean(sum.x)
ggplot(df , aes(sum.x)) + geom_histogram()

x1.3 <- rnorm(500,80,5)
x2.3 <- rnorm(400,65,3)
x3 <- sample(c(x1.3, x2.3))
df <- data.frame(x3)
mean(x3)
sd(x3)

sum.x2 <- x1 + x2 + x3
mean(sum.x2)
ggplot(df , aes(sum.x2)) + geom_histogram()

x1.4 <- rnorm(500,80,5)
x2.4 <- rnorm(400,65,3)
x4 <- sample(c(x1.4, x2.4))
df <- data.frame(x4)
mean(x4)
sd(x4)

sum.x3 <- x1 + x2 + x3 +x4
mean(sum.x3)
ggplot(df , aes(sum.x3)) + geom_histogram()

sum.x2 <- x1 + x2 + x3
mean(sum.x2)
ggplot(df , aes(sum.x2)) + geom_histogram()
