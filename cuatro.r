mu <- 15
sgm <- 30
x <- rnorm(1000, mu, sgm)
mean(x)
sd(x)
hist(x)
z <- (x-mu)/sgm
mean(z)
sd(z)
hist(z)
