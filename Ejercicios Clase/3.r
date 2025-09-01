n = 20
  p = 0.4
  x = 0:n
  f_binom <- dbinom(x,n,p)
  F_binom <- cumsum(f_binom) # pbinom(x,n,p)
  (df <- data.frame(x,f_binom, F_binom))
  library(ggplot2)
  ggplot(data=df, aes(x,f_binom))+geom_col()+geom_col(aes(x,F_binom), fill="purple", alpha = 0.7)
  qbinom(0.5,n,p)
  
  simulaciones <- rbinom(10000, n, p)
  mean(simulaciones <= 8)
  mean(simulaciones <= 7)


lam = 7
n = 20
x = 0:n

f_poisson <- dpois(x,lam)
F_poisson <- cumsum(f_poisson)
(df <- data.frame(x,f_poisson, F_poisson))
library(ggplot2)
ggplot(data=df, aes(x,f_poisson))+geom_col()+geom_col(aes(x,F_poisson), fill="purple", alpha = 0.7)
qpois(0.7,lam)

simulaciones <- rpois(n,lam)
simulaciones <= 8
simulaciones <= 7
mean(simulaciones <= 8)
mean(simulaciones <= 7)


v1 <- runif(10000)
v2 <- runif(10000)
v3 <- runif(10000)
v4 <- runif(10000)
v5 <- runif(10000)
suma <- v1 + v2 + v3 + v4 + v5
df <- data.frame(v1, v2, v3, v4, v5, suma)
library(ggplot2)
ggplot(data=df, aes(suma))+ geom_histogram(bins = 100)


(x <- seq(0,300,by = 0.1))
mu <- 150
sgm <- 50

f_norm <- (1/sqrt(2*pi*sgm^2))* exp((-1/2)*((x-mu)/sgm)^2)
f_dnorm <- dnorm(x, mu, sgm)
F_norm <- pnorm(x,mu,sgm)
(df <- data.frame(x, f_norm, f_dnorm, F_norm))
ggplot(df,aes(x,f_norm))+ geom_line() + geom_line(aes(x, f_dnorm),col="red", linetype = 2) + geom_line(aes(x, F_norm),col="blue", linetype = 2)
qnorm(0.8,mu, sgm)


(x <- seq(-3.5, 3.5, by = 0.1))

fz <- dnorm(x)
Fz <- pnorm(x)
(df <- data.frame(x, fz, Fz))
ggplot(df,aes(x,fz))+ geom_line() + geom_line(aes(x, fz),col="red", linetype = 2) # + geom_line(aes(x, Fz),col="blue", linetype = 2)
qnorm(0.975, lower.tail = FALSE)
pnorm(3) - pnorm(-3)
