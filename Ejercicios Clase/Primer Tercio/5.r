library(tidyverse)
n <- 1000
p <- 0.7
x <- 0:n
mu <- n*p
q <- 1-p
sgm <- sqrt(n*p*q)
f.binom <- dbinom(x,n,p)
f.norm <- dnorm(x, mu, sgm)
df <- data_frame(x,f.binom, f.norm)
ggplot(df, aes(x, f.binom, fill = x <= 6)) + geom_col() + geom_line(aes(x,f.norm))
pbinom(690,n,p)
pnorm(690.5,mu,sgm)
