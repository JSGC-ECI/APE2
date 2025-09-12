library(ggplot2)

n <- 10
mu <-  15
sgm <- 3
s2 <- c()
(var(muestra))
iter <- 30000
for(i in 1:iter){
  muestra <- rnorm(n, mu, sgm)        # nueva muestra en cada iteración
  media <- mean(muestra)              # media muestral
  s2[i] <- sum((muestra - media)^2) / (n - 1)  # varianza muestral manual
}
s2
x <- seq(0, 25, by = 0.1)
df <- data.frame(s2)
fx <- dchisq(x, n-1)

df2 <- data.frame(x,fx)
ggplot() + geom_histogram(data=df,
                          aes(s2, y = ..density..)) + geom_line(data=df2, aes(x,fx, group = 1))

