x <- 1: 6
fx <- 1/6
promedio_fx <- sum(x*fx)
gx <- c(rep (2/15, 5),1/3)
promedio_gx <- sum(x*gx)
simul_f <- sample(x, 1e6, fx, replace = T)
simul_g <- sample(x, 1e6, gx, replace = T)
length(simul_g) 
table(simul_g)rf
varx <- (sum(x, -promedio_fx))*(sum(x, -promedio_fx)) * fx

vary
