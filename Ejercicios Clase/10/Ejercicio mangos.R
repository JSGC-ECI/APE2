library(ggplot2)

# Ejercicio de campesinos
(pnorm(185,200, 4/sqrt(50)))

# Parámetros
n <- 50       # tamaño de muestra
mu <- 200     # media bajo H0
sgm <- 4      # desviación estándar poblacional
iter <- 30000 # número de simulaciones

# Simular muchas medias muestrales
xbarras <- numeric(iter)
for(i in 1:iter){
  muestra <- rnorm(n, mu, sgm)
  xbarras[i] <- mean(muestra)
}

# Pasar a data.frame
df <- data.frame(xbarras = xbarras)

# Graficar histograma + marcar valor observado (185)
ggplot(df, aes(x = xbarras)) +
  geom_histogram(aes(y = ..density..))



(pnorm(198,200, 4/sqrt(50)))

pchisq(25.33, 19 )