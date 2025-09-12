library(ggplot2)
library(tidyverse)
library(patchwork)
library(plotly)

# Parámetros
n <- 100
lamda <- 0.2      # rate (λ)
iter <- 30000

# Simulación de medias
xbarras <- replicate(iter, mean(rexp(n, rate = lamda)))

# Estadísticos simulados
mean(xbarras)
var(xbarras)

# Dataframes para graficar
df <- data.frame(xbarras)

# Valores teóricos
mu <- 1/lamda
sigma <- 1/lamda / sqrt(n)

x <- seq(mu - 3*sigma, mu + 3*sigma, by = 0.01)
fx <- dnorm(x, mean = mu, sd = sigma)
df2 <- data.frame(x, fx)

# Gráfico
ggplot() +
  geom_histogram(data = df, aes(xbarras, y = ..density..), bins = 50, fill = "skyblue", color = "black") +
  geom_line(data = df2, aes(x, fx), color = "red", size = 1) +
  labs(title = "Distribución de medias muestrales (TCL)",
       x = "Media muestral", y = "Densidad") +
  theme_minimal()
