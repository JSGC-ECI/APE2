library(ggplot2)
library(dplyr)

# --- Función para generar una muestra ---
generar_muestra <- function(n1 = 500, mu1 = 80, sd1 = 5,
                            n2 = 400, mu2 = 65, sd2 = 3) {
  x1 <- rnorm(n1, mu1, sd1)
  x2 <- rnorm(n2, mu2, sd2)
  X  <- sample(c(x1, x2))
  return(X)
}
n <- 50
# --- Generar 9 muestras ---
set.seed(123) # para reproducibilidad
muestras <- replicate(n, generar_muestra(), simplify = FALSE)

# Calcular medias y desviaciones estándar
estadisticas <- tibble(
  muestra = 1:9,
  media   = sapply(muestras, mean),
  sd      = sapply(muestras, sd)
)

print(estadisticas)

# --- Suma de las 9 simulaciones ---
suma.x <- Reduce(`+`, muestras)

df <- data.frame(suma.x)

# --- Distribución teórica ---
y  <- seq(n*60, n*90, by = 0.1)
fy <- dnorm(y, mean = n * 73.4, sd = sqrt(n) * 8.61)
df2 <- data.frame(y, fy)

# --- Gráfico ---
ggplot(df, aes(suma.x, y = ..density..)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_line(data = df2, aes(x = y, y = fy), color = "red", size = 1)
