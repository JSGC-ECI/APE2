library(ggplot2)
library(dplyr)

# --- Función para generar una muestra ---
generar_muestra <- function(n1 = 500, n2 = 400) {
  x1 <- rnorm(n1)
  x2 <- rnorm(n2)
  X  <- sample(c(x1, x2))
  return(X)
}

# --- Generar 9 muestras ---
set.seed(123) # reproducibilidad
muestras <- replicate(9, generar_muestra(), simplify = FALSE)

# Medias y desviaciones estándar
estadisticas <- tibble(
  muestra = 1:9,
  media   = sapply(muestras, mean),
  sd      = sapply(muestras, sd)
)

print(estadisticas)

# --- Suma de cuadrados de las 9 simulaciones ---
suma.x <- Reduce(`+`, lapply(muestras, function(x) x^2))

df <- data.frame(suma.x)

# --- Distribución teórica Chi-cuadrado ---
k <- 10
y  <- seq(0, 20, by = 0.1)
fy <- dchisq(y, df = k)
df2 <- data.frame(y, fy)

# --- Gráfico ---
ggplot(df, aes(suma.x, y = ..density..)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_line(data = df2, aes(x = y, y = fy), color = "red", size = 1)

