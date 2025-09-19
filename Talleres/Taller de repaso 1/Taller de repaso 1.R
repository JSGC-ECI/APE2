# 1.  Se lanza una moneda 100 veces, y la probabilidad de obtener cara es de 0,5. Utilice la aproximación normal a la distribución binomial para estimar la probabilidad de que el número de caras esté entre 40 y 60 (inclusive).
# Parámetros
n = 100
p = 0.5
mu <- n*p
sgm <- sqrt(n*p*(1-p))

# Intervalo
x <- 40
y <- 60
lower <- x - 0.5
upper <- y + 0.5

# Aproximación normal con corrección por continuidad
prob_normal <- pnorm(upper, mean = mu, sd = sgm) - pnorm(lower, mean = mu, sd = sgm)

# Mostrar resultados
cat("Aproximación normal (con corrección):", round(prob_normal, 6), "\n")

# 2. . En una población grande, el 8% de las personas son zurdas. Se selecciona una muestra aleatoria de 500 personas. Utilice la aproximación normal a la distribución binomial para calcular la probabilidad de que el número de personas zurdas sea al menos 50, pero no más de 60.
# Parámetros
n <- 500
p <- 0.08
mu <- n * p
sgm <- sqrt(n * p * (1 - p))

# Intervalo
x <- 50
y <- 60
lower <- x - 0.5
upper <- y + 0.5

# Aproximación normal con corrección por continuidad
prob_normal <- pnorm(upper, mean = mu, sd = sgm) - pnorm(lower, mean = mu, sd = sgm)

# Mostrar resultados
cat("Aproximación normal (con corrección):", round(prob_normal, 6), "\n")



##############################################
# Ejercicio 3
# Distribución normal con media = 70 y sd = 10
##############################################

# Definir parámetros de la distribución
miu <- 70   # media
sgm <- 10   # desviación estándar

# Calcular la probabilidad de que X > 85
# pnorm calcula P(X ≤ valor), por eso usamos 1 - pnorm()
p1 <- pnorm(85, miu, sgm)   # P(X ≤ 85)
prob_mayor_85 <- 1 - p1     # P(X > 85)
prob_mayor_85   # Resultado esperado ≈ 0.0668

# Calcular el cuartil correspondiente al percentil 90
# qnorm calcula el valor de X tal que P(X ≤ x) = probabilidad
per <- qnorm(0.90, miu, sgm)
per   # Resultado esperado ≈ 82.815


##############################################
# Ejercicio 11
# Distribución exponencial con media = 500
##############################################

# A) Probabilidad de que exactamente 3 bombillas (de 10) duren más de 600 horas
# Se da que cada bombilla tiene probabilidad 0.2 de durar más de 600 horas
n <- 10        # número de bombillas
p <- 0.2       # probabilidad de éxito
p_binomial <- dbinom(3, n, p)  # P(X = 3) con X ~ Binomial(10, 0.2)
p_binomial    # Resultado esperado ≈ 0.2013

# B) Para una sola bombilla, calcular P(X > 600) usando la distribución exponencial
miu <- 500
lambda <- 1/miu   # parámetro λ de la exponencial
p_exp <- 1 - pexp(600, lambda)   # P(X > 600)
p_exp   # Resultado esperado ≈ 0.3012


##############################################
# Ejercicio 13
# Peso de manzanas ~ Normal(150, 20), muestra de tamaño 25
##############################################

# Parámetros poblacionales
miu <- 150
sgm <- 20
n <- 25

# Media de la distribución muestral (igual a la media poblacional)
media_muestral <- miu
media_muestral  # 150

# Desviación estándar de la media muestral
sgm1 <- sgm/sqrt(n)
sgm1   # Resultado esperado = 4

# Calcular la probabilidad de que la media de la muestra sea menor a 145
prob_media_menor_145 <- pnorm(145, miu, sgm1)
prob_media_menor_145   # Resultado esperado ≈ 0.1056


##############################################
# Ejercicio 18
# Distribución conjunta discreta
##############################################

# Definir los valores de X, Y y sus probabilidades
x <- c(0, 1, 2)                # número de defectuosos
y <- c(2, 1, 0)                # número de no defectuosos
p <- c(0.4, 0.3, 0.3)          # probabilidades asociadas

# Crear un data frame con la distribución conjunta
df <- data.frame(x, y, p)
df

# Verificar que la suma de probabilidades sea 1 (condición de validez)
suma_prob <- sum(df$p)
suma_prob   # Resultado esperado = 1

# Distribución marginal de X (sumar probabilidades por cada valor de X)
marginal_X <- tapply(df$p, df$x, sum)
marginal_X   # P(X=0)=0.4, P(X=1)=0.3, P(X=2)=0.3

# Distribución marginal de Y (sumar probabilidades por cada valor de Y)
marginal_Y <- tapply(df$p, df$y, sum)
marginal_Y   # P(Y=2)=0.4, P(Y=1)=0.3, P(Y=0)=0.3


