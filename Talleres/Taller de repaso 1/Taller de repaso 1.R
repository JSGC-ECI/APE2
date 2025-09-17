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

