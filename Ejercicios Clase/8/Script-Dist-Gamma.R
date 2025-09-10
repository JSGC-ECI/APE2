library(ggplot2); library(dplyr)

# ::::::: ::::::: :::::::
# Función Gamma
# ::::::: ::::::: :::::::

z <- seq(0.1,5, by=0.1)
z_gamma <- gamma(z)
df <- data.frame(z, z_gamma)

# Convexidad del logaritmo en base 10 de Gamma
ggplot(df, aes(z, log(z_gamma))) +
  geom_line()

# ::::::: ::::::: :::::::
# Distribución Gamma
# ::::::: ::::::: :::::::

alphas <- c(0.5, 1, 2, 5)   # distintas formas
theta  <- 1                 # misma escala
x <- seq(0, 12, length.out = 600)

df <- expand.grid(x = x, alpha = alphas) |>
  mutate(theta = theta,
         dens  = dgamma(x, shape = alpha, scale = theta),
         label = paste0("α=", alpha, ", θ=", theta))
dim(df)
ggplot() +
  geom_line(data=df, aes(x, dens, color = label),
            linewidth = 1, alpha=0.5) +
  labs(title = "Densidades Gamma: efecto de la forma α",
       x = "x", y = "f(x)", color = "Parámetros") +
  theme_minimal(base_size = 13)



thetas <- c(0.5, 1, 2, 5)   # distintas formas
alphas  <- 1                 # misma escala
x <- seq(0, 12, length.out = 600)
df2 <- expand.grid(x = x, theta = thetas) |>
  mutate(alpha = alphas,
         dens  = dgamma(x, shape = alpha, scale = theta),
         label = paste0("α=", alpha, ", θ=", theta))
names(df)

ggplot(data=df2) +
  geom_line(aes(x, dens, color = label),
            linewidth = 1) +
  labs(title = "Densidades Gamma: efecto de la escala θ",
       x = "x", y = "f(x)", color = "Parámetros") +
  theme_minimal(base_size = 13)

# ::::::: ::::::: :::::::
# Distribución Exponencial
# ::::::: ::::::: :::::::

set.seed(123)
lambda <- 0.5
n <- 5000
inter <- rexp(n, rate = 1/lambda)   # interarribos ~ Exp(lambda)
pexp(3,1/lambda, lower.tail = FALSE)  # Probabilidad de que la primera llamada ocurra despues de 3


library(ggplot2)
ggplot(data.frame(x = inter), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.6) +
  #stat_function(fun = function(x) lambda*exp(-lambda*x), linewidth = 1) +
  stat_function(fun = function(x) dexp(x, 1/lambda), linewidth = 1) +
  labs(title = "Interarribos de un proceso de Poisson = Exponencial(λ)",
       subtitle = paste("λ =", lambda),
       x = "Tiempo entre eventos", y = "Densidad") +
  theme_minimal(base_size = 13)

