#Parcial final - Primer tercio
#Estadística inferencial
#María Katalina Leyva Diaz

############################################## Ejercicio 1:
miu <- 70
sigma <- 10

#a. Cuál es la calificación mínima que debe obtener un estudiante para estar en el 90% superior 
# de las calificaciones.

not_min <- qnorm(0.9, 70, 10)
#Para estar sobre el 90% de las calificaciones el estudiante debe obtener mínimo 82.

#b. Si se selecciona al azar un estudiante, cuál es la probabilidad de que su calificación esté 
# entre 65 y 85?

prob_1 <- pnorm(85, 70, 10) - pnorm(65, 70, 10)
#La probabilidad de que la calificación del estudiante esté entre 65 y 85 es del 62.4%

############################################## Ejercicio 2:
tiempos <- c(45, 50, 47, 52, 48, 46, 51, 49, 53, 47, 50, 48, 46, 54, 49, 52)

#a. calcule el tercer cuartil (percentil 75) de los tiempos muestrales.
sort(tiempos)[(16/2)/2*3]
#Entonces el tercer cuartil corresponde a 51.25 mniutos.
#comprobación:
summary(tiempos) 

#b. Qué se podría decir sobre la opinión de alguien que afirma que el tiempo promedio en que se 
# completa la tarea es de 45 minutos.
#Si se calcula, 
mean(tiempos)
#Se obtiene que el promedio de tiempo es de 49 minutos aproximadamente, por lo tanto, la opinión es 
#inválida.

############################################## Ejercicio 3:
#Laboratorio X:
s_2x <- 1.5
n_x <- 12

#Laboratorio Y:
s_2y <- 0.9
n_y <- 15

#Con base a la distribución F, determine la probabilidad de obtener una razón de varianzas mayor o 
#igual que la observada. ¿Es razonable suponer que ambos laboratorios tienen la misma precisión en sus 
#mediciones?

f <- s_2x/s_2y

p_3 <- pf(f, 11, 14, lower.tail = FALSE)
# - Por lo tanto la probabilidad de obtener una razón de varianzas mayor o igual que la observada es de 18%

# - No es razonable suponer que ambos laboratorios tiene la misma precisión ya que la razón de sus varianzas
#   es diferente de 1. De hecho, la variabilidad del Laboratorio X es 1.7 veces mayor que la del Laboratorio
#   Y.

############################################## Ejercicio 4:
#a. Utilizando la distribución F, encuentra la probabilidad asociada al valor obtenido de F.

f <- 1
#El valor se obtiene de simplificar F = (s2_x/sigma_x)/(s2_y/sigma_y)

p_4 <- df(1, 24, 19)
#La probabilidad asociada a F = 1 es 90%

#b. Qué conclusiones puedes extraer sobre la diferencia en la variabilidad de calificaciones entre los 
#los dos métodos de enseñanza.
#Como la razón de la variabilidad es 1, entonces se puede afirmar que la variabilidad de los métodos 
#de enseñanza es la misma.

############################################## Ejercicio 5:
#10 números aleatorios independientes siguiendo una distribución normal estándar.

#a. ¿Qué distribución tiene la suma de los cuadrados de estos números?
#   La suma de los cuadrados de estos números siguen una distribución chi-cuadrado ya que se define como
#   la suma de de v.a. normales estándar elevadas al cuadrado. En este caso, tiene 10 grados de libertad.

#b. ¿Cuál es la probabilidad de que esta suma sea menor que 15?

p_5 <- pchisq(15, 10)
#Por lo tanto, la probabilidad de que la suma sea menor que 15 es 86%.





