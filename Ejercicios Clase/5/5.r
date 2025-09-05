x <- iris$Sepal.Length
y <- iris$Sepal.Width

xbar <- mean(x)
ybar <- mean(y)

n <- nrow(iris)

cova <- (1/(n-1))* sum((x-xbar)*(y-ybar))
cova
cov(iris[,-5])

varx <- sd(iris$Sepal.Length)
vary <- sd(iris$Sepal.Width)

corre <- cova/(varx * vary)
corre

corr <- cor(iris[,-5])
corr

