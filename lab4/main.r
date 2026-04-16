
# 1
dane = c(0.46, 0.61, 0.52, 0.48, 0.57, 0.54, 0.47, 0.63, 0.51, 0.49, 0.58, 0.55)

# b)
mean(dane)
var(dane)

# c)
n = length(dane)
x_bar = mean(dane)
s = sd(dane)
alpha = 0.05
t = qt(1-alpha/2, n-1)
L = x_bar - t * s /sqrt(n)
U = x_bar + t * s /sqrt(n)
c(L,U)

t.test(dane,conf.level = 0.95)

# e)
n <- length(dane)
s2 <- var(dane)
alpha <- 0.05
chi_gorne <- qchisq(1 - alpha/2, df = n - 1)
chi_dolne <- qchisq(alpha/2, df = n - 1)
L <- (n - 1) * s2 / chi_gorne
U <- (n - 1) * s2 / chi_dolne
c(L, U)

library("TeachingDemos")
sigma.test(dane, conf.level = 0.95)

# 2
dane = c(16, 0, 0, 2, 3, 6, 8, 2, 5, 0, 12, 10, 5, 7, 2, 3, 8, 17, 9, 1);
# b)
mean(dane)

# c)
t.test(dane, conf.level = 0.95)

# 3
dane = c(1.87, 2.28, 1.77, 2.13, 1.43, 1.64, 2.38, 1.39, 1.94, 2.68, 1.95, 0.86, 1.98, 1.69, 1.15)
s = 0.7

# a)
n <- length(dane)
x_bar <- mean(dane)
sigma <- s
alpha <- 0.02
z <- qnorm(1 - alpha/2)
L <- x_bar - z * sigma / sqrt(n)
U <- x_bar + z * sigma / sqrt(n)
c(L, U)

# b)
alpha <- 0.02
sigma <- s
E <- 0.3/2
z <- qnorm(1 - alpha/2)
n_min <- (z * sigma / E)^2
ceiling(n_min)

# 4)
dane = c(4.28, 3.3, 4.22, 2.77, 2.75, 2.93, 3.86, 3.05, 4.12, 2.88, 3.94, 4.99, 2.08, 4.35, 2.7, 4.09, 2.81, 2.82)

# a)
mean(dane)
var(dane)

# b)
t.test(dane, conf.level = 0.9)

# c)
library("TeachingDemos")
sigma.test(dane, conf.level = 0.95)

# 5)
dane = c(17, 21, 20, 18, 19, 22, 20, 21, 16, 19)
s = 3

mean(dane)

n <- length(dane)
x_bar <- mean(dane)
sigma <- s
alpha <- 0.05
z <- qnorm(1 - alpha/2)
L <- x_bar - z * sigma / sqrt(n)
U <- x_bar + z * sigma / sqrt(n)
c(L, U)

# 6)
alpha <- 0.05
sigma <- sqrt(25)
E <- 1
z <- qnorm(1 - alpha/2)
n_min <- (z * sigma / E)^2
ceiling(n_min)

# 7)

# a)
n <- 365
x_bar <- 102
sigma <- sqrt(81)
alpha <- 0.02
z <- qnorm(1 - alpha/2)
L <- x_bar - z * sigma / sqrt(n)
U <- x_bar + z * sigma / sqrt(n)
c(L, U)

# b)
# to jest nieprawdopodobna sytuacja ze zuzycie osiagnie 122 hl

# 8)
s = 0.3

# 0.9 ufnosci
alpha <- 0.1
sigma <- s
E <- 0.1
z <- qnorm(1 - alpha/2)
n_min <- (z * sigma / E)^2
ceiling(n_min)

# 0.99 ufnosci
alpha <- 0.01
sigma <- s
E <- 0.1
z <- qnorm(1 - alpha/2)
n_min <- (z * sigma / E)^2
ceiling(n_min)

# 9)
x = 4
n = 100
p_hat <- x / n
alpha <- 0.05
z <- qnorm(1 - alpha/2)
SE <- sqrt(p_hat * (1 - p_hat) / n)
L <- p_hat - z * SE
U <- p_hat + z * SE
c(L, U)

