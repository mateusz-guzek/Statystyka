# 1
IrisSepalWidth = read.csv("ai1/IrisSepalWidth.csv", sep = ";", dec = ".")

#a)
for (i in 1:length(colnames(IrisSepalWidth))) {
  print(paste("dane dla: ", colnames(IrisSepalWidth)[i]))
  print(paste("srednia: ", mean(IrisSepalWidth[, i])))
  print(paste("mediana: ", median(IrisSepalWidth[, i])))
  print(paste("kwantyl 1: ", quantile(IrisSepalWidth[, i], prob = c(0.25))))
  print(paste("kwantyl 3: ", quantile(IrisSepalWidth[, i], prob = c(0.75))))
  print(paste("odchylenie standardowe: ", sd(IrisSepalWidth[, i])))
  wsp_zmiennosci = sd(IrisSepalWidth[, i]) / mean(IrisSepalWidth[, i])
  print(paste("wspolczynnik zmiennosci: ", wsp_zmiennosci))
  print("====================================================")
}

#b)
par(mfrow = c(1, 3))
for (i in 1:length(colnames(IrisSepalWidth))) {
  hist(
    IrisSepalWidth[, i],
    freq = T,
    main = colnames(IrisSepalWidth)[i],
    col = rainbow(360, 1, 1, i / 4),
    xlab = "szerokość kielicha"
  )
}


# table(cut(IrisSepalWidth[, 1], breaks = seq(from = 0, to = 50, by = 10)))

#c)

par(mfrow = c(1, 1))
boxplot(IrisSepalWidth)


# 2
p = 0.3
n = 10
#a)
# i dokladnie 4
dbinom(4, n, p)
# ii co najmniej 5
1 - pbinom(5 - 1, n, p)
# iii mniej niz 3
pbinom(2, n, p)

#b)
n * p

#c)
sqrt(n * p * (1 - p))

# 3
srednia = 1 / 5.3
#a)
par(mfrow = c(1, 1))
curve(dexp(x, srednia), from = 0, to = 40)

#b)
#i
1 - pexp(10, srednia)
#ii
pexp(10, srednia) - pexp(5, srednia)
#iii
integrate(
  function(x) {
    dexp(x, srednia)
  },
  lower = 10,
  upper = Inf
)
integrate(
  function(x) {
    dexp(x, srednia)
  },
  lower = 5,
  upper = 10
)

# 4
srednia = 5
sr_odch = 2
#a)
#i
pnorm(5.1, srednia, sr_odch / sqrt(50)) -
  pnorm(4.8, srednia, sr_odch / sqrt(50))
#ii
pnorm(60 * 4, srednia * 50, sqrt(50) * sr_odch)

#b)
proby = replicate(2000, rnorm(50, srednia, sr_odch))

proby_srednie = colMeans(proby)

proby_sumy = colSums(proby)
par(mfrow=c(1,2))
hist(
  proby_srednie,
  breaks = seq(
    from = min(proby_srednie) - 0.5,
    to = max(proby_srednie) + 0.5,
    by = 0.02
  ),
  freq = F,
  col = "blue"
)

curve(dnorm(x, srednia, sr_odch / sqrt(50)), add = T, lwd = 3, col="red")

hist(
  proby_sumy,
  breaks = seq(
    from = min(proby_sumy) - 10.0,
    to = max(proby_sumy) + 10.0,
    by = 1
  ),
  freq = F,
  col = "blue"
)

curve(dnorm(x, srednia*50, sr_odch * sqrt(50)), add = T, lwd = 3, col="red")

# 5

probki = c(30, 29, 24, 25, 26, 27, 29, 22, 24, 31, 28, 25, 27, 23, 28)

#a)
# populacja = cała produkcja nowego typu betonu
# próba = probki zdefiniowane powyzej (15 probek betonu nowego typu)
# badana cecha = czas schnięcia
#b)
n = length(probki)
x_bar = mean(probki)
s = sd(probki)
alpha = 0.05
t = qt(1 - alpha/2, df = n - 1)
L = x_bar - t * s / sqrt(n)
U = x_bar + t * s / sqrt(n)
c(L, U)

#c)
n = length(probki)
s2 = var(probki)
alpha = 0.04
chi_gorne = qchisq(1 - alpha/2, df = n - 1)
chi_dolne = qchisq(alpha/2, df = n - 1)
L = (n - 1) * s2 / chi_gorne
U = (n - 1) * s2 / chi_dolne
c(L, U)

# 6
x = 120
n = 1000
#a)
x/n

#b)

p_hat <- x / n
alpha <- 0.1
z <- qnorm(1 - alpha/2)
SE <- sqrt(p_hat * (1 - p_hat) / n)
L <- p_hat - z * SE
U <- p_hat + z * SE
c(L, U)

#c)
alpha <- 0.05
E <- 0.04
p <- 0.12
z <- qnorm(1 - alpha/2)
n_min <- z^2 * p * (1 - p) / E^2
ceiling(n_min)
