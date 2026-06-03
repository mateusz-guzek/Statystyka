# 1
dane = c(5.9, 4.4, 5.4, 3.8, 4.0, 4.2, 3.4, 3.6, 4.6, 6.5, 5.6, 4.8)
alfa = 0.05
mu0 = 4
n = length(dane)
srednia = mean(dane)
s = sd(dane)
stat = (srednia - mu0) / (s / sqrt(n))
# H0: mu <= 4: okolice darłowa nie nadają się na budowę farmy wiatrowej
# H1: mu > 4: okolice darłowa nadają się na budowę farmy wiatrowej
kwantyl = qt(1 - alfa, df = n - 1)
# p_value = pt(stat, df = n - 1, lower.tail = FALSE)
if (stat > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

wynik = t.test(dane, mu = mu0, alternative = "greater")
pval = wynik$p.value
if (pval < alfa) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# 2
dane = c(142.8, 130.5, 138.6, 137.4, 145.7, 143.9, 141.1, 144.2, 145.4, 152.2, 135.1, 146.4, 142.1, 141.7, 144.7)
alfa = 0.05
mu0 = 140
sigma = 5
n = length(dane)
srednia = mean(dane)
stat = (srednia - mu0) / (sigma / sqrt(n))
# H0: mu <= 140: baterie nie spełniają normy
# H1: mu > 140: baterie spełniają normę
kwantyl = qnorm(1 - alfa)
if (stat > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# 3
dane = c(0.048, 0.028, 0.037, 0.033, 0.054, 0.046, 0.041, 0.043, 0.044, 0.050, 0.047,0.052, 0.053, 0.048, 0.027, 0.056,
0.058, 0.039, 0.026, 0.034, 0.043, 0.042, 0.047, 0.022, 0.046, 0.040, 0.036, 0.043, 0.041, 0.044, 0.043, 0.044,
0.038, 0.046, 0.041, 0.038, 0.047, 0.030, 0.041, 0.049)

alfa = 0.02
mu0 = 0.04
n = length(dane)
srednia = mean(dane)
s = sd(dane)
stat = (srednia - mu0) / (s / sqrt(n))
# H0: mu = 0.04: produkowane blaszki spełniają wymóg nominalnej grubości
# H1: mu != 0.04: produkowane blaszki nie spełniają wymogu nominalnej grubości
kwantyl = qnorm(1 - alfa / 2)
if (abs(stat) > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")


# 4

n = 100
srednia = 60
s = 20
s2 = s^2
alfa = 0.01

mu0 = 55
sigma0 = 18 
sigma2_0 = sigma0^2

# H0: mu <= 55
# H1: mu > 55
stat = (srednia - mu0) / (s / sqrt(n))
kwantyl = qnorm(1 - alfa)
if (stat > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# Firma w rzeczywistości nie spełnia normy średniej zanieczyszczenia
stat = (n - 1) * s2 / sigma2_0
# H0: sigma <= 18 
# H1: sigma > 18
kwantyl = qchisq(1 - alfa, df = n - 1)
if (stat > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# Firma spełnia normę odchylenia według pomiarów, ale przekracza normę średniej zanieczyszczenia, więc ŁAMIE PRAWO

# 5
dane = c(5.0, 5.5, 4.4, 5.5, 5.6)
alfa = 0.1
sigma2_0 = 0.5
n = length(dane)
wariancja = var(dane)
stat = (n - 1) * wariancja / sigma2_0
# H0: sigma^2 >= 0.5mm
# H1: sigma^2 < 0.5mm
kwantyl = qchisq(alfa, df = n - 1)
# p_value = pchisq(stat, df = n - 1)
if (stat < kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# 6
alfa = 0.05
k = 16 # liczba sukcesow
n = 1200 # liczebnosc proby
p0 = 0.02
p_hat = k / n
stat = (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
# H0: p >= 0.02
# H1: p < 0.02
kwantyl = qnorm(alfa)
# p_value = pnorm(stat)
if (stat < kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

wynik = prop.test(k, n, p = p0, alternative = "less", correct = FALSE)
pval = wynik$p.value
if (pval < alfa) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# 7
alfa = 0.05
k = 1600 # liczba sukcesow
n = 2500 # liczebnosc proby
p0 = 0.6
p_hat = k / n
stat = (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
# H0: ilość ludzi którzy zagłosują wynosi 60%
# H1: ilosć ludzi którzy zagłosują nie wynosi 60%
kwantyl = qnorm(1 - alfa / 2)
if (abs(stat) > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
