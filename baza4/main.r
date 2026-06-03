# 1
dane = read.csv2("baza4/dane_K2.csv")
czas = na.omit(dane$czas)
alfa = 0.05
sigma2_0 = 0.8
n = length(czas)
wariancja = var(czas)
stat = (n - 1) * wariancja / sigma2_0
# H0: sigma^2 >= sigma2_0
# H1: sigma^2 < sigma2_0
kwantyl = qchisq(alfa, df = n - 1)
# p_value = pchisq(stat, df = n - 1)
if (stat < kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# brak podstaw do twierdzenia że wariancja jest mniejsza od 0.8

# 2
dane = read.csv("baza4/firma.csv")
dane1 = dane$stary
dane2 = dane$nowy
mu0 = 0
# H0: muD <= mu0
# H1: muD > mu0
wynik = t.test(dane1, dane2, paired = TRUE, mu = mu0, alternative = "greater", conf.level = 1 - alfa)
pval = wynik$p.value; wynik$conf.int
if (pval < alfa) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# nowy algorytm jest szybszy