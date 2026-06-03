
dane = read.csv("lab7/dane_dwie_populacje.csv")

# 1
dane1 = na.omit(dane$prywatny)
dane2 = na.omit(dane$publiczny)
alfa = 0.1
n1 = length(dane1);
n2 = length(dane2)
wariancja1 = var(dane1);
wariancja2 = var(dane2)
df1 = n1 - 1; df2 = n2 - 1
stat = wariancja1 / wariancja2
# H0: sigma1^2 = sigma2^2
# H1: sigma1^2 != sigma2^2
L = qf(alfa / 2, df1, df2); P = qf(1 - alfa / 2, df1, df2)
if (stat < L || stat > P) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# wariancje są równe

alfa = 0.1; mu0 = 0
n1 = length(dane1); n2 = length(dane2)
srednia1 = mean(dane1); srednia2 = mean(dane2)
wariancja1 = var(dane1); wariancja2 = var(dane2)
Sp2 = ((n1 - 1) * wariancja1 + (n2 - 1) * wariancja2) / (n1 + n2 - 2)
SE = sqrt(Sp2 * (1 / n1 + 1 / n2)); df = n1 + n2 - 2
stat = (srednia1 - srednia2 - mu0) / SE
# H0: mu_prywatny <= mu_publiczny
# H1: mu_prywatny > mu_publiczny

kwantyl = qt(1 - alfa, df)

if (stat > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")

# możemy stwierdzić że publiczne źródła finansowania udzielają, prze-
# ciętnie rzecz biorąc, mniejszych kredytów

# 2
dane1 = na.omit(dane$celuloza1)
dane2 = na.omit(dane$celuloza2)
alfa = 0.02

# a)
# H0: sigma1^2 == sigma2^2
# H1: sigma1^2 != sigma2^2
wynik = var.test(dane1, dane2, ratio = 1, alternative = "two.sided", conf.level = 1 - alfa)
pval = wynik$p.value; wynik$conf.int
if (pval < alfa) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# wariancje są równe

# b)
# H0: mu1 - mu2 = mu0
# H1: mu1 - mu2 != mu0
mu0 = alfa
wynik = t.test(dane1, dane2, var.equal = TRUE, conf.level = 1 - alfa)
pval = wynik$p.value; wynik$conf.int
if (pval < alfa) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# średnie nie różnią się istotnie

# c)
wynik
# 98 percent confidence interval:
#  -13.519332   3.143023
# 0 należy do tego przedziału. czyli jest brak podstaw do stwierdzenia różnicy średnich

# 3
alfa = 0.1; mu0 = 0
n1 = 130; srednia1 = 6; s1 = 2.4
n2 = 120; srednia2 = 7; s2 = 3.2
SE = sqrt(s1^2 / n1 + s2^2 / n2)
stat = (srednia1 - srednia2 - mu0) / SE

# H0: mu1 - mu2 = mu0
# H1: mu1 - mu2 != mu0
kwantyl = qnorm(1 - alfa / 2)
# p_value = 2 * pnorm(abs(stat), lower.tail = FALSE)
if (abs(stat) > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# srednie nie są równe

# 4
dane1 = c(15, 4, 9, 9, 10, 10, 12, 17, 14)
dane2 = c(14, 4, 10, 8, 10, 9, 10, 15, 14)
alfa = 0.05; mu0 = 0
d = dane1 - dane2
n = length(d)
srednia_d = mean(d); s_d = sd(d)
SE = s_d / sqrt(n)
stat = (srednia_d - mu0) / SE

# H0: muD = mu0
# H1: muD != mu0
kwantyl = qt(1 - alfa / 2, df = n - 1)
if (abs(stat) > kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# nowy rodzaj leku nie zmienia wartosci parametru

# 5
dane1 = na.omit(dane$zawodnik1)
dane2 = na.omit(dane$zawodnik2)
alfa = 0.05
# H0: sigma1^2 >= sigma2^2
# H1: sigma1^2 < sigma2^2
wynik = var.test(
  dane1, dane2,
  ratio = 1,
  alternative = "less",
  conf.level = 1 - alfa
)

pval = wynik$p.value

if (pval < alfa) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# nie możemy stwierdzić, że zawodnik 1 jest bardziej regularny

# 6
alfa = 0.9
k1 = 1200*0.78; n1 = 1200; p1_hat = k1 / n1
k2 = 2000*0.2; n2 = 2000; p2_hat = k2 / n2
p_wspolne = (k1 + k2) / (n1 + n2)
stat = (p1_hat - p2_hat) / sqrt(p_wspolne * (1 - p_wspolne) * (1 / n1 + 1 / n2))

# 7
dane1 = na.omit(dane$nowy)
dane2 = na.omit(dane$stary)
alfa = 0.02; mu0 = 0
n1 = length(dane1); srednia1 = mean(dane1); s1 = sd(dane1)
n2 = length(dane2); srednia2 = mean(dane2); s2 = sd(dane2)
SE = sqrt(s1^2 / n1 + s2^2 / n2)
stat = (srednia1 - srednia2 - mu0) / SE

# H0: mu1 - mu2 >= mu0
# H1: mu1 - mu2 < mu0
kwantyl = qnorm(alfa)
if (stat < kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# nowy algorytm jest szybszy

# 8
dane2 = c(12.5, 13.1, 11.9, 12.8, 13.4, 12.2, 13.0, 12.6, 12.9, 13.3)
dane1 = c(10.8, 11.4, 10.5, 11.2, 11.6, 10.9, 11.3, 10.7, 11.0, 11.5)
alfa = 0.05; mu0 = 0
d = dane1 - dane2
n = length(d)
srednia_d = mean(d); s_d = sd(d)
SE = s_d / sqrt(n)
stat = (srednia_d - mu0) / SE

# H0: muD >= mu0
# H1: muD < mu0
kwantyl = qt(alfa, df = n - 1)
# p_value = pt(stat, df = n - 1)
if (stat < kwantyl) print("Odrzucamy H0") else print("Brak podstaw do odrzucenia H0")
# nowy algorytm jest lepszy