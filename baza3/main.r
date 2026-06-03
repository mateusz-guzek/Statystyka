# 1
p0 = 0.46
n = 500
k = 300
alfa = 0.09
p_hat = k / n
wynik = prop.test(k, n, p = p0, alternative = "two.sided", correct = FALSE)
pval = wynik$p.value
# H1: Studenci zachowują się inaczej niż reszta Polaków
# H0: Studenci zachowują się tak samo jak reszta Polaków
if (pval < alfa) {
  print("Odrzucamy H0, Studenci zachowują się tak samo")
} else {
  print("Brak podstaw do odrzucenia H0, Studenci zachowują się inaczej")
}

# 2
dane = read.csv("baza3/firma.csv")
alfa = 0.02
mu0 = 0
dane1 = dane$czas_stary_s
dane2 = dane$czas_nowy_s
# H0: muD <= mu0
# H1: muD > mu0
wynik = t.test(
  dane1,
  dane2,
  paired = TRUE,
  mu = mu0,
  alternative = "greater",
  conf.level = 1 - alfa
)
pval = wynik$p.value
wynik$conf.int
if (pval < alfa) {
  print("Odrzucamy H0")
} else {
  print("Brak podstaw do odrzucenia H0")
}
# Nowy algorytm jest szybszy

# 3
dane = read.csv("baza3/ANOVA_technologia.csv")
alfa = 0.05
wyniki = dane$stezenie_olowiu_ug_dl
obiekty = dane$technologia
# H0: wszystkie srednie sa rowne
# H1: nie wszystkie srednie sa rowne
model = lm(wyniki ~ obiekty)
wynikA = anova(model)
print(wynikA)
pvalA = wynikA$`Pr(>F)`[1]
if (pvalA < alfa) {
  print("Odrzucamy H0: czynnik ma istotny wplyw.")
} else {
  print("Brak podstaw do odrzucenia H0: nie wykazano wplywu czynnika.")
}
modelAOV = aov(wyniki ~ obiekty)
wynikT = TukeyHSD(modelAOV, conf.level = 1 - alfa)
print(wynikT)
plot(wynikT) # os X i porownywane pary opisuje R
title(ylab = "Porownywane grupy") # opcjonalny opis osi Y
# grupa 1: T1, T2
# grupa 2: T3

# 4
dane = read.csv2("baza3/Reg_tlen.csv")
# a)
temperatura = dane$temperatura
tlen = dane$tlen
cor(temperatura, tlen)
# wystepuje bardzo silny zwiazek liniowy

# b)
model = lm(tlen ~ temperatura)
plot(temperatura, tlen, pch = 19, col="red")
abline(model,col="blue")
model$coefficients
# c)
predict(model, data.frame(temperatura = 15))
