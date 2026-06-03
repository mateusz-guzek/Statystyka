# 1
plik = read.csv2("lab8/anova_cisnienie.csv")

g1 = na.omit(plik[["Niskie"]])
g2 = na.omit(plik[["Srednie"]])
g3 = na.omit(plik[["Silne"]])
g4 = na.omit(plik[["BardzoSilne"]])
wyniki = c(g1, g2, g3, g4)
obiekty = factor(c(
rep("Niskie", length(g1)),
rep("Srednie", length(g2)),
rep("Silne", length(g3)),
rep("BardzoSilne", length(g4))
))
alfa = 0.05

# H0: wariancje we wszystkich grupach sa rowne
# H1: co najmniej jedna wariancja jest inna
wynikB = bartlett.test(wyniki ~ obiekty)
print(wynikB)
if (wynikB$p.value < alfa) {
print("Odrzucamy H0: wariancje nie sa jednorodne.")
} else {
print("Brak podstaw do odrzucenia H0: przyjmujemy rownosc wariancji.")
}

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

# 2
plik = read.csv2("lab8/anova_mikrometr.csv")

g1 = na.omit(plik[["mikrometrI"]])
g2 = na.omit(plik[["mikrometrII"]])
g3 = na.omit(plik[["mikrometrIII"]])
wyniki = c(g1, g2, g3)
obiekty = factor(c(
rep("mikrometrI", length(g1)),
rep("mikrometrII", length(g2)),
rep("mikrometrIII", length(g3))
))
alfa = 0.05

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