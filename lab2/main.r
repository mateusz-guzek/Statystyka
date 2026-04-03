#### laboratoria 2

### zad 1
loty = read.csv("lab2/loty.csv", sep = ";", head = TRUE)

# a)

loty
sapply(loty, class)

# b)

dane = data.frame(
  rok = colnames(loty),
  srednia = sapply(loty, mean),
  kwartyle0.25 = sapply(loty, quantile, prob = (0.25)),
  kwartyle0.50 = sapply(loty, quantile, prob = (0.5)),
  kwartyle0.75 = sapply(loty, quantile, prob = (0.75)),
  std = sapply(loty, sd)
)
dane$wsp_zmiennosci = 100 * dane$std / dane$srednia
dane$rok = sub("^X", "", dane$rok)

print(dane[dane$rok == "X1955", ])

# c)

par(mfrow = c(2, 3))


for (i in 1:ncol(loty)) {
  hist(
    loty[[i]],
    main = colnames(loty)[i],
    xlab = "Liczba lotów",
    ylab = "Liczebność"
  )
}

# d)

par(mfrow = c(1, 1))
boxplot(loty, xlab = "Lata", ylab = "Liczebnosc lotów")

### zad 2

# a)
oceny = read.csv2("lab2/oceny.csv")

# b)

oceny_wszystkie = na.omit(unlist(oceny, use.names = FALSE))

mean(oceny_wszystkie) # srednia
quantile(oceny_wszystkie) # kwantyle
sd(oceny_wszystkie) # odchylenie
sd(oceny_wszystkie) / mean(oceny_wszystkie) # wspolczynnik zmiennosci

# c)

oceny_staty = data.frame(
  grupa = colnames(oceny),
  srednia = sapply(oceny, mean, na.rm = TRUE),
  Q1 = sapply(oceny, quantile, probs = (0.25), na.rm = TRUE),
  Q2 = sapply(oceny, quantile, probs = (0.5), na.rm = TRUE),
  Q3 = sapply(oceny, quantile, probs = (0.75), na.rm = TRUE),
  sd = sapply(oceny, sd, na.rm = TRUE)
)
oceny_staty$wsp_zmiennosci = oceny_staty$sd / oceny_staty$srednia

par(mfrow = c(2, 3))
for (i in 2:length(oceny_staty)) {
  barplot(
    as.vector(oceny_staty[, i]), # wyciagniecie kolumny
    names.arg = oceny_staty$grupa, # nazwy pod slupkami
    main = colnames(oceny_staty)[i], # tytul
    ylab = colnames(oceny_staty)[i], # podpis osi y
    las = 2 # podpisy pionowo
  )
}


# d)
par(mfrow = c(1, 1))
boxplot(oceny, na.rm = TRUE)

# e)
par(mfrow = c(2, 2))
for (i in 1:length(oceny)) {
  pie(table(oceny[, i]), main = colnames(oceny)[i])
}

