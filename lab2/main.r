#### laboratoria 2

### zad 1
# W pliku loty.csv zebrane są dane dotyczące liczby pasażerów pewnej linii lotniczej w kolejnych miesiącach
# i latach. Wykonaj polecenia
loty = read.csv("lab2/loty.csv", sep = ";", head = TRUE)

# a) sprawdź, jakie wartości zawiera plik i jaki jest typ danych

loty
sapply(loty, class)

# b) wyznacz podstawowe miary statystyczne dla kolejnych lat (średnia, kwartyle, odchylenie standardowe,
# współczynnik zmienności), zinterpretuj je dla roku 1955

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

# c) narysuj histogramy liczebności dla danych z kolejnych lat w jednym oknie

par(mfrow = c(2, 3))


for (i in 1:ncol(loty)) {
  hist(
    loty[[i]],
    main = colnames(loty)[i],
    xlab = "Liczba lotów",
    ylab = "Liczebność"
  )
}

# d) porównaj dane z kolejnych lat za pomocą wykresów pudełkowych

par(mfrow = c(1, 1))
boxplot(loty, xlab = "Lata", ylab = "Liczebnosc lotów")

### zad 2
# W pliku oceny.csv zebrane są dane dotyczące ocen. Wykonaj polecenia:

# a) wczytaj dane tak, aby oceny były numeryczne
oceny = read.csv2("lab2/oceny.csv")

# b) wyznacz i zinterpretuj podstawowe miary statystyczne łącznie, bez rozróżnienia na grupy (średnia,
# kwartyle, odchylenie standardowe, współczynnik zmienności

oceny_wszystkie = na.omit(unlist(oceny, use.names = FALSE))

mean(oceny_wszystkie) # srednia
quantile(oceny_wszystkie) # kwantyle
sd(oceny_wszystkie) # odchylenie
sd(oceny_wszystkie) / mean(oceny_wszystkie) # wspolczynnik zmiennosci

# c) narysuj diagramy odcinkowe dla danych z kolejnych grup w jednym oknie

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


# d) porównaj oceny z różnych grup za pomocą wykresów pudełkowych

par(mfrow = c(1, 1))
boxplot(oceny, na.rm = TRUE)

# e) sporządź szeregi rozdzielcze punktowe ocen w poszczególnych grupach i pokaż je na wykresach
# kołowych

par(mfrow = c(2, 2))
for (i in 1:length(oceny)) {
  pie(table(oceny[, i]), main = colnames(oceny)[i])
}

### zad 3
# W pliku truskawki.csv zebrane są dane dotyczące plonów truskawek w dwóch latach. Wykonaj polecenia

# a) wczytaj dane
truskawki = read.csv2("lab2/truskawki.csv")

# b) wyznacz i zinterpretuj podstawowe miary statystyczne (średnia, kwartyle, odchylenie standardowe,
# współczynnik zmienności)
truskawki_staty = data.frame(
  srednia = sapply(truskawki, mean, na.rm = TRUE),
  Q1 = sapply(truskawki, quantile, probs = c(0.25), na.rm = TRUE),
  Q2 = sapply(truskawki, quantile, probs = c(0.5), na.rm = TRUE),
  Q3 = sapply(truskawki, quantile, probs = c(0.75), na.rm = TRUE),
  odchylenie = sapply(truskawki, sd, na.rm = TRUE),
  wsp_zmiennosci = sapply(truskawki, sd, na.rm = TRUE) /
    sapply(truskawki, mean, na.rm = TRUE)
)

print(truskawki_staty)

# c) porządź szeregi rozdzielcze przedziałowe plonów w poszczególnych latach i pokaż je na wykresach
# kołowych

par(mfrow = c(1, 1))
for (y in 1:length(truskawki)) {
  pie(
    main = colnames(truskawki)[y],
    table(cut(
      truskawki[[y]],
      breaks = seq(from = 0, to = 150, by = 50)
    )),
    las = 1
  )
}

# d) narysuj histogramy probabilistyczne bazując na szeregu rozdzielczym z poprzedniego podpunktu
for (y in 1:length(truskawki)) {
  hist(
    main = colnames(truskawki)[y],
    breaks = seq(from = 0, to = 150, by = 50),
    truskawki[[y]],
    las = 1,
    probability = TRUE,
    xlab = "plon",
    ylab = "gęstość"
  )
}

# e) porównaj dane z kolejnych lat za pomocą wykresów pudełkowych.
boxplot(truskawki, ylab = "plony", main = "porównanie plonów z lat 2000 i 2010")


### zad 4
# W zbiorze danych seriesIMDB z paczki PogromcyDanych mamy informacje o odcinkach około 200 seriali.
# Stwórz wykres, w którym na osi Y będzie nazwa serialu, a na osi X wykres pudełkowy ocen odcinków danego
# serialu. Popraw wykres tak, aby seriale były posortowane według median ocen. Stwórz kolejny wykres
# pudełkowy pionowy z pięcioma serialami o najwyższej średniej ocenie
install.packages("PogromcyDanych")
library(PogromcyDanych)

boxplot(
  ocena ~ reorder(serial, ocena, FUN = median, na.rm = TRUE),
  data = serialeIMDB,
  horizontal = TRUE,
  las = 1,
  cex.axis = 0.5,
  xlab = "Ocena odcinka",
  ylab = "Serial",
  main = "Oceny odcinków seriali"
)


srednie = aggregate(ocena ~ serial, data = serialeIMDB, FUN = mean, na.rm = TRUE)
top5 = head(srednie[order(-srednie$ocena), "serial"], 5)

top5_dane = subset(serialeIMDB, serial %in% top5)

top5_srednie = srednie[srednie$serial %in% top5, ]
top5_srednie = top5_srednie[order(-top5_srednie$ocena), ]

top5_dane$serial = factor(top5_dane$serial, levels = top5_srednie$serial)


par(mar = c(10, 4, 4, 2))

boxplot(
  ocena ~ serial,
  data = top5_dane,
  las = 2,
  xlab = "Serial",
  ylab = "Ocena odcinka",
  main = "5 seriali o najwyższej średniej ocenie"
)

### zad 5 W zbiorze danych seriesIMDB z paczki PogromcyDanych mamy informacje o odcinkach około 200 seriali. Ile
# średnio głosów oddano na serial? Ile wynoszą średnie oceny dla 5 najlepiej ocenianych seriali?

serialeIMDB

srednie_glosow = aggregate(glosow ~ serial, data = serialeIMDB, FUN = mean)

print(paste("srednia liczba glosow na serial to", floor(mean(srednie_glosow$glosow))))

srednie_oceny = aggregate(ocena ~ serial, data = serialeIMDB, FUN = mean, na.rm = TRUE)
srednie_oceny = srednie_oceny[order(-srednie_oceny$ocena), ]
top5_srednie_oceny = head(srednie_oceny, 5)

print(top5_srednie_oceny)
