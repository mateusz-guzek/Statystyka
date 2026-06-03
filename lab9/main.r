# 1.
dane = read.csv2("lab9/Reg_chemikalia.csv")

# a)
plot(dane$surowiec, dane$produkt, xlab="surowiec",ylab="produkcja")

# b)
cov(dane$surowiec, dane$produkt)

# c)
r = cor(dane$surowiec, dane$produkt)

# d)
prosta = lm(produkt ~ surowiec, data = dane)

# e)
plot(dane$surowiec, dane$produkt, xlab="surowiec",ylab="produkcja")
abline(prosta)

# f)
coef(prosta)[2]

# g)
predict(prosta, data.frame(surowiec = 20))

# h)
predict(prosta, data.frame(surowiec = 15))

# i)
r^2
# j)
summary(prosta)

# 2.
dane = read.csv2("lab9/Reg_urzadzenie.csv")
x = dane$efektywnosc
y = dane$zywotnosc
# a)
plot(x,y,pch=19, xlab = "efektywność", ylab = "żywotność")

# b)
cov(x,y)

# c)
cor(x,y)

# d)
prosta = lm(y ~ x)
abline(prosta)

# e)
coef(prosta)[2]

# f)
predict(prosta,data.frame(x=11))

# g)
predict(prosta, data.frame(x=19))
