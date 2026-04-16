dane = read.csv("baza1/dane.csv")

# 1
czas = na.omit(dane$czas)

# a)
print("wspolczynnik zmiennosci")
wsp_zmiennosci = sd(czas) / mean(czas)

# b)
tabela_czest = table(cut(czas, breaks = seq(from=15, to=65, length=7)))

# c)
pie(tabela_czest, main="Rozklad czasu obslugi klientow", col=rainbow(6))

# 2
p = 0.3
h = 5
# a)
szansa = pbinom(h,10,p)
szansa

# b)
rozklad = data.frame(0:10,(dbinom(0:10,10,p)))
rozklad
10*p

# 3
h = 5
avg = 35+h
sd = 3

# a) rozklad normalny pnorm(a,avg,sd/sqrt(50))

# b)
pnorm(40,avg,sd/sqrt(50)) - pnorm(35,avg,sd/sqrt(50))

# 4
witaminy = na.omit(dane$witaminaC)


n = length(witaminy)
x_bar = mean(witaminy)
s = sd(witaminy)
conf = 1 - 0.95
t = qt(1-conf/2, df = n-1)
L = x_bar - t * s / sqrt(n)
U = x_bar + t * s / sqrt(n)
c(L,U)

t.test(witaminy, conf.level = 0.95)


x_ = mean(witaminy)
z = qnorm(1 - 0.05/2)
sigma = 2
n = length(witaminy)

nn = (z*sigma / 0.2)^2
ceiling(nn)








