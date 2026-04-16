dane = read.csv("baza1/dane.csv")
# 1
# a)
czas = na.omit(dane$czas)
quantile(czas, probs=0.25)

# b)
czest = table(cut(czas,breaks=seq(from=10, to=60,length=6)))

# c)
hist(czas, breaks = seq(from = 10, to = 60, length = 6), freq = FALSE)

# 2
h = 5
curve(expr = dexp(x,rate=1/h), from=2,to=50)

1 - pexp(2,1/h)

# 3
mu = 40
sigma = 2
n = 75

# b)
pnorm(2750,mu*n,sigma*sqrt(n))

# 4

prop.test(40, 130, conf.level = 0.95, correct = FALSE)
