#### laboratoria 1

### zad 1
sin(2 * pi)

cos(3/4)

tan(pi)

log(100, 10)

log(15)

log(1/7,7)

exp(3)

64^(1/3)

### zad 2
sum(c(1:10))

### zad 3
x <- seq(from=2,to=20,by=2)

# a)
length(x)

# b)
y <- rev(x)

# c)
x*x
x^2

# d)
sum(x*x)^(1/2)

# e)
t(x) %*% y

x %*% t(y)

### zad 4
seq(from=5,to=10, length.out = 13)

### zad 5
z1 <- rep(c(1,2),times=5)
z2 <- rep(c(1,2),each=5)

# a)
z1 + 4

# b)
z3 <- z2[-length(z2)]

# c)
c <- z1 + z3

# d)
z1[z1>1]

### zad 6
a <- c(1,3,6,2,7,4)

# a)
min(a)

# b)
which.min(a)

# c)
which(a>4)

# d)
sum(a)

# e)
sum(a^2)

# f)
sum(a^2)^(1/2)

# g)
a[3]

# h)
b = a[-4]

### zad 7

A = rbind(c(2,3,0),c(1,-1,2),c(1,1,-1))

# a)
A^2
A%*%A

# b)
t(A)
det(A)
solve(A)

# c)
A[3,]

### zad 8
x = seq(from=1,to=37,length.out = 10)
y = seq(from=1,to=17,length.out = 10)

# a)
plot(x,y)
help(plot)

# b)
plot(data.frame(x = x, y = y))

# c)
plot(cbind(x,y))

plot(rbind(x,y))
# nie dziala, trzeba transponowac rbind

plot(t(rbind(x,y)))

### zad 9
curve(x^2 + 3*x - 5, from = -3, to = 4)

curve(sin(x) + 0.01*x^3 + 0.02*x - 5, from = -3, to = 4)
