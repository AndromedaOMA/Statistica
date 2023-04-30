library(ggplot2)

#------------------

a=10
b=15
sum(a,b)
log(a,b)
c=2
a^c

#-------------------

x=c(1,2,3,4,5,6)
x[2:5]
x[-4]
x^2
y=c(1,1,1,1,1,1)
x+y
length(y)
z=c(4,2,3,1,5)
sort(z)

#-------------------

x=seq(0.001, 10, length=200)
y=log2(x)
plot(x,y,type ="l", main="Grafic", sub="Subtitlul",xlab = "axa OX",ylab="axa OY")

x=seq(0.001, 10, length=200)
y=log2(x)
plot(x,y,type ="p", main="Grafic", sub="Subtitlul",xlab = "axa OX",ylab="axa OY")

n = 20
x = seq(0,n,1) #x contine valorile de la 0 la 20
y = dbinom(x,n,0.4)
barplot(y, space = 0, main="Grafic", sub = "Subtitlul", xlab ="axa Ox", ylab = "axa Oy")

#------------------

# Generam valorile pentru distribuția Poisson(λ) cu λ = 3 și k = 0, 1, 2, ..., 10
lambda = 3
k = 0:10
poisson = dpois(k, lambda)

# Creăm graficul
plot(k, poisson, type = "o", pch = 16, col = "blue", 
     xlab = "Valoarea k", ylab = "Densitatea de probabilitate",
     main = "Distribuția Poisson(λ)")

#---------------------

# Generăm valorile pentru distribuția Geometric(p) cu p = 0.4 și k = 0, 1, 2, ..., 10
p = 0.4
k = 0:10
geometric = dgeom(k, p)

# Creăm graficul
plot(k, geometric, type = "o", pch = 16, col = "blue", 
     xlab = "Valoarea k", ylab = "Densitatea de probabilitate",
     main = "Distribuția Geometric(p)")

#-------------------------

# Generăm valorile pentru distribuția B(n,p) cu n = 10, p = 0.6 și k = 0, 1, 2, ..., 10
n = 10
p = 0.6
k = 0:10
binomial = dbinom(k, n, p)

# Creăm graficul
plot(k, binomial, type = "o", pch = 16, col = "blue", 
     xlab = "Valoarea k", ylab = "Densitatea de probabilitate",
     main = "Distribuția B(n,p)")




#------------------------------
# a)

# Setăm dimensiunea și marimea graficului global
par(mfrow = c(1,3), mar = c(5, 4, 4, 2) + 0.1)

#LEGENDA: Poisson -> blue; Geometric -> red; B -> green;

# Generăm valorile pentru distribuția Poisson(λ) cu λ = 3 și k = 0, 1, 2, ..., 10
lambda = 3
k = 0:10
poisson = dpois(k, lambda)

# Adăugăm distribuția Poisson la grafic
plot(k, poisson, type = "o", pch = 16, col = "blue", 
     xlab = "Valoarea k", ylab = "Densitatea de probabilitate",
     main = "Toate cele trei distribuții")

# Generăm valorile pentru distribuția Geometric(p) cu p = 0.4 și k = 0, 1, 2, ..., 10
p = 0.4
geometric = dgeom(k, p)

# Adăugăm distribuția Geometric la grafic
lines(k, geometric, type = "o", pch = 17, col = "red")

# Generăm valorile pentru distribuția B(n,p) cu n = 10, p = 0.6 și k = 0, 1, 2, ..., 10
n = 10
p = 0.6
binomial = dbinom(k, n, p)

# Adăugăm distribuția B(n,p) la grafic
lines(k, binomial, type = "o", pch = 15, col = "green")


#------------------------------
# b)

# Setam probabilitatea de succes
p <- 0.3

# Probabilitatea ca variabila aleatoare sa fie impara
p_impar <- 1 / (2 - p)
print(p_impar)

# Probabilitatea ca variabila aleatoare sa fie mai mare sau egala cu 4
p_mai_mare_sau_egal_4 = dgeom(3, p, lower.tail = FALSE)
print(p_mai_mare_sau_egal_4)

# Probabilitatea ca variabila aleatoare sa fie mai mica sau egala cu 20
p_mai_mica_sau_egal_20 = pgeom(20, p, lower.tail = TRUE)
print(p_mai_mica_sau_egal_20)

#------------------------------
# c)

#mamma mia


#-----------------------------
