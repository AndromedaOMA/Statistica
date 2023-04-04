#exercitii rezolvate la seminar

x = c(1,3,2,15,66,21,34,53,7)
x = c(T,T,F,T,F)
length(x)

x = seq(0.001, 10, length = 200)
y = log2(x)
plot(x, y, type = 'l', main='grac', sub = 'subtilul', xlab ='axa x', ylab = 'axa y')
# tipul gracului: linie

n = 20
x = seq(0,n,1)
y = (dbinom(x, n, 0.4))
barplot(y, space = 0, main='barplot', sub = 'subtitlul', xlab ='axa x', ylab = 'axa y')
# ^ntre coloane spatiul este zero

dispersie = function(x,p){
  media =sum(p*x)
  dispersie= sum(p*(x-media)^2)
  return (dispersie)
}
y=c(23,32,31,27,37,33,25,21)
q=c(1/8,1/16,1/8,1/16,1/8,1/16,1/8,5/16)
dispersie(y,q)

#functie
vecto_sqrt = function(x) {
  for(i in 1:length(x)) {
    if(x[i] > 0)
      x[i] = sqrt(x[i])
    else
      x[i] = sqrt(-x[i])
  }
  return (x)
}
vecto_sqrt(x)

#-------------------------------------

dev.off()

#ex. 9
n=c(23,32,31,27,37,33,25,21)
p=c(1/8,1/16,1/8,1/16,1/8,1/16,1/8,5/16)

grafic_geometric <- function(p, n) {
  pr <- dgeom(0:(n-1), p)
  n <- paste("p(", 0:(n-1), ")", sep="")
  barplot(pr, names.arg=n, col="red", main="Reprezentarea grafică a probabilităților geometrice")
}
grafic_geometric(p,n)

#ex. 10
grafic_poisson <- function(lambda, n) {
  pr <- dpois(0:(n-1), lambda)
  n <- paste("P(", 0:(n-1), ")", sep="")
  barplot(pr, names.arg=n, col="blue", main="Reprezentarea grafică a probabilităților Poisson")
}
grafic_poisson(2,5)