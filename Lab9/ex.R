#I.Ex.

densityexponential=function(lambda,n,a)
{ 
    x=seq(0,a,n)
    y=dexp(x,lambda)
    plot(x,y,type="l")
}
densityexponential(3,9,1)

#I.1.

#a)
densitygamma=function(a,l)
{
x = seq(0, 10, by = 0.01)
y = dgamma(x, shape = a, rate = l)

plot(x, y, type = "l", col = "blue",
     main = "Distribuția Gamma", xlab = "x", ylab = "Densitatea")
}
densitygamma(5,1)

#b)
densitystud=function(df)
{
x = seq(-5, 5, 0.01)
y = dt(x, df = df)

plot(x, y, type = "l", col = "red",
     main = "Distribuția Student", xlab = "x",  ylab = "Densitatea")
}
densitystud(10)

#c)
densityn=function(m,s)
{
x = seq(-5, 5, 0.01)
y = dnorm(x, mean = m, sd = s)

plot(x, y, type = "l", col = "green",
     main = "Distribuția Normală", xlab = "x", ylab = "Densitatea")
}
densityn(0,1)





#II.Ex.

#1
LLNPoisson=function(lam,n)
{
    sum=0
    for(i in 1:n)
    {
        u=rpois(1,lam)
        sum=sum+u
    }
    return(sum/n)
}
LLNPoisson(3,10)

#sau

LLNPoisson=function(lambda,n)
{
    return(mean(rpois(n,lambda)))
}
LLNPoisson(3,10)

#2
LLNGamma=function(alfa,lambda,n)
{
    return(mean(rgamma(n,alfa,lambda)))
}
LLNGamma(1,2,3)



#II.1.
#a)
LNMExponential=function(lambda,n)
{
  return(mean(rnorm(n,mean = 1/lambda, sd = 1/lambda^2)))
}
LNMExponential(3,4)

#b)
LNMBinomial=function(m,p,n)
{
  return(mean(rnorm(n,mean = m*p, sd = sqrt(m*p*(1-p)))))
}
LNMBinomial(3,0.4,5)

#II.2.
LLNStudent = function(r, n)
{
esantion = replicate(1000, mean(rt(n, r)))
esantion_mediu = mean(esantion)
esantion_var = var(esantion)
cat(paste0("r = ", r, ", n = ", n, "\n"))
cat(paste0("Media mediei esantionului: ", esantion_mediu, "\n"))
cat(paste0("Varianța mediei esantionului: ", esantion_var, "\n\n"))
}
LLNStudent(1000,3)
LLNStudent(10000,4)
LLNStudent(100000,5)
LLNStudent(1000000,6)

#sau

# valori ale lui n si r
n_values <- c(1000, 10000, 100000, 1000000)
r_values <- c(2, 3, 4, 5)

# deviatia standard
for (r in r_values) {
  if (r == 2) {
    next  # nu putem folosi teorema centrala a limitei pentru r=2
  }
  sigma <- sqrt(r/(r-2))  # deviatia standard a variabilei aleatoare Student
  for (n in n_values) {
    sigma_medie <- sigma/sqrt(n)  # deviatia standard a mediei aritmetice a n variabile aleatoare Xi
    cat("Pentru n =", n, "si r =", r, ", deviatia standard a mediei aritmetice a", n, "variabile aleatoare Xi este", sigma_medie, "\n")
  }
}
