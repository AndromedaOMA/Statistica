#I.Ex.
densityexponential=function(lambda,n,a)
{ 
    x=seq(0,a,n)
    y=dexp(x,lambda)
    plot(x,y,type="l")
}
densityexponential(1,2,10)

#I.1.
#a)
densitygamma=function(a,l)
{
x = seq(0, 10, by = 0.01)
y = dgamma(x, a, l)

plot(x, y, type = "l")
}
densitygamma(5,1)

#b)
densitystud=function(df)
{
x = seq(-5, 5, 0.01)
y = dt(x, df)
plot(x, y, type = "l")
}
densitystud(10)

#c)
densitynorm=function(m,s)
{
x = seq(-5, 5, 0.01)
y = dnorm(x, m, s)
plot(x, y, type = "l")
}
densitynorm(2,2)





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
LLNPoisson(3,1000)

#sau

LLNPoisson=function(lambda,n)
{
    return(mean(rpois(n,lambda)))
}
LLNPoisson(3,1000)

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
  return(mean(rnorm(n, 1/lambda, 1/lambda^2)))
}
LNMExponential(3,4)

#b)
LNMBinomial=function(m,p,n)
{
  return(mean(rnorm(n, m*p, sqrt(m*p*(1-p)))))
}
LNMBinomial(3,0.4,5)

#II.2.
# Student <- function()
# {
#   # valori ale lui n si r
#   n_val <- c(1000, 10000, 100000, 1000000)
#   r_val <- c(1, 2, 3, 4, 5)
  
#   cat("\nSTART\n")
#   for (r in r_val)
#   { 
#     if (r == 1 || r == 2)
#       next  # nu putem folosi teorema pentru r=1
#     sigma <- sqrt(r/(r-2))  # deviatia standard
#     for (n in n_val)
#     {# deviatia standard a mediei aritmetice
#       sigma_medie <- sigma/sqrt(n)  
#       cat("Pentru n =", n, "si r =", r, ", deviatia standard a mediei aritmetice a", n, "variabile aleatoare Xi este", sigma_medie, "\n")
#     }
#   }
#   cat("\nEND\n")
# }
# Student()

LNMStudent=function(n,df)
{
  return(mean(rt(n,df)))
}
LNMStudent(5,1)



#III.Ex.
CLTPoisson=function(lambda,n,N,z)
{
    expectation=lambda
    stdev=lambda
    upperbound=z*stdev/sqrt(n)+expectation
    sum=0
    for(i in 1:N)
    {
        xn=mean(rpois(n,lambda))
        if(xn <= upperbound)
            sum=sum+1
    }
    return(c(sum/N, pnorm(z)))
}
CLTPoisson(2,30,10000,1)

#III.1.
CLTExp=function(lambda,n,N,z)
{
    expectation=1/lambda
    stdev=lambda
    upperbound=z*stdev/sqrt(n)+expectation
    sum=0
    for(i in 1:N)
    {
        xn=mean(rexp(n,lambda))
        if(xn <= upperbound)
            sum=sum+1
    }
    return(c(sum/N, pnorm(z)))
}
CLTExp(2,30,10000,1)

#III.2.
#Rh pt distrib binmila in distrib normala



#IV.Ex.
binomialprobability=function(n,p,k)
{
    expectation=n*p;
    variance=n*p*(1-p);
    standarddeviation=sqrt(variance);
    q=(k+0.5)/standarddeviation;
    return(1-pnorm(q));
 }
 binomialprobability(50,0.3,10)

#IV.1.
binprobability=function(n,p,k)
{
  return(pbinom(k-1, n, p));
}
binprobability(50,0.3,10)

