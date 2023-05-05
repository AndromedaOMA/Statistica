#B1
N = c(5000, 10000, 100000, 500000)
P = c(0.2, 0.4, 0.6, 0.8)
M = {}
LNM_Geometric = function()
{
    for (i in 1:4)
       {
         cat("For N =", N[i], "and P =", P[i], ":\n",  
        "Probabilitatea este ", 1/P[i], "\n",  
        "Media esantionului curent este ", mean(rgeom(N[i], P[i])), "\n",
        "Diferenta dintre probabilitatea si media esantionului curent este ", 1/P[i]-mean(rgeom(N[i], P[i])), "\n\n")
         M[i] = 1/P[i]-mean(rgeom(N[i], P[i]))
       }
       
      if(M[2]<M[4])
        cat("Sirul de variabile aleatoare X_i nu respecta proprietatea LNM: media aritmetica a esantioanelor nu se apropie de 1/p pe masura ce numarul de esantioanelor creste")
      else 
        cat("Sirul de variabile aleatoare X_i respecta proprietatea LNM: media aritmetica a esantioanelor se apropie de 1/p pe masura ce numarul de esantioanelor creste")
}
LNM_Geometric()



#B2
n = 50
N = c(5000, 10000, 20000)
z = c(-1.5, 0, 1.5)
TLC_Student = function(r)
{
  var = r/(r-2)
  sigma = sqrt(sqrt(var/n)/n)
  nr = 0
  for(i in 1:3)
  {
    val = rt(1, r)
    for (j in 1:n)
      val[j] = rt(1, r)
    if (mean(val) <= z[i]*sigma)
      nr = nr+1
  }
  return(nr/3)
}
TLC_Student(5)



#B3
h=13
k=27
B = function(n, p)
{
  return(pnorm((k - n * p) / sqrt(n * p * (1 - p))) - pnorm((h - n * p) / sqrt(n * p * (1 - p))))
}
B(100,0.2)