#I.Ex.
disc_area=function(N)
{
    NC=0
    for(i in 1:N)
    {
        x=runif(1,-1,1)
        y=runif(1,-1,1)
        if(x*x+y*y<=1)
            NC=NC+1
    }
    return(4*NC/N)
}
disc_area(50000)

#I.1.
Sphere_vol=function(N)
{
  NC=0
  for(i in 1:N)
    {
        x=runif(1,-1,1)
        y=runif(1,-1,1)
        z=runif(1,-1,1)
        if(x^2 + y^2 + z^2 <= 1)
            NC=NC+1
    }

  V=8*(NC/N)
  eroareAbsoluta=abs(V-(4*pi)/3)
  eroareRelativa=eroareAbsoluta/((4*pi)/3)

  cat("Volumul sferei unitate este ", V,", eroarea absoluta este ", eroareAbsoluta,", iar eroarea relativa este ", eroareRelativa)
}
Sphere_vol(50000)


#I.2.
parabola = function(x)
{
  return(-2*x^2 + 5*x - 2)
}

n = 10000
x = runif(n, 0, 2)
y = runif(n, 0, 2)

subparabola = y <= parabola(x)
aria_estimata = sum(subparabola) / n * 2 * max(parabola(x))
aria_exacta = abs(integrate(parabola, 0, 2)$value)
eroare_relativa = abs(aria_estimata - aria_exacta) / aria_exacta

cat("Aria estimata: ", aria_estimata, "\n",
"Aria exacta: ", aria_exacta, "\n",
"Eroarea relativa: ", eroare_relativa, "\n")


#=============================================================================
#II.Ex1.
MCintegration=function(N)
{
  sum=0
  for(i in 1:N)
  { 
    u=runif(1,0,10)
    sum=sum+exp(-u*u/2)
  } 
  return(10*sum/N)
}
MCintegration(10000)

estimates={}
MCintegraverage=function(k,N)
{ 
  for(i in 1:k) 
    estimates[i]=MCintegration(N)
  print(mean(estimates))
  print(sd(estimates))
}
MCintegraverage(30,10)
MCintegraverage(30,100)
MCintegraverage(30,1000)



#II.Ex2.
MCimprovedintegration=function(N)
{ 
  sum=0
  for(i in 1:N)
  { 
    u=rexp(1,1)
    sum=sum+exp(-u*u)/exp(-u)
  } 
  return(sum/N)
}
MCimprovedintegration(1000)


estimates={}
MCimprvdintegraverage=function(k,N)
{ 
  estimates=0
  for(i in 1:k) 
    estimates[i]=MCimprovedintegration(N)
  print(mean(estimates))
  print(sd(estimates))
}
MCimprvdintegraverage(30,20000)


#II.1.a)
MCimprovedintegration=function(N)
{ 
  sum=0
  for(i in 1:N)
  { 
    u=runif(1,1,N)
    sum=sum+sin(u)^2
  } 
  return(pi*sum/N)
}
MCimprovedintegration(1000)
cat("Valoarea absoluta este: ", abs(MCimprovedintegration(10000)-pi/2), "\n",
    " Eroarea relativa este: ",  abs(MCimprovedintegration(10000)-pi/2)/(pi/2))

#II.1.b)
MCimprovedintegrationb = function(N) {  #nr de puncte generate pentru estimarea integralei
  sum = 0 #suma valorilor fct in punctele generate
  for(i in 1:N) {
    u = runif(1, 1, 4) #a=1 si b=4
    sum = sum + exp(u)
  }
  return(3*sum/N) #((b-a)*sum/N)
}
exact_values = c(51.87987)
estimated_values = MCimprovedintegrationb(50000)
absolute_errors = abs(estimated_values - exact_values)
relative_errors = absolute_errors / exact_values

cat("Valorile exacte ale integralelor: ", exact_values, 
"Valorile estimate ale integralelor: ", estimated_values, 
"Erorile absolute corespunzătoare: ", absolute_errors, 
"Erorile relative corespunzătoare: ", relative_errors)

#II.1.d)
MCimprovedintegration = function(a, N) {
  sum = 0
  for(i in 1:N) {
    u = runif(1, a, a + 10)  # Folosim limita superioară a + 10 pentru a aproxima infinitul u = rexp(1, 1)
    sum = sum + 1/(4*u^2-1)
  }
  return(sum/N)
}
exact_values = c(log(3/4))
estimated_values = MCimprovedintegration(1,50000)

# Calcularea erorilor absolute și relative
absolute_errors = abs(estimated_values - exact_values)
relative_errors = absolute_errors / exact_values

cat("Valorile exacte ale integralelor: ", exact_values, 
"Valorile estimate ale integralelor: ", estimated_values, 
"Erorile absolute corespunzătoare: ", absolute_errors, 
"Erorile relative corespunzătoare: ", relative_errors)



#II.2.
MCimprovedintegration = function(k,N)
{
  sum = 0
  for (i in 1:N)
  {
    u = rexp(1, k)
    sum = sum + exp(-2 * u^2)
  }
  return(sqrt(pi/8) * (sum / N))
}
MCimprovedintegration(3,50000)



#=============================================================================
#III
Nrdays=function()
{
  nrdays=1
  lasterrors=c(27,31)
  nrerrors=27
  while(nrerrors>0)
  {
    lambda=min(lasterrors)
    nrerrors=rpois(1,lambda)
    lasterrors=c(nrerrors,lasterrors[1])
    nrdays=nrdays+1
  }
  return(nrdays)
}
Nrdays()

MCnrdays=function(N)
{
  s=0
  for(i in 1:N)
    s=s+Nrdays()
  return(s/N)
}
MCnrdays(1000)



#III.1.
Nrdays_media = function()
{
  nrdays = 1
  lasterrors = c(9, 15, 13)
  nrerrors = sum(lasterrors)
  while (nrerrors > 0)
  {
    lambda = median(lasterrors)
    nrerrors = rpois(1, lambda)
    lasterrors = c(nrerrors, lasterrors[1:2])
    nrdays = nrdays + 1
  }
  return(nrdays)
}
Nrdays_media()

MCnrdays_media = function(N)
{
  s = 0
  for (i in 1:N)
    s = s + Nrdays_media()
  return(s/N)
}
MCnrdays_media(100)

#III.2.
estimate_mean_X = function(N)
{
  val = runif(1)
  # Daca val <= 3/4, alegem primul mecanic cu parametrul lambda = 4
  if (val <= 0.75)
    X = rexp(1, 4)
  else# Altfel, alegem al doilea mecanic cu parametrul lambda = 12
    X = (rexp(1, 12) + 3)

  sum_X = 0
  for (i in 1:N)
    sum_X = sum_X + X

  return(sum_X / N)
}
estimate_mean_X(100)


#=============================================================================
#IV
Nrdays=function()
{
  nrdays=2
  lasterrors=c(18,22,28)
  nrerrors=18
  while(nrerrors>0)
  {
    lambda=min(lasterrors)
    nrerrors=rpois(1,lambda)
    lasterrors=c(nrerrors,lasterrors[1:2])
    nrdays=nrdays+1
  }
  return(nrdays)
}
Nrdays()

MCnrdays21=function(N)
{
  s=0
  for(i in 1:N)
  {
    if(Nrdays()>21)
    s=s+1
  } 
  return(s/N)
}
MCnrdays21(21)


#IV.1.
p_X = 0.3
p_Y = 0.5

N = 100
z = qnorm(0.975)

X = rgeom(N, p_X)
Y = rgeom(N, p_Y)

prob_antet = mean(X < Y/2)
sigma = sqrt(prob_antet * (1 - prob_antet) / N)

e = 0.005
n = (z * sigma / e)^2

cat(n)