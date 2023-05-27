#C1
a=c(2, 4, 10)
N=c(10000, 20000, 50000)
estimate_volume = function()
{
  NC = 0
  for (i in 1:3)
  {
    x = runif(1, -sqrt(a[i]), sqrt(a[i]))
    y = runif(1, 0, a[i])

    if (x^2 + 1 <= y)
        NC = NC + 1

    V = NC / N[i] * (2 * sqrt(a[i]))^3
    eroareAbsoluta = abs(V - pi/2*a[i]^2)
    eroareRelativa = eroareAbsoluta / (pi/2*a[i]^2)

    cat("Pentru a=",a[i], " si N=",N[i],",\n",
    "volumul calculat este ", V, "\n",
    "eroarea absoluta este ", eroareAbsoluta,",\n",
     "iar eroarea relativa este ", eroareRelativa, "\n \n")
    }
}
estimate_volume()

#================================================================================

#C2
#prin calcul matematic avem:
x_min = 0
x_max = 6
y_min = 0
y_max = 4

patrulater_area = function(N) {
  NC = 0
  for (i in 1:N) {
    x = runif(1, x_min, x_max)
    y = runif(1, y_min, y_max)
    if (3 * y <= x + 6 & y <= 12 - 3 * x) {
      NC = NC + 1
    }
  }
  return(((x_max - x_min) * (y_max - y_min) * NC) / N)
}
patrulater_area(50000)

#================================================================================

#C3
#a)
MCimprovedintegration=function(N)
{
    sum = 0
    for(i in 1:N)
    {
        u = runif(1, -1, 1)
        sum = sum + (u + 1) / sqrt(4 - u^2)
    }
    return(2 * sum / N)
}
MCimprovedintegration(10000)

#b)
MCimprovedintegration = function(N)
{
  sum = 0
  for(i in 1:N) {
    #u = runif(1, -Inf, 0) #eroare la -Inf...
    u = runif(1, -N, 0) 
    sum = sum + (1/(u^2+4))
  }
  return(sum/N)
}
MCimprovedintegration(1000)

#c)
MCimprovedintegration = function(N)
{
  sum = 0
  for(i in 1:N) {
    #u = runif(1, -Inf, 0) #eroare la -Inf...
    u = runif(1, -N, 0)
    sum = sum + u*exp(-u)
  }
  return(sum/N)
}
MCimprovedintegration(1000)

#================================================================================

#C4
# a)
ex4a = function(N, m, n, p, q)
{
  numar_zile = function()
  {
    nr_zile = 1
    nr_cont_fals = m
    while (nr_cont_fals > 0)
    {
      nou_cont_fals = nr_cont_fals
      for (i in 1:nr_cont_fals)
      {
        p_del = runif(1, 0, 1)
        if (p_del <= q)
          nou_cont_fals = nou_cont_fals - 1
      }
      nr_cont_fals = nou_cont_fals
      nr_cont_fals = nou_cont_fals + rbinom(1, n, p)
      nr_zile = nr_zile + 1
    }
    return (nr_zile)
  }

  s = 0
  for (i in 1:N)
    {
      s = s + numar_zile()
    }=
  return(s/N)
}
ex4a(20, 100, 500, 0.5, 0.1)

# b)
ex4b = function(N, m, n, p, q)
{
  get_nr_days = function()
  {
    nr_zile = 1
    nr_cont_fals = m
    while (nr_zile < 40 && nr_cont_fals > 0)
    {
      nou_cont_fals = nr_cont_fals
      for (i in 1:nr_cont_fals)
      {
        p_del = runif(1, 0, 1)
        if (p_del <= q)
          nou_cont_fals = nou_cont_fals - 1
      }
      nr_cont_fals = nou_cont_fals
      nr_cont_fals = nou_cont_fals + rbinom(1, n, p)
      nr_zile = nr_zile + 1
    }
    return (nr_cont_fals)
  }
  s = 0
  for (i in 1:N)
  {
    if (get_nr_days() <= 50000)
      s = s + 1
  }
  return(s/N)
}
print(ex4b(20, 100, 10, 0.2, 0.8))

# c)
ex4c = function(N, m, n, p, q)
{
  alfa = 0.01
  z = qnorm(alfa/2)
  epsilon = 0.01
  P = ex4b(N, m, n, p, q)
  N_min = p * (1 - p) * (z / epsilon)^2
  x = ex4b(N_min, m, n, p, q)
  return(x)
}
print(ex4c(20, 100, 10, 0.2, 0.8))