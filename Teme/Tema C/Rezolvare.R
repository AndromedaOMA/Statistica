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
MCimprovedintegration <- function(N) {
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
MCimprovedintegration <- function(N) {
  sum = 0
  for(i in 1:N) {
    #u = runif(1, -Inf, 0) #eroare la -Inf...
    u = runif(1, -N, 0)
    sum = sum + u*exp(-u)
  }
  return(sum/N)
}
MCimprovedintegration(1000)



#C4
ex4a = function(N, m, n, p, q)
{
  get_nr_days = function()
  {
    nr_days = 1
    nr_fake_acc = m
    while (nr_fake_acc > 0)
    {
      new_fake_acc = nr_fake_acc
      for (i in 1:nr_fake_acc)
      {
        p_del = runif(1, 0, 1)
        if (p_del <= q)
          new_fake_acc = new_fake_acc - 1
      }
      nr_fake_acc = new_fake_acc
      nr_fake_acc = new_fake_acc + rbinom(1, n, p)
      nr_days = nr_days + 1
    }
    return (nr_days)
  }
  s = 0
  for (i in 1:N)
  {
    s = s + get_nr_days()
  }<-
  return(s/N)
}
print(ex4a(20, 100, 500, 0.5, 0.1))# 1000 10 0.2 0.8

ex4b = function(N, m, n, p, q)
{
  get_nr_days = function()
  {
    nr_days = 1
    nr_fake_acc = m
    while (nr_days < 40 && nr_fake_acc > 0)
    {
      new_fake_acc = nr_fake_acc
      for (i in 1:nr_fake_acc)
      {
        p_del = runif(1, 0, 1)
        if (p_del <= q)
          new_fake_acc = new_fake_acc - 1
      }
      nr_fake_acc = new_fake_acc
      nr_fake_acc = new_fake_acc + rbinom(1, n, p)
      nr_days = nr_days + 1
    }
    return (nr_fake_acc)
  }
  s = 0
  for (i in 1:N)
  {
    if (get_nr_days() <= 50000)
      s = s + 1
  }
  return(s/N)
}
print(ex4b(20, 100, 10, 0.2, 0.8))# 1000 10 0.2 0.8

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