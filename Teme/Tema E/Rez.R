#1
nivelele_de_incredere = function(n, sample_mean, s, alpha)
{
  se = s / sqrt(n)# eroarea standard a mediei eșantionului
  critical_t = qt(1 - alpha / 2, n - 1)
  a = sample_mean - critical_t * se
  b = sample_mean + critical_t * se
  interval = c(a, b)
  return(interval)
}

n = 10
sample_mean = 138  # media
s = 11  # Deviatia standard

# 90%
alpha = 0.10
nivelele_de_incredere(n, sample_mean, s, alpha)
# 95%
alpha = 0.05
nivelele_de_incredere(n, sample_mean, s, alpha)
# 99%
alpha = 0.01
nivelele_de_incredere(n, sample_mean, s, alpha)




#2
interval_incredere = function(n, sample_mean, s, alpha)
{
  se = s / sqrt(n)
  critical_z = qnorm(1 - alpha / 2)
  a = sample_mean - critical_z * se
  b = sample_mean + critical_z * se
  interval = c(a, b)
  return(interval)
}

n = 256  # Numarul de indivizi din eșantion
sample_mean = 18  # Media de selectie
s = sqrt(1.44)  # Deviatia standard de selectie
alpha = 0.05

interval_incredere(n, sample_mean, s, alpha)




#3
test_ipoteza = function(n, x, p, alpha)
{
  pr = x / n  # Procent clienti nemultumiti
  se = sqrt(p * (1 - p) / n)  # Deviatia standard
  z = (pr - p) / se  # Statistica testului Z
  critical_z = qnorm(1 - alpha / 2)  # Valoarea critica

  if (abs(z) > critical_z)
    print("Da")  # respingem ipoteza nula
  else
    print("Nu")  # nu respingem ipoteza nula
}

n = 153  # Numarul total de clienti din eșsantion
x = 17  # Numarul de clienti nemultumiti din esantion
p = 0.12  # Procentul de clienti nemultumiti inainte de schimbare

# 1%
alpha = 0.01
test_ipoteza(n, x, p, alpha)

# 5%
alpha = 0.05
test_ipoteza(n, x, p, alpha)
