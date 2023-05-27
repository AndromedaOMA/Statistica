#A1.
# a)

#LEGENDA: Poisson -> purple; Geometric -> red; B -> black;
lam = 3
k = 0:10
pois = dpois(k:n, lam)
plot(k, pois, type = "l", col = "#ab11ad",
xlab = "Valoarea k", ylab = "Masa de probab", main = "Distrib. Poisson, Geom si B")

p = 0.8
geom = dgeom(k:n, p)
lines(k, geom, type = "l", col = "#ff0000")

n = 10
p = 0.4
binom = dbinom(k:n, n, p)
lines(k, binom, type = "l", col = "#000000")


#aici am incercat sa o implementez ca o functie...
toate_reprezentarile = function(lam, p, n, k)
{
  pois = dpois(k:n, lam)
  plot(k, pois, type = "l", col = "#ab11ad",
  xlab = "Valoarea k", ylab = "Masa de probab", main = "Distrib. Poisson, Geom si B")

  geom = dgeom(k:n, p)
  lines(k, geom, type = "l", col = "#ff0000")

  binom = dbinom(k:n, n, p)
  lines(k, binom, type = "l", col = "#000000")
}
toate_reprezentarile(2, 0.6, 10, 1)

#------------------------------
# b)

#prob Geom
p = 0.1

p_impar = 1 / (2 - p)
print(p_impar)

p_mare_4 = pgeom(3, p, lower.tail = FALSE)
print(p_mare_4)

p_mic_20 = pgeom(20, p, lower.tail = TRUE)
print(p_mic_20)

#------------------------------
# c)

#mamma mia

#================================================================================

# A2.

#TEST_start---------------------------
# primesc eroare cand citesc fisierul "note.txt", drept urmare voi lucra cu csv
#     y = read.table(header=T, "note.txt")

# fisierul csv l-am gasit pe site-ul domnului profesor Olariu.
# primesc eroare si la note.csv, de aceea voi lucra cu PATH-ul acestuia
#     y = read.csv(file="note.csv", header =T)

y = read.csv(file="D:/FACULTATE/RStudio/Statistica/Teme/Tema A/note.csv", header =T)

y1 = y[['P']]
y2 = y[['S']]

# Merge:)
#TEST_final---------------------------

# a)
stat_list = function(nume)
{
  data = read.csv(nume, stringsAsFactors = FALSE)
  
  mediana = median(unlist(data))
  media = mean(unlist(data))
  deviatie_standard = sd(unlist(data))
  
  cvartila_1 = quantile(unlist(data), 0.25)
  cvartila_2 = quantile(unlist(data), 0.75)
  
  print(paste("Mediana:", mediana))
  print(paste("Media:", media))
  print(paste("Deviația standard:", deviatie_standard))
  print(paste("Prima cvartilă:", cvartila_1))
  print(paste("A doua cvartilă:", cvartila_2))
}
#stat_list("note.csv")
stat_list("D:/FACULTATE/RStudio/Statistica/Teme/Tema A/note.csv")


# b)
elim_val_aberante = function(nume_fisier, nume_esantion)
{  
  y = read.csv(file = nume_fisier, header = T)
  esantion = switch(nume_esantion, "P" = y[["P"]], "S" = y[["S"]])
  
  media_esantion = mean(esantion)
  deviatia_standard = sd(esantion)
  
  valori_aberante = esantion[abs(esantion - media_esantion) >= 3 * deviatia_standard]
  esantion_filtrat = esantion[abs(esantion - media_esantion) < 3 * deviatia_standard]

  return(esantion_filtrat)
}
#esantion_P_filtrat = elim_val_aberante("note.csv", "P")
#esantion_P_filtrat = elim_val_aberante("note.csv", "S")
elim_val_aberante("D:/FACULTATE/RStudio/Statistica/Teme/Tema A/note.csv", "P")
elim_val_aberante("D:/FACULTATE/RStudio/Statistica/Teme/Tema A/note.csv", "S")

# c)
reprezentare = function(nume_fisier) {
  data = read.csv(nume_fisier, header = TRUE)
  note = data[["Nota"]]
  intervale = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  frecvente = table(cut(note, breaks = intervale, right = FALSE))
  plot(frecvente, type = "p", lwd = 10, lcol = "red", xlab = "Interval", ylab = "Frecventa", main = "Distributie")
}
#reprezentare("note.csv")
reprezentare("D:/FACULTATE/RStudio/Statistica/Teme/Tema A/note.csv")