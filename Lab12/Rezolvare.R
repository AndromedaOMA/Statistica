#1
selectionmean=function(filename)
{
    x=scan(filename)
    m=mean(x)
    return m
}
selectionmean("sample.txt")

#2
alfa=0.1
samplemean=20
n=100
sigma=sqrt(9)
criticalz=qnorm(1-alfa/2,0,1)
a=samplemean-criticalz*sigma/sqrt(n)
b=samplemean+criticalz*sigma/sqrt(n)
interval=c(a,b)
interval

#2.1
zconfidence_interval = function(n, samplemean, alfa, sigma)
{
    criticalz = qnorm(1 - alfa/2, 0, 1) #pct critic
    a = samplemean - criticalz*sigma/sqrt(n)#capetele intervalului
    b = samplemean + criticalz*sigma/sqrt(n)
    interval = c(a, b)
    return(interval)
}
zconfidence_interval(100,20,0.1,3)
#2.2
zconfidence_interval(25,67.53,0.1,10)
#2.3
zconfidence_interval(50,5,0.05,0.5)


#-----------------------------
#2.6
zconfidence_interval_from_file = function(file_path, alfa, sigma) {
  data = scan(file_path)
  samplemean = mean(data)#PB
  n = length(data)

  #verificare
  print(data)
  print(samplemean)
  print(n)

  criticalz = qnorm(1 - alfa/2, 0, 1)
  a = samplemean - criticalz * sigma / sqrt(n)
  b = samplemean + criticalz * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}
zconfidence_interval_from_file("D:/FACULTATE/RStudio/Statistica/Lab12/fisier2-5.csv",0.1,2)

#2.6VARIANTA
zconfidence_interval_from_file = function(file_path, alfa, sigma) {
  data = read.csv(file_path, header = FALSE)
  samplemean = mean(data)#PB
  n = length(data)

  #verificare
  print(data)
  print(samplemean)
  print(n)

  criticalz = qnorm(1 - alfa/2, 0, 1)
  a = samplemean - criticalz * sigma / sqrt(n)
  b = samplemean + criticalz * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}
zconfidence_interval_from_file("D:/FACULTATE/RStudio/Statistica/Lab12/fisier2-5.txt",0.1,2)







#3
alfa=0.05
samplemean=3.3
n=60
s=0.4
se=s/sqrt(n)
criticalt=qt(1-alfa/2,n-1)
a=samplemean-criticalt*se
b=samplemean+criticalt*se
interval=c(a,b)
interval

#3.1
t_conf_interval = function(n,sample_mean,s,alfa)
{
    se=s/sqrt(n)
    criticalt=qt(1-alfa/2,n-1)
    a=samplemean-criticalt*se
    b=samplemean+criticalt*se
    interval=c(a,b)
    return(interval)
}
#3.2
t_conf_interval(196,44.65,sqrt(2.25),0.01)
#3.3.a)
t_conf_interval(49,12,1.75,0.01)
t_conf_interval(49,12,1.75,0.05)
#3.3.b)
t_conf_interval(49,13,1.25,0.05)

#----------------
#3.4
 selectionmean = function(filename,alfa) {
   x = scan(filename)
   n = length(x)
   mediasel = mean(x)
   s = sd(x)
   se = s / sqrt(n)
   critical_t = qt(1 - alfa/2, n - 1)
   
   a = mediasel - critical_t * se
   b = mediasel + critical_t * se
   
   interval = c(a, b)
   return(interval)
 }
 selectionmean("D:/FACULTATE/RStudio/Statistica/Lab12/fisier3-4.txt",0.1)





#4
alfa=0.01
n=100
succese=63
pprim=succese/n
p0=0.6
zscore=(pprim-p0)/sqrt(p0(1-p0)/n)
criticalz=qnorm(1-alfa,0,1)
zscore
criticalz

#4.1
test_proportion =function( alfa, n, succese, p0, tip_ipoteza){
    pprim = succese/n
    zscore = (pprim - p0)/sqrt(p0*(1 - p0)/n)
    cat("zscore=",zscore)
    #In fct de tipul de ipoteza
    if(tip_ipoteza=="r"){
         criticalz = qnorm(1 - alfa, 0, 1)
         cat("criticalz=",criticalz)
         if(zscore < criticalz)
           print("HO nu se poate respinge")
         else 
           print("HO se respinge")
         }
    if(tip_ipoteza=="l"){
       criticalz = qnorm(alfa, 0, 1)
       cat("criticalz=",criticalz)
       if(zscore > criticalz)
           print("HO nu se poate respinge")
       else 
           print("HO se respinge")
        }
    if(tip_ipoteza=="s"){
      criticalz = qnorm(alfa, 0, 1)
      cat("criticalz=",criticalz)
         if(abs(zscore) <= criticalz)
            print("HO nu se poate respinge")
         else 
            print("HO se respinge")
    }
  }
  test_proportion(0.01, 100, 63, 0.6,"r")