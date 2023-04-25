x=c(11,14,21,32,17,24,21,35,52,44,21,28,36,49,41,19,20,34,37,29)
stem(x)

sample=scan("sample1.txt")
min=min(sample)
max=max(sample)
interval=seq(40,100,10)
hist(sample,breaks=interval,right=F,freq=T)
a=6
hist(sample,breaks=a,right=F,col="blue")

frecv=c(9,8,12,3,17,41,29,35,32,40,19,8)
barplot(frecv,space=0)

#I. Ex. 1
stem(sample)

#I. Ex. 2
tablou= read.csv("unemploy2012.csv", header = T, sep= ';') 
rate = tablou[['rate']]
interval = c(0,4,6,8,19,12,14,30)
hist(rate, breaks = interval, right = T, freq= F)

#LAB. I. Ex. 3
tablou= read.csv("life_expext.csv", header = T) 
country = tablou[['country']]
female = tablou[['female']]
male = tablou[['male']]
hist(male, breaks = 7, right = T)
hist(female, breaks = 7, right = T)


#II. Ex. 1
mean(sample)
median(sample)

#II. Ex. 2
tablou2 = read.csv("life_expect.csv", header =T)
country = tablou2[["country"]]
female = tablou2[["female"]]
male = tablou2[["male"]]
mean(female)
median(female)
mean(male)
median(male)

sample=c(9,8,12,3,17,41,29,35,32,40,19,8)
summary(sample)

sample=c(1,91,38,72,13,27,11,85,5,22,20,19,8,17,11,15,13,23,14,17)
m=mean(sample)
s=sd(sample)
outliers=vector()
j=0
for(i in 1:length(sample))
  if(sample[i] < m-2 * s | sample[i] > m+2*s){
    j=j+1 
    outliers[j]=sample[i]
    }
outliers

#III. Ex. 1

outliers_mean = function(sample){
  m=mean(sample)
  s=sd(sample)
  outliers=vector()
  j=0
  for(i in 1:length(sample))
    if(sample[i] < m-2 * s | sample[i] > m+2*s){
      j=j+1 
      outliers[j]=sample[i]
    }
}

#III. Ex. 2


outliers_mean_igr = function(sample){
  q1 = as.vector(quantile(sample))[2]
  q2 = as.vector(quantile(sample))[4]
  igr=q2-q1
  outliers=vector()
  j=0
  for(i in 1:length(sample))
    if(sample[i] < m-2 * s | sample[i] > m+2*s){
      j=j+1 
      outliers[j]=sample[i]
    }
  return(outliers)
}
outliers_mean_igr(sample)