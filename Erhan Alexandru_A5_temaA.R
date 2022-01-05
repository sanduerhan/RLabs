#ex1
ex1p = function(k,l,lambda) {
  print(dpois(k:(l-1), lambda))
  barplot(dpois(k:(l-1), lambda))
}

ex1p(4,12,1)
ex1g = function(p,k,l) {
  print(dgeom(k:l, p))
  barplot(dgeom(k:l,p))
}

ex1g(0.25, 4, 12)

ex2a=function(a) {
  media=mean(a)
  mediana=median(a)
  deviatia=sd(a)
  q1=as.vector(quantile(a))[2]
  q3=as.vector(quantile(a))[4]
  rez=c(media, mediana, deviatia, q1, q3)
  return (rez)
  
}

esantion=c(79 ,71, 89, 57, 76, 64, 82, 82 ,67 ,80 ,81, 65, 73, 79, 79, 60, 58, 83, 74 ,68, 78 ,80, 78, 81, 76, 65, 70, 76, 58, 82 ,59, 73, 72 ,79, 87 ,63, 74 ,90 ,69,35, 83, 76, 61, 66 ,71 ,51, 57, 81 ,57, 65, 81, 78 ,77,81, 81, 73, 75, 66 ,56 ,62, 75 ,60 ,74, 74, 70, 71, 56, 74, 63,72,81,54,72,91,92)
ex2a(esantion)

ex2b=function(a) {
  media=mean(a)
  deviatia=sd(a)
  b=vector()
  j=1
  for(i in 1:length(a)) {
    if(a[i]<(media-2*deviatia) | a[i]>(media+2*deviatia)) {
        b[j]=a[i]
        j=j+1
    }
  }
  for(i in 1:length(b)) {
    a=a[a!=b[i]] }
  return(a)
}

a=ex2b(esantion)

interval=seq(40,max(a)+5,5)
hist(a, breaks=interval, right=T)


