#ex1
nivel_calciu=function(sigma,n, sample_mean,population_mean, alfa, ipoteza )
{
  tscore=(sample_mean-population_mean)/(sigma/sqrt(n))
  if(ipoteza == 'left'){
    criticalt=qt(alfa,n-1)
    if(tscore<criticalt){
      cat("Ipoteza nula este respinsa astfel se accepta ipoteza alternativa")
    }
    else{
      cat("Nu exista suficiente dovezi pentru a respinge ipoteza nula si a accepta ipoteza alternativa")
    }
  }
  if(ipoteza == 'right'){
    criticalt=qt(1-alfa,n-1)
    if(tscore > criticalt){
      cat("Ipoteza nula este respinsa astfel se accepta ipoteza alternativa")
      
    }
    else{
      cat("Nu exista suficiente dovezi pentru a respinge ipoteza nula si a accepta ipoteza alternativa ")
      
    }
  }
  if(ipoteza == 'sim'){
    criticalt=qt(1-alfa/2,n-1)
    if(abs(tscore) > abs(criticalt)){
      cat("Ipoteza nula este respinsa astfel se accepta ipoteza alternativa")
      
    }
    else{
      cat("Nu exista suficiente dovezi pentru a respinge ipoteza nula si a accepta ipoteza alternativa ")
      
    }
  }
  
}
x=c(10.51,10.22,9.87,9.93,10.34,10.48,10.17)
n=length(x)
sample_mean=mean(x)
sample_mean
alfa=0.01
population_mean=10.3
sigma=sd(x)

#sample_mean=10.21714 < 10.3 => ipoteza asimetrica la stanga
nivel_calciu(sigma, n, sample_mean, population_mean, alfa, 'left')

#nu exista suficiente dovezi pentru a respinge

#ex2

ex2= function(alfa) {
  #ipoteza asimetrica la dreapta
  z=1.85
  n=65
  z_star=qnorm(1-alfa,0,1)
  if(abs(z)>abs(z_star)) {
    return("ipoteza nula este respinsa")
  }
  else {
    return ("nu am avut destul dovezi pentru a respinge ipoteza")
  }
}

ex2(0.05)
ex2(0.01)
#ex3

ex3 = function(n1, n2, m0, sample_mean1, sample_mean2, stdev1, stdev2, alfa, ipoteza){
  
  z_score = (sample_mean1 - sample_mean2 - m0)/sqrt(stdev1^2/n1+ stdev2^2/n2);
  if(ipoteza=='left'){
    critical_z = qnorm(alfa); # critical_z < 0
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa);# critical_z > 0
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}

alfa=0.01 #alfa=0.05
sample1_mean=76.56
sample2_mean=72.23
n1=21
n2=22
sigma1=2.95
sigma2=3.12
m0=0
#n1-n2!=0 => ipoteza simetrica
ex3(n1,n2,m0,sample1_mean,sample2_mean,sigma1,sigma2,alfa, 'sim')
#nu se poate respinge ipoteza nula




#ex4
alfa=0.01 #alfa=0.05
sample1_mean=20.5
sample2_mean=21.6
n1=87
n2=91
sigma1=1.15
sigma2=0.92
m0=0
#n1-n2!=0 => ipoteza simetrica
ex3(n1,n2,m0,sample1_mean,sample2_mean,sigma1,sigma2,alfa,'sim')

#ex5

  ex5=function(n1, n2, sigma1, sigma2, alfa, ipoteza )
  {
  fscore=(sigma1/sigma2)^2
  if(ipoteza == 'right'){
    criticalz=qf(1-alfa,n1-1,n2-1)
    if(fscore > criticalz){
      cat("Ipoteza nula este respinsa, se accepta ipoteza alternativa")
      
    }
    else{
      cat("Nu sunt suficiente dovezi sa respingem ipoteza nula ")
      
    }
  }
  if(ipoteza == 'sim'){
    criticalfs=qf(alfa/2,n1-1,n2-1)
    criticalfd=qf(1-alfa/2, n1-1, n2-1)
    if((fscore<criticalfs) | (fscore>criticalfd)){
      cat("Ipoteza nula este respinsa, se accepta ipoteza alternativa")
      
    }
    else{
      cat("Nu sunt suficiente dovezi sa respingem ipoteza nula ")
      
    }
  }
  
  }
  
  n1=235
  n2=197
  sigma1=1.83
  sigma2=2.11
  alfa=0.01
  #simga1/sigma2<1 !=1 => ipoteza asimetrica la dreapta
  ex5(n1,n2,sigma1,sigma2,alfa, 'right')
  #nu sunt suficiente dovezi pentru a respinge ipoteza nula
  















