#D1


nivel_de_incredere=function(incredere,dispersie,n,sample_mean) {
  alfa=1-incredere
  se=dispersie/sqrt(n)
  critical_t=qnorm(1-alfa/2,0,1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  return(interval)
}

nivel_de_incredere(0.99,sqrt(0.0025),8,4.5)

#D2

nivel_de_incredere(0.99,sqrt(10),225,40)

#D3

n=125 #esantion
succes=24 #confirma rata
rata=0.171
alfa=0.01 # si 0.05
p_prim=succes/n; 
p_prim # p_prim=0.192 => p_prim>rata => ipoteza asimetrica la dreapta
test_score=(p_prim-rata)/(sqrt(rata*(1-rata)/n))
critical_value=qnorm(1-alfa,0,1)
test_score
critical_value


test= function(n, p0, succese, alfa, ipoteza){
  p_prim = succese/n;
  z_score = (p_prim - p0)/(sqrt(p0*(1 - p0)/n));
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula ");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula ");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula ");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}
#p_prim # p_prim=0.192 => p_prim>rata => ipoteza asimetrica la dreapta

test(125,0.171,24,0.05,'right')
test(125,0.171,24,0.01,'right')

#pentru ambele nivele de semnificatie
#test_score<critical_value => nu exista suficiente dovezi 
#pentru a respinge ipoteza nula 
#si a accepta ipoteza alternativa


  
  #D4
  n=90
  succes=50
  rata=0.58
  alfa=0.05 # si 0.05
  p_prim=succes/n
  p_prim #p_prim=0.55<rata => ipoteza asimetrica la stanga
  test_score=(p_prim-rata)/(sqrt(rata*(1-rata)/n))
  critical_value=qnorm(alfa,0,1)
  test_score
  critical_value
  test(90,0.58,50,0.05,'left')
  test(90,0.58,50,0.01,'left')
  
  #pentru ambele nivele de semnificatie
  #test_score>critical_value => nu exista suficiente dovezi;
  
  
  
  
  
