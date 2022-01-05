#ex1
volum_con=function(N) {
  c=0;
  for(i in 1:N) {
    x=runif(1, -2, 2);
    y=runif(1, -2, 2);
    z=runif(1, 0, 3);
    if((x*x+y*y)<= (4/9)*z*z)
      c=c+1;
  }
  return((4*4*3*c)/N);
}

a=volum_con(10000)
b=volum_con(20000)
c=volum_con(50000) #mai aproape de volumul exact ca celelalte
volum_exact=12.566370614359172953850573533118

eroare_relativa=function(x,y) {
   z=(abs(x-y))/abs(y);
   return(z)
}
eroare_relativa(a, volum_exact)
eroare_relativa(b, volum_exact)
eroare_relativa(c, volum_exact)

#ex2
triunghi=function(N,a,b,c,d) {
  Nc=0;
  for(i in 1:N) {
    x=runif(1,a,b);
    y=runif(1,c,d);
    if(2*y<=x & (x+y)<=3)
      Nc=Nc+1;
  }
  return(((b-a)*(d-c)*Nc)/N)
}

triunghi(15000,0,3,0,3);
#zona rectangulara este [0,3]x[0,3];

#ex3
integrare=function(N) {
  sum=0;
  for(i in 1:N) {
    x=runif(1,0,1);
    sum=sum+(2*sqrt(x)/(x+1));
  }
  return(sum/N);
}
integrare(10000)
rez_exact=0.8584073464102067615373566167205

integrare2=function(N, lambda) {
  sum=0;
  for(i in 1:N) {
    x=rexp(1,lambda);
    sum=sum +((1+(2*x))*(exp(-x))/(lambda*exp(-lambda*x)));
  }
  return(sum/N);
}
integrare2(50000, 1);
rez_exact2=3;

#ex4
#lambda=2, prob=0.35,0.4,0.25;
virus=function(N) {
  sum=0;
  for(i in 1:N) {
    G1=rgamma(1, 6, 4);
    G2=rgamma(1, 6, 2);
    G3=rgamma(1, 5, 3);
    latency=rexp(1,2);
    G=c(G1,G2,G3);
    probability=c(0.35,0.4,0.25);
    sum=sum+sample(G,1,prob=probability)+latency;
  }
  return(sum/N);
}

virus(1000)

#ex6
n=98;
m=196;
initializarePadure=function(n,m) {
  padure=matrix(rep(-1),nrow=n,ncol=m);
  k=0;
  for(i in 1:n) {
    for(j in (1+k):(m-k)) {
      if((i+j)%%2==0)
        padure[i,j]=0
      k=k+1;
    }
  }
  return(padure);
}

isneighborburning = function(padure,i,j)
{
    if(padure[i-1,j-1] > 0) 
    {
      prob=0.4;
      tree=sample(c(0, 1), 1, prob = 0,4)
      if(tree== 1)
        return(TRUE)
    }
    else if(padure[i+1,j+1] > 0) 
    {
      prob=0.4;
      tree=sample(c(0, 1), 1, prob = 0,4)
      if(tree== 1)
        return(TRUE)
    }
    else if(padure[i-1, j+1]>0)
    {
      prob=0.7
      tree=sample(c(0,1),1,prob=0.7)
      if(tree==1)
        return(TRUE)
    }
   else if(padure[i+1, j-1]>0)
   {
    prob=0.7
    tree=sample(c(0,1),1,prob=0.7)
    if(tree==1)
      return(TRUE)
   }
   else return(FALSE)
}

startfire = function(padure)
{
  padure[50, 100] =1
  start=1
  ars=1
  while(padure[98, 98] == 0)
  {
    for(i in 1 : 98)
      for(j in 1 : 196)
        if(padure[i, j] == 0) 
        {
          if(isneighborburning(padure,i,j)==TRUE) {
            ars=ars+1
            padure[i,j]=sample(c(0,1),1,prob=c(0.4,0.7))
        }
    start=start+1
    
        }
  }
    k=0
    for(i in 1:98) {
      for(j in (1+k):(196-k)) {
        if((i+j)%%2==0)
          padure[i,j]=0
        k=k+1;
      }
    }
    if(ars>k/4)
      return(TRUE)
    else return(FALSE)
}

hour = function(padure)
{
  for(i in 1 : 100)
  {
    media = media + startfire(padure)
  }
  return(c / 100)
}





