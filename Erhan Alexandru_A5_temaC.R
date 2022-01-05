#C1 (a)
index=function(a,x) {
  i=0
  j=1
  n=length(x)
  found=FALSE
  while(!found) {
    i=sample(n,1);
    newlist=list(i,j)
    if(x[i]==a)
      return(newlist)
    j=j+1
  }
}
x=c(3,4,5,4,3,1,6,7,8,2,9,7);
index(4,x)

x=sample(1:20, 20)
y=rep(x, 10)
index(15,y) #prima returneaza pozitia si a doua din a cata incercare a gasit

compara=function(a,NC) {
  sum = 0
  nr=array();
  n=length(x)
  for(i in 1 : NC)
  {
    nr=index(a,y)
    sum = sum+nr[[2]];
  }
  if(sum/NC< n/10) 
    print("Au fost efectuate mai putine iteratii")
  else print("Au fost efectuate mai multe iteratii")
}

compara(15,100)

#C2
nthroot=function(x,n) {
  return(x^(1/n))
}


mediana=function(S)
{
  n=length(S)
  N=n*n*n
  root=nthroot(N,4);
  R=sample(S,root,replace=TRUE);
  R=sort(R);
  d=R[trunc((nthroot(N,4)/2)-sqrt(n))]
  u=R[trunc((nthroot(N,4)/2)+sqrt(n))+1]
  C=array() # C va include mediana lui S cu prob mare si C e suficient de mica sa poata fi sortata
  nr=1;
  i=1;
  for(i in S) {
    if(i>=d & i<=u) 
    {
      C[nr]=i;
      nr=nr+1;
    }
  }
  Ld=0; #contine elementele din S care sunt mai mici ca d;
  Lu=0; #contine elementele din S care sunt mai mari ca u;
  Lc=0; # elementele din S mai mari ca d si mai mici ca u;
  for(i in S)
  {
    if(i<d) {
      Ld=Ld+1
    }
    else if(i>u) {
      Lu=Lu+1
    }
    else Lc=Lc+1
  }
  m=length(C)
  if (Ld>(n/2)|Lu>(n/2)|m>4*root)
    return("Can't find the median")
  C=sort(C)
  if(n%%2==0) {
    return((C[(n/2)-Ld+1]+C[(n/2)-Ld])/2)
  }
  return(C[(n/2)-Ld+1])
}
M =sample(1:204, 204)

mediana(M)
median(M)


#C3

egalitate=function(n) {
  p=0;
  p=sample(1:(3*n),1)
  f=c(sample(1:p, n+1, replace=TRUE))
  g=c(sample(1:p, n+1, replace=TRUE))
  h=c(sample(1:p, 2*n+1, replace=TRUE))
  tmp=convolve(f,rev(g),type="open")
  all(tmp==h) 
}
egalitate(2)
# f=c(2,1) g=c(-1,3) h=c(-2,5,3), de ex pt aceste 3 polinoame da TRUE


#C4

prim=function(n) {
  #n-1=2^k*m
  k=0;
  n2=n-1;
  while(n2 %% 2 == 0)
  {
    k = k + 1;
    n2 = n2 / 2;
  }
  m=n2;
  a=sample(2:(n-2),1)
  b=(a^m)%%n
  for(i in 1:k) {
    if((b!=1)&(b!=(n-1)))
      return(FALSE)
    else if(b==(n-1))
      return(TRUE)
    b=(b*b)%%n
  }
  if(b!=1) {
    return(FALSE) }
  else return(TRUE)
  
}

prim(7)


