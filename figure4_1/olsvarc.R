# OLSVARC.M
# Lutz Kilian
# University of Michigan
# April 1997
# This program estimates a level VAR with intercept in companion format by LS
# translate to R by André Suriane ECONS ECONOMIA UFJF 


options(digits = 12)

olsvarc<-function(y,p) {
  dime<-dim(y) #[t,q]
  t<-dime[1]; q<-dime[2]
  y<-t(y)
  Y=y[,p:t]	
  if(p>1){
  for (i in 1:(p-1)) {
   	Y=rbind(Y, y[,(p-i):(t-i)])
  }
  }
  X<-rbind(rep(1,t-p), Y[,1:(t-p)])

  Y=Y[,2:(t-p+1)]
  
  #A=(Y%*%t(X)%*%solve(X%*%t(X)))
  A <-tcrossprod(Y,X)%*%solve(tcrossprod(X)) 
  #A<- t(solve(tcrossprod(X),t(tcrossprod(Y,X))))
  
  U=Y-A%*%X
  SIGMA=tcrossprod(U)/(t-p-p*q-1)
  V=A[,1]
  A=A[,2:(q*p+1)]

  return(list(A =A,SIGMA= SIGMA,Uhat = U,V= V,X=X))
}
          
