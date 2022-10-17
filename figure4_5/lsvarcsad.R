# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# LSVARCSA.M
#
# Function estimates a VAR(p) with seasonal dummies by LS
# Dan Murphy, modified by Lutz Kilian, modified by Christiane Baumeister
# May 2011

lsvarcsad <- function(y,p) {

# Set up regressors and regressand
   t<-nrow(y); q<-ncol(y)
   y <- t(y)
   Y=y[,p:t]	
  if(p>1){
    for (i in 1:(p-1)) {
      Y=rbind(Y, y[,(p-i):(t-i)])
    }
   }

# Creating seasonal dummies
 x<-rbind(diag(11),matrix(0,1,11))
   
X2=x
  for (i in 2:floor((t-p)/12)) {  #number of years
      X2=rbind(X2,x);
  }
l<-nrow(X2); w<-ncol(X2);
if ((11-((t-p)-l))>0 ){
last=rbind(diag((t-p)-l), matrix(0,(t-p)-l,11-((t-p)-l)))
} else {
  last<-diag((t-p)-l)
}
X2=rbind(X2,last)
X2=cbind(ones(t-p,1), X2)


X=rbind(t(X2), Y[,1:(t-p)])
Y=y[,(p+1):t]

#Run LS regression
B<-tcrossprod(Y,X)%*%solve(tcrossprod(X)) 
U=Y-B%*%X
SIGMA=tcrossprod(U)/(t-p-p*q-1)
V=B[,1:12]
A=B[,13:(q*p+12)]


A=rbind(A,cbind(diag(p*q-q), matrix(0,p*q-q,q)))
return(list(A=A,SIGMA=SIGMA,U=U,V=V))
}
