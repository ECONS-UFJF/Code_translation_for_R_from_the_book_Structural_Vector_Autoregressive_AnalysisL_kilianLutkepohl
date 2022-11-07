# IRFVAR.M
# # Lutz Kilian
# University of Michigan
# April 1997
# translate to R by André Suriane ECONS ECONOMIA UFJF 

irfvar<-function(A,SIGMA,p,h,K) {
require("expm")
J<- cbind(diag(K),matrix(0 ,K,K*(p-1)) )

IRF<-matrix(J%*%(A%^%0)%*%t(J)%*%t(chol(SIGMA)),K^2,1)

for (i in 1:h) {
	IRF= cbind(IRF, matrix(J%*%(A%^%i)%*%t(J)%*%t(chol(SIGMA)),K^2,1))
}
return(IRF)
}


