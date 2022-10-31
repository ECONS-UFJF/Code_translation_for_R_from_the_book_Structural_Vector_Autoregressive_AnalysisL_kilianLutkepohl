# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# IRFVAR.M
# # Lutz Kilian
# University of Michigan
# April 1997


irfvar<-function(A,B0inv,p,K,H) {
require("expm")
J<- cbind(diag(K),matrix(0 ,K,K*(p-1)) )

IRF<-matrix(J%*%(A%^%0)%*%t(J)%*%B0inv,K^2,1)

for (i in 1:H) {
	IRF= cbind(IRF, matrix(J%*%(A%^%i)%*%t(J)%*%B0inv,K^2,1))
}
return(IRF)
}


