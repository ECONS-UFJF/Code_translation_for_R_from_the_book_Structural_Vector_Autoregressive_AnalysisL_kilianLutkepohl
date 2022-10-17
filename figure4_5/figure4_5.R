## translate to R by André Suriane ECONS ECONOMIA UFJF 
# FIGURE4_5.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.5
#
# The code is adapted from Baumeister and Kilian (2014).

rm(list = ls())
library(R.matlab)
library(expm)
library(pracma)
source("lagn.R")
source("lsvarcsad.R")
# Load real-time data available as of December 2010
  # File contains data from Dec 2010 vintage plus nowcasts up to 2010.12
data0 <- readMat("data.mat")
for (x in names(data0)) {
  assign(paste0(x),data0[[x]])
}
rm(data0)


qo=lagn(100*log(data[,1]),1);                    # World oil production growth
rpo=log(100*data[,2]/data[,3]);                # Real U.S. RAC for imports deflated by US CPI
oecd_inv=lagn(data[,4]*data[,5],1);            # Proxy for chamge in world crude oil inventories 
rea=100*detrend(log(data[,6])-log(data[,3]),'linear'); # Global real economic activity indicator

yy=cbind(qo, rea[2:nrow(rea),1], rpo[2:length(rpo)], oecd_inv)

# Estimate reduced-form VAR(p) with seasonal dummies
t<- nrow(yy); K <-ncol(yy); p=24; h=12;     
lvsRES <-lsvarcsad(yy,p);#  [A,SIGMA,Uhat,V]

for (x in names(lvsRES)) {
  assign(paste0(x),lvsRES[[x]])
}
rm(lvsRES)


SIGMA<-SIGMA[1:K,1:K]
A<-A[1:K,]
C<-t(V[1:K,1]) 
D<-rbind(t(V[1:K,2:ncol(V)]), matrix(0,1,4)) 
DD<-rep(1,4) %x% D  # DD<-rbind(rbind(D,D),rbind(D,D))

# Generate h-step ahead iterative BVAR(24) baseline forecast for real price
# of oil
yy=rbind(yy, matrix(0,h,K))

#for (i in 1:h) {
#   yy[(t+i),]<-  C + DD[(i),] + 
#    yy[(t+i-1),]%*%t(A[,(1):(1*K)])       +yy[(t+i-2),]%*%t(A[,(1*K+1):(2*K)])   +yy[(t+i-3),]%*%t(A[,(2*K+1):(3*K)]) +
#  yy[(t+i-4),]%*%t(A[,(3*K+1):(4*K)])   +yy[(t+i-5),]%*%t(A[,(4*K+1):(5*K)])   +yy[(t+i-6),]%*%t(A[,(5*K+1):(6*K)]) +
#  yy[(t+i-7),]%*%t(A[,(6*K+1):(7*K)])   +yy[(t+i-8),]%*%t(A[,(7*K+1):(8*K)])   +yy[(t+i-9),]%*%t(A[,(8*K+1):(9*K)]) +
#  yy[(t+i-10),]%*%t(A[,(9*K+1):(10*K)]) +yy[(t+i-11),]%*%t(A[,(10*K+1):(11*K)])+yy[(t+i-12),]%*%t(A[,(11*K+1):(12*K)])  +
#  yy[(t+i-13),]%*%t(A[,(12*K+1):(13*K)])+yy[(t+i-14),]%*%t(A[,(13*K+1):(14*K)])+yy[(t+i-15),]%*%t(A[,(14*K+1):(15*K)]) +
#  yy[(t+i-16),]%*%t(A[,(15*K+1):(16*K)])+yy[(t+i-17),]%*%t(A[,(16*K+1):(17*K)])+yy[(t+i-18),]%*%t(A[,(17*K+1):(18*K)]) +
#  yy[(t+i-19),]%*%t(A[,(18*K+1):(19*K)])+yy[(t+i-20),]%*%t(A[,(19*K+1):(20*K)])+yy[(t+i-21),]%*%t(A[,(20*K+1):(21*K)]) +
#  yy[(t+i-22),]%*%t(A[,(21*K+1):(22*K)])+yy[(t+i-23),]%*%t(A[,(22*K+1):(23*K)])+yy[(t+i-24),]%*%t(A[,(23*K+1):(24*K)])
#}
# Abaixo igual ao somatario anterior mas em forma reduzida 
for (i in 1:h) {
  yyij<-matrix(0,1,K)
  for (j  in 1:p) { #p=24
    yyij<-yyij+yy[(t + i- j),]%*%t(A[,( (j-1)*K + 1):( j*K)])
  }  
  yy[(t + i),]= C + DD[i,] + yyij
}

baseline_bvar=exp(yy[(p+1):nrow(yy),3])   

# Load scenarios s1,...,s7 from Baumeister and Kilian (2014) expressed as percent deviations from baseline
# forecast
data0<- readMat("scenarios.mat")
for (x in names(data0)) {
  assign(paste0(x),data0[[x]])
}
rm(data0)


# Construct conversion factor to 2010.12 dollar oil price
po_L<-data[1:nrow(data),2] # Nominal U.S. RAC for imports
rpo_L<-100*data[,2]/data[,3] # Real U.S. RAC for imports deflated by US CPI
ratio<-po_L[length(po_L)]/rpo_L[length(rpo_L)]   # Normalization factor to 2010.12 dollars

# Plot forecast paths in 2010.12 dollars
time=seq(1973+2/12+p/12,2010+12/12+h/12, by =1/12)

plot(time,ratio*baseline_bvar, type = "l", col = 1,pch = 0, xlim = c(2010.4,2012), ylim = c(0,150),ylab='2010.12 Dollars') +
lines(time,ratio*baseline_bvar*(1+s1/100), type = "b", col =2, pch =1) +
lines(time,ratio*baseline_bvar*(1+s2/100), type = "b", col =3, pch =6) +
lines(time,ratio*baseline_bvar*(1+s3/100), type = "b", col =4, pch =5) +
lines(time,ratio*baseline_bvar*(1+s4/100), type = "b", col =5, pch =4) +
lines(time,ratio*baseline_bvar*(1+s6/100), type = "b", col =6, pch =20) +
lines(time,ratio*baseline_bvar*(1+s7/100), type = "b", col =7, pch =17) +
abline(v=2011, col = "brown") +
legend(2010.4, 150
       , legend= c('Baseline','Iraq full capacity','Libya','Contagion 1','Contagion 2','Global Recovery','Lehman')
       , lty= rep(1, 7)
       , pch = c(NA, 1,6,5,4,20,17)
       , col = 1:7
         , cex=0.8)
