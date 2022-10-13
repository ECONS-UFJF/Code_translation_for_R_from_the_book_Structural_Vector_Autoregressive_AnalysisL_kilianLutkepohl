# TABLE4_1.M
#
# tradução André Suriane / ECONS
# 
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates row h of Table 4.1

rm(list=ls())

library(expm) # matrix algebra

# Data from Kilian and Park (2009) for 1973.2-2006.12:
# data [dprod rea rpoil dd] where
# drpod  Global oil production growth 
# rea  Kilian (2009) business cycle index of global real economic activity
# rpoil  Real price of oil in percent deviations from mean
# dd  CRSP real dividend growth in percent
  
source("olsvarc.R")
y <- read.table("data.txt")

# Estimate reduced-form model

dime<-dim(y); t= dime[1];K=dime[2];p = 24
olsVres<-olsvarc(y,p); #[A,SIGMA,Uhat,V,X]
A<-olsVres$A
SIGMA<-olsVres$SIGMA[1:K,1:K];

# Structural forecast error variance Decomposition at horizon h, 
# where h=600 ~ h=infinity

tb4_1<-data.frame()
for (h in c(1,2,3,12,600)) {
# h =1
J<- cbind(diag(K),matrix(0 ,K,K*(p-1)) )
TH1<-J%*%(A%^%0)%*%t(J); # A^0 é uma matriz identidade do tammanho de A
TH<-TH1%*%t(chol(SIGMA)); 
TH<-t(TH);
TH2<-(TH*TH); 
TH3<-TH2;

if (h>=2) {
for (i in 2:h) {
TH<-(J%*%(A%^%(i-1))%*%t(J))%*%t(chol(SIGMA)); 
TH<-t(TH);
TH2<-(TH*TH); 
TH3<-TH3+TH2;
}
}


TH4=colSums(TH3);

VC=matrix(rep(0, K*K) ,K,K)

for (j in 1:K) {
VC[j,]=TH3[j,]/TH4;
}

# VDC in percentage terms at horizon h
# Columns refer to shocks j=1,...,q that explain any given variable
# Rows refer to variables whose variation is to be explained
# Here we care about the last row (the stock return variable) and all
# columns (i.e., the contribution of each shock)

#print(t(VC)*100)

# Forecast error variance decomposition at horizon h for real dividen
# growth
#print('Row h of Table 4.1')
#print(tail(t(VC),1)*100)
tb4_1<-rbind(tb4_1,cbind(h,tail(t(VC),1)*100))

}

round(tb4_1,5)
