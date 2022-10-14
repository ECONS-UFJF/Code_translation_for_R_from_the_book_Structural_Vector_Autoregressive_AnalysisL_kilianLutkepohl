# FIGURE4_3.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.3

rm(list= ls())
library(R.matlab)
require("expm")
source("irfvar.R")
source("deseasonal.R")
# Load Kilian and Lee (2014) data and estimates:
# y       Data matrix
# B0inv   Structural multiplier matrix
# A       Companion matrix
# p       Lag order
# Uhat    Residual matrix
data0 <- readMat("kilianlee5000000.mat")
for (x in names(data0)) {
  assign(paste0(x),data0[[x]])
}
rm(data0)

  
# Compute structural multipliers for a horizon of t-p
t <- nrow(y); K<-ncol(y)
IRF=irfvar(A,B0inv,p,K,t-p-1);

# Compute structural shocks What from reduced-form shocks Uhat 
What=tcrossprod(solve(B0inv), Uhat)

# Cross-multiply the weights for the effect of a given shock on the real
# oil price (given by the relevant row of IRF) with the structural shock
# in question
  yhat1<-
  yhat2<-
  yhat3<-
  yhat4<-matrix(0,t-p,1)

for( i in 1:(t-p)) {
  yhat1[i,]<- `%*%`(IRF[3,1:i] ,What[1,i:1])
  yhat2[i,]<- `%*%`(IRF[7,1:i] ,What[2,i:1])
  yhat3[i,]<- `%*%`(IRF[11,1:i],What[3,i:1])  
  yhat4[i,]<- `%*%`(IRF[15,1:i],What[4,i:1])  
}
  
# Real price of oil expressed in exact percent deviations from mean
rpoil=exp(y[,3]); # Real price in levels 
rpoil=103.26*rpoil/rpoil[length(rpoil)]; 
rpoilmean=mean(rpoil);
rpoilexa=((rpoil-mean(rpoil))/mean(rpoil))*100; 

# Focus on 2003.1-2008.6
ytrue=deseasonal(rpoilexa); 
ytrue=ytrue[(336+p):nrow(ytrue),1];
yhat1=yhat1[336:nrow(yhat1),1];
yhat2=yhat2[336:nrow(yhat2),1];
yhat3=yhat3[336:nrow(yhat3),1];
yhat4=yhat4[336:nrow(yhat4),1];
           
# Bar chart cumulative real oil price change: 2003.1-2008.6
Dtrue=rpoilmean*(1+ytrue[length(ytrue)-47]/100)-rpoilmean*(1+ytrue[1]/100)
D1=rpoilmean*(1+yhat1[length(yhat1)-47]/100)-rpoilmean*(1+yhat1[1]/100)
D2=rpoilmean*(1+yhat2[length(yhat2)-47]/100)-rpoilmean*(1+yhat2[1]/100)
D3=rpoilmean*(1+yhat3[length(yhat3)-47]/100)-rpoilmean*(1+yhat3[1]/100)
D4=rpoilmean*(1+yhat4[length(yhat4)-47]/100)-rpoilmean*(1+yhat4[1]/100)
    BarPlotn<-c(D1, D2, D3, D4, Dtrue)    

barplot(BarPlotn, ylab = '2012.5 Dollars', col = viridis::viridis(5), legend.text = c("D1", "D2", "D3", "D4", "Dtrue"), )

        