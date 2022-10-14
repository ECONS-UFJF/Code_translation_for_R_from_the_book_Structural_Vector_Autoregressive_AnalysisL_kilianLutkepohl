# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_4.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.4
rm(list= ls())
library(R.matlab)
library(expm)

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
  

# Convert real oil price data back to level scale (normalized to 2012.5)
ydollar=rpoil[(1+p):length(rpoil)] 
ydollar=103.26*(ydollar/ydollar[length(ydollar)])
    
yhatno1=ydollar-mean(ydollar)*(1+yhat1/100)+mean(ydollar);
yhatno2=ydollar-mean(ydollar)*(1+yhat2/100)+mean(ydollar);
yhatno3=ydollar-mean(ydollar)*(1+yhat3/100)+mean(ydollar);
yhatno4=ydollar-mean(ydollar)*(1+yhat4/100)+mean(ydollar);

# Time line for plots (adjust time axis later)
p<-p[[1]]
time=seq((1973+2/12+p/12),(2012+5/12) , by=(1/12))

par(mfrow =c(3,1), mar= c(5,5,2,2))

  plot(time,ydollar,col = 'blue', type = "l", ylim = c(0,140), xlim = c(2003,2008.3), xlab= ""
    , main = 'Real Price of Crude Oil with and without Cumulative Effect of Flow Supply Shock'
    , ylab ='2012.5 Dollars') +
    lines(time,yhatno1, col = "red", lty = 2, lwd = 2)
    legend(2003, 120, legend=c("Actual", "Counterfactual"),
         col=c( "blue","red"), lty=1:2, cex=0.8,  lwd = 1:2)
  plot(time,ydollar,col = 'blue', type = "l", ylim = c(0,140), xlim = c(2003,2008.3), xlab= ""
       , main = 'Real Price of Crude Oil with and without Cumulative Effect of Flow Demand Shock'
       , ylab ='2012.5 Dollars') +
    lines(time,yhatno2, col = "red", lty = 2, lwd = 2)
  plot(time,ydollar,col = 'blue', type = "l", ylim = c(0,140), xlim = c(2003,2008.3), xlab= ""
       , main = 'Real Price of Crude Oil with and without Cumulative Effect of Speculative Demand Shock'
       , ylab ='2012.5 Dollars') +
    lines(time,yhatno3, col = "red", lty = 2, lwd = 2)

  