# FIGURE4_2.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.2
#
# translate to R by André Suriane ECONS ECONOMIA UFJF 

rm(list = ls())
setwd("/mnt/Disco1/Jupyter/R/Atendimentos/Wilson TS/figure4_2")
source("irfvar.R")


# Load Kilian and Lee (2014) data and estimates:
# y       Data matrix
# B0inv   Structural multiplier matrix
# A       Companion matrix
# p       Lag order
# Uhat    Residual matrix
kilian <- R.matlab::readMat("kilianlee5000000.mat")
for (x in names(kilian)) {
  assign(paste0(x),kilian[[x]])
}
rm(kilian)  
# Compute structural multipliers for a horizon of t-p
dime<-dim(y); t= dime[1];K=dime[2]; rm(dime)
H = t-p-1
IRF=irfvar(A,B0inv,p,K,H);

# Compute structural shocks What from reduced form shocks Uhat
What=solve(B0inv)%*%t(Uhat)

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

###########################################################################
# Plot actual demeaned real price of oil and its historical decomposition
library(pracma)
p = 24
time<-seq((1973+2/12+p[[1]]/12),(2012+5/12), by= 1/12) 


par(mfrow=c(4,1), mar=c(2,5,1,0))
yhat0<- detrend(y[25:nrow(y),3],'constant')*100
plot(time, yhat0, type= "l", ylab = "percent" , xlim = c(1978,2012) , ylim = c(-100,100)
     , main  = "Demeaned Real Price of Crude Oil")
plot(time, yhat1, type= "l", ylab = "percent" , xlim = c(1978,2012) , ylim = c(-100,100)
     , main = "Cumulative Effect of Flow Supply Shock on Real Price of Crude Oil")
plot(time, yhat2, type= "l", ylab = "percent" , xlim = c(1978,2012)  , ylim = c(-100,100)
     , main = "Cumulative Effect of Flow Demand Shock on Real Price of Crude Oil")
plot(time, yhat3, type= "l", ylab = "percent" , xlim = c(1978,2012)  , ylim = c(-100,100)
     , main = "Cumulative Effect of Speculative Demand Shock on Real Price of Crude Oil")









