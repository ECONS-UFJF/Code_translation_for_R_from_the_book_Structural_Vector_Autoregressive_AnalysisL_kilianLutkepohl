# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_9.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.9



rm(list = ls())
source("irfvar.R")
library(pracma)
library(expm)

# Load model estimate from Kilian (2017)
# B0inv   Structural impact multiplier matrix
# A       Companion matrix
# h       Horizon for counterfactual
# K       Number of model variables
# t       Sample size
# p       VAR lag order
# Uhat    Residual matrix
# y1fit   Deterministic seasonal pattern in global oil production growth
kilian <- R.matlab::readMat("kilian.mat")
for (x in names(kilian)) {
  assign(paste0(x),kilian[[x]])
}
rm(kilian)  

##########################################################################
# Structural multipliers 
K<-as.numeric(K)
p<-as.numeric(p)
h<-as.numeric(h)
IRF <- irfvar(A,B0inv,p,K,(431+h) )

# Compute structural shocks Ehat from reduced-form shocks Uhat
What <- solve(B0inv)%*%t(Uhat)
Whatold <- What         # Baseline structural shocks

# Load couterfactuals for world oil production and convert into percent
# growth rates for 2008.12-2015.6
# Let x be the counterfactual percent growth rate of world oil production.
# Adjust for seasonal deterministic pattern.
data0 <- R.matlab::readMat("counterfactual.mat")
for (x in names(data0)) {
  assign(paste0(x),data0[[x]])
}
rm(data0)  


x <- ((counterfactual[432:512]-counterfactual[431:511])/counterfactual[431:511])-y1fit[432:512]


# Construct sequence of oil supply shocks required for implementing the counterfactual 
# growth rate scenario in x

 What=cbind(What[,1:(431-p-1)], rbind(matrix(0,1,h), What[2:K,(432-p-1):(t-p)]));

 xhat<- xhat1<-  xhat2<-  xhat3<- xhat4<-matrix(0,h,1)
                
for (i in (432-p-1):(512-p-1)) {
    xhat1[i-432+p+2,]=`%*%`(IRF[1,1:i],What[1,i:1]);   
    xhat2[i-432+p+2,]=`%*%`(IRF[5,1:i],What[2,i:1]);
    xhat3[i-432+p+2,]=`%*%`(IRF[9,1:i],What[3,i:1]);  
    xhat4[i-432+p+2,]=`%*%`(IRF[13,1:i],What[4,i:1]);
    xhat=xhat1+xhat2+xhat3+xhat4;  # Predicted x when current supply shock is zero
    What[1,i] = (x[i-432+p+2]-xhat2[i-432+p+2,1]-xhat3[i-432+p+2,1]-xhat4[i-432+p+2,1]-xhat1[i-432+p+2,1])/(IRF[1,1]); # Supply shock required to make x match scenario
}

# Plot cumulative effect of oil supply shocks on real price of oil (RAC
# imports)under the counterfactual.
 yhat1old<-yhat1<- yhat2<- yhat3<- yhat4<- matrix(0,t-p,1)
 
for (i in 1:(t-p)) {
          yhat1old[i,]<-`%*%`(IRF[3,1:i],Whatold[1,i:1])
          yhat1[i,]<- `%*%`(IRF[3,1:i] ,What[1,i:1])
          yhat2[i,]<- `%*%`(IRF[7,1:i] ,What[2,i:1])
          yhat3[i,]<- `%*%`(IRF[11,1:i],What[3,i:1])  
          yhat4[i,]<- `%*%`(IRF[15,1:i],What[4,i:1])  
}
yhat <- yhat1+yhat2+yhat3+yhat4;
yhatold <- yhat1old+yhat2+yhat3+yhat4;

# Map counterfactual into nominal Brent price
yhat <- yhat[(431-p-1):length(yhat)]
yhatold <- yhatold[(431-p-1):length(yhatold)]
spread <- yhat-yhatold;

nominalbrent<-unlist(read.table("nominalbrent.txt")) # 2008.11-2015.8
cpiaucsa0811<-read.table("cpiaucsa0811.txt"); 

cpi <- cpiaucsa0811[,3] #2008.11-2015.8

realbrent<-nominalbrent/cpi 
counterfactualrealbrent <-realbrent*(1+spread) 
counterfactualbrent<- counterfactualrealbrent*cpi

time=seq(2008+12/12,2015+8/12, by =1/12)

par( mfrow = c(1,1) , mar=c(5,5,5,5))
plot(time,counterfactualbrent[-1],col = 'red' , type = "l", lty = 2, yla = c('U.S. Dollars'))
lines(time,nominalbrent[-1],col = "blue", lty = 1)
legend(2009, 120, legend = c('Counterfactual','Actual'), col = c("red", "blue"), lty = 2:1)


