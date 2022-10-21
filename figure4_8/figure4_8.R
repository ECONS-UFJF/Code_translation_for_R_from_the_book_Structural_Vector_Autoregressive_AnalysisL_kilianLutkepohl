## translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_8.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.8
rm(list= ls())
library(R.matlab)
library(expm)

source("irfvar.R")

# Load model estimate from Kilian (2017)
# B0inv   Structural impact multiplier matrix
# A       Companion matrix
# h       Horizon for counterfactual
# K       Number of model variables
# t       Sample size
# p       VAR lag order
# Uhat    Residual matrix
# y1fit   Deterministic seasonal pattern in global oil production growth
 
data0 <- readMat("kilian.mat")
for (x in names(data0)) {
  assign(paste0(x),data0[[x]])
}
rm(data0)
for (x in c("h","K","p")) {
  assign(paste0(x),as.numeric(get(x)))
}



##########################################################################
# Structural multipliers 
IRF<-irfvar(A,B0inv,p,K,431+h);   

# Compute structural shocks Ehat from reduced-form shocks Uhat
What <- solve(B0inv)%*%t(Uhat);
Whatold=What;         # Baseline structural shocks

# Load couterfactuals for world oil production and convert into percent
# growth rates for 2008.12-2015.6
# Let x be the counterfactual percent growth rate of world oil production.
# Adjust for seasonal deterministic pattern.
data0 <- readMat("counterfactual.mat")
for (x in names(data0)) {
  assign(paste0(x),data0[[x]])
}
rm(data0)

x<-((counterfactual[432:512]-counterfactual[431:511])/counterfactual[431:511])-y1fit[432:512];

# Construct sequence of oil supply shocks required for implementing the counterfactual 
# growth rate scenario in x

      
What<- cbind(What[,1:(431-p-1)],rbind( matrix(0,1,h), What[2:K,(432-p-1):(t-p)]))


xhat<- xhat1<- xhat2<- xhat3<- xhat4<- matrix(0,h,1) 
for (i in (432-p-1):(512-p-1)) {
    xhat1[i-432+p+2,]<-`%*%`(IRF[1,1:i], What[1,i:1])   
    xhat2[i-432+p+2,]<-`%*%`(IRF[5,1:i], What[2,i:1])
    xhat3[i-432+p+2,]<-`%*%`(IRF[9,1:i], What[3,i:1])  
    xhat4[i-432+p+2,]<-`%*%`(IRF[13,1:i],What[4,i:1])
    xhat<-xhat1+xhat2+xhat3+xhat4;  # Predicted x when current supply shock is zero
    What[1,i]<-(x[i-432+p+2]-xhat2[i-432+p+2,1]-xhat3[i-432+p+2,1]-xhat4[i-432+p+2,1]-xhat1[i-432+p+2,1])/(IRF[1,1]); # Supply shock required to make x match scenario
}

# Plot sequence of flow supply shocks under counterfactual and check for
# potential Lucas critique problems such as shocks that are stronly serially
# correlated after Novemeber 2008 or large by the historical standard 
time<-seq((1973+2/12+p/12),(2015+8/12), by = 1/12) 
plot(time,-What[1,], type= "l", col = "blue", xlim = c((1979+6/12) ,(2015)))
abline(v = 2008+11/12, col="darkblue")
