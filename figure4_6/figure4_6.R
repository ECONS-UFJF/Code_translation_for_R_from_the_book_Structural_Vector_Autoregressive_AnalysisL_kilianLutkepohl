## translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_6.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.6
#
# The code is adapted from Baumeister and Kilian (2014).
rm(list=ls())
source("kernel.R")

# pred.txt contains the predictions for horizons h=1, 3, 6, 9, 12 and 24 for 
# the VAR(24) model. 20,000 x 6 (forecast horizons h) matrix, 
# i.e. 1st column contains 20000 bootstrap replications for h=1, etc.
pred<-read.table("pred.txt")

# wgt.txt
# 17 x 6 matrix that contains the percent deviations for the 17 scenarios in the order  
# below for the 6 forecast horizons. All definitions of scenarios involve "just this 
# event"; the notation deviates from Table 2 in the paper. Here an event
# means that only that event holds and nothing else (no overlap with other events):
# C2 (contagion 2)
# L+C2
# N2=L+C2+R (nightmare 2)
# C2+R
# N1=L+C1+R (nightmare 1)
# L+C1
# C1 (contagion 1)
# R+C1
# L+R
# Le (Lehman)
# Le+L
# Le+C1
# Le+C1+L
# Le+C2
# Le+C2+L
# R (recovery)
# L (Libya)
wgt<-read.table("wgt.txt")

# Baseline unconditional forecast (red line): 1 x 6, where 6 refers to the
# horizons
base<-read.table("base.txt")

# Construct predictive density at horizon 12 (i=5)
i=5; 
    
    # Baseline
    pred0=pred[,i]*1; 

    # Alternative scenario 1    
    
    pred1=pred[,i]*0.41 +
    ((wgt[1,i]/100)*base[1,i]+pred[,i])*0.1 +
    ((wgt[2,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[3,i]/100)*base[1,i]+pred[,i])*0.03 +
    ((wgt[4,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[5,i]/100)*base[1,i]+pred[,i])*0.03 +
    ((wgt[6,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[7,i]/100)*base[1,i]+pred[,i])*0.1 +
    ((wgt[8,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[9,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[10,i]/100)*base[1,i]+pred[,i])*0.1 + 
    ((wgt[11,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[12,i]/100)*base[1,i]+pred[,i])*0 +
    ((wgt[13,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[14,i]/100)*base[1,i]+pred[,i])*0 + 
    ((wgt[15,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[16,i]/100)*base[1,i]+pred[,i])*0.05 +
    ((wgt[17,i]/100)*base[1,i]+pred[,i])*0.1;
    
    # Alternative scenario 2 
    
    pred2=pred[,i]*0.31 +
    ((wgt[1,i]/100)*base[1,i]+pred[,i])*0.1 +
    ((wgt[2,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[3,i]/100)*base[1,i]+pred[,i])*0.03 +
    ((wgt[4,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[5,i]/100)*base[1,i]+pred[,i])*0.03 +
    ((wgt[6,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[7,i]/100)*base[1,i]+pred[,i])*0.1 +
    ((wgt[8,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[9,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[10,i]/100)*base[1,i]+pred[,i])*0.2 + 
    ((wgt[11,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[12,i]/100)*base[1,i]+pred[,i])*0 +
    ((wgt[13,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[14,i]/100)*base[1,i]+pred[,i])*0 + 
    ((wgt[15,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[16,i]/100)*base[1,i]+pred[,i])*0.05 +
    ((wgt[17,i]/100)*base[1,i]+pred[,i])*0.1;
    
    # Alternative scenario 3    
    
    pred3=pred[,i]*0.16 +
    ((wgt[1,i]/100)*base[1,i]+pred[,i])*0.05 +
    ((wgt[2,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[3,i]/100)*base[1,i]+pred[,i])*0.03 +
    ((wgt[4,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[5,i]/100)*base[1,i]+pred[,i])*0.03 +
    ((wgt[6,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[7,i]/100)*base[1,i]+pred[,i])*0.05 +
    ((wgt[8,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[9,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[10,i]/100)*base[1,i]+pred[,i])*0.05 + 
    ((wgt[11,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[12,i]/100)*base[1,i]+pred[,i])*0 +
    ((wgt[13,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[14,i]/100)*base[1,i]+pred[,i])*0 + 
    ((wgt[15,i]/100)*base[1,i]+pred[,i])*0.01 +
    ((wgt[16,i]/100)*base[1,i]+pred[,i])*0.5 +
    ((wgt[17,i]/100)*base[1,i]+pred[,i])*0.05;


# Construct estimates of the conditional predictive densities
k1<-kernel(pred0)
k2<-kernel(pred1)
k3<-kernel(pred2)
k4<-kernel(pred3)

plot(k1, xlab ='2010.12 Dollars/Barrel', type ="l", col = "red", xlim = c(0,200)) +
  lines(k2, lty = 4) +
  lines(k3, lty = 3) +
  lines(k4, lty = 2) +
  legend(150, 0.020 
   , legend = c('Baseline Scenario','Moderately Pessimistic Scenario','Pessimistic Scenario','Optimistic Scenario')
   , col = c("red", rep(1,3))
   , lty= c(1,4:2)
   , cex = .7
  )
#hold
