# TABLE4_2.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Table 4.2
#
# The code is adapted from Baumeister and Kilian (2014).

rm(list=ls())

# pred.txt contains the predictions for horizons h=1, 3, 6, 9, 12 and 24 for 
# the VAR(24) model. 20,000 x 6 (forecast horizons h) matrix, 
# i.e. 1st column contains 20000 bootstrap replications for h=1, etc.
#pred<-read.table("pred.txt")

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

# Construct predictive density
# Select scenario:
# s=1; # Baseline scenario
# s=2; # Moderately pressimistic scenario
# s=3; # Pessimistic scenario
# s=4; # Optimistic scenario
tabela<-data.frame()
for (s in 1:4) {
#s=4;
# Select horizon: Set i = 2, 3, 5 for h = 3, 6, 12
for (i in c(2,3,5)) {
#i=3; 
  pred<-read.table("pred.txt")
if (s==1) {

    # Baseline
    pred=pred[,i]; 

} else if (s==2){
    
    # Alternative scenario 1: Moderately pessimistic    

    pred=pred[,i]*0.41 +     
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
    ((wgt[17,i]/100)*base[1,i]+pred[,i])*0.1

} else if (s==3){
    
    # Alternative scenario 2: Pessimistic

    pred=pred[,i]*0.31 +     
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
    ((wgt[17,i]/100)*base[1,i]+pred[,i])*0.1

} else if (s==4){
    
    # Alternative scenario 3: Optimistic   

    pred=pred[,i]*0.16 +     
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
    ((wgt[17,i]/100)*base[1,i]+pred[,i])*0.05
}

# Construct risks as in Kilian and Manganelli (2007, 2008) and Alquist 
# et al. (Hdbk chapter, 2013):

# Probability of exceeding $100:
probabove100=mean(pred>100); 
  
# Tail conditional expectation with respect to threshold of #100:
expabove100=sum((pred>100)*(pred-100))/sum(pred>100); 
 
# Weighted expected excess:
# mean(pred>100)*expabove100
   
# Risk estimates as reported in Table 4.2:
secs<-cbind(s,i,mean(pred>100), expabove100, mean(pred>100)*expabove100)
tabela<-rbind(tabela,secs)

}
}
names(tabela) <- c("Scenario", "h", "P(R_{t+h}>100)" , "E(R_{t+h}-100|R_{t+h}>100)", "E(R_{t+h}-100|R_{t+h}>100) x P(R_{t+h}>100)")
tabela$Scenario<-factor(tabela$Scenario,labels = c('Baseline','Moderately Pessimistic','Pessimistic','Optimistic') )
tabela$h<-factor(tabela$h,labels = c("3","6","12"))
