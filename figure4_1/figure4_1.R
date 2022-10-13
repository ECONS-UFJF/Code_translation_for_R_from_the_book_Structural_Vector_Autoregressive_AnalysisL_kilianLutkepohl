# FIGURE4_1.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.1
# translate to R by André Suriane ECONS ECONOMIA UFJF 


require("expm")
rm(list=ls())

source("olsvarc.R")
source("irfvar.R")

# Data from Kilian and Park (2009) for 1973.2-2006.12
# data=[dprod rea rpoil dd] where
# drpod: Global oil production growth 
# rea: Kilian (2009) business cycle index of global real economic activity
# rpoil: Real price of oil in percent deviations from mean
# dd: CRSP real dividend growth in percent

y<-read.table("data.txt")
t<-nrow(y)
q<-ncol(y)

# Point estimate
h=15;           # Maximum impulse response horizon
p=24;           # Lag order
olsRes<-olsvarc(y,p);					 #[A,SIGMA,Uhat,V,X]
A<-olsRes$A
SIGMA<-olsRes$SIGMA[1:q,1:q]

# VAR impulse response analysis (all structural shocks are normalized to 
# generate an increase in the real price of oil)
horizon<-0:h;
IRF=irfvar(A,SIGMA,p,h);
IRF[1,]=cumsum(IRF[1,]);
IRF[5,]=cumsum(IRF[5,]);
IRF[9,]=cumsum(IRF[9,]);
IRF[13,]=cumsum(IRF[13,]);

IRF[4,]=cumsum(IRF[4,]);
IRF[8,]=cumsum(IRF[8,]);
IRF[12,]=cumsum(IRF[12,]);
IRF[16,]=cumsum(IRF[16,]);

par(mfrow=c(3,1), mar=c(2,5,1,0))

plot(horizon,-IRF[4,], type = "l", main = 'Oil supply shock', ylab =  'Percent', xlab = 'Months', col = "red", ylim = c(-2,2)) +
  abline(h = 0, col = "blue", lwd=2)


plot(horizon,IRF[8,], type = "l" , main = 'Aggregate demand shock' , ylab = 'Percent'  , xlab = 'Months', col = "red", ylim = c(-2,2))+
  abline(h = 0, col = "blue", lwd=2)


plot(horizon,IRF[12,], type = "l"  , main = 'Oil-specific demand shock' , ylab = 'Percent' , xlab = 'Months', col = "red", ylim = c(-2,2))+
  abline(h = 0, col = "blue", lwd=2)



