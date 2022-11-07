# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
## translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_10.m
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.10
#
# The code has been adapted from Kilian and Lewis (2011).

rm(list= ls())
library(R.matlab)
library(expm)

###########################################################################
# The intermediate results needed to generate the figure are created by 
# running runsystem.m and have been saved in the *.mat file below:
#file1 = readMat('monthlysystem-1967.05-1987.07.mat');

#oimpulses = file1$oimpulses;
#bgwcfimpulses = file1$bgwcfimpulses;
#oilcfimpulses = file1$oilcfimpulses;

#oilpos = as.numeric(file1$oilpos)
#h = as.numeric(file1$h)
#diffspecbool = as.numeric(file1$diffspecbool)

load('monthlysystem-1967.05-1987.07.RData')

###########################################################################
# Scale impulse responses for plotting
osimpulses1 = oimpulses/oimpulses[1,oilpos,1]*10;
bgwcfsimpulses1 = bgwcfimpulses/bgwcfimpulses[1,oilpos,1]*10;
oilcfsimpulses1 = oilcfimpulses/oilcfimpulses[1,oilpos,1]*10;

# Need to adjust scaling to take into account that CFNAI is not a rate, but a
# level
osimpulses1[,3,] = osimpulses1[,3,]/100;
bgwcfsimpulses1[,3,] = bgwcfsimpulses1[,3,]/100;
oilcfsimpulses1[,3,] = oilcfsimpulses1[,3,]/100;

###########################################################################
# Plot the BGW and KL counterfactuals
{
titles <- c('Real Price of Commodities','Real Price of Oil','CFNAI','CPI Inflation','Federal Funds Rate')
ylabels <- c('Percent','Percent','Index','Percent','Percent')
bounds <- matrix(c(-2,2,0,25,-0.6,0.3,-0.3,0.3,-1,1),5,2,1)

ival <- 1:(h+1)

par( mfrow = c(3,1) , mar = c(2,2,2,2))
 i = 3
     plot(    ival-1,osimpulses1[ival,i,1]    ,col = "black", lty = 1, 
              type = "l", xlim = c(0,h), ylim = c(-.8,.4), main = titles[i], ylab =ylabels[i] ) +
     lines(   ival-1,bgwcfsimpulses1[ival,i,1],col = "red",   lty = 2) +
     lines(   ival-1,oilcfsimpulses1[ival,i,1],col = "blue",  lty = 2, lwd  =2) +
     abline(h = 0) 
 i = 4
     plot(    ival-1,osimpulses1[ival,i,1] ,col = "black", lty = 1,  
              type = "l", xlim = c(0,h), ylim = bounds[i,], main = titles[i], ylab =ylabels[i] ) +
     lines(   ival-1,bgwcfsimpulses1[ival,i,1],col = "red",   lty = 2) +
     lines(   ival-1,oilcfsimpulses1[ival,i,1],col = "blue",  lty = 2, lwd  =2) +
     abline(h = 0) 

 i = 5
    plot(   ival-1,osimpulses1[ival,i,1],col = "black", lty = 1,  
            type = "l", xlim = c(0,h), ylim = bounds[i,], main = titles[i], ylab =ylabels[i] ) +
    lines(   ival-1,bgwcfsimpulses1[ival,i,1],col = "red",   lty = 2) +
    lines(   ival-1,oilcfsimpulses1[ival,i,1],col = "blue",  lty = 2, lwd  =2) 



legend(20,1 
       , legend = c('Unrestricted','BGW Counterfactual','KL Counterfactual')
       , lty = c(1,2,2) 
       , col = c(1,2,4)
       , lwd = c(1,1,2)
       , bty = c("o")
       , cex = .9)

}

