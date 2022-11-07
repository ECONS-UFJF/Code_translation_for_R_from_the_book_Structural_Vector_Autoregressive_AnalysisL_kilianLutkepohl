# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_11.m
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.11
#
# The code has been adapted from Kilian and Lewis (2011).
rm(list= ls())
library(expm)
library(R.matlab)

###########################################################################
# The intermediate results needed to generate the figure are created by 
# running runsystem.m and have been saved in the *.mat file below:

#file1 = readMat('monthlysystem-1967.05-1987.07.mat');

#FFRpos  = file1$FFRpos;
#oilpos  = file1$oilpos;
#h       = file1$h;

#oilcfshocks = file1$oilcfshocks;
#oilcfimpulses = file1$oilcfimpulses;

load('monthlysystem-1967.05-1987.07.RData')
##########################################################
oilcfsshocks = oilcfshocks/oilcfimpulses[1,oilpos,1]*10;


ival = 1:(h+1)*2;   # Plot a longer series to see long-term behavior

plot(ival-1, oilcfsshocks[ival]*100, type = "l", ylab = 'Basis Points',xlab = 'Months')
abline(h = 0)

