# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
## function: monetarysystem.m
# Load monthly dataset and run VAR
#
# Input:
#   fileloc, the location of the data (monthlydata.mat)
#   saveloc, what to store the data as
#   cfnaibool, whether to use CFNAI or the BGW GDP measure.  
#       if false, uses dataconv.txt
#   startdate, yyyy.mm starting month
#   enddate, yyyy.mm
# Output:
#   file saveloc, with estimated system and parameters
# Logan Lewis
# May, 2009
# This version: 25-Sept-2009
#-------------------------------------------------------------------------------
#monetarysystem<-function(fileloc,saveloc,cfnaibool,startdate,enddate) {
#-------------------------------------------------------------------------------
# Constants
require("R.matlab") ; require("pracma");  require("expm"); require("Matrix")
source("datasplit.R"); source("irfvar.R"); source("multivar.R"); 
source("devec.R"); source("irdecomp.R"); source("cfir.R");  source("histdecomp.R")
p           = 12;           # Lag order
h           = 24;           # Number of periods for impulse responses
m           = 5000;        # Number of bootstrap replications for bootci.m
#fileloc= 'monthlydata.mat'; saveloc = 'monthlysystem-1967.05-1987.07.RData'; cfnaibool <- TRUE; startdate= 1967.05; enddate = 1987.07;
# Data options
#cfnaibool       <- TRUE;     # Use CFNAI (vs BGW GDP measure)
diffspecbool    <- TRUE;     # Specify oil and commodity prices as differences

#-------------------------------------------------------------------------------
# Load the data
dataimport <- readMat(fileloc)
data <- data.frame(dataimport$data)
dataimport$colheaders<-unlist(dataimport$colheaders)
names(data)<-dataimport$colheaders
rm(dataimport)

# data has the following columns:
# 'Date'    'COM'    'OIL'    'CFNAI'    'INFLATION'    'FEDFUNDS'    'CPI'
# 'INDPRO'
 datelist <- data[,1]
 
  startpos <- which(abs(datelist - startdate) == min(abs(datelist - startdate)), arr.ind=TRUE)
   endpos <-  which(abs(datelist - enddate) == min(abs(datelist - enddate)), arr.ind=TRUE) 

ival = startpos:endpos

# Real commodity prices (deflated here by CPI)
com <- log(data[ival,2]) - log(data[ival,7])

# Linearly detrend commodity prices
com <- detrend(com, 'linear');

# Real oil prices (deflated by CPI)
oil <- log(data[ival,3]) - log(data[ival,7])

# Real activity index
if (cfnaibool){   
  cfnai <- data[ival,4]
}
# Inflation
# inflation defined as precise percent change
#inflation = data[ival,5);
# inflation as log differences
inflation <- log(data[ival,7]) - log(data[ival-1,7])

# Federal Funds Rate
FFR <- data[ival,6]

# Industrial production (specified in first differences)
indpro <- log(data[ival,8]) - log(data[ival-1,8])

# Calculate differenced commodity prices and oil prices
comdiff <- log(data[ival,2]) - log(data[ival,7]) - (log(data[ival-1,2]) - log(data[(ival-1),7]))
oildiff <- log(data[ival,3]) - log(data[ival,7]) - (log(data[ival-1,3]) - log(data[ival-1,7]))
#-------------------------------------------------------------------------------
# Assemble the data
if (cfnaibool){
    if (diffspecbool) {
        vardata = data.frame(comdiff, oildiff, cfnai, inflation, FFR/100)
    } else {
        vardata = data.frame(com, oil, cfnai, inflation, FFR/100)
    }
} else {
  bgwdata<-read.table("bgwdata.txt")
    T2 <- nrow(bgwdata)
    K2 <- ncol(bgwdata)
  
    if (startdate == 1967.05) {
        startpos2 = 29;
    } else {
        print('Error: Using BGW GDP data on starting date other than 1967.05');
    }
    if (enddate == 1995.12) {
        endpos2      = T2;
    } else { # Allows for smaller subsamples (e.g. 1987.07)
        endpos2 = startpos2 + length(FFR) - 1;
    }
    bgwgdp <- bgwdata[startpos2:endpos2,1] # This corresponds to dates 
    
    if (diffspecbool) {
        vardata <-data.frame(comdiff, oildiff, bgwgdp, inflation, FFR/100)
    } else {
        vardata = data.frame(com, oil, bgwgdp, inflation, FFR/100)
    }
}
compos = 1; oilpos = 2;         # Position of oil variable
outpos = 3; infpos = 4; FFRpos = 5;         # Position of FFR variable
#------------------------------------------------------------------------------
# Estimate the system
ds <-  datasplit(vardata,p)
X <- ds$X; Y <- ds$Y; T <- nrow(Y); K <- ncol(Y)

dates <- matrix(0,T,1)

# Startdate is in year.mo format, convert it to fraction of year
mo = startdate - floor(startdate);
startdate2 = floor(startdate) + (mo*100 - 1)*(1/12)

for (i in 1:T) {
    dates[i,1] <- startdate2 + (1/12)*(p+i-1)
}

datesfull <- matrix(0, T+p,1)
for (i in 1:(T+p)) {
    datesfull[i,1] <- startdate2 + 1/12*(i-1);
}

resmultvar <- multivar(Y,X,p) # [bhat, bhatstd, ehat, sigmauhat, sigmabhat] 
for (x in names(resmultvar)) {
  assign(paste0(x),resmultvar[[x]])
}
rm(resmultvar)

yhat <- X%*%bhat
# Construct the A matrix for use with IRFs
A <- t(bhat)
A <- A[,2:(K*p+1)];
A <- rbind(A, cbind(diag(K*(p-1)), matrix(0,K*(p-1),K)))

#------------------------------------------------------------------------------
# Structure of IRF matrices:
# [,:,1) are the point estimates
moimpulses <- array(0,c(T+1,K+1,5))    # Orthogonalized impulse responses to FFR shock

oimpulses     <-       # Orthogonalized impulse responses to oil shock
bgwcfimpulses <-       # BGW counterfactual impulse responses
oilcfimpulses <-       # Ignore only oil impulse responses
infcfimpulses <- array(0,c(T+1,K,5))   # Respond only to inflation (and lags of FFR) impulse responses 
FFRdimpulses  <-      # Decomposition of FFR impulse response
# Structure of counterfactual comparisons
bgwdiff       <-         # Difference between BGW CF and unrestricted
oildiff       <-          # Difference between oil CF and unrestricted
bgwoildiff    <-  array(0,c(h+1,K,5))      # Difference between BGW CF and oil CF

#-------------------------------------------------------------------------------
# Calculate orthognalized impulse responses
irffull = irfvar(A,sigmauhat,p,T,K);
oimpulses[,,1] <- t(irffull[((oilpos - 1)*5 + 1):(oilpos*5),])
moimpulses[,1:K,1] <- t(irffull[((FFRpos - 1)*5 + 1):(FFRpos*5),])
irffull <- t(irffull)
#-------------------------------------------------------------------------------
# Calculate shock series
 Phat   <- t(chol(sigmauhat))
 shocks <- tcrossprod(solve(Phat),ehat)
 shocks <- t(shocks)

 # Calculate IRF decomposition and counterfactual point estimates
 # These functions as written are not general
if (oilpos == 2 && FFRpos == 5 && K == 5) {
    irDres <- irdecomp(bhat,h,K,p,Phat); tmp<-irDres$oimpulses;idc<-irDres$d  
    FFRdimpulses[,,1] <- idc
    rescfir <- cfir(bhat,Phat,K,T,p) # [oilirf oilcfshocks bgwirf bgwcfshocks]
     oilirf<-rescfir[[1]] ; oilcfshocks<-rescfir[[2]]; bgwirf<- rescfir[[3]]; bgwcfshocks<- rescfir[[4]]
    bgwcfimpulses[,,1] <- t(bgwirf)
    oilcfimpulses[,,1] <- t(oilirf)
}

ohd   <- histdecomp(shocks[,oilpos],    oimpulses[,,1]);
bgwhd <- histdecomp(shocks[,oilpos],bgwcfimpulses[,,1]);
oilhd <- histdecomp(shocks[,oilpos],oilcfimpulses[,,1]);

#-------------------------------------------------------------------------------
# Cumulate the commodity price and oil price impulses now
if (diffspecbool) {
    oimpulses[,compos,1]     <- cumsum(oimpulses[,compos,1]);
    oimpulses[,oilpos,1]     <- cumsum(oimpulses[,oilpos,1]);
    moimpulses[,compos,1]    <- cumsum(moimpulses[,compos,1]);
    moimpulses[,oilpos,1]    <- cumsum(moimpulses[,oilpos,1]);
    bgwcfimpulses[,compos,1] <- cumsum(bgwcfimpulses[,compos,1]);
    bgwcfimpulses[,oilpos,1] <- cumsum(bgwcfimpulses[,oilpos,1]);
    oilcfimpulses[,compos,1] <- cumsum(oilcfimpulses[,compos,1]);
    oilcfimpulses[,oilpos,1] <- cumsum(oilcfimpulses[,oilpos,1]);
}

# Store cumulated inflation for monetary shock
moimpulses[,6,1] <- cumsum(moimpulses[,infpos,1])
#-------------------------------------------------------------------------------
# Assemble a large matrix of historical decompositions for how well they match
# up to yhat
hdfull = zeros(T,K*K);
for (i in 1:K) {
    hdfull[,((i-1)*K+1):(i*K)] <- histdecomp(shocks[,i],irffull[,((i-1)*K+1):(i*K)])
}
#-------------------------------------------------------------------------------
# Clean up a few un-needed variables

# Save the data
save.image(saveloc)

#return(0)
#}
