# function: cfir.m
# Implementation of counterfactuals exactly as written in Kilian and Lewis
# (2011)
#
# This version only implements two counterfactuals: the BGW one and
# the new counterfacctual described in Kilian and Lewis (2009)
# 
# ***NOTE***
#   For now, requires oil to be in position 2 and FFR to be in position 5
#-------------------------------------------------------------------------------
# Input:    bhat, matrix of estimated reduced form coefficients
#           Phat, structural error term matrix (often chol(sigmauhat)')
#           K, number of variables in the system
#           horiz, number of periods to estimate
#           p, number of lags
# Output:   newirf, a matrix of new counterfactual impulse responses
#           newcfshocks, a vector of shocks to FFR to implement new CF          
#           bgwirf, a matrix of BGW counterfactual impulse responses
#           bgw cfshocks, a vector of shocks to FFR to implement BGW CF
#-------------------------------------------------------------------------------

# Logan Lewis
# September, 2009
# This version: 25-Sep-2009
#-------------------------------------------------------------------------------
cfir<-function(bhat,Phat,K,horiz,p) {
  #-------------------------------------------------------------------------------
  # As in irdecomp, construct the B matrix
  # Drop the intercept term from bhat
  bhat <- bhat[2:nrow(bhat),]
  bhat <- t(bhat)
  
  # First, assemble structural A matrices A1 ... Ap for inclusion into "B"
  B = zeros(K,K*(p + 1));
  
  # Next, determine C, the matrix of contemporaneous coefficients
  for (i in 1:K) {
      A0 = inv(Phat);
      ey = zeros(1,K);
      ey[i] = 1;
      B[i,1:K]= -A0[i,]/A0[i,i] + ey
  }
  
  A0 = eye(K,K) - B[,1:K]
  
  
  #######################################
  
  
  for (i in 1:p) {
      B[,(i*K+1):((i+1)*K)] <- A0%*%bhat[,((i-1)*K+1):(i*K)]
  }
  #-------------------------------------------------------------------------------
  # Counterfactual simulations
  
  # First, calculate the new counterfactual
  x = zeros(5,horiz+1);
  z = zeros(5,horiz+1);
  newcfshocks = zeros(1,horiz+1);
  
  # Time 0
  # 2: oil shock
  x[,1] <- Phat[,2]
  newcfshocks[1,1] <- -B[5,1:K]%*%x[,1]
  # z really changes to only the FFR itself since it is ordered last.
  z[,1] = x[,1] + Phat[,5]*newcfshocks[1,1]/Phat[5,5];
  
  # This could be vectorized, but write it exactly as written in the paper
  for (h in 1:horiz) {
      # Simulate the system
      for (i in 1:K) {
          for (m in 1:min(p,h)) {
              for (j in 1:K) {
                  x[i,h+1] <- x[i,h+1] + B[i,m*K+j]*z[j,h-m+1]
              }
          }
          if (i > 1) {
              for (j in 1:(i-1)) {
                  x[i,h+1] <- x[i,h+1] + B[i,j]*x[j,h+1]
              }
          }
      }
    
      # Counterfactual
      for (m in 1:min(p,h)) {
          newcfshocks[1,h+1] <- newcfshocks[1,h+1] - B[5,m*K+2]*z[2,h-m+1]
      }
      # Contemporaneous response
      newcfshocks[1,h+1] <- newcfshocks[1,h+1] - B[5,2]*x[2,h+1];
      
      # Re-calculate z.  here, Phat[,5)/Phat(5,5) = 1 in the FFR position, 0
      # everywhere, so it could be omitted.
      z[,h+1] <- x[,h+1] + Phat[,5]*newcfshocks[1,h+1]/Phat[5,5]
  }
  newirf = z;
  
  
  # Calculate the BGW counterfactual
  x = zeros(5,horiz+1);
  z = zeros(5,horiz+1);
  bgwcfshocks = zeros(1,horiz+1);
  
  # Time 0
  # 2: oil shock
  x[,1] = Phat[,2]
  bgwcfshocks[1,1] = -B[5,1:K]%*%x[,1];
  # z really changes to only the FFR itself since it is ordered last.
  z[,1] = x[,1] + Phat[,5]*bgwcfshocks[1,1]/Phat[5,5];
  
  # This could be vectorized, but write it exactly as written in the paper
  for (h in 1:horiz) {
      # Simulate the system
      for (i in 1:K) {
          for (m in 1:min(p,h)) {
              for (j in 1:K) {
                  x[i,h+1] <- x[i,h+1] + B[i,m*K+j]*z[j,h-m+1]
              }
          }
          if (i > 1) {
              for (j in 1:(i-1)) {
                  x[i,h+1] <- x[i,h+1] + B[i,j]*x[j,h+1];
              }
          }
      }
      # Counterfactual
      for (m in 1:min(p,h)) {
          for (j in 1:K) {
              bgwcfshocks[1,h+1] <- bgwcfshocks[1,h+1] - B[5,m*K+j]*z[j,h-m+1]
          }
      }
      for (j in 1:4) {
          bgwcfshocks[1,h+1] <- bgwcfshocks[1,h+1] - B[5,j]*x[j,h+1]
      }
      # Re-calculate z
      z[,h+1] = x[,h+1] + Phat[,5]*bgwcfshocks[1,h+1]/Phat[5,5];
  }
  bgwirf = z;

return(list(newirf=newirf, newcfshocks=newcfshocks, bgwirf=bgwirf, bgwcfshocks=bgwcfshocks))
}

