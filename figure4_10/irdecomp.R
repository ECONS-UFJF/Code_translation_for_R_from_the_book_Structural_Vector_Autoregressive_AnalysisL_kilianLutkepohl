# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
## function: irdecomp.m
# Implementation of irdecomp.m, with exactly the same formulation as
# that in Kilian and Lewis (2009)
# 
# *** NOTE ***
# This version only implements the decomposition described in the paper: the
# decomposed effects of a FFR IRF (variable 5) to an oil price shock (variable
# 2).
#
# Input:
#           bhat, K*p+1xK matrix of estimated betahat coef. from an AR(p) 
#           h, the number of periods to estimate
#           K, the number of variables in the system
#           p, the lag order of the system
#           Phat, the orthogonalized variance/covariance matrix
#
# Output:
#           oimpulses, the standard orthogonalized impulse responses
#           d, the decomposed impulse response of the dvarth variable

# Logan Lewis
# September, 2009
# This version: 25-Sept-2009
#-------------------------------------------------------------------------------
irdecomp<-function(bhat,horiz,K,p,Phat) {
#-------------------------------------------------------------------------------
# First, calculate the standard impulse responses as usual
  A <- t(bhat)
  A <- A[,(2:(K*p+1))]
  A <- rbind(A, cbind(diag(K*(p-1)), matrix(0,K*(p-1),K)))
  
  J <- cbind(diag(K), matrix(0,K,K*(p-1)))
  # Could reduce the number of loops here with clever matrices
  oimpulses <- matrix(0,horiz+1,K^2)
  
  for (i in 1:K) {
      for (j in 1:K) {
          for (m in 0:horiz) {
                mat <- (J%*%(A%^%m)%*%t(J))%*%Phat
                oimpulses[m+1,(i-1)*K + j] <- mat[j,i]
          }
      }
  }
  
  # We only want the impulse responses for oil price shock
  oimpulses <- oimpulses[,(1*K+1):(2*K)]
  #-------------------------------------------------------------------------------
  # Drop the intercept term from bhat
  bhat <- bhat[2:nrow(bhat),]
  bhat <- t(bhat)
  
  oimpulses = t(oimpulses)
  
  # First, assemble structural A matrices A1 ... Ap for inclusion into "B"
  
  B = matrix(0,K,K*(p + 1))
  
  # Next, determine C, the matrix of contemporaneous coefficients
  for (i in 1:K) {
      A0 <- solve(Phat)
      ey <- matrix(0,1,K)
      ey[i] <- 1;
      B[i,1:K] <- -A0[i,]/A0[i,i] + ey;
  }
  
  A0 <- diag(K) - B[,1:K]
  for (i in 1:p) {
      B[,(i*K+1):((i+1)*K)] <- A0%*%bhat[,((i-1)*K+1):(i*K)]
  }
  
  # Finally, calculate the decomposed impulse response of the FFR to an oil price
  # shock for each i = 1:K and horizon k = 1:h+1
  
  d = zeros(5,horiz+1);
  
  for (i in 1:K) {
      # Careful with indices here, since Matlab matrices index at 1.
      for (h in 0:horiz) {
          for (m in 0:min(p,h)) {
              d[i,h+1] =  d[i,h+1] + B[5,m*K+i]*oimpulses[i,h-m+1]
          }
      }
  }
  
  d <- t(d)
  
  return(list(oimpulses=oimpulses, d=d))
}
