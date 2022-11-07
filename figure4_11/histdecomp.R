# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
## function: histdecomp.m
# Decomposes the effects of a desired shock in both standard and counterfactual
# environments (given by the full-period impulse response functions)
#
# Logan Lewis
# February, 2008
# This version: 27-Oct-2008
#
# Input:
#   shocks, a vector of structural shocks 
#   impulses, a TxK matrix of impulse responses corresponding to shocks
#
# Output:
#   hd, a TxK matrix of the historical decomposition of the effect of shockvar
histdecomp<-function(shocks,impulses) {

#[T K] = size(shocks);
 T <- length(shocks)
 K <- size(impulses)[[2]]
 hd = zeros(T,K);

# Check to make sure that impulses is not too big (indicating that wrong input
# was passed)
#[a b] = size(impulses);
#if (b > K)
#    disp('Warning: too many impulse response functions passed, results may be incorrect.');
#end

  for (i in 1:T) {
      for (j in 1:K) {
          hd[i,j] <- dot(impulses[1:i,j],shocks[i:1])
     }
  }
  return(as.matrix(hd))
}
