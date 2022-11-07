# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
## Function: datasplit.m
# Take in data as columns, split into appropriate X and Y matrices
# Logan Lewis
# February, 2007
# Input:	data, a matrix of observations, each variable in a column
#		p, the lag order to estimate
# Output:	Y, the matrix of regressands
#		X, the matrix of regressors

datasplit<- function(data,p) {
  T <- nrow(data)
  K <- ncol(data) # K variables
  T = T - p;              # Need to remove p observations 

# Y is a matrix of T observations, each variable in a column
# X is a matrix of p presample values, each row a time period, 
# cols are lags of Y (each of size 1xK), and a col of ones

X <- as.data.frame(matrix(1, T,(K*p+1)))
  
for (i in 1:p){
    X[,(2+K*(i-1)):(1+K*(i))] <- data[(p-i+1):(p-i+T),]
}

Y = data[(p+1):(T+p),]

return(list(Y=as.matrix(Y), X=as.matrix(X)))
}
