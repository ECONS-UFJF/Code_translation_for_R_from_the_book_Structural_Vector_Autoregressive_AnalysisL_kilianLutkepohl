# Function: multivar.m
# Estimate a VAR(p) with multivariate least squares
# Logan Lewis
# February, 2007

# For a textbook reference on this procedure, see Lutkepohl (2005), ch. 3

# To translate between notation styles, Z' = X (Z is used by Lutkepohl),
# while X generates the familiar inv(X'X)*X'Y
# This also means that the Ys will be transposes

# Input:    Y, the matrix of regressands
#           X, the matrix of regressors
#           p, the lag order
# Output:   bhat, the estimated coefficients
#           bhatstd, standard errors of bhat
#           ehat, residuals
#           sigmauhat, OLS estimate of sigma_u
#           sigmabhat, the full variance/covariance matrix for bhat

multivar<-function(Y,X,p) {
T<- nrow(Y)
K <-ncol(Y)

bhat = solve(crossprod(X))%*%crossprod(X,Y)

# Calculate the standard error matrix
sigmauhat <- 1/(T - K*p - 1)*(crossprod(Y) - crossprod(Y,X)%*%solve(crossprod(X))%*%crossprod(X,Y));
sigmabhat <- kron(solve(crossprod(X)),sigmauhat); 

### continuar daqui

bhatstd = t(devec(sqrt(diag(sigmabhat)),K*p+1))

# Calculate the prediction errors
ehat = Y - X%*%bhat; 

return(list(bhat=bhat, bhatstd=bhatstd, ehat=ehat, sigmauhat=sigmauhat, sigmabhat=sigmabhat) )
}