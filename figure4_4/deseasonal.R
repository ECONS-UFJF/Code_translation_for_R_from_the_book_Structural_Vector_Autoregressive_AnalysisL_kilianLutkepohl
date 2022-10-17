# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# DESEASONAL.M
# Lutz Kilian
# University of Michigan
# August 2010
# This program fits monthly seasonal dummies to a univariate time series 
# and returns the residuals

deseasonal<-function(y) {

t=length(y);

X<-diag(12);

for (i in 1:((t/12)+1)){
    X=rbind(X,diag(12));
}
X=X[1:t,];

# Regression coefficients and residuals
bhat= solve(crossprod(X)) %*% crossprod(X,y);
yhat=X%*%bhat;
ehat=y-yhat;           
    return(ehat)
}



