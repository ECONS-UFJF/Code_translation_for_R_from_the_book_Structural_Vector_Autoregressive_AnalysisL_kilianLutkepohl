## translate to R by André Suriane ECONS ECONOMIA UFJF 
#
kernel<-function(x){

# kernel(x) returns the length(x)-point Gaussian
# kernel density estimate where h is a smoothing parameter

   n<-length(x)
  h<-1.06*sd(x)*n^(-1/5) # std -> sd

  x<-sort(x) 
  f<-c()
  for (i in 1:n){
    f[i]=1/(n*h*sqrt(2*pi))*sum(exp(-.5*((x-x[i])/h)^2));
  }
  return(data.frame(x=x, f=f))
}


