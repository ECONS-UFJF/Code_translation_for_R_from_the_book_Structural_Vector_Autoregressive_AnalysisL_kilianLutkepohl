# translate to R by André Suriane ECONS ECONOMIA UFJF 
#

lagn<-function(data,m) {
#input: data matrix and lag m
data=data[(m+1):length(data)]-data[1:(length(data)-m)];
data
}
