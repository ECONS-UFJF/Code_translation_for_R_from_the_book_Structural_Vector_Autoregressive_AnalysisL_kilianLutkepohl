# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
## Function: devec.m
# Returns the matrix form of the vector passed, given the number of cols
# Logan Lewis

devec<-function(vector,col){
  T = length(vector)
  matrix = matrix(0,T/col,col)
 for (i in 1:col) {
    matrix[,i] = vector[((i-1)*T/col + 1):(i*T/col)]
  }
  return(matrix)
}
