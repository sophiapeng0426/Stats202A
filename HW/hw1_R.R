mySweep <- function (A,m)
{
  n = dim(A)[1]
  for (k in 1:m)
  {
    for (i in 1:n)
      for (j in 1:n)
        if (i!= k & j!= k)
          A[i,j] = A[i,j] - A[i,k]*A[k,j]/A[k,k]
    
    for (i in 1:n)
      if (i!= k)
        A[i,k] = A[i,k]/A[k,k]
    
    for(j in 1:n)
      if (j!= k)
        A[k,j] = A[k,j]/A[k,k]
    
    A[k,k] = -1/A[k,k]
  }
  return(A)
}



MyGaussJordan = function(A,m){
  n = dim(A)[1]
  B = cbind(A,diag(rep(1,n)))
  for (k in 1:m)
  {
    a = B[k,k]
    for (j in 1:(n*2)){
      B[k,j] = B[k,j]/a
     }
    
    for (i in 1:n){
      if (i!=k){
        a = B[i,k]
        
       for (j in 1:(n*2)){
         B[i,j] = B[i,j] - B[k,j]*a
         } 
      }
    }
  }
  return(B)
  }
  
myGaussJordan_vec = function (A,m){
  ## check if A is square matrix,
  ## check if A is invertible
  n = dim(A)[1]
  c = dim(A)[2]
  if(n != c){ warning("argument is not a square matrix: returning NA")}
  else {
   B = cbind(A,diag(rep(1,n)))
   for (k in 1:m){
     if (abs(B[k,k]) >= 1e-8){
       B[k,] = B[k,]/B[k,k]
       for (i in 1:n)
         if(i!=k){
           B[i,] = B[i,] - B[k,]*B[i,k]
         }
      }
     else {
       #warning("argument is not invertable: returning NA") 
       B = "argument is not invertable: returning NA"
       break
     }
  }
 }
return(B)
}

A = matrix(c(1,2,3,7,11,13,17,21,23),3,3)
B = matrix(c(1,2,3,7,11,13),3,2)
C = matrix(c(2,1,6,3),2,2)   ## non invertible
