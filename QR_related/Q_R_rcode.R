myqr = function(A){
  n = nrow(A)
  m = ncol(A)
  R = A
  Q = diag(n) ## record all operations on A
  
  for (k in 1:(m-1)){
    x = matrix(rep(0,n), nrow = n)
    x[k:n,1] = R[k:n,k]
    g = sqrt(x^2)
    v = -x
    v[z] = g-x[k]
    
    s = sqrt(sum(v^2))
    u = v/s
    R = R - 2*u%*%t(u)%*%R
    Q = Q - 2*u%*%t(u)%*%Q
  }
  
  result = list(Q,R)
  return(result)
    
}