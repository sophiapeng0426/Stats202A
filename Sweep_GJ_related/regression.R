n = 100
p = 5
X = matrix(rnorm(n*p), nrow = n)
beta = matrix(1:p, nrow = p)
Y = X%*%beta + rnorm(n)

## sweep operator
Z = cbind(rep(1,n),X,Y)
A = t(Z)%*%Z

S = mySweep(A,p+1) ## column one is intercept
beta_hat = S[1:(p+1),(p+2)]

