n = 100
p =5
X = matrix(rnorm(n*p),nrow = n)
beta = matrix(1:p,nrow = p)
Y = X%*%beta + rnorm(n)
lm(Y ~ X)

Z = cbind(rep(1,n),X,Y)
R = myqr(Z)$R

R1 = R[1:(p+1),1:(p+1)]
Y1 = R[1:(p+1),p+2]
beta = solve(R1,Y1)
