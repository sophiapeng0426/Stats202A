##lm() in r
n = 100
X1 = rnorm(n)
X2 = rnorm(n)
Y = X1 + 2*X2 + rnorm(n)
lm(Y~X1 + X2)

A = data.frame(x1 = X1,x2 = X2,y = Y)
lm(y~x1+x2,data = A)

ori = trees
lt = log(trees)
m = lm(Volume~ Height+Girth,data = lt)
summary(m)
