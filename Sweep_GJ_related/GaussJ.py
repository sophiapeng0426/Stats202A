import numpy as np 
def myGaussGordan(A,m):
	n = A.shape(0)
	B = np.nstack(A,np.identity(n))
	for k in range(m):
		B[k,] = B[k,]/B[k,k]
		for i in range(n):
			if i!= k: 
				B[i,:] = B[i,:]-B[i,:]*B[i,k]

	return B


Det = 1
for (k in range(1,n))
{
  Det = Det + A[k,k]
  A = swp[k]A

}