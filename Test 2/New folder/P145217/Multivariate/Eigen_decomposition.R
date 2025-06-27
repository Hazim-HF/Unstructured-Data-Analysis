A = matrix(c(1,2,2,3), nrow=2)
A

eigen(A)

Q = eigen(A)$vectors
B = diag(eigen(A)$values)

# Spectral decomposition A = QBQ^t
Q%*%B%*%t(Q)

# ===== using iris data =====
A = cov(iris[,c(1,2)])
A

eigen(A)

Q = eigen(A)$vectors
B = diag(eigen(A)$values)

# Spectral decomposition A = QBQ^t
Q%*%B%*%t(Q)

# Square root of A
C = Q%*%(sqrt(B))%*%t(Q)


# To draw contour of a 2-d normal distribution
# Centre at u (miu) (0,0)
# Two Axes 
# First : (0,0) +- sqrt(5.99) * sqrt(1.5) * (0.707, 0.707)
a = sqrt(5.99)*sqrt(1.5)
a1 = a * c(0.707,0.707)
a2 = -a1
a1
a2
# Second : (0,0) +- sqrt(5.99) * sqrt(0.5) * (-0.707, 0.707)
b = sqrt(5.99) * sqrt(0.5)
b1 = b * c(-0.707,0.707)
b2 = -b1
b1
b2
