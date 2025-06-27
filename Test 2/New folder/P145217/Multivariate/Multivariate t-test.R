# Example 1
4*qf(0.95, df1 = 2, df2 = 1)

x = c(8,6)
u = c(9,5)

x-u
t(x-u)
A = matrix(c(4,-3,-3,9), nrow=2)
A_inv <- solve(A)

3*t(x-u) %*% A_inv %*% (x-u) 



#Example2 (20 healthy female with sweat data)
n = 20
p = 3
x = c(4.64,45.4,9.965)
S = matrix(c(2.879,10.010,-1.810,10.010,199.788,-5.640,-1.810,-5.640,3.628),nrow=3)
T2 = n * t(x-u) %*% solve(S) %*% (x-u)
T2
alpha = (n-1)*p/(n-p) * qf(0.9,df1=p,df2=n-p)
alpha


# Example3 Mineral analysis of peruvian hair
x1 = c(0.48, 40.53, 2.19, 0.55, 0.74, 0.66, 0.93, 0.37 ,0.22)
x2 = c(12.57, 73.68, 11.13, 20.03, 20.29, 0.78, 4.64, 0.43, 1.08)

xbar = c(mean(x1), mean(x2))
u = c(2,1)

cov = cov(data.frame(cbind(x1,x2)))
eigenvalues = eigen(cov)$values
eigenvectors = eigen(cov)$vectors

p = 2
n = 9
alpha = 0.9

c2 = (n-1)*p/(n-p) * qf(alpha, df1=p, df2=n-p)
c = sqrt(c2)

axes_length_const = sqrt(eigenvalues) * c / sqrt(n)
eigenvectors

xbar + axes_length_const[1] * eigenvectors[,1] 
xbar - axes_length_const[2] * eigenvectors[,2] 

-29.81 -46.40
40.18 78.54
14.43 10.88
-4.06 21.25


# Example4 Microwave oven
xbar = c(.564, .603)
u = c(.562, .589)
S = matrix(c(.0144, .0117, .0117, .0146), nrow=2)
lambda = c(.026, .002)
e1 = c(.704, .710)
e2 = c(-0.710, .704)
n = 42
p = 2
alpha = 0.95

c2 = p*(n-1)/(n-p) * qf(alpha,p,n-p)
c = sqrt(c2)

t2 = n * t(xbar-u) %*% solve(S) %*% (xbar-u)

t2
c2
# t2 < c2, cannot reject

# draw confidence region

xbar + sqrt(lambda[1]) * c / sqrt(n) * e1
xbar - sqrt(lambda[1]) * c / sqrt(n) * e1
xbar + sqrt(lambda[2]) * c / sqrt(n) * e2
xbar - sqrt(lambda[2]) * c / sqrt(n) * e2


# Example5 Musical aptitude Profile
x_u = c(28.1, 26.6, 35.4, 34.2, 23.6, 22.0, 22.7)
s = c(5.76, 5.85, 3.82, 5.12, 3.76, 3.93, 4.03)
chi = sqrt(qchisq(0.9, df = 7))
