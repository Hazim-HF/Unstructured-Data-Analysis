# Multivariate RMD

u0 = c(300,400,500,500)
xbar = c(368.21, 404.63, 479.26, 502.89)
S = matrix(c(2819.29, 3568.42, 2943.49, 2295.35, 3568.42, 7963.14, 5303.98, 4065.44, 2943.49, 5303.98, 6851.32, 4499.63, 2295.35, 4065.44, 4499.63, 4878.99), nrow=4, byrow=T)
C = matrix(c(-0.1,-0.1,0.1,0.1,1,-1,1,-1,1,-1,-1,1), nrow=3, byrow=T)
C%*%xbar
C%*% S %*% t(C)
n=19 

cx = C%*%xbar
cu0 = C%*%u0
csc = C %*% S %*% t(C)

t2 = n * t(cx-cu0) %*% solve(csc) %*% (cx-cu0)
t2


##	Example	on	producing	plastic	film	from	Krzanowski	(1998,	p.	381)	
tear	<-	c(6.5,	6.2,	5.8,	6.5,	6.5,	6.9,	7.2,	6.9,	6.1,	6.3,	
          6.7,	6.6,	7.2,	7.1,	6.8,	7.1,	7.0,	7.2,	7.5,	7.6)	
gloss	<-	c(9.5,	9.9,	9.6,	9.6,	9.2,	9.1,	10.0,	9.9,	9.5,	9.4,	
           9.1,	9.3,	8.3,	8.4,	8.5,	9.2,	8.8,	9.7,	10.1,	9.2)	
opacity	<-	c(4.4,	6.4,	3.0,	4.1,	0.8,	5.7,	2.0,	3.9,	1.9,	5.7,	
             2.8,	4.1,	3.8,	1.6,	3.4,	8.4,	5.2,	6.9,	2.7,	1.9)	
Y	<-	cbind(tear,	gloss,	opacity)	
rate					<-	gl(2,10,	labels	=	c("Low",	"High"))	
additive	<-	gl(2,	5,	length	=	20,	labels	=	c("Low",	"High"))	
fit	<-	manova(Y	~	rate	*	additive)	
summary(fit,	test	=	"Wilks")		
summary.aov(fit)	
