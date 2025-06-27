######################################################################################################
######################################################################################################
##
## Entering the raw data for the life expectancy example:
##
"life" <- 
  structure(.Data = list(c(63., 34., 38., 59., 56., 62., 50., 65., 56., 69., 65., 64., 56., 60., 61., 49., 59., 63., 59., 65., 65., 64.,
                           64., 67., 61., 68., 67., 65., 59., 58., 57.)
                         , c(51., 29., 30., 42., 38., 44., 39., 44., 46., 47., 48., 50., 44., 44., 45., 40., 42., 44., 44., 48., 48., 63.,
                             43., 45., 40., 46., 45., 46., 43., 44., 46.)
                         , c(30., 13., 17., 20., 18., 24., 20., 22., 24., 24., 26., 28., 25., 22., 22., 22., 22., 23., 24., 28., 26., 21.,
                             21., 23., 21., 23., 23., 24., 23., 24., 28.)
                         , c(13., 5., 7., 6., 7., 7., 7., 7., 11., 8., 9., 11., 10., 6., 8., 9., 6., 8., 8., 14., 9., 7., 6., 8., 10., 8.,
                             8., 9., 10., 9., 9.)
                         , c(67., 38., 38., 64., 62., 69., 55., 72., 63., 75., 68., 66., 61., 65., 65., 51., 61., 67., 63., 68., 67., 68.,
                             68., 74., 67., 75., 74., 71., 66., 62., 60.)
                         , c(54., 32., 34., 46., 46., 50., 43., 50., 54., 53., 50., 51., 48., 45., 49., 41., 43., 48., 46., 51., 49., 47.,
                             47., 51., 46., 52., 51., 51., 49., 47., 49.)
                         , c(34., 17., 20., 25., 25., 28., 23., 27., 33., 29., 27., 29., 27., 25., 27., 23., 22., 26., 25., 29., 27., 25.,
                             24., 28., 25., 29., 28., 28., 27., 25., 28.)
                         , c(15., 6., 7., 8., 10., 14., 8., 9., 19., 10., 10., 11., 12., 9., 10., 8., 7., 9., 8., 13., 10., 9., 8., 10., 11.,
                             10., 10., 10., 12., 10., 11.)
  )
  , class = "data.frame"
  , names = c("m0", "m25", "m50", "m75", "w0", "w25", "w50", "w75")
  , row.names = c("Algeria", "Cameroon", "Madagascar", "Mauritius", "Reunion", "Seychelles", "South Africa(C)", "South Africa(W)",
                  "Tunisia", "Canada", "Costa Rica", "Dominican Rep", "El Salvador", "Greenland", "Grenada", "Guatemala",
                  "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Trinidad(62)", "Trinidad (67)", 
                  "United States (66)", "United States (NW66)", "United States (W66)", "United States (67)", "Argentina",
                  "Chile", "Columbia", "Ecuador")
  )
##
##
######################################################################################################
######################################################################################################

factanal(life,factors=1, rotation="varimax")
# One factor is clearly not enough (tiny P-value).

factanal(life,factors=2, rotation="varimax")
factanal(life,factors=2, rotation="none")
# Two factors are clearly not enough (tiny P-value).

factanal(life,factors=3, rotation="varimax")
# Three factors may be enough (P-value = 0.458).

# How could we interpret the three factors?

# Saving the result as life.fa.3.v:

life.fa.3.v <- factanal(life,factors=3, rotation="varimax")

# The communalities:

1-life.fa.3.v$uniquenesses

# We see that m0, m50, w0, w25, w50 share very much of their variances with the other variables via the factors.
# m25 and m75 are more "unique".
# What does this mean?

# Estimating factor scores for the life expectancy data set:

life.fa.3.scores <- factanal(life,factors=3, rotation="varimax",scores="regression")$scores

#### Plotting the 3 factor scores for this data set using a 3-D plot:

# Creating a data frame with the original data and the scores:

life.df <- data.frame(life, life.fa.3.scores)
attach(life.df)

### Doing a 3-D plot:

library(lattice)  # loading the lattice package
cloud(Factor3 ~ Factor1 * Factor2, xlim=range(Factor1), ylim=range(Factor2), zlim=range(Factor3), 
      pch=row.names(life.df),
      scales = list(distance = rep(1, 3), arrows = FALSE))

#=================================================================================================================

#estimate loadings and specific factors using PC method

#step1
R=cor(data)

#step2
E=eigen(R)

#step3
#determine the % variance explained (appropriate number of factors)
E$values/sum(E$values)
cumsum(E$values/sum(E$values))

#step4
#estimate loadings (common factors)
#example here is for m=2, change accordingly (based on step3)
Lhat=as.matrix(cbind(sqrt(E$values[1])*E$vectors[,1],sqrt(E$values[2])*E$vectors[,2]))

#step5
#estimate specific factors
diag(R-Lhat%*%t(Lhat))
#in matrix form
diag(diag(R-Lhat%*%t(Lhat)))

#step6
#compute communalities
h1=sum(Lhat[1,]^2) #example of first communality
h1

#=================================================================================================================

# The Duncan, Haller, and Portes Peer-Influences Model
# Duncan, Haller, and Portes’s (nonrecursive) peer-influences
# model: {1, respondent’s IQ; {2, respondent’s family SES; {3, best friend’s
# family SES; {4, best friend’s IQ; |5 , respondent’s occupational aspiration;
# |6, best friend’s occupational aspiration.

library(sem)
library(DiagrammeR)

R.DHP <- matrix(c(     
  1,      0,      0,      0,      0,      0,      0,      0,      0,      0,            
  .6247,   1,     0,      0,      0,      0,      0,      0,      0,      0,            
  .3269,  .3669,  1,      0,      0,      0,      0,      0,      0,      0,            
  .4216,  .3275,  .6404,  1,      0,      0,      0,      0,      0,      0,
  .2137,  .2742,  .1124,  .0839,  1,      0,      0,      0,      0,      0,
  .4105,  .4043,  .2903,  .2598,  .1839,  1,      0,      0,      0,      0,
  .3240,  .4047,  .3054,  .2786,  .0489,  .2220,  1,      0,      0,      0,
  .2930,  .2407,  .4105,  .3607,  .0186,  .1861,  .2707,  1,      0,      0,
  .2995,  .2863,  .5191,  .5007,  .0782,  .3355,  .2302,  .2950,  1,      0,
  .0760,  .0702,  .2784,  .1988,  .1147,  .1021,  .0931, -.0438,  .2087,  1
), ncol=10, byrow=TRUE)

model.dhp <- matrix(c(
  'RParAsp  -> RGenAsp', 'gam11',  NA,
  'RIQ      -> RGenAsp', 'gam12',  NA,
  'RSES     -> RGenAsp', 'gam13',  NA,
  'FSES     -> RGenAsp', 'gam14',  NA,
  'RSES     -> FGenAsp', 'gam23',  NA,
  'FSES     -> FGenAsp', 'gam24',  NA,
  'FIQ      -> FGenAsp', 'gam25',  NA,
  'FParAsp  -> FGenAsp', 'gam26',  NA,
  'FGenAsp  -> RGenAsp', 'beta12', NA,
  'RGenAsp  -> FGenAsp', 'beta21', NA,
  'RGenAsp  -> ROccAsp',  NA,       1,
  'RGenAsp  -> REdAsp',  'lam21',  NA,
  'FGenAsp  -> FOccAsp',  NA,       1,
  'FGenAsp  -> FEdAsp',  'lam42',  NA,
  'RGenAsp <-> RGenAsp', 'ps11',   NA,
  'FGenAsp <-> FGenAsp', 'ps22',   NA,
  'RGenAsp <-> FGenAsp', 'ps12',   NA,
  'ROccAsp <-> ROccAsp', 'theta1', NA,
  'REdAsp  <-> REdAsp',  'theta2', NA,
  'FOccAsp <-> FOccAsp', 'theta3', NA,
  'FEdAsp  <-> FEdAsp',  'theta4', NA),
  ncol=3, byrow=TRUE)

rownames(R.DHP) <- colnames(R.DHP) <- c('ROccAsp', 'REdAsp', 'FOccAsp', 
                                        'FEdAsp', 'RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp')

sem.dhp <- sem(model.dhp, R.DHP, 329,
               fixed.x=c('RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp'))

summary(sem.dhp)

path.diagram(sem.dhp, min.rank='RIQ, RSES, RParAsp, FParAsp, FSES, FIQ', 
             max.rank='ROccAsp, REdAsp, FEdAsp, FOccAsp')

##   digraph "sem.dhp" {
##   rankdir=LR;
##   size="8,8";
##   node [fontname="Helvetica" fontsize=14 shape=box];
##   edge [fontname="Helvetica" fontsize=10];
##   center=1;
##   {rank=min "RIQ" "RSES" "RParAsp" "FParAsp" "FSES" "FIQ"}
##   {rank=max "ROccAsp" "REdAsp" "FEdAsp" "FOccAsp"}
##   "RGenAsp" [shape=ellipse]
##   "FGenAsp" [shape=ellipse]
##   "RParAsp" -> "RGenAsp" [label="gam11"];
##   "RIQ" -> "RGenAsp" [label="gam12"];
##   "RSES" -> "RGenAsp" [label="gam13"];
##   "FSES" -> "RGenAsp" [label="gam14"];
##   "RSES" -> "FGenAsp" [label="gam23"];
##   "FSES" -> "FGenAsp" [label="gam24"];
##   "FIQ" -> "FGenAsp" [label="gam25"];
##   "FParAsp" -> "FGenAsp" [label="gam26"];
##   "FGenAsp" -> "RGenAsp" [label="beta12"];
##   "RGenAsp" -> "FGenAsp" [label="beta21"];
##   "RGenAsp" -> "ROccAsp" [label=""];
##   "RGenAsp" -> "REdAsp" [label="lam21"];
##   "FGenAsp" -> "FOccAsp" [label=""];
##   "FGenAsp" -> "FEdAsp" [label="lam42"];
##   }

# ================================================
# Additional example
corr = matrix(c(1, .02, .96, .42, .01,
                .02, 1, .13, .71, .85,
                .96, .13, 1, .50, .11,
                .42, .71, .50, 1, .79,
                .01, .85, .11, .79, 1), nrow=5, byrow=T)
E= eigen(corr)
E

Lhat=as.matrix(cbind(sqrt(E$values[1])*E$vectors[,1],sqrt(E$values[2])*E$vectors[,2]))
Lhat
#step5
#estimate specific factor(specific variance?)
diag(corr-Lhat%*%t(Lhat))
#in matrix form
diag(diag(corr-Lhat%*%t(Lhat)))

# also specific variance
sv=rep(1,5)-Lhat[,1]^2-Lhat[,2]^2
sv

h=Lhat[,1]^2+Lhat[,2]^2
# this is the loading matrix with communality and specific variance
cbind(Lhat,h,sv)


# ==================================
# Decathlon dataset example
library(FactoMineR)
data(decathlon)
data = decathlon[,1:10]
data

# Estimate loading using PCA method
C = cor(data)
E = eigen(C)
m = 4
E$values
E$vectors

# Loading of first factor
sqrt(E$values[1]) * E$vectors[,1]

loading_matrix = c()
for (i in 1:4) {
  loading_matrix = cbind(loading_matrix, sqrt(E$values[i]) * E$vectors[,i])
}
loading_matrix

comm = apply(loading_matrix,MARGIN = 1,FUN = function(x) { sum(x^2)})
sv = apply(loading_matrix,MARGIN = 1,FUN = function(x) { 1 - sum(x^2)})

# PCA resulted factor loadings. communality, and specific variance
cbind(loading_matrix, comm, sv)
resid = cor(data) - loading_matrix %*% t(loading_matrix) - diag(sv)
round(resid, digits=2)

# Estimate factor loading using MLE method 
fa = factanal(data,factors=4, rotation="varimax")
loading_matrix = fa$loadings
comm = 1 - fa$uniquenesses
sv = fa$uniquenesses
cbind(loading_matrix, comm, sv)
resid = cor(data) - fa$loadings %*% t(fa$loadings) - diag(fa$uniquenesses)
round(resid, digits=2)


# ==================================
# Rotation example (9.8)