###-----------missing data in meta-analysis----------###
###Latest edit date: 05/14/2015
###Author: Yajuan Si

########################################################
library()

set.seed(20150514)
###------Case 1: response inconsistency------###
###transform
S=5; #studies
n_s=c(100,300,500,1000,1500); #samples sizes by study
n=sum(n_s); #total sample size
S_i=c(rep(1:S),n_s)
###treatment
Z=sample(c(0,1),n,replace=TRUE,prob=c(0.5,0.5)); #treatment randomized
###covariates
q1=3; X_c=matrix(0,n,q1) #q1 continuous variables
for (j in 1:q1){
  X_c[,j]=rnorm(n) * j + j-2
}
q2=6; X_d=matrix(0,n,q2) #q2 binary variables
for (j in 1:q2){
  X_d[,j]=sample(c(0,1),n,replace=TRUE,prob=c(1/j,1-1/j))
}
###outcome
beta=rnorm(2+q1+q2) + seq(-3,3,2+q1+q2)
#constant shift
#differential constant effect by study 
Y=cbind(rep(1,n),X_c,X_d,Z) * beta + S_i * 2 

#shift by treatment
#differntial effect by interaction of study and treatment
Y=cbind(rep(1,n),X_c,X_d,Z) * beta + Z * S_i * 2 

#shift by interaction between treatment and one covariate
#differential effect by interaction of study and treatment and covariate
#fixed effect
Y=cbind(rep(1,n),X_c,X_d,Z) * beta + Z * X_c[,1] * S_i * 2 

#shift by treatment
#differential effect by interaction of study and treatment
#random effect
beta_s=rnorm(S) * 2 #sd=2 independent
#sigma_s=diag(S) * 2 + 0.1
#beta_s=mvrnorm(1,0,sigma_s)
Y=cbind(rep(1,n),X_c,X_d,Z) * beta + Z * (2 * S_i + beta_s[S_i]) 

###---missing outcome due to MAR---###
#missingness indicator
R=as.numeric((runif(n) < ((X_c[,1]<0) * 0.9))) #24%

###------Interest: treatment effect estimation---------###
Y_obs=Y[R==1]

###------Case 2: differential selection bias------###


###------Case 3: response inconsistency and differential bias------###
