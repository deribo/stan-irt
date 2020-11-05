data{
int<lower = 1> D; // number of dimensions (3)
int<lower = 1> N; // number of examinees
int<lower = 1> K; // number of items
int<lower = 1> Ntot; // number of data points
int<lower = 1> jj[Ntot]; // item id
int<lower = 1> ii[Ntot]; // person id
int<lower=0,upper=1> y[Ntot]; //responses
int<lower=0,upper=1> d[Ntot]; // missingness indicators
real logt[Ntot]; //log response times
vector[D] Zero; // a vector of Zeros (fixed means for person parameter)
}
parameters{
corr_matrix[D] correlP; // person parameter correlations
vector<lower=0>[D] sigmaP; // person parameter variances
vector[K] iota; // engagement difficulties
vector[K] b; // difficulties
vector<lower=0>[K] diffbeta; // time intensity offset parameters
row_vector[D] PersPar[N]; // person parameters
real<lower = 0> sigmaE; // residual variance logRT disengaged
real<lower = 0> sigmaD; // common variance logRT disengaged
real muC; // common mean logRT not engaged
real gamma0; // regression intercept
real gamma1; // regression parameter ability
real gamma2; // regression parameter speed
real<lower = 0, upper=1> pCorrNE; // probability correct disengaged
}
transformed parameters{
cov_matrix[D] SigmaP; // person variance covariance matrix
real<lower=0,upper=1> pDelta[Ntot]; // disengagement probability
real<lower=0,upper=1> pCorrE[Ntot]; // probability correct engaged
real<lower=0,upper=1> pOmitNE[Ntot]; // probability omit instead of guess
vector[K] beta; // time intensities
SigmaP=quad_form_diag(correlP, sigmaP);
for(n in 1:Ntot){
pDelta[n] = 1/(1+exp(-PersPar[ii[n],1] + iota[jj[n]] ));
pCorrE[n] = 1/(1+exp(-PersPar[ii[n],2] + b[jj[n]]));
pOmitNE[n] = 1/(1+exp(-gamma0 - gamma1*PersPar[ii[n],2]-gamma2*PersPar[ii[n],3]));
}
for(j in 1:K){
beta[j]=muC+diffbeta[j];
}
}
model{ 
// prior person parameter
PersPar~ multi_normal(Zero,SigmaP);
sigmaP ~ cauchy(0,5);
correlP ~ lkj_corr(1);
// prior item parameter
iota ~ normal(0, 10);
b ~ normal(0, 10);
diffbeta ~ normal(0, 10);
pCorrNE ~ beta(1,1);
gamma0 ~ normal(0,10);
gamma1 ~ normal(0,10);
gamma2 ~ normal(0,10);
muC ~ normal(0, 10);
sigmaE ~ cauchy(0,5);
sigmaD ~ cauchy(0,5);
// target distribution
for(n in 1:Ntot){
target += log_mix(pDelta[n],
(1-d[n])*bernoulli_lpmf(y[n]| pCorrE[n])+bernoulli_lpmf(d[n]|0)+
(1-d[n])*normal_lpdf(logt[n]|beta[jj[n]] - PersPar[ii[n],3], sigmaE), // engaged
(1-d[n])*bernoulli_lpmf(y[n]| pCorrNE)+bernoulli_lpmf(d[n]| pOmitNE[n])+
normal_lpdf(logt[n]| muC, sigmaD)); // disengaged
}
 }