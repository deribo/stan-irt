data {
int\lower=0 . n_student;
int\lower=0 . n_item;
int\lower=0,upper=1 . Y[n_student,n_item];
}
parameters {
vector[n_student] theta;
vector \lower=0 . [n_item] alpha;
vector[n_item] beta;
vector\lower=0,upper=1 . [n_item] gamma; //item pseudo-guessing
real mu_beta;
real\lower=0 . sigma_alpha;
real\lower=0 . sigma_beta;
}
model {
theta ~ normal(0,1);
beta ~ normal(mu_beta,sigma_beta);
mu_beta ~ normal(0,5);
sigma_beta ~ cauchy(0,5);
alpha ~ lognormal(0,sigma_alpha);
sigma_alpha ~ cauchy(0,5);
gamma ~ beta(5,23);
for(i in 1:n_student){
for (j in 1:n_item){
real p; //create a local variable within the loop to make Stan code more readable
p= inv_logit(alpha[j]*(theta[i] - beta[j]));
Y[i,j] ~ bernoulli(gamma[j] + (1-gamma[j])*p);
}}
}
generated quantities {
vector[n_item] log_lik[n_student];
for (i in 1: n_student){
for (j in 1: n_item){
real p;
p= inv_logit(alpha[j]*(theta[i] -beta[j]));
log_lik[i, j] = bernoulli_log(Y[i, j], gamma[j] + (1-gamma[j])*p);
}}
}