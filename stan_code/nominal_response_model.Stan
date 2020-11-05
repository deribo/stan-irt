data{
int\lower=2, upper=4 . K;
int \lower=0 . n_student;
int \lower=0 . n_item;
int\lower=1,upper=K . Y[n_student,n_item];
}
parameters {
vector[K] zeta[n_item]; //freely estimated intercept
vector[K] lambda[n_item]; //freely estimated slope
vector[n_student] theta;
}
transformed parameters {
vector[K] zetan[n_item]; //intercept with constraints
vector[K] lambdan[n_item]; //slope with constraints
for (k in 1:n_item) {
for (l in 1:K) {
zetan[k,l]=zeta[k,l]-mean(zeta[k]); //constrain intercept sum for each item to 0
lambdan[k,l]=lambda[k,l]-mean(lambda[k]); //constrain slope sum for each item to 0
}}
}
model{
theta ~ normal(0,1);
for (i in 1: n_item){
zeta[i] ~ normal(0,2);
lambda[i] ~ normal(0,2);
}
for (i in 1:n_student)
for (j in 1:n_item)
Y[i,j] ~ categorical_logit(zetan[j] + lambdan[j]*theta[i]);
}
generated quantities {
vector[n_item] log_lik[n_student];
for (i in 1: n_student){
for (j in 1: n_item){
log_lik[i, j] = categorical_logit_log (Y[i, j], zetan[j] + lambdan[j]*theta[i]);
}}
}