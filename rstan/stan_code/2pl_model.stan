// The input data

data {
  int<lower=1> I;               // # questions
  int<lower=1> J;               // # persons
  int<lower=1> N;               // # observations
  int<lower=1, upper=I> ii[N];  // question for n
  int<lower=1, upper=J> jj[N];  // person for n
  int<lower=0, upper=1> y[N];   // correctness for n
}

// The parameters accepted by the model. Our model

parameters {
  vector<lower=0>[I] alpha;     // discrimination for item i
  vector[I] beta;               // difficulty for item i
  vector[J] theta;              // ability for person j
}


// The model to be estimated. 

model {
  vector[N] eta;
  
  // Priors
  alpha ~ lognormal(0.5,1);
  beta ~ normal(0,10);
  theta ~ normal(0,1);
  
  // Likelihood
  for (n in 1:N) {
    eta[n] = alpha[ii[n]] * (theta[jj[n]] - beta[ii[n]]);
  }
  y ~ bernoulli_logit(eta);
}

// generated_quantities for assessment of model fit

// generated quantities {
//     real theta_rep[J];
//     int y_rep[N];
//     for(j in 1:J)
//       theta_rep[j] = normal_rng(0, 1);
//     for (n in 1:N) 
//       y_rep[n] = bernoulli_rng(inv_logit(alpha[ii[n]] * (theta_rep[jj[n]] - beta[ii[n]])));
// }

