
data {
  int<lower=1> n_subs; // number of subjects
  int N; // total number of data points
  array[N] int choices;
  array[N] int sub_ns;
  array[N] real expin; // normed logit of being in the category
  array[N] real az; // angle z scores
  array[N] real rz; // radius z scores
  array[N] real az_sq; // angle z squared (distance metric)
  array[N] real rz_sq; // radius z squared (distance metric)
}


parameters {
  real icpt_mu;
  real b_expin_mu;
  real b_az_mu;
  real b_az_sq_mu;
  real b_rz_mu;
  real b_rz_sq_mu;
  real<lower=0.01> icpt_sd;
  real b_sd;
  //real<lower=0.01> b_sd
  // real<lower=0.01> b_az_sd;
  // real<lower=0.01> b_az_sq_sd;
  // real<lower=0.01> b_rz_sd;
  // real<lower=0.01> b_rz_sq_sd;
  
  vector[n_subs] icpt;
  vector[n_subs] b_expin;
  vector[n_subs] b_az;
  vector[n_subs] b_az_sq;
  vector[n_subs] b_rz;
  vector[n_subs] b_rz_sq;
  
}

model {
  b_expin_mu  ~ normal(0,1);
  b_az_mu ~ normal(0,1);
  b_az_sq_mu ~ normal(0,1);
  b_rz_mu ~ normal(0,1);
  b_rz_sq_mu ~ normal(0,1);
  for(s in 1:n_subs){
    icpt[s] ~ normal(icpt_mu, icpt_sd);
    b_expin[s] ~ normal(b_expin_mu, b_expin_sd);
    b_az[s] ~ normal(b_az_mu, b_az_sd);
    b_az_sq[s] ~ normal(b_az_sq_mu, b_az_sq_sd);
    b_rz[s] ~ normal(b_rz_mu, b_rz_sd);
    b_rz_sq[s] ~ normal(b_rz_sq_mu, b_rz_sq_sd);
  }
  for(i in 1:N){
    choices[i] ~ bernoulli_logit(icpt[sub_ns[i]] + b_expin[sub_ns[i]]*expin[i] + b_az[sub_ns[i]]*az[i] + b_az_sq[sub_ns[i]]*az_sq[i] + b_rz[sub_ns[i]]*rz[i] + b_rz_sq[sub_ns[i]]*rz_sq[sub_ns[i]]);
  }
}

generated quantities{
  array[N] real pred;
  for(k in 1:N){
    pred[k] ~ inv_logit(icpt[sub_ns[k]] + b_expin[sub_ns[k]]*expin[k] + b_az[sub_ns[k]]*az[k] + b_az_sq[sub_ns[k]]*az_sq[k] + b_rz[sub_ns[k]]*rz[k] + b_rz_sq[sub_ns[k]]*rz_sq[sub_ns[k]]);
  }
}

