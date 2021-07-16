
// Data block
data{
  int<lower=0> n;
  int<lower=0> p;
  int<lower=0> q;
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> atau_delta;
  real<lower=0> btau_delta;
  real<lower=0> atau_xi;
  real<lower=0> btau_xi;
  int<lower = 0, upper = 1> spatial_theta;
  int<lower = 0, upper = 1> spatial_zeta;
  vector[n] Y;
  matrix[n,p] X;
  matrix[n,q] W;
  vector[p] mean_betas;
  vector[p] variance_betas;
  vector[q] mean_gammas;
  vector[q] variance_gammas;
  matrix[n,n] cov_delta;
  matrix[n,n] cov_xi;
}

// Parameters block
parameters{
  vector[p] betas;
  vector[q] gammas;
  vector[n] delta;
  vector[n] xi;

  real<lower = 0> tau_delta;
  real<lower = 0> tau_xi;
  real<lower = 0> zeta_e;
  real<lower = 0, upper = 1> theta_e;
}

//Parameter block without priori
transformed parameters{
  vector[n] theta;
  vector[n] zeta;
  vector[n] lpredt;
  vector[n] lpredz;


  if(p!=0){
    if(spatial_theta == 1){
      lpredt = X * betas + delta;
    }
    else{
      lpredt = X * betas;
    }
    theta = exp(lpredt)./(1+exp(lpredt));
  }
  if(q!=0){
    if(spatial_zeta == 1){
        lpredz = W * gammas + xi;
    }
    else{
        lpredz = W * gammas;
    }
    zeta = exp(lpredz);
  }
}


// Model block
model{
  //Likelihood
  if(p!=0){
    if(q!=0){
      Y ~ beta(zeta .* theta, zeta .* (1-theta));
    }
    else{
      for(i in 1:n){
        Y[i] ~ beta(zeta_e*theta[i],zeta_e*(1-theta[i]));
      }
    }
  }
  else{
    if(p!=0){
      for(i in 1:n){
        Y[i] ~ beta(zeta[i]*theta_e,zeta[i]*(1-theta_e));
      }
    }
  }

  // Priori distributions
  betas ~ normal(mean_betas,variance_betas);
  theta_e ~ beta(a,b);
  gammas ~ normal(mean_gammas,variance_gammas);
  zeta_e ~ gamma(a,b);
  if(spatial_theta == 1){
    tau_delta ~ gamma(atau_delta,btau_delta);
    delta ~ multi_normal(rep_vector(0,n),tau_delta * cov_delta);
  }
  if(spatial_zeta == 1){
    tau_xi ~ gamma(atau_xi,btau_xi);
    xi ~ multi_normal(rep_vector(0,n),tau_xi * cov_xi);
  }
}
