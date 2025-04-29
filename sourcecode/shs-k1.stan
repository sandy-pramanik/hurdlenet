
data{
  
  int<lower=1> n;   // number of countries 
  int<lower=1> p;   // number of predictors
  int<lower=1> nTime;   // number of time points
  vector[nTime*n*(n-1)] yvec;   // trade volumes
  matrix[nTime*n*(n-1),p] Xmatvec;   // covariates
  int<lower=0,upper=1> deltavec[nTime*n*(n-1)];   // occurences
  int<lower=0,upper=nTime*n*(n-1)> nNz;   // number of nonzero trades
  int<lower=1,upper=nTime*n*(n-1)> nzid[nNz];   // nonzero trade occurence id-s training
  int offdiagindex[n-1,n];
  
}

parameters{
  
  real mu0;
  vector[n] mui;
  
  real logZ11;
  vector[n-1] Zrestv;
  
  vector[p] betareg_cont;
  real<lower=0, upper=1> alpha;
  real<lower=0> sigma2;
  
  vector[p] betareg_binary;
  real f_shift;
  real<lower=0> f_rate;
  real<lower=0> f_nu;
  
}

transformed parameters {
  
  vector[n] h = mu0 + mui;
  matrix[n,1] Z;
  vector<lower=0>[n] normZ;
  matrix[n,n] Lmat_t;
  matrix[n,n] Lmat_itoj_t;
  matrix[n,n] Lmat_ij_t;
  vector[n*(n-1)] Lvec;
  vector[nTime*n*(n-1)] nzmeanvec;
  vector[nTime*n*(n-1)] Xbetareg_binary;
  vector<lower=0, upper=1>[nTime*n*(n-1)] probitprobvec;
  vector[nTime*n*(n-1)] loglik;
  
  Z[1,1] = exp(logZ11);
  Z[2:n,1] = Zrestv;
    
  Lmat_t = Z * (Z');
  for(j in 1:n){
    
    normZ[j] = sqrt(sum(Z[j,] .* Z[j,]));
    
    Lmat_itoj_t[,j] = Lmat_t[,j]/normZ[j];
    
  }
  
  Lmat_ij_t = alpha*Lmat_itoj_t + (1-alpha)*(Lmat_itoj_t');
  
  for(j in 1:n){
    
    Lvec[((j-1)*(n-1) + 1):(j*(n-1))] = Lmat_ij_t[offdiagindex[,j],j];
    
  }
  
  for(t in 1:nTime){
    
    nzmeanvec[((t-1)*n*(n-1) + 1):(t*n*(n-1))] = to_vector(Xmatvec[((t-1)*n*(n-1) + 1):(t*n*(n-1)),]*betareg_cont) +
                                                 Lvec;
    
    Xbetareg_binary[((t-1)*n*(n-1) + 1):(t*n*(n-1))] = to_vector(Xmatvec[((t-1)*n*(n-1) + 1):(t*n*(n-1)),]*betareg_binary) +
                                                       Lvec;
    
    for(l in 1:(n*(n-1))){
      
      probitprobvec[(t-1)*n*(n-1) + l] = (1 + exp(f_shift-f_rate*Xbetareg_binary[(t-1)*n*(n-1) + l]))^(-1/f_nu);
      
      loglik[(t-1)*n*(n-1) + l] = bernoulli_lpmf(deltavec[(t-1)*n*(n-1) + l] | probitprobvec[(t-1)*n*(n-1) + l]);
                                                        
    }
    
  }
  
  for(l in 1:nNz){
    
    loglik[nzid[l]] += normal_lpdf(yvec[nzid[l]] | nzmeanvec[nzid[l]], sqrt(sigma2));
    
  }
  
}

model{
  
  target += mu0/2 - log1p_exp(mu0) - log(pi());
  target += mui/2 - log1p_exp(mui) - log(pi());
  
  target += normal_lpdf(logZ11/exp(h[1]/2) | 0, 1) -
            h[1]/2;
  target += normal_lpdf(Zrestv ./ exp(h[2:n]/2) | 0, 1) -
            h[2:n]/2;
  
  target += normal_lpdf(betareg_cont | 0, 1e+5);
  target += -log(sigma2);
  target += uniform_lpdf(alpha | 0, 1);
  
  target += normal_lpdf(betareg_binary | 0, 1e+5);
  target += normal_lpdf(f_shift | 0, 1e+5);
  target += log(2) + normal_lpdf(f_rate | 0, 1e+5);
  target += gamma_lpdf(f_nu | 2, (2-1)/1.0);
  
  target += loglik;
  
}
