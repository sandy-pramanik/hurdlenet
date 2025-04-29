
data{
  
  int<lower=1> n;   // number of countries 
  int<lower=1> p;   // number of predictors
  int<lower=1> nTime;   // number of time points
  matrix[nTime*n*(n-1),p] Xmatvec;   // covariates
  int<lower=0,upper=1> deltavec[nTime*n*(n-1)];   // occurences
  int offdiagindex[n-1,n];
  
}

parameters{
  
  real mu0;
  vector[n] mui;
  vector[nTime*n] eta;
  vector<lower=0, upper=1>[n] phi;
  
  vector[nTime] logZ11;
  vector[nTime*(n-1)] Zrestv;
  
  vector[p] betareg;
  real<lower=0, upper=1> alpha;
  
}

transformed parameters {
  
  vector[nTime*n] h = mu0 + eta;
  matrix[nTime*n,1] Z;
  vector<lower=0>[nTime*n] normZ;
  matrix[n,n] Lmat_t;
  matrix[n,n] Lmat_itoj_t;
  matrix[n,n] Lmat_ij_t;
  vector[nTime*n*(n-1)] Lvec;
  vector[nTime*n*(n-1)] nzmeanvec;
  vector<lower=0, upper=1>[nTime*n*(n-1)] probitprobvec;
  vector[nTime*n*(n-1)] loglik;
  
  for(t in 1:nTime){
    
    if(t==1){
      
      h[((t-1)*n + 1):(t*n)] += mui;
    
    }else if(t>1){
      
      h[((t-1)*n + 1):(t*n)] += mui + phi .* (h[((t-2)*n + 1):((t-1)*n)] - mu0 - mui);
    
    }
    
    Z[(t-1)*n + 1,1] = exp(logZ11[t]);
    Z[((t-1)*n + 2):(t*n),1] = Zrestv[((t-1)*(n-1) + 1):(t*(n-1))];
    
    Lmat_t = Z[((t-1)*n + 1):(t*n),] * (Z[((t-1)*n + 1):(t*n),]');
    for(j in 1:n){
      
      normZ[(t-1)*n + j] = sqrt(sum(Z[(t-1)*n + j,] .* Z[(t-1)*n + j,]));
      
      Lmat_itoj_t[,j] = Lmat_t[,j]/normZ[(t-1)*n + j];
      
    }
    
    Lmat_ij_t = alpha*Lmat_itoj_t + (1-alpha)*(Lmat_itoj_t');
    
    for(j in 1:n){
      
      Lvec[((t-1)*n*(n-1) + (j-1)*(n-1) + 1):((t-1)*n*(n-1) + j*(n-1))] = Lmat_ij_t[offdiagindex[,j],j];
      
    }
    
    nzmeanvec[((t-1)*n*(n-1) + 1):(t*n*(n-1))] = to_vector(Xmatvec[((t-1)*n*(n-1) + 1):(t*n*(n-1)),]*betareg) +
                                                 Lvec[((t-1)*n*(n-1) + 1):(t*n*(n-1))];
    
    for(l in 1:(n*(n-1))){
      
      probitprobvec[(t-1)*n*(n-1) + l] = Phi_approx(nzmeanvec[(t-1)*n*(n-1) + l]);
      
      loglik[(t-1)*n*(n-1) + l] = bernoulli_lpmf(deltavec[(t-1)*n*(n-1) + l] | probitprobvec[(t-1)*n*(n-1) + l]);
                                                        
    }
    
  }
  
}

model{
  
  target += mu0/2 - log1p_exp(mu0) - log(pi());
  target += mui/2 - log1p_exp(mui) - log(pi());
  target += eta/2 - log1p_exp(eta) - log(pi());
  
  target += uniform_lpdf(phi | 0, 1);
  
  target += normal_lpdf(logZ11[1]/exp(h[1]/2) | 0, 1) -
            h[1]/2;
  target += normal_lpdf(Zrestv[1:(n-1)] ./ exp(h[2:n]/2) | 0, 1) -
            h[2:n]/2;
  for(t in 2:nTime){
    
    target += normal_lpdf((logZ11[t]-logZ11[t-1])/exp(h[(t-1)*n + 1]/2) | 0, 1) -
              h[(t-1)*n + 1]/2;
    target += normal_lpdf((Zrestv[((t-1)*(n-1) + 1):(t*(n-1))]-Zrestv[((t-2)*(n-1) + 1):((t-1)*(n-1))]) ./ exp(h[((t-1)*n + 2):(t*n)]/2) | 0, 1) -
              h[((t-1)*n + 2):(t*n)]/2;
    
  }
  
  target += normal_lpdf(betareg | 0, 1e+5);
  target += uniform_lpdf(alpha | 0, 1);
  
  target += loglik;
  
}
