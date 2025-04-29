
data{
  
  int<lower=1> n;   // number of countries 
  int<lower=1> p;   // number of predictors
  int<lower=1> nTime;   // number of time points
  vector[nTime*n*(n-1)] yvec;   // trade volumes
  matrix[nTime*n*(n-1),p] Xmatvec;   // covariates
  int<lower=0,upper=nTime*n*(n-1)> nNz;   // number of nonzero trades
  int<lower=1,upper=nTime*n*(n-1)> nzid[nNz];   // nonzero trade occurence id-s training
  int offdiagindex[n-1,n];
  int<lower=2> K;   // latent dimension
  
}

parameters{
  
  real mu0;
  vector[n] mui;
  vector[nTime*n] eta;
  vector<lower=0, upper=1>[n] phi;
  
  vector[nTime] logZ11;
  vector[nTime*(n-1 + n*(K-1) - (K*(K-1))/2)] Zrestv;
  
  vector[p] betareg;
  real<lower=0, upper=1> alpha;
  real<lower=0> sigma2;
  
}

transformed parameters {
  
  vector[nTime*n] h = mu0 + eta;
  matrix[nTime*n,K] Z = rep_matrix(1e-20, nTime*n, K);
  vector<lower=0>[nTime*n] normZ;
  matrix[n,n] Lmat_t;
  matrix[n,n] Lmat_itoj_t;
  matrix[n,n] Lmat_ij_t;
  vector[nTime*n*(n-1)] Lvec;
  vector[nTime*n*(n-1)] nzmeanvec;
  vector[nTime*n*(n-1)] loglik = rep_vector(0, nTime*n*(n-1));
  
  for(t in 1:nTime){
    
    if(t==1){
      
      h[((t-1)*n + 1):(t*n)] += mui;
    
    }else if(t>1){
      
      h[((t-1)*n + 1):(t*n)] += mui + phi .* (h[((t-2)*n + 1):((t-1)*n)] - mu0 - mui);
    
    }
    
    Z[(t-1)*n + 1,1] = exp(logZ11[t]);
    Z[((t-1)*n + 2):(t*n),1] = Zrestv[((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + 1):((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1)];
    for(k in 2:K){
      
      Z[((t-1)*n + k):(t*n),k] = Zrestv[((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1 + n*(k-2) - ((k-1)*(k-2))/2 + 1):((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1 + n*(k-1) - (k*(k-1))/2)];
      
    }
    
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
    
  }
  
  for(l in 1:nNz){
    
    loglik[nzid[l]] += normal_lpdf(yvec[nzid[l]] | nzmeanvec[nzid[l]], sqrt(sigma2));
    
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
  
  for(k in 2:K){
    
    target += normal_lpdf(Zrestv[(n-1 + n*(k-2) - ((k-1)*(k-2))/2 + 1):(n-1 + n*(k-1) - (k*(k-1))/2)] ./ exp(h[k:n]/2) | 0, 1) -
              h[k:n]/2;
    
  }
  
  for(t in 2:nTime){
  
    target += normal_lpdf((logZ11[t]-logZ11[t-1])/exp(h[(t-1)*n + 1]/2) | 0, 1) -
              h[(t-1)*n + 1]/2;
    target += normal_lpdf((Zrestv[((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + 1):((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1)] -
                           Zrestv[((t-2)*(n-1 + n*(K-1) - (K*(K-1))/2) + 1):((t-2)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1)]) ./ exp(h[((t-1)*n + 2):(t*n)]/2) | 0, 1) -
              h[((t-1)*n + 2):(t*n)]/2;
    
    for(k in 2:K){
      
      target += normal_lpdf((Zrestv[((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1 + n*(k-2) - ((k-1)*(k-2))/2 + 1):((t-1)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1 + n*(k-1) - (k*(k-1))/2)]-
                             Zrestv[((t-2)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1 + n*(k-2) - ((k-1)*(k-2))/2 + 1):((t-2)*(n-1 + n*(K-1) - (K*(K-1))/2) + n-1 + n*(k-1) - (k*(k-1))/2)]) ./ exp(h[((t-1)*n + k):(t*n)]/2) | 0, 1) -
                h[((t-1)*n + k):(t*n)]/2;
      
    }
    
  }
  
  target += normal_lpdf(betareg | 0, 1e+5);
  target += -log(sigma2);
  target += uniform_lpdf(alpha | 0, 1);
  
  target += loglik;
  
}
