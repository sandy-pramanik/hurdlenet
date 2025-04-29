

# load libraries ----
# library(loo, lib.loc="/users/spramani/R/4.2.x")
# library(abind, lib.loc="/users/spramani/R/4.2.x")

library(doParallel)
library(rstan)
rstan_options(auto_write = TRUE)


# stan object ----
D1dhs_k1.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                          'D1dhs-k1.stan'),
                                         auto_write = rstan_options("auto_write"=TRUE))
D1dhs_k2toK.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                             'D1dhs-k2toK.stan'),
                                            auto_write = rstan_options("auto_write"=TRUE))
D0dhs_k1.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                          'D0dhs-k1.stan'),
                                         auto_write = rstan_options("auto_write"=TRUE))
D0dhs_k2toK.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                             'D0dhs-k2toK.stan'),
                                            auto_write = rstan_options("auto_write"=TRUE))
shs_k1.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                        'shs-k1.stan'),
                                       auto_write = rstan_options("auto_write"=TRUE))
shs_k2toK.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                           'shs-k2toK.stan'),
                                          auto_write = rstan_options("auto_write"=TRUE))
# nolatent.stan_object = rstan::stan_model(file = file.path(source.code.path,
#                                                           'nolatent.stan'),
#                                          auto_write = rstan_options("auto_write"=TRUE))
indep_cont_k1.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                               'indep-cont-k1.stan'),
                                              auto_write = rstan_options("auto_write"=TRUE))
indep_cont_k2toK.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                                  'indep-cont-k2toK.stan'),
                                                 auto_write = rstan_options("auto_write"=TRUE))
indep_binary_k1.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                                 'indep-binary-k1.stan'),
                                                auto_write = rstan_options("auto_write"=TRUE))
indep_binary_k2toK.stan_object = rstan::stan_model(file = file.path(source.code.path,
                                                                    'indep-binary-k2toK.stan'),
                                                   auto_write = rstan_options("auto_write"=TRUE))


# helper function for fit given a data ----
## combining outputs for D0dhs, D1dhs ----
hurdlenet_combine = function(...){
  
  list.combined = list(...)
  length.list.combined = length(list.combined)
  list.out = list.combined[[1]]
  
  for(k in 2:length.list.combined){
    
    list.out$betareg_cont = abind::abind(list.out$betareg_cont,
                                         list.combined[[k]]$betareg_cont,
                                         along = 3)
    
    list.out$betareg_binary = abind::abind(list.out$betareg_binary,
                                         list.combined[[k]]$betareg_binary,
                                         along = 3)
    
    list.out$alpha = cbind(list.out$alpha, list.combined[[k]]$alpha)
    
    list.out$sigma2 = cbind(list.out$sigma2, list.combined[[k]]$sigma2)
    
    list.out$f_shift = cbind(list.out$f_shift, list.combined[[k]]$f_shift)
    
    list.out$f_rate = cbind(list.out$f_rate, list.combined[[k]]$f_rate)
    
    list.out$f_nu = cbind(list.out$f_nu, list.combined[[k]]$f_nu)
    
    list.out$h = abind::abind(list.out$h,
                              list.combined[[k]]$h,
                              along = 3)
    
    list.out$Z = abind::abind(list.out$Z,
                              list.combined[[k]]$Z,
                              along = 4)
    
    list.out$loglik = abind::abind(list.out$loglik, 
                                   list.combined[[k]]$loglik,
                                   along = 3)
    
    list.out$ic = cbind(list.out$ic, list.combined[[k]]$ic)
    
    list.out$mcmc.diagnostic = cbind(list.out$mcmc.diagnostic,
                                     list.combined[[k]]$mcmc.diagnostic)
    
    if("mu0" %in% names(list.out)){
      
      list.out$mu0 = cbind(list.out$mu0,
                           list.combined[[k]]$mu0)
      
    }
    
    if("mui" %in% names(list.out)){
      
      
      list.out$mui = abind::abind(list.out$mui,
                                  list.combined[[k]]$mui,
                                  along = 3)
      
    }
    
    if("eta" %in% names(list.out)){
      
      list.out$eta = abind::abind(list.out$eta,
                                  list.combined[[k]]$eta,
                                  along = 3)
      
    }
    
    if("phi" %in% names(list.out)){
      
      list.out$phi = abind::abind(list.out$phi,
                                  list.combined[[k]]$phi,
                                  along = 3)
      
    }
    
    if("Lvec" %in% names(list.out)){
      
      list.out$Lvec = abind::abind(list.out$Lvec,
                                   list.combined[[k]]$Lvec,
                                   along = 3)
      
    }
    
    if("nzmeanvec" %in% names(list.out)){
      
      list.out$nzmeanvec = abind::abind(list.out$nzmeanvec,
                                        list.combined[[k]]$nzmeanvec,
                                        along = 3)
      
    }
    
    if("probitprobvec" %in% names(list.out)){
      
      list.out$probitprobvec = abind::abind(list.out$probitprobvec,
                                            list.combined[[k]]$probitprobvec,
                                            along = 3)
      
    }
    
  }
  
  return(list.out)
  
}

## combining outputs for shs ----
hurdlenet_combine_shs = function(...){
  
  list.combined = list(...)
  length.list.combined = length(list.combined)
  list.out = list.combined[[1]]
  
  for(k in 2:length.list.combined){
    
    list.out$betareg_cont = abind::abind(list.out$betareg_cont,
                                         list.combined[[k]]$betareg_cont,
                                         along = 3)
    
    list.out$betareg_binary = abind::abind(list.out$betareg_binary,
                                         list.combined[[k]]$betareg_binary,
                                         along = 3)
    
    list.out$alpha = cbind(list.out$alpha, list.combined[[k]]$alpha)
    
    list.out$sigma2 = cbind(list.out$sigma2, list.combined[[k]]$sigma2)
    
    list.out$f_rate = cbind(list.out$f_rate, list.combined[[k]]$f_rate)
    
    list.out$f_shift = cbind(list.out$f_shift, list.combined[[k]]$f_shift)
    
    list.out$f_nu = cbind(list.out$f_nu, list.combined[[k]]$f_nu)
    
    list.out$h = abind::abind(list.out$h,
                              list.combined[[k]]$h,
                              along = 3)
    
    list.out$Z = abind::abind(list.out$Z,
                              list.combined[[k]]$Z,
                              along = 4)
    
    list.out$loglik = abind::abind(list.out$loglik, 
                                   list.combined[[k]]$loglik,
                                   along = 3)
    
    list.out$ic = cbind(list.out$ic, list.combined[[k]]$ic)
    
    list.out$mcmc.diagnostic = cbind(list.out$mcmc.diagnostic,
                                     list.combined[[k]]$mcmc.diagnostic)
    
    if("mu0" %in% names(list.out)){
      
      list.out$mu0 = cbind(list.out$mu0,
                           list.combined[[k]]$mu0)
      
    }
    
    if("mui" %in% names(list.out)){
      
      list.out$mui = abind::abind(list.out$mui,
                                  list.combined[[k]]$mui,
                                  along = 3)
      
    }
    
    if("Lvec" %in% names(list.out)){
      
      list.out$Lvec = abind::abind(list.out$Lvec,
                                   list.combined[[k]]$Lvec,
                                   along = 3)
      
    }
    
    if("nzmeanvec" %in% names(list.out)){
      
      list.out$nzmeanvec = abind::abind(list.out$nzmeanvec,
                                        list.combined[[k]]$nzmeanvec,
                                        along = 3)
      
    }
    
    if("probitprobvec" %in% names(list.out)){
      
      list.out$probitprobvec = abind::abind(list.out$probitprobvec,
                                            list.combined[[k]]$probitprobvec,
                                            along = 3)
      
    }
    
  }
  
  return(list.out)
  
}

## combining outputs for indep ----
### combining in cont and binary ----
hurdlenet_combine_indep_type = function(...){
  
  list.combined = list(...)
  length.list.combined = length(list.combined)
  list.out = list.combined[[1]]
  
  for(k in 2:length.list.combined){
    
    list.out$betareg = abind::abind(list.out$betareg,
                                    list.combined[[k]]$betareg,
                                    along = 3)
    
    list.out$alpha = cbind(list.out$alpha, list.combined[[k]]$alpha)
    
    list.out$sigma2 = cbind(list.out$sigma2, list.combined[[k]]$sigma2)
    
    list.out$h = abind::abind(list.out$h,
                              list.combined[[k]]$h,
                              along = 3)
    
    list.out$Z = abind::abind(list.out$Z,
                              list.combined[[k]]$Z,
                              along = 4)
    
    list.out$loglik = abind::abind(list.out$loglik, 
                                   list.combined[[k]]$loglik,
                                   along = 3)
    
    list.out$ic = cbind(list.out$ic, list.combined[[k]]$ic)
    
    list.out$mcmc.diagnostic = cbind(list.out$mcmc.diagnostic,
                                     list.combined[[k]]$mcmc.diagnostic)
    
    if("mu0" %in% names(list.out)){
      
      list.out$mu0 = cbind(list.out$mu0,
                           list.combined[[k]]$mu0)
      
    }
    
    if("mui" %in% names(list.out)){
      
      list.out$mui = abind::abind(list.out$mui,
                                  list.combined[[k]]$mui,
                                  along = 3)
      
    }
    
    if("eta" %in% names(list.out)){
      
      list.out$eta = abind::abind(list.out$eta,
                                  list.combined[[k]]$eta,
                                  along = 3)
      
    }
    
    if("phi" %in% names(list.out)){
      
      list.out$phi = abind::abind(list.out$phi,
                                  list.combined[[k]]$phi,
                                  along = 3)
      
    }
    
    if("Lvec" %in% names(list.out)){
      
      list.out$Lvec = abind::abind(list.out$Lvec,
                                   list.combined[[k]]$Lvec,
                                   along = 3)
      
    }
    
    if("nzmeanvec" %in% names(list.out)){
      
      list.out$nzmeanvec = abind::abind(list.out$nzmeanvec,
                                        list.combined[[k]]$nzmeanvec,
                                        along = 3)
      
    }
    
    if("probitprobvec" %in% names(list.out)){
      
      list.out$probitprobvec = abind::abind(list.out$probitprobvec,
                                            list.combined[[k]]$probitprobvec,
                                            along = 3)
      
    }
    
  }
  
  return(list.out)
  
}

### combining cont and binary ----
hurdlenet_combine_indep = function(...){
  
  list.combined = list(...)
  length.list.combined = length(list.combined)
  list.out = list.combined[[1]]
  
  for(k in 2:length.list.combined){
    
    list.out$betareg = abind::abind(list.out$betareg,
                                    list.combined[[k]]$betareg,
                                    along = 4)
    
    list.out$alpha = abind::abind(list.out$alpha, 
                                  list.combined[[k]]$alpha,
                                  along = 3)
    
    list.out$sigma2 = abind::abind(list.out$sigma2, 
                                   list.combined[[k]]$sigma2,
                                   along = 3)
    
    list.out$h = abind::abind(list.out$h,
                              list.combined[[k]]$h,
                              along = 4)
    
    list.out$Z = abind::abind(list.out$Z,
                              list.combined[[k]]$Z,
                              along = 5)
    
    list.out$loglik = abind::abind(list.out$loglik, 
                                   list.combined[[k]]$loglik,
                                   along = 4)
    
    list.out$ic = abind::abind(list.out$ic, list.combined[[k]]$ic,
                               along = 3)
    
    list.out$mcmc.diagnostic = abind::abind(list.out$mcmc.diagnostic,
                                            list.combined[[k]]$mcmc.diagnostic,
                                            along = 3)
    
    if("mu0" %in% names(list.out)){
      
      list.out$mu0 = abind::abind(list.out$mu0,
                                  list.combined[[k]]$mu0, 
                                  along = 3)
      
    }
    
    if("mui" %in% names(list.out)){
      
      list.out$mui = abind::abind(list.out$mui,
                                  list.combined[[k]]$mui,
                                  along = 4)
      
    }
    
    if("eta" %in% names(list.out)){
      
      list.out$eta = abind::abind(list.out$eta,
                                  list.combined[[k]]$eta,
                                  along = 4)
      
    }
    
    if("phi" %in% names(list.out)){
      
      list.out$phi = abind::abind(list.out$phi,
                                  list.combined[[k]]$phi,
                                  along = 4)
      
    }
    
    if("Lvec" %in% names(list.out)){
      
      list.out$Lvec = abind::abind(list.out$Lvec,
                                   list.combined[[k]]$Lvec,
                                   along = 4)
      
    }
    
    if("nzmeanvec" %in% names(list.out)){
      
      list.out$nzmeanvec = abind::abind(list.out$nzmeanvec,
                                        list.combined[[k]]$nzmeanvec,
                                        along = 4)
      
    }
    
    if("probitprobvec" %in% names(list.out)){
      
      list.out$probitprobvec = abind::abind(list.out$probitprobvec,
                                            list.combined[[k]]$probitprobvec,
                                            along = 4)
      
    }
    
  }
  
  return(list.out)
  
}


# helper function for simulation ----
## combining outputs for nolatent ----
sim_combine_latent = function(...){
  
  list.combined = list(...)
  length.list.combined = length(list.combined)
  list.out = list.combined[[1]]
  
  for(k in 2:length.list.combined){
    
    list.out$ic = abind::abind(list.out$ic,
                               list.combined[[k]]$ic,
                               along = 3)
    
    list.out$betareg_cont = abind::abind(list.out$betareg_cont,
                                         list.combined[[k]]$betareg_cont,
                                         along = 4)
    
    list.out$sigma = abind::abind(list.out$sigma,
                                  list.combined[[k]]$sigma,
                                  along = 3)
    
    list.out$nzmean_train = abind::abind(list.out$nzmean_train,
                                         list.combined[[k]]$nzmean_train,
                                         along = 4)
    
    list.out$probitprob_train = abind::abind(list.out$probitprob_train,
                                             list.combined[[k]]$probitprob_train,
                                             along = 4)
    
    list.out$betareg_binary = abind::abind(list.out$betareg_binary,
                                           list.combined[[k]]$betareg_binary,
                                           along = 4)
    
    list.out$y_test = abind::abind(list.out$y_test,
                                   list.combined[[k]]$y_test,
                                   along = 4)
    
    list.out$probitprob_test = abind::abind(list.out$probitprob_test,
                                            list.combined[[k]]$probitprob_test,
                                            along = 4)
    
  }
  
  return(list.out)
  
}

## combining outputs for nolatent ----
sim_combine_nolatent = function(...){
  
  list.combined = list(...)
  length.list.combined = length(list.combined)
  list.out = list.combined[[1]]
  
  for(k in 2:length.list.combined){
    
    list.out$ic = cbind(list.out$ic,
                        list.combined[[k]]$ic)
    
    list.out$betareg_cont = abind::abind(list.out$betareg_cont,
                                         list.combined[[k]]$betareg_cont,
                                         along = 3)
    
    list.out$sigma = cbind(list.out$sigma,
                           list.combined[[k]]$sigma)
    
    list.out$nzmean_train = abind::abind(list.out$nzmean_train,
                                         list.combined[[k]]$nzmean_train,
                                         along = 3)
    
    list.out$probitprob_train = abind::abind(list.out$probitprob_train,
                                             list.combined[[k]]$probitprob_train,
                                             along = 3)
    
    list.out$betareg_binary = abind::abind(list.out$betareg_binary,
                                           list.combined[[k]]$betareg_binary,
                                           along = 3)
    
    list.out$y_test = cbind(list.out$y_test,
                            list.combined[[k]]$y_test)
    
    list.out$probitprob_test = cbind(list.out$probitprob_test,
                                     list.combined[[k]]$probitprob_test)
    
  }
  
  return(list.out)
  
}


# fitting hurdlenet for different model choices ----
#
#
## input
# datalist : named list of following components
#
#           "n"           : integer. number of nodes
#
#           "p"           : integer. number of covariates corresponding to each edge.
#                           equals to 2*p1 + p2 where p1 and p2 are node and node-pair specific covariates.
#
#           "nTime.train" : integer. number of time points in the training data
#
#           "y.train"     : numeric vector. zero-inflated time series data vectorized across node pairs and time points.
#                           length n*(n-1)*nTime.train.
#
#                           let u_ijt denote the edge weight from i to j at time t.
#                           this concatenates the following vector at time t over all time points
#                             (u_21t, u_31t, ... , u_n1t,   # j = 1
#                              u_12t, u_32t, ... , u_n2t,   # j = 2
#                              ... , 
#                              u_1nt, u_2nt, ... , u_(n-1)nt)   # j = n
#
#           "X.train"     : matrix. covariate matrix corresponding to each edge.
#                           dimension n*(n-1)*nTime.train X p.
#
#                           rows correspond to different node pair and time point combinations as in the arrangement of y.train.
#                           columns correspond to (node and node-pair-specific) covariates.
#
#                           that is, X.train[l,] denote the covariate vector corresponding to y.train[l].
#                           X.train[1,] corresponds to u_211, X.train[2,] corresponds to u_311, and so on.
#
#                           if l corresponds to the edge from i to j at time t, X.train[l,] = c(X.node[(i,t),], X.node[(j,t),], X.nodepair[(i,j,t),]), where
#                           with slight abuse of subsetting notation,
#                             X.node[(i,t),] are node-i specific covariates (exporters/source/parent) at time t
#                             X.node[(j,t),] are node-j specific covariates (importers/receiver/child) at time t
#                             X.nodepair[(i,j,t),] are node-pair (i,j) specific covariates at time t
#
# standardize.y : logical. whether y.train should be standardized for modeling. Default is FALSE.
#
# standardize.X : logical. whether covariates (columns of X.train) should be standardized for modeling. Default is FALSE.
#
# columnid.X : integer vector. ids of covariates to standardize. subset of 1:p
#
# model.choice : character. Methods compared in Pramanik et al. (2025+).
#                'D1dhs' for Hurdle-Net(1) (proposed method)
#                'D0dhs' for Hurdle-Net(0)
#                'indep' for independent modeling
#                'static' for static modeling
#
# K.fit : integer. maximum latent dimension K in Pramanik et al. (2025+).
#
# nBurn : integer. burn-in for posterior sampling in STAN. Default 2000.
#
# nMCMC : integer. MCMC sample size (after burn-in) for posterior inference. Default 1000.
#
# adapt.delta.stan : numeric in (0,1). adapt_delta in STAN sampling. Default .8.
#
# nCore.K : integer. number of cores to parallely fit the models for k=1:K.fit. Default 1 (fits sequentially).
#
# nCore.indep : integer. number of cores to parallely fit binary and continuous models for model.choice="indep". Default 1 (fits sequentially).
#
# verbose : 0,1 or 2. amount of intermediate progress report. 0 (no), 1 (moderate), 2 (detailed).
#
# source.code.path : character. path to the "sourcecode" folder.
#
# saveoutput : logical. whether to save output. Default is TRUE.
#
# saveoutput_type : null or character vector. name of the parameters to save outputs for. Default is NULL (saves everything).
#
# output.dir : character. name of the folder to save output in the working directory. Default 'hurdlenet_output'.
#
# output.filename : character. name of the output file. Default "model.choice".
#
#
## output
# named list of following components
#
# MCMCout : named list of posterior outputs
#
#         for model.choice = 'D1dhs' : named list with following components. notations are similar to Pramanik et al. (2025+).
#             "mu0" : matrix. dimension nMCMC X K.fit. posterior samples of \mu_0.
#             "mui" : array. dimension nMCMC X n X K.fit. posterior samples of \mu_i.
#             "phi" : array. dimension nMCMC X n X K.fit. posterior samples of \phi_i.
#             "betareg_cont" : array. dimension nMCMC X p X K.fit. posterior samples of \beta in the continuous model.
#             "alpha" : matrix. dimension nMCMC X K.fit. posterior samples of \alpha.
#             "sigma2" : matrix. dimension nMCMC X K.fit. posterior samples of \sigma^2.
#             "betareg_binary" : array. dimension nMCMC X p X K.fit. posterior samples of \beta in the binary model.
#             "f_shift" : matrix. dimension nMCMC X K.fit. posterior samples of a.
#             "f_rate" : matrix. dimension nMCMC X K.fit. posterior samples of b.
#             "f_nu" : matrix. dimension nMCMC X K.fit. posterior samples of \nu.
#             "h" : array. dimension nMCMC X (n*nTime.train) X K.fit. posterior samples of h_{it} in the dynamic horseshoe process (DHS) prior.
#             "Z" : array. dimension nMCMC X (n*nTime.train) X K.fit X K.fit. Z[,,1:k,k] are posterior samples of latent variables Z.
#             "loglik" : array. dimension nMCMC X (n*(n-1)*nTime.train) X K.fit. log-likelihood for each observation (combination of a country pair and a time point: (i, j, t)) for each posterior sample.
#             "lp__" : log posterior values from STAN fit.
#             "ic" : data.frame. (number of outputs) X K.fit. outputs from loo::loo() and loo::waic() for each model fit with k=1:K.fit.
#             "mcmc.diagnostic" : data.frame. (number of outputs) X K.fit. mcmc diagnostic outputs for each model fit with k=1:K.fit.
#
#         outputs are similarly structured for other model.choice inputs.
#
#         2 dimensions in outputs for model.choice="indep" correspond to continuous and binary models.
#         for example, for betareg, it's an array with dimension nMCMC X p X K.fit X 2 models (continuous and binary)
#
# input : named list of inputs to the function.
#         Additionally sd.y and sd.X are values used to standardize nonzero datalist$y.train and covariates (columns of datalist$X.train[,columnid.X])

hurdlenet = function(datalist = NULL, 
                     standadize.y = F, standadize.X = F, columnid.X = NULL,
                     model.choice = 'D1dhs', K.fit = 3,
                     nBurn = 2000, nMCMC = 1000, adapt.delta.stan = .8,
                     nCore.K = 1, nCore.indep = 1,
                     verbose = 2,
                     source.code.path = file.path(getwd(), 'hurdlenet-sourcecode'),
                     saveoutput = T, saveoutput_type = NULL,
                     output.dir = 'hurdlenet_output', output.filename = NULL){
  
  
  if(missing(nCore.K)) nCore.K = parallel::detectCores() - 1
  
  input.list = as.list(environment())
  
  
  # creating delta.train and nzid.train ====
  datalist$nzid.train = datalist$y.train!=0
  datalist$delta.train = as.numeric(datalist$nzid.train)
  
  
  # standardize y ====
  sd.y = 1
  if(standadize.y){
    
    sd.y = sd(datalist$y.train[datalist$nzid.train])
    y.std = datalist$y.train/sd.y
    
  }else{y.std = datalist$y.train}
  
  
  # standardize X ====
  sd.X = rep(1, ncol(datalist$X.train))
  if(standadize.X){
    
    if(is.null(columnid.X)) columnid.X = 1:ncol(datalist$X.train)
    
    sd.X[columnid.X] = apply(X = datalist$X.train[,columnid.X],
                             2, sd)
    X.std = scale(x = datalist$X.train, center = F, scale = sd.X)
    
  }else{X.std = datalist$X.train}
  
  
  # starting modeling ====
  if(model.choice=='D1dhs'){
    
    ## D1dhs ====
    
    if(length(intersect(verbose, 1:2))>0){
      
      cat('\n')
      print(paste0('Fitting hurdlenet ...'))
      
    }
    
    # saveoutput_type = c('mu0', 'mui', 'eta', 'phi',
    #                     'betareg_cont', 'alpha', 'sigma2',
    #                     'betareg_binary', 'f_shift', 'f_rate', 'f_nu',
    #                     'h', 'Z', 'Lvec', 'nzmeanvec', 'probitprobvec',
    #                     'loglik')
    
    if(is.null(saveoutput_type)){
      
      param_return = c('mu0', 'mui', 'phi',
                       'betareg_cont', 'alpha', 'sigma2',
                       'betareg_binary', 'f_shift', 'f_rate', 'f_nu',
                       'h', 'Z',
                       'loglik')
      
    }else if(is.character(saveoutput_type)){
      
      param_return = saveoutput_type
      
    }
    
    doParallel::registerDoParallel(cores = nCore.K)
    hurdlenet.out = foreach::foreach(k = 1:K.fit, .combine = 'hurdlenet_combine' , .multicombine = T) %dopar% {
      
      set.seed(k)
      
      if(length(intersect(verbose, 1:2))>0){
        
        cat('\n')
        print(paste0('Model fitting with K=', k,' ...'))
        
      }
      
      # storage  
      Zstore = array(dim = c(nMCMC, datalist$n*datalist$nTime.train, K.fit))
      Z_t_temp = matrix(nrow = datalist$n, ncol = k)
      to_diagnose_Z = do.call('rbind',
                              lapply(1:datalist$nTime.train,
                                     FUN = function(t){
                                       
                                       row(Z_t_temp)>=col(Z_t_temp)
                                       
                                     }))
      rm(Z_t_temp)
      
      ### stan fit ====
      if(k==1){
        
        #### k=1 ====
        stanfit = rstan::sampling(D1dhs_k1.stan_object,
                                  pars = param_return,
                                  include = T,
                                  data = list('n' = datalist$n, 
                                              'p' = datalist$p,
                                              'nTime' = datalist$nTime.train,
                                              'yvec' = y.std,
                                              'Xmatvec' = X.std,
                                              'deltavec' = datalist$delta.train,
                                              'nNz' = sum(datalist$nzid),
                                              'nzid' = which(datalist$nzid),
                                              'offdiagindex' = do.call('cbind',
                                                                       lapply(1:datalist$n,
                                                                              FUN = function(j){
                                                                                
                                                                                (1:datalist$n)[-j]
                                                                                
                                                                              }))),
                                  chains = 1, iter = nBurn + nMCMC, 
                                  warmup = nBurn,
                                  control = list('adapt_delta' = adapt.delta.stan),
                                  refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
        
      }else{
        
        #### k>1 ====
        stanfit = rstan::sampling(D1dhs_k2toK.stan_object,
                                  pars = param_return,
                                  include = T,
                                  data = list('n' = datalist$n, 
                                              'p' = datalist$p,
                                              'nTime' = datalist$nTime.train,
                                              'yvec' = y.std, 
                                              'Xmatvec' = X.std,
                                              'deltavec' = datalist$delta.train,
                                              'nNz' = sum(datalist$nzid),
                                              'nzid' = which(datalist$nzid),
                                              'offdiagindex' = do.call('cbind',
                                                                       lapply(1:datalist$n,
                                                                              FUN = function(j){
                                                                                
                                                                                (1:datalist$n)[-j]
                                                                                
                                                                              })),
                                              'K' = k),
                                  chains = 1, iter = nBurn + nMCMC, 
                                  warmup = nBurn,
                                  control = list('adapt_delta' = adapt.delta.stan),
                                  refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
        
      }   # end of if-else loop of k=1 or >1
      
      # posterior samples
      MCMCout = rstan::extract(stanfit)
      
      ### model diagnostics considering model parameters ====
      #### max Rhat ====
      max_Rhat = max(apply(X = MCMCout$betareg_cont, 2,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           }),
                     rstan::Rhat(c(MCMCout$alpha)),
                     rstan::Rhat(c(MCMCout$sigma2)),
                     apply(X = MCMCout$betareg_binary, 2,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           }),
                     rstan::Rhat(c(MCMCout$f_shift)),
                     rstan::Rhat(c(MCMCout$f_rate)),
                     rstan::Rhat(c(MCMCout$f_nu)),
                     apply(X = MCMCout$Z, 2:3,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           })[to_diagnose_Z])
      
      #### min bulk ESS ====
      min_ess_bulk = min(apply(X = MCMCout$betareg_cont, 2,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               }),
                         rstan::ess_bulk(c(MCMCout$alpha)),
                         rstan::ess_bulk(c(MCMCout$sigma2)),
                         apply(X = MCMCout$betareg_binary, 2,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               }),
                         rstan::ess_bulk(c(MCMCout$f_shift)),
                         rstan::ess_bulk(c(MCMCout$f_rate)),
                         rstan::ess_bulk(c(MCMCout$f_nu)),
                         apply(X = MCMCout$Z, 2:3,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               })[to_diagnose_Z])/nMCMC
      
      #### loo ic ====
      MCMCout$loo.out = loo::loo(MCMCout$loglik, 
                                 r_eff = loo::relative_eff(exp(MCMCout$loglik), 
                                                           chain_id = rep(1, nrow(MCMCout$loglik))), 
                                 cores = 1)
      
      #### waic ====
      MCMCout$waic.out = loo::waic(MCMCout$loglik)
      
      ic.df = rbind(MCMCout$waic.out$estimates,
                    MCMCout$loo.out$estimates)
      
      ic.df.melt = as.numeric(t(ic.df))
      names(ic.df.melt) = paste0(rep(rownames(ic.df), each = ncol(ic.df)),'_',rep(colnames(ic.df), nrow(ic.df)))
      
      ### output list ====
      Zstore[,,1:k] = MCMCout$Z
      MCMCout$Z = Zstore
      
      MCMCout = c(MCMCout,
                  list('ic' = ic.df.melt,
                       'mcmc.diagnostic' = c('max_Rhat' = max_Rhat, 'min_ess_bulk' = min_ess_bulk,
                                             'num_divergent' = rstan::get_num_divergent(stanfit),
                                             'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit))))
      
      if(length(intersect(verbose, 1:2))>0){
        
        cat('\n')
        print(paste0('Model fitting with K=', k,' ... Done.'))
        
      }
      
      print(MCMCout['mcmc.diagnostic'])
      
      MCMCout
      
    }   # end of loop over k
    
    
    ### bringing back to original y, X scale ----
    if("betareg_cont" %in% names(hurdlenet.out)){
      
      for(r in 1:ncol(datalist$X.train)){
        
        hurdlenet.out$betareg_cont[,r,] = (sd.y/sd.X[r])*hurdlenet.out$betareg_cont[,r,]
        hurdlenet.out$betareg_binary[,r,] = hurdlenet.out$betareg_binary[,r,]/sd.X[r]
        
      }
      
    }
    
    if("Z" %in% names(hurdlenet.out)) hurdlenet.out$Z = sd.y*hurdlenet.out$Z
    
    if("sigma2" %in% names(hurdlenet.out)) hurdlenet.out$sigma2 = (sd.y^2)*hurdlenet.out$sigma2
    
    if("f_rate" %in% names(hurdlenet.out)) hurdlenet.out$f_rate = hurdlenet.out$f_rate/sd.y
    
    if("Lvec" %in% names(hurdlenet.out)) hurdlenet.out$Lvec = sd.y*hurdlenet.out$Lvec
    
    if("nzmeanvec" %in% names(hurdlenet.out)) hurdlenet.out$nzmeanvec = sd.y*hurdlenet.out$nzmeanvec
    
    if("h" %in% names(hurdlenet.out)) hurdlenet.out$h = 2*log(sd.y) + hurdlenet.out$h
    
    if("mu0" %in% names(hurdlenet.out)) hurdlenet.out$mu0 = 2*log(sd.y) + hurdlenet.out$mu0
    
  }else if(model.choice=='D0dhs'){
    
    ## D0dhs ====
    
    if(length(intersect(verbose, 1:2))>0){
      
      cat('\n')
      print(paste0('Fitting hurdlenet ...'))
      
    }
    
    # saveoutput_type = c('mu0', 'mui', 'eta', 'phi',
    #                     'betareg_cont', 'alpha', 'sigma2',
    #                     'betareg_binary', 'f_shift', 'f_rate', 'f_nu',
    #                     'h', 'Z', 'Lvec', 'nzmeanvec', 'probitprobvec',
    #                     'loglik')
    
    if(is.null(saveoutput_type)){
      
      param_return = c('mu0', 'mui', 'phi',
                       'betareg_cont', 'alpha', 'sigma2',
                       'betareg_binary', 'f_shift', 'f_rate', 'f_nu',
                       'h', 'Z',
                       'loglik')
      
    }else if(is.character(saveoutput_type)){
      
      param_return = saveoutput_type
      
    }
    
    doParallel::registerDoParallel(cores = nCore.K)
    hurdlenet.out = foreach::foreach(k = 1:K.fit, .combine = 'hurdlenet_combine' , .multicombine = T) %dopar% {
      
      set.seed(k)
      
      if(length(intersect(verbose, 1:2))>0){
        
        cat('\n')
        print(paste0('Model fitting with K=', k,' ...'))
        
      }
      
      # storage  
      Zstore = array(dim = c(nMCMC, datalist$n*datalist$nTime.train, K.fit))
      Z_t_temp = matrix(nrow = datalist$n, ncol = k)
      to_diagnose_Z = do.call('rbind',
                              lapply(1:datalist$nTime.train,
                                     FUN = function(t){
                                       
                                       row(Z_t_temp)>=col(Z_t_temp)
                                       
                                     }))
      rm(Z_t_temp)
      
      ### stan fit ====
      if(k==1){
        
        #### k=1 ====
        stanfit = rstan::sampling(D0dhs_k1.stan_object,
                                  pars = param_return,
                                  include = T,
                                  data = list('n' = datalist$n, 
                                              'p' = datalist$p,
                                              'nTime' = datalist$nTime.train,
                                              'yvec' = y.std, 
                                              'Xmatvec' = X.std,
                                              'deltavec' = datalist$delta.train,
                                              'nNz' = sum(datalist$nzid),
                                              'nzid' = which(datalist$nzid),
                                              'offdiagindex' = do.call('cbind',
                                                                       lapply(1:datalist$n,
                                                                              FUN = function(j){
                                                                                
                                                                                (1:datalist$n)[-j]
                                                                                
                                                                              }))),
                                  chains = 1, iter = nBurn + nMCMC, 
                                  warmup = nBurn,
                                  control = list('adapt_delta' = adapt.delta.stan),
                                  refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
        
      }else{
        
        #### k>1 ====
        stanfit = rstan::sampling(D0dhs_k2toK.stan_object,
                                  pars = param_return,
                                  include = T,
                                  data = list('n' = datalist$n, 
                                              'p' = datalist$p,
                                              'nTime' = datalist$nTime.train,
                                              'yvec' = y.std, 
                                              'Xmatvec' = X.std,
                                              'deltavec' = datalist$delta.train,
                                              'nNz' = sum(datalist$nzid),
                                              'nzid' = which(datalist$nzid),
                                              'offdiagindex' = do.call('cbind',
                                                                       lapply(1:datalist$n,
                                                                              FUN = function(j){
                                                                                
                                                                                (1:datalist$n)[-j]
                                                                                
                                                                              })),
                                              'K' = k),
                                  chains = 1, iter = nBurn + nMCMC, 
                                  warmup = nBurn,
                                  control = list('adapt_delta' = adapt.delta.stan),
                                  refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
        
      }   # end of if-else loop of k=1 or >1
      
      # posterior samples
      MCMCout = rstan::extract(stanfit)
      
      ### model diagnostics considering model parameters ====
      #### max Rhat ====
      max_Rhat = max(apply(X = MCMCout$betareg_cont, 2,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           }),
                     rstan::Rhat(c(MCMCout$alpha)),
                     rstan::Rhat(c(MCMCout$sigma2)),
                     apply(X = MCMCout$betareg_binary, 2,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           }),
                     rstan::Rhat(c(MCMCout$f_shift)),
                     rstan::Rhat(c(MCMCout$f_rate)),
                     rstan::Rhat(c(MCMCout$f_nu)),
                     apply(X = MCMCout$Z, 2:3,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           })[to_diagnose_Z])
      
      #### min bulk ESS ====
      min_ess_bulk = min(apply(X = MCMCout$betareg_cont, 2,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               }),
                         rstan::ess_bulk(c(MCMCout$alpha)),
                         rstan::ess_bulk(c(MCMCout$sigma2)),
                         apply(X = MCMCout$betareg_binary, 2,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               }),
                         rstan::ess_bulk(c(MCMCout$f_shift)),
                         rstan::ess_bulk(c(MCMCout$f_rate)),
                         rstan::ess_bulk(c(MCMCout$f_nu)),
                         apply(X = MCMCout$Z, 2:3,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               })[to_diagnose_Z])/nMCMC
      
      #### loo ic ====
      MCMCout$loo.out = loo::loo(MCMCout$loglik, 
                                 r_eff = loo::relative_eff(exp(MCMCout$loglik), 
                                                           chain_id = rep(1, nrow(MCMCout$loglik))), 
                                 cores = 1)
      
      #### waic ====
      MCMCout$waic.out = loo::waic(MCMCout$loglik)
      
      ic.df = rbind(MCMCout$waic.out$estimates,
                    MCMCout$loo.out$estimates)
      
      ic.df.melt = as.numeric(t(ic.df))
      names(ic.df.melt) = paste0(rep(rownames(ic.df), each = ncol(ic.df)),'_',rep(colnames(ic.df), nrow(ic.df)))
      
      ### output list ====
      Zstore[,,1:k] = MCMCout$Z
      MCMCout$Z = Zstore
      
      MCMCout = c(MCMCout,
                  list('ic' = ic.df.melt,
                       'mcmc.diagnostic' = c('max_Rhat' = max_Rhat, 'min_ess_bulk' = min_ess_bulk,
                                             'num_divergent' = rstan::get_num_divergent(stanfit),
                                             'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit))))
      
      if(length(intersect(verbose, 1:2))>0){
        
        cat('\n')
        print(paste0('Model fitting with K=', k,' ... Done.'))
        
      }
      
      print(MCMCout['mcmc.diagnostic'])
      
      MCMCout
      
    }   # end of loop over k
    
    
    ### bringing back to original y, X scale ----
    if("betareg_cont" %in% names(hurdlenet.out)){
      
      for(r in 1:ncol(datalist$X.train)){
        
        hurdlenet.out$betareg_cont[,r,] = (sd.y/sd.X[r])*hurdlenet.out$betareg_cont[,r,]
        hurdlenet.out$betareg_binary[,r,] = hurdlenet.out$betareg_binary[,r,]/sd.X[r]
        
      }
      
    }
    
    if("Z" %in% names(hurdlenet.out)) hurdlenet.out$Z = sd.y*hurdlenet.out$Z
    
    if("sigma2" %in% names(hurdlenet.out)) hurdlenet.out$sigma2 = (sd.y^2)*hurdlenet.out$sigma2
    
    if("f_rate" %in% names(hurdlenet.out)) hurdlenet.out$f_rate = hurdlenet.out$f_rate/sd.y
    
    if("Lvec" %in% names(hurdlenet.out)) hurdlenet.out$Lvec = sd.y*hurdlenet.out$Lvec
    
    if("nzmeanvec" %in% names(hurdlenet.out)) hurdlenet.out$nzmeanvec = sd.y*hurdlenet.out$nzmeanvec
    
    if("h" %in% names(hurdlenet.out)) hurdlenet.out$h = 2*log(sd.y) + hurdlenet.out$h
    
    if("mu0" %in% names(hurdlenet.out)) hurdlenet.out$mu0 = 2*log(sd.y) + hurdlenet.out$mu0
    
  }else if(model.choice=='static'){
    
    ## static ====
    
    if(length(intersect(verbose, 1:2))>0){
      
      cat('\n')
      print(paste0('Fitting hurdlenet ...'))
      
    }
    
    # saveoutput_type = c('mu0', 'mui',
    #                     'betareg_cont', 'alpha', 'sigma2',
    #                     'betareg_binary', 'f_shift', 'f_rate', 'f_nu',
    #                     'h', 'Z', 'Lvec', 'nzmeanvec', 'probitprobvec',
    #                     'loglik')
    
    if(is.null(saveoutput_type)){
      
      param_return = c('mu0', 'mui',
                       'betareg_cont', 'alpha', 'sigma2',
                       'betareg_binary', 'f_shift', 'f_rate', 'f_nu',
                       'Z', 'Lvec',
                       'loglik')
      
    }else if(is.character(saveoutput_type)){
      
      param_return = saveoutput_type
      
    }
    
    doParallel::registerDoParallel(cores = nCore.K)
    hurdlenet.out = foreach::foreach(k = 1:K.fit, .combine = 'hurdlenet_combine_shs' , .multicombine = T) %dopar% {
      
      set.seed(k)
      
      if(length(intersect(verbose, 1:2))>0){
        
        cat('\n')
        print(paste0('Model fitting with K=', k,' ...'))
        
      }
      
      # storage  
      Zstore = array(dim = c(nMCMC, datalist$n, K.fit))
      Z_temp = matrix(nrow = datalist$n, ncol = k)
      to_diagnose_Z = row(Z_temp)>=col(Z_temp)
      rm(Z_temp)
      
      ### stan fit ====
      if(k==1){
        
        #### k=1 ====
        stanfit = rstan::sampling(shs_k1.stan_object,
                                  pars = param_return,
                                  include = T,
                                  data = list('n' = datalist$n, 
                                              'p' = datalist$p,
                                              'nTime' = datalist$nTime.train,
                                              'yvec' = y.std, 
                                              'Xmatvec' = X.std,
                                              'deltavec' = datalist$delta.train,
                                              'nNz' = sum(datalist$nzid),
                                              'nzid' = which(datalist$nzid),
                                              'offdiagindex' = do.call('cbind',
                                                                       lapply(1:datalist$n,
                                                                              FUN = function(j){
                                                                                
                                                                                (1:datalist$n)[-j]
                                                                                
                                                                              }))),
                                  chains = 1, iter = nBurn + nMCMC, 
                                  warmup = nBurn,
                                  control = list('adapt_delta' = adapt.delta.stan),
                                  refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
        
      }else{
        
        #### k>1 ====
        stanfit = rstan::sampling(shs_k2toK.stan_object,
                                  pars = param_return,
                                  include = T,
                                  data = list('n' = datalist$n, 
                                              'p' = datalist$p,
                                              'nTime' = datalist$nTime.train,
                                              'yvec' = y.std, 
                                              'Xmatvec' = X.std,
                                              'deltavec' = datalist$delta.train,
                                              'nNz' = sum(datalist$nzid),
                                              'nzid' = which(datalist$nzid),
                                              'offdiagindex' = do.call('cbind',
                                                                       lapply(1:datalist$n,
                                                                              FUN = function(j){
                                                                                
                                                                                (1:datalist$n)[-j]
                                                                                
                                                                              })),
                                              'K' = k),
                                  chains = 1, iter = nBurn + nMCMC, 
                                  warmup = nBurn,
                                  control = list('adapt_delta' = adapt.delta.stan),
                                  refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
        
      }   # end of if-else loop of k=1 or >1
      
      # posterior samples
      MCMCout = rstan::extract(stanfit)
      
      ### model diagnostics considering model parameters ====
      #### max Rhat ====
      max_Rhat = max(apply(X = MCMCout$betareg_cont, 2,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           }),
                     rstan::Rhat(c(MCMCout$alpha)),
                     rstan::Rhat(c(MCMCout$sigma2)),
                     apply(X = MCMCout$betareg_binary, 2,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           }),
                     rstan::Rhat(c(MCMCout$f_shift)),
                     rstan::Rhat(c(MCMCout$f_rate)),
                     rstan::Rhat(c(MCMCout$f_nu)),
                     apply(X = MCMCout$Z, 2:3,
                           FUN = function(v){
                             
                             rstan::Rhat(v)
                             
                           })[to_diagnose_Z])
      
      #### min bulk ESS ====
      min_ess_bulk = min(apply(X = MCMCout$betareg_cont, 2,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               }),
                         rstan::ess_bulk(c(MCMCout$alpha)),
                         rstan::ess_bulk(c(MCMCout$sigma2)),
                         apply(X = MCMCout$betareg_binary, 2,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               }),
                         rstan::ess_bulk(c(MCMCout$f_shift)),
                         rstan::ess_bulk(c(MCMCout$f_rate)),
                         rstan::ess_bulk(c(MCMCout$f_nu)),
                         apply(X = MCMCout$Z, 2:3,
                               FUN = function(v){
                                 
                                 rstan::ess_bulk(v)
                                 
                               })[to_diagnose_Z])/nMCMC
      
      #### loo ic ====
      MCMCout$loo.out = loo::loo(MCMCout$loglik, 
                                 r_eff = loo::relative_eff(exp(MCMCout$loglik), 
                                                           chain_id = rep(1, nrow(MCMCout$loglik))), 
                                 cores = 1)
      
      #### waic ====
      MCMCout$waic.out = loo::waic(MCMCout$loglik)
      
      ic.df = rbind(MCMCout$waic.out$estimates,
                    MCMCout$loo.out$estimates)
      
      ic.df.melt = as.numeric(t(ic.df))
      names(ic.df.melt) = paste0(rep(rownames(ic.df), each = ncol(ic.df)),'_',rep(colnames(ic.df), nrow(ic.df)))
      
      ### output list ====
      Zstore[,,1:k] = MCMCout$Z
      MCMCout$Z = Zstore
      
      MCMCout = c(MCMCout,
                  list('ic' = ic.df.melt,
                       'mcmc.diagnostic' = c('max_Rhat' = max_Rhat, 'min_ess_bulk' = min_ess_bulk,
                                             'num_divergent' = rstan::get_num_divergent(stanfit),
                                             'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit))))
      
      if(length(intersect(verbose, 1:2))>0){
        
        cat('\n')
        print(paste0('Model fitting with K=', k,' ... Done.'))
        
      }
      
      print(MCMCout['mcmc.diagnostic'])
      
      MCMCout
      
    }   # end of loop over k
    
    
    ### bringing back to original y, X scale ----
    if("betareg_cont" %in% names(hurdlenet.out)){
      
      for(r in 1:ncol(datalist$X.train)){
        
        hurdlenet.out$betareg_cont[,r,] = (sd.y/sd.X[r])*hurdlenet.out$betareg_cont[,r,]
        hurdlenet.out$betareg_binary[,r,] = hurdlenet.out$betareg_binary[,r,]/sd.X[r]
        
      }
      
    }
    
    if("Z" %in% names(hurdlenet.out)) hurdlenet.out$Z = sd.y*hurdlenet.out$Z
    
    if("sigma2" %in% names(hurdlenet.out)) hurdlenet.out$sigma2 = (sd.y^2)*hurdlenet.out$sigma2
    
    if("f_rate" %in% names(hurdlenet.out)) hurdlenet.out$f_rate = hurdlenet.out$f_rate/sd.y
    
    if("Lvec" %in% names(hurdlenet.out)) hurdlenet.out$Lvec = sd.y*hurdlenet.out$Lvec
    
    if("nzmeanvec" %in% names(hurdlenet.out)) hurdlenet.out$nzmeanvec = sd.y*hurdlenet.out$nzmeanvec
    
    if("h" %in% names(hurdlenet.out)) hurdlenet.out$h = 2*log(sd.y) + hurdlenet.out$h
    
    if("mu0" %in% names(hurdlenet.out)) hurdlenet.out$mu0 = 2*log(sd.y) + hurdlenet.out$mu0
    
  }else if(model.choice=='indep'){
    
    ## indep ====
    
    if(length(intersect(verbose, 1:2))>0){
      
      cat('\n')
      print(paste0('Fitting hurdlenet ...'))
      
    }
    
    doParallel::registerDoParallel(cores = nCore.indep)
    hurdlenet.out = foreach::foreach(indep_type = c('cont', 'binary'), .combine = 'hurdlenet_combine_indep' , .multicombine = T) %dopar% {
      
      if(indep_type=='cont'){
        
        ### continuous modeling ====
        
        # saveoutput_type = c('mu0', 'mui', 'eta', 'phi',
        #                     'betareg', 'alpha', 'sigma2',
        #                     'h', 'Z', 'Lvec', 'nzmeanvec',
        #                     'loglik')
        
        if(is.null(saveoutput_type)){
          
          param_return = c('mu0', 'mui', 'phi',
                           'betareg', 'alpha', 'sigma2',
                           'h', 'Z',
                           'loglik')
          
        }else if(is.character(saveoutput_type)){
          
          param_return = saveoutput_type
          
        }
        
        doParallel::registerDoParallel(cores = nCore.K)
        hurdlenet.out_indep_type = foreach::foreach(k = 1:K.fit, .combine = 'hurdlenet_combine_indep_type' , .multicombine = T) %dopar% {
          
          set.seed(k)
          
          if(length(intersect(verbose, 1:2))>0){
            
            cat('\n')
            print(paste0('Continuous model fitting with K=', k,' ...'))
            
          }
          
          # storage  
          Zstore = array(dim = c(nMCMC, datalist$n*datalist$nTime.train, K.fit))
          Z_t_temp = matrix(nrow = datalist$n, ncol = k)
          to_diagnose_Z = do.call('rbind',
                                  lapply(1:datalist$nTime.train,
                                         FUN = function(t){
                                           
                                           row(Z_t_temp)>=col(Z_t_temp)
                                           
                                         }))
          rm(Z_t_temp)
          
          ### stan fit ====
          if(k==1){
            
            #### k=1 ====
            stanfit = rstan::sampling(indep_cont_k1.stan_object,
                                      pars = param_return,
                                      include = T,
                                      data = list('n' = datalist$n, 
                                                  'p' = datalist$p,
                                                  'nTime' = datalist$nTime.train,
                                                  'yvec' = y.std, 
                                                  'Xmatvec' = X.std,
                                                  'nNz' = sum(datalist$nzid),
                                                  'nzid' = which(datalist$nzid),
                                                  'offdiagindex' = do.call('cbind',
                                                                           lapply(1:datalist$n,
                                                                                  FUN = function(j){
                                                                                    
                                                                                    (1:datalist$n)[-j]
                                                                                    
                                                                                  }))),
                                      chains = 1, iter = nBurn + nMCMC, 
                                      warmup = nBurn,
                                      control = list('adapt_delta' = adapt.delta.stan),
                                      refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
            
          }else{
            
            #### k>1 ====
            stanfit = rstan::sampling(indep_cont_k2toK.stan_object,
                                      pars = param_return,
                                      include = T,
                                      data = list('n' = datalist$n, 
                                                  'p' = datalist$p,
                                                  'nTime' = datalist$nTime.train,
                                                  'yvec' = y.std, 
                                                  'Xmatvec' = X.std,
                                                  'nNz' = sum(datalist$nzid),
                                                  'nzid' = which(datalist$nzid),
                                                  'offdiagindex' = do.call('cbind',
                                                                           lapply(1:datalist$n,
                                                                                  FUN = function(j){
                                                                                    
                                                                                    (1:datalist$n)[-j]
                                                                                    
                                                                                  })),
                                                  'K' = k),
                                      chains = 1, iter = nBurn + nMCMC, 
                                      warmup = nBurn,
                                      control = list('adapt_delta' = adapt.delta.stan),
                                      refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
            
          }   # end of if-else loop of k=1 or >1
          
          # posterior samples
          MCMCout = rstan::extract(stanfit)
          
          ### model diagnostics considering model parameters ====
          #### max Rhat ====
          max_Rhat = max(apply(X = MCMCout$betareg, 2,
                               FUN = function(v){
                                 
                                 rstan::Rhat(v)
                                 
                               }),
                         rstan::Rhat(c(MCMCout$alpha)),
                         rstan::Rhat(c(MCMCout$sigma2)),
                         apply(X = MCMCout$Z, 2:3,
                               FUN = function(v){
                                 
                                 rstan::Rhat(v)
                                 
                               })[to_diagnose_Z])
          
          #### min bulk ESS ====
          min_ess_bulk = min(apply(X = MCMCout$betareg, 2,
                                   FUN = function(v){
                                     
                                     rstan::ess_bulk(v)
                                     
                                   }),
                             rstan::ess_bulk(c(MCMCout$alpha)),
                             rstan::ess_bulk(c(MCMCout$sigma2)),
                             apply(X = MCMCout$Z, 2:3,
                                   FUN = function(v){
                                     
                                     rstan::ess_bulk(v)
                                     
                                   })[to_diagnose_Z])/nMCMC
          
          ### output list ====
          Zstore[,,1:k] = MCMCout$Z
          MCMCout$Z = Zstore
          
          MCMCout = c(MCMCout,
                      list('mcmc.diagnostic' = c('max_Rhat' = max_Rhat, 'min_ess_bulk' = min_ess_bulk,
                                                 'num_divergent' = rstan::get_num_divergent(stanfit),
                                                 'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit))))
          
          if(length(intersect(verbose, 1:2))>0){
            
            cat('\n')
            print(paste0('Continuous model fitting with K=', k,' ... Done.'))
            
          }
          
          print(MCMCout['mcmc.diagnostic'])
          
          MCMCout
          
        }   # end of loop over k
        
        if(is.character(saveoutput_type) & ("probitprobvec" %in% saveoutput_type)){
          
          hurdlenet.out_indep_type$probitprobvec =
            array(dim = c(nMCMC, datalist$nTime.train*datalist$n*(datalist$n-1), K.fit))
          
        }
        
      }else if(indep_type=='binary'){
        
        ### binary modeling ====
        
        # saveoutput_type = c('mu0', 'mui', 'eta', 'phi',
        #                     'betareg', 'alpha',
        #                     'h', 'Z', 'Lvec', 'nzmeanvec', 'probitprobvec',
        #                     'loglik')
        
        if(is.null(saveoutput_type)){
          
          param_return = c('mu0', 'mui', 'phi',
                           'betareg', 'alpha',
                           'h', 'Z',
                           'loglik')
          
        }else if(is.character(saveoutput_type)){
          
          param_return = saveoutput_type
          
        }
        
        doParallel::registerDoParallel(cores = nCore.K)
        hurdlenet.out_indep_type = foreach::foreach(k = 1:K.fit, .combine = 'hurdlenet_combine_indep_type' , .multicombine = T) %dopar% {
          
          set.seed(k)
          
          if(length(intersect(verbose, 1:2))>0){
            
            cat('\n')
            print(paste0('Binary model fitting with K=', k,' ...'))
            
          }
          
          # storage  
          Zstore = array(dim = c(nMCMC, datalist$n*datalist$nTime.train, K.fit))
          Z_t_temp = matrix(nrow = datalist$n, ncol = k)
          to_diagnose_Z = do.call('rbind',
                                  lapply(1:datalist$nTime.train,
                                         FUN = function(t){
                                           
                                           row(Z_t_temp)>=col(Z_t_temp)
                                           
                                         }))
          rm(Z_t_temp)
          
          ### stan fit ====
          if(k==1){
            
            #### k=1 ====
            stanfit = rstan::sampling(indep_binary_k1.stan_object,
                                      pars = param_return,
                                      include = T,
                                      data = list('n' = datalist$n, 
                                                  'p' = datalist$p,
                                                  'nTime' = datalist$nTime.train,
                                                  'Xmatvec' = X.std,
                                                  'deltavec' = datalist$delta.train,
                                                  'offdiagindex' = do.call('cbind',
                                                                           lapply(1:datalist$n,
                                                                                  FUN = function(j){
                                                                                    
                                                                                    (1:datalist$n)[-j]
                                                                                    
                                                                                  }))),
                                      chains = 1, iter = nBurn + nMCMC, 
                                      warmup = nBurn,
                                      control = list('adapt_delta' = adapt.delta.stan),
                                      refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
            
          }else{
            
            #### k>1 ====
            stanfit = rstan::sampling(indep_binary_k2toK.stan_object,
                                      pars = param_return,
                                      include = T,
                                      data = list('n' = datalist$n, 
                                                  'p' = datalist$p,
                                                  'nTime' = datalist$nTime.train,
                                                  'Xmatvec' = X.std,
                                                  'deltavec' = datalist$delta.train,
                                                  'offdiagindex' = do.call('cbind',
                                                                           lapply(1:datalist$n,
                                                                                  FUN = function(j){
                                                                                    
                                                                                    (1:datalist$n)[-j]
                                                                                    
                                                                                  })),
                                                  'K' = k),
                                      chains = 1, iter = nBurn + nMCMC, 
                                      warmup = nBurn,
                                      control = list('adapt_delta' = adapt.delta.stan),
                                      refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
            
          }   # end of if-else loop of k=1 or >1
          
          # posterior samples
          MCMCout = rstan::extract(stanfit)
          
          ### model diagnostics considering model parameters ====
          #### max Rhat ====
          max_Rhat = max(apply(X = MCMCout$betareg, 2,
                               FUN = function(v){
                                 
                                 rstan::Rhat(v)
                                 
                               }),
                         rstan::Rhat(c(MCMCout$alpha)),
                         apply(X = MCMCout$Z, 2:3,
                               FUN = function(v){
                                 
                                 rstan::Rhat(v)
                                 
                               })[to_diagnose_Z])
          
          #### min bulk ESS ====
          min_ess_bulk = min(apply(X = MCMCout$betareg, 2,
                                   FUN = function(v){
                                     
                                     rstan::ess_bulk(v)
                                     
                                   }),
                             rstan::ess_bulk(c(MCMCout$alpha)),
                             apply(X = MCMCout$Z, 2:3,
                                   FUN = function(v){
                                     
                                     rstan::ess_bulk(v)
                                     
                                   })[to_diagnose_Z])/nMCMC
          
          ### output list ====
          Zstore[,,1:k] = MCMCout$Z
          MCMCout$Z = Zstore
          
          MCMCout = c(MCMCout,
                      list('mcmc.diagnostic' = c('max_Rhat' = max_Rhat, 'min_ess_bulk' = min_ess_bulk,
                                                 'num_divergent' = rstan::get_num_divergent(stanfit),
                                                 'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit))))
          
          if(length(intersect(verbose, 1:2))>0){
            
            cat('\n')
            print(paste0('Binary model fitting with K=', k,' ... Done.'))
            
          }
          
          print(MCMCout['mcmc.diagnostic'])
          
          MCMCout
          
        }   # end of loop over k
        
        hurdlenet.out_indep_type$sigma2 =
          matrix(nrow = nrow(hurdlenet.out_indep_type$alpha),
                 ncol = ncol(hurdlenet.out_indep_type$alpha))
        
      }   # end of if-else loop over cont and binary
      
      hurdlenet.out_indep_type
      
    }   # end of loop over cont and binary
    
    # computing log-likelihood
    hurdlenet.out$loglik_cont.bin = hurdlenet.out$loglik
    hurdlenet.out$loglik = hurdlenet.out$loglik_cont.bin[,,,1] + hurdlenet.out$loglik_cont.bin[,,,2]
    
    hurdlenet.out$ic = do.call('cbind',
                               lapply(1:K.fit,
                                      FUN = function(k){
                                        
                                        ##### loo ic ====
                                        loo.out = loo::loo(hurdlenet.out$loglik[,,k], 
                                                           r_eff = loo::relative_eff(exp(hurdlenet.out$loglik[,,k]), 
                                                                                     chain_id = rep(1, nrow(hurdlenet.out$loglik[,,k]))), 
                                                           cores = 1)
                                        
                                        ##### waic ====
                                        waic.out = loo::waic(hurdlenet.out$loglik[,,k])
                                        
                                        ic.df = rbind(waic.out$estimates,
                                                      loo.out$estimates)
                                        
                                        ic.df.melt = as.numeric(t(ic.df))
                                        names(ic.df.melt) = paste0(rep(rownames(ic.df), each = ncol(ic.df)),'_',rep(colnames(ic.df), nrow(ic.df)))
                                        
                                        ic.df.melt
                                        
                                      }))
    
    
    ### bringing back to original y, X scale ----
    if("betareg" %in% names(hurdlenet.out)){
      
      for(r in 1:ncol(datalist$X.train)){
        
        hurdlenet.out$betareg[,r,,1] = (sd.y/sd.X[r])*hurdlenet.out$betareg[,r,,1]
        hurdlenet.out$betareg[,r,,2] = hurdlenet.out$betareg[,r,,2]/sd.X[r]
        
      }
      
    }
    
    if("Z" %in% names(hurdlenet.out)) hurdlenet.out$Z[,,,,1] = sd.y*hurdlenet.out$Z[,,,,1]
    
    if("sigma2" %in% names(hurdlenet.out)) hurdlenet.out$sigma2[,,1] = (sd.y^2)*hurdlenet.out$sigma2[,,1]
    
    if("Lvec" %in% names(hurdlenet.out)) hurdlenet.out$Lvec[,,,1] = sd.y*hurdlenet.out$Lvec[,,,1]
    
    if("nzmeanvec" %in% names(hurdlenet.out)) hurdlenet.out$nzmeanvec[,,,1] = sd.y*hurdlenet.out$nzmeanvec[,,,1]
    
    if("h" %in% names(hurdlenet.out)) hurdlenet.out$h[,,,1] = 2*log(sd.y) + hurdlenet.out$h[,,,1]
    
    if("mu0" %in% names(hurdlenet.out)) hurdlenet.out$mu0[,,1] = 2*log(sd.y) + hurdlenet.out$mu0[,,1]
    
  }   # end of if-else loop over methods
  
  # else if(model.choice=='nolatent'){
  #   
  #   ## nolatent ====
  #   
  #   if(length(intersect(verbose, 1:2))>0){
  #     
  #     cat('\n')
  #     print(paste0('Fitting hurdlenet ...'))
  #     
  #   }
  #   
  #   set.seed(1)
  #   
  #   if(length(intersect(verbose, 1:2))>0){
  #     
  #     cat('\n')
  #     print('Model fitting with no latent variables ...')
  #     
  #   }
  #   
  #   ### stan fit ====
  #   stanfit = rstan::sampling(nolatent.stan_object,
  #                             pars = c('beta0_cont', 'betareg_cont', 'sigma2',
  #                                      'freal', 'fpos2',
  #                                      'nzmeanvec', 'probitprobvec',
  #                                      'loglik'),
  #                             include = T,
  #                             data = list('n' = datalist$n, 
  #                                         'p' = datalist$p,
  #                                         'nTime' = datalist$nTime.train,
  #                                         'yvec' = y.std, 
  #                                         'Xmatvec' = X.std,
  #                                         'deltavec' = datalist$delta.train,
  #                                         'nNz' = sum(datalist$nzid),
  #                                         'nzid' = which(datalist$nzid),
  #                                         'offdiagindex' = do.call('cbind',
  #                                                                  lapply(1:datalist$n,
  #                                                                         FUN = function(j){
  #                                                                           
  #                                                                           (1:datalist$n)[-j]
  #                                                                           
  #                                                                         }))),
  #                             chains = 1, iter = nBurn + nMCMC, 
  #                             warmup = nBurn,
  #                             control = list('adapt_delta' = adapt.delta.stan),
  #                             refresh = 0*(length(intersect(verbose,2))==0) + max((nBurn + nMCMC)/10, 1)*(length(intersect(verbose,2))>0))
  #   
  #   # posterior samples
  #   MCMCout = rstan::extract(stanfit)
  #   
  #   ### model diagnostics considering model parameters ====
  #   #### max Rhat ====
  #   max_Rhat = max(rstan::Rhat(c(MCMCout$beta0_cont)),
  #                  apply(X = MCMCout$betareg_cont, 2,
  #                        FUN = function(v){
  #                          
  #                          rstan::Rhat(v)
  #                          
  #                        }),
  #                  rstan::Rhat(c(MCMCout$sigma2)),
  #                  rstan::Rhat(c(MCMCout$freal)),
  #                  rstan::Rhat(c(MCMCout$fpos2)))
  #   
  #   #### min bulk ESS ====
  #   min_ess_bulk = min(rstan::ess_bulk(c(MCMCout$beta0_cont)),
  #                      apply(X = MCMCout$betareg_cont, 2,
  #                            FUN = function(v){
  #                              
  #                              rstan::ess_bulk(v)
  #                              
  #                            }),
  #                      rstan::ess_bulk(c(MCMCout$sigma2)),
  #                      rstan::ess_bulk(c(MCMCout$freal)),
  #                      rstan::ess_bulk(c(MCMCout$fpos2)))/nMCMC
  #   
  #   #### loo ic ====
  #   MCMCout$loo.out = loo::loo(MCMCout$loglik, 
  #                              r_eff = loo::relative_eff(exp(MCMCout$loglik), 
  #                                                        chain_id = rep(1, nrow(MCMCout$loglik))), 
  #                              cores = 1)
  #   
  #   #### waic ====
  #   MCMCout$waic.out = loo::waic(MCMCout$loglik)
  #   
  #   ic.df = rbind(MCMCout$waic.out$estimates,
  #                 MCMCout$loo.out$estimates)
  #   
  #   ic.df.melt = as.numeric(t(ic.df))
  #   names(ic.df.melt) = paste0(rep(rownames(ic.df), each = ncol(ic.df)),'_',rep(colnames(ic.df), nrow(ic.df)))
  #   
  #   ### output list ====
  #   MCMCout = c(MCMCout,
  #               list('ic' = ic.df.melt,
  #                    'mcmc.diagnostic' = c('max_Rhat' = max_Rhat, 'min_ess_bulk' = min_ess_bulk,
  #                                          'num_divergent' = rstan::get_num_divergent(stanfit),
  #                                          'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit))))
  #   
  #   if(length(intersect(verbose, 1:2))>0){
  #     
  #     cat('\n')
  #     print(paste0('Model fitting with no latent variables ... Done.'))
  #     
  #   }
  #   
  #   hurdlenet.out = MCMCout[c('beta0_cont', 'betareg_cont', 'sigma2',
  #                             'freal', 'fpos2',
  #                             'nzmeanvec', 'probitprobvec',
  #                             'loglik',
  #                             'ic', 'mcmc.diagnostic')]
  #   
  #   
  #   ### bringing back to original y, X scale ----
  #   hurdlenet.out$beta0_cont = sd.y*hurdlenet.out$beta0_cont
  #   for(r in 1:ncol(datalist$X.train)){
  #     
  #     hurdlenet.out$betareg_cont[,r] = (sd.y/sd.X[r])*hurdlenet.out$betareg_cont[,r]
  #     
  #   }
  #   
  #   hurdlenet.out$nzmeanvec = sd.y*hurdlenet.out$nzmeanvec
  #   
  #   hurdlenet.out$sigma2 = (sd.y^2)*hurdlenet.out$sigma2
  #   
  #   
  # }
  
  if(length(intersect(verbose, 1:2))>0){
    
    print(paste0('Fitting hurdlenet ... Done.'))
    cat('\n')
    
  }
  
  #### returning output ====
  # print(this_file_dir)
  # setwd(this_file_dir)
  hurdlenet.out = list('MCMCout' = hurdlenet.out,
                       'input' = c(input.list,
                                   list('sd.y' = sd.y, 'sd.X' = sd.X,
                                        'columnid.X' = columnid.X)))
  
  if(saveoutput){
    
    if(is.null(output.filename)) output.filename = model.choice
    
    hurdlenet.out$input$output.filename = output.filename
    
    if(!dir.exists(output.dir)){
      
      dir.create(output.dir)
      
    }
    
    saveRDS(hurdlenet.out, file.path(output.dir, output.filename))
    
  }else{
    
    return(hurdlenet.out)
    
  }
  
}


# fitted quantities from hurdlenet outputs ----
#
#
## input
# hurdlenet.out : output from hurdlenet().
#
# fitted.type : character
#               "latent" for computing latent term L_{ijt} for each posterior samples.
#               "y" for computing E( y_{ijt} | \delta_{ijt} = 1 ) = x_{ijt}^T \beta_C + L_{ijt} for each posterior sample.
#               "probit" for computing P( \delta_{ijt} = 1 ) = g( x_{ijt}^T \beta_P + L_{ijt} ) for each posterior sample.
#
# verbose : logical. whether or not to print intermediate progress. Default TRUE.
#
#
## output
# named list of following components. below is an example for model.choice = 'D1dhs'. similar for other model.choice.
#
#   for fitted.type = 'latent' : named list with following components:
#             "Lvec" : array. dimension nMCMC X (n*(n-1)*nTime.train) X K.fit. L_{ijt} for all posterior samples.
#
#   for fitted.type = 'y' : named list with following components:
#             "Lvec" : same as for fitted.type = 'latent'
#             "EY" : array. dimension nMCMC X (n*(n-1)*nTime.train) X K.fit. E( y_{ijt} | \delta_{ijt} = 1 ) = x_{ijt}^T \beta_C + L_{ijt} for all posterior samples.
#
#   for fitted.type = 'probit' : named list with following components:
#             "Lvec" : same as for fitted.type = 'latent'
#             "probit" : array. dimension nMCMC X (n*(n-1)*nTime.train) X K.fit. P( \delta_{ijt} = 1 ) = g( x_{ijt}^T \beta_P + L_{ijt} ) for all posterior samples.

fitted.hurdlenet = function(hurdlenet.out, fitted.type, verbose = T){
  
  if(hurdlenet.out$input$model.choice %in% c('D1dhs', 'D0dhs')){
    
    # D1dhs, D0dhs ----
    Lvec = 
      array(dim = c(hurdlenet.out$input$nMCMC,
                    hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)*hurdlenet.out$input$datalist$nTime.train,
                    hurdlenet.out$input$K.fit))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z = array(dim = c(hurdlenet.out$input$nMCMC,
                        hurdlenet.out$input$datalist$n*hurdlenet.out$input$datalist$nTime.train,
                        K))
      Z[,,1:K] = hurdlenet.out$MCMCout$Z[,,1:K,K]
      normZ_K = apply(Z, 1:2, FUN = function(v){sqrt(sum(v^2))})
      
      pb = txtProgressBar(min = 1, max = hurdlenet.out$input$datalist$nTime.train)
      for(t in 1:hurdlenet.out$input$datalist$nTime.train){
        
        for(r in 1:hurdlenet.out$input$nMCMC){
          
          Lmat_Ktr = scale(x = Matrix::tcrossprod(Z[r,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),]),
                           center = F,
                           scale = normZ_K[r,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n)])
          Lmat_Ktr = hurdlenet.out$MCMCout$alpha[r,K]*Lmat_Ktr +
            (1 - hurdlenet.out$MCMCout$alpha[r,K])*t(Lmat_Ktr)
          Lvec[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K] =
            Lmat_Ktr[row(Lmat_Ktr)!=col(Lmat_Ktr)]
          
        }
        
        if(verbose) setTxtProgressBar(pb, t)
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    if(fitted.type=='latent'){
      
      return(list("Lvec" = Lvec))
      
    }else if(fitted.type=='y'){
      
      return(list("Lvec" = Lvec,
                  "EY" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                FUN = function(k){
                                  
                                  t(hurdlenet.out$input$datalist$X.train %*% t(hurdlenet.out$MCMCout$betareg_cont[,,k])) +
                                    Lvec[,,k]
                                  
                                }, SIMPLIFY = 'array')))
      
    }else if(fitted.type=='probit'){
      
      return(list("Lvec" = Lvec,
                  "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                        FUN = function(k){
                                          
                                          (1 + exp(hurdlenet.out$MCMCout$f_shift[,k] -
                                                     hurdlenet.out$MCMCout$f_rate[,k]*(t(hurdlenet.out$input$datalist$X.train %*% t(hurdlenet.out$MCMCout$betareg_binary[,,k])) +
                                                                                         Lvec[,,k])))^
                                            matrix(data = -1/hurdlenet.out$MCMCout$f_nu[,k],
                                                   nrow = dim(Lvec)[1], ncol = dim(Lvec)[2],
                                                   byrow = F)
                                          
                                        }, SIMPLIFY = 'array')))
      
    }
    
  }else if(hurdlenet.out$input$model.choice=='static'){
    
    # static ----
    Lvec = 
      array(dim = c(hurdlenet.out$input$nMCMC,
                    hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1),
                    hurdlenet.out$input$K.fit))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z = array(dim = c(hurdlenet.out$input$nMCMC,
                        hurdlenet.out$input$datalist$n,
                        K))
      Z[,,1:K] = hurdlenet.out$MCMCout$Z[,,1:K,K]
      normZ_K = apply(Z, 1:2, FUN = function(v){sqrt(sum(v^2))})
      
      pb = txtProgressBar(min = 1, max = hurdlenet.out$input$nMCMC)
      for(r in 1:hurdlenet.out$input$nMCMC){
        
        Lmat_Kr = scale(x = Matrix::tcrossprod(Z[r,,]),
                         center = F,
                         scale = normZ_K[r,])
        Lmat_Kr = hurdlenet.out$MCMCout$alpha[r,K]*Lmat_Kr +
          (1 - hurdlenet.out$MCMCout$alpha[r,K])*t(Lmat_Kr)
        Lvec[r,,K] = Lmat_Kr[row(Lmat_Kr)!=col(Lmat_Kr)]
        
        if(verbose) setTxtProgressBar(pb, r)
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    if(fitted.type=='latent'){
      
      return(list("Lvec" = Lvec))
      
    }else if(fitted.type=='y'){
      
      return(list("Lvec" = Lvec,
                  "EY" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                FUN = function(k){
                                  
                                  t(hurdlenet.out$input$datalist$X.train %*% t(hurdlenet.out$MCMCout$betareg_cont[,,k])) +
                                    do.call('cbind',
                                            lapply(1:hurdlenet.out$input$datalist$nTime.train,
                                                   FUN = function(t){
                                                     
                                                     Lvec[,,k]
                                                     
                                                   }))
                                  
                                }, SIMPLIFY = 'array')))
      
    }else if(fitted.type=='probit'){
      
      return(list("Lvec" = Lvec,
                  "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                        FUN = function(k){
                                          
                                          (1 + exp(hurdlenet.out$MCMCout$f_shift[,k] -
                                                     hurdlenet.out$MCMCout$f_rate[,k]*(t(hurdlenet.out$input$datalist$X.train %*% t(hurdlenet.out$MCMCout$betareg_binary[,,k])) +
                                                                                         do.call('cbind',
                                                                                                 lapply(1:hurdlenet.out$input$datalist$nTime.train,
                                                                                                        FUN = function(t){
                                                                                                          
                                                                                                          Lvec[,,k]
                                                                                                          
                                                                                                        })))))^
                                            matrix(data = -1/hurdlenet.out$MCMCout$f_nu[,k],
                                                   nrow = dim(Lvec)[1], ncol = hurdlenet.out$input$datalist$nTime.train*dim(Lvec)[2],
                                                   byrow = F)
                                          
                                        }, SIMPLIFY = 'array')))
      
    }
    
  }else if(hurdlenet.out$input$model.choice=='indep'){
    
    # indep ----
    Lvec = 
      array(dim = c(hurdlenet.out$input$nMCMC,
                    hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)*hurdlenet.out$input$datalist$nTime.train,
                    hurdlenet.out$input$K.fit,2))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z = array(dim = c(hurdlenet.out$input$nMCMC,
                        hurdlenet.out$input$datalist$n*hurdlenet.out$input$datalist$nTime.train,
                        K,2))
      Z[,,1:K,] = hurdlenet.out$MCMCout$Z[,,1:K,K,]
      normZ_K = apply(Z, c(1:2,4), FUN = function(v){sqrt(sum(v^2))})
      
      pb = txtProgressBar(min = 1, max = hurdlenet.out$input$datalist$nTime.train)
      for(t in 1:hurdlenet.out$input$datalist$nTime.train){
        
        for(r in 1:hurdlenet.out$input$nMCMC){
          
          # cont
          Lmat_Ktr = scale(x = Matrix::tcrossprod(Z[r,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),,1]),
                           center = F,
                           scale = normZ_K[r,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),1])
          Lmat_Ktr = hurdlenet.out$MCMCout$alpha[r,K,1]*Lmat_Ktr +
            (1 - hurdlenet.out$MCMCout$alpha[r,K,1])*t(Lmat_Ktr)
          Lvec[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K,1] =
            Lmat_Ktr[row(Lmat_Ktr)!=col(Lmat_Ktr)]
          
          # binary
          Lmat_Ktr = scale(x = Matrix::tcrossprod(Z[r,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),,2]),
                           center = F,
                           scale = normZ_K[r,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),2])
          Lmat_Ktr = hurdlenet.out$MCMCout$alpha[r,K,2]*Lmat_Ktr +
            (1 - hurdlenet.out$MCMCout$alpha[r,K,2])*t(Lmat_Ktr)
          Lvec[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K,2] =
            Lmat_Ktr[row(Lmat_Ktr)!=col(Lmat_Ktr)]
          
        }
        
        if(verbose) setTxtProgressBar(pb, t)
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    
    if(fitted.type=='latent'){
      
      return(list("Lvec" = Lvec))
      
    }else if(fitted.type=='y'){
      
      return(list("Lvec" = Lvec,
                  "EY" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                        FUN = function(k){
                                          
                                          t(hurdlenet.out$input$datalist$X.train %*% t(hurdlenet.out$MCMCout$betareg[,,k,1])) +
                                            Lvec[,,k,1]
                                          
                                        }, SIMPLIFY = 'array')))
      
    }else if(fitted.type=='probit'){
      
      return(list("Lvec" = Lvec,
                  "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                FUN = function(k){
                                  
                                  pnorm(q = t(hurdlenet.out$input$datalist$X.train %*% t(hurdlenet.out$MCMCout$betareg[,,k,2])) +
                                          Lvec[,,k,2])
                                  
                                }, SIMPLIFY = 'array')))
      
    }
    
  }
  # else if(hurdlenet.out$input$model.choice=='nolatent'){
  #   
  #   # nolatent ----
  #   if(fitted.type=='y'){
  #     
  #     return(list("y" = as.numeric(hurdlenet.out$MCMCout$beta0_cont) +
  #                   t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_cont)) +
  #                   rnorm(n = hurdlenet.out$input$nMCMC,
  #                         sd = sqrt(hurdlenet.out$MCMCout$sigma2))))
  #     
  #   }else if(fitted.type=='probit'){
  #     
  #     return(list("probitprob" = (1 + exp(as.numeric(hurdlenet.out$MCMCout$freal) - 
  #                                           t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_binary))))^
  #                   matrix(data = -1/as.numeric(hurdlenet.out$MCMCout$fpos2),
  #                          nrow = hurdlenet.out$input$nMCMC, 
  #                          ncol = nrow(datalist$X.test),
  #                          byrow = F)))
  #     
  #   }
  #   
  # }
  
}


# predicted quantities from hurdlenet outputs ----
#
#
## input
#
# nTime.pred : positive integer. number of time points to predict for.
#
# predict.type : character. what to predict?
#               "latent" for latent term L_{ijt}
#               "y" for edge y_{ijt} according to N( x_{ijt}^T \beta_C + L_{ijt}, \sigma^2)
#               "probit" for \delta_{ijt} according to Bernoulli( g( x_{ijt}^T \beta_P + L_{ijt} ) )
#
# datalist : named list with component name "X.test", that is, list("X.test" = Xmat), where
#            Xmat : matrix. covariate matrix corresponding to each edge for all the predicted time points.
#                   dimension n*(n-1)*nTime.pred X p.
#
#            similarly structured as datalist$X.train in hurdlenet()
#            rows correspond to different node pair (column major order at each predicted time point) and predicted time point combinations.
#            columns correspond to (node and node-pair-specific) covariates.
#
#            if l corresponds to the edge from i to j at predicted time t, Xmat[l,] = c(X.node[(i,t),], X.node[(j,t),], X.nodepair[(i,j,t),]), where
#            with slight abuse of subsetting notation,
#             X.node[(i,t),] are node-i specific covariates (exporters/source/parent) at time t
#             X.node[(j,t),] are node-j specific covariates (importers/receiver/child) at time t
#             X.nodepair[(i,j,t),] are node-pair (i,j) specific covariates at time t
#
#            not required for predict.type = "latent"
#
# hurdlenet.out : hurdlenet() output from fitting training data.
#
# verbose : logical. whether or not to print intermediate progress. Default TRUE.
#
#
## output
# named list of following components. below is an example for model.choice = 'D1dhs'. similar for other model.choice.
#
#   outputs are similar and similarly structured from fitted.hurdlenet()
#
#   for predict.type = 'latent' : named list with following components:
#             "Z" : array. dimension nMCMC X (n*nTime.pred) X K.fit X K.fit. Z[,,1:k,k] are predictive samples of latent variables Z.
#             "Lvec" : array. dimension nMCMC X (n*(n-1)*nTime.pred) X K.fit. L_{ijt} for all predictive samples.
#
#   for predict.type = 'y' : named list with following components:
#             "Z", "Lvec" : same as for predict.type = 'latent'
#             "y" : array. dimension nMCMC X (n*(n-1)*nTime.pred) X K.fit. predictive samples of y_{ijt}.
#
#   for predict.type = 'probit' : named list with following components:
#             "Z", "Lvec" : same as for predict.type = 'latent'
#             "probitprob" : array. dimension nMCMC X (n*(n-1)*nTime.pred) X K.fit. predictive samples of g( x_{ijt}^T \beta_P + L_{ijt} ).

predict.hurdlenet = function(nTime.pred, predict.type, datalist, hurdlenet.out, verbose = T){
  
  set.seed(1)
  
  if(hurdlenet.out$input$model.choice=='D1dhs'){
    
    # D1dhs ----
    Z_pred = array(dim = c(hurdlenet.out$input$nMCMC,
                           nTime.pred*hurdlenet.out$input$datalist$n,
                           hurdlenet.out$input$K.fit, hurdlenet.out$input$K.fit))
    Lvec_pred = array(dim = c(hurdlenet.out$input$nMCMC,
                              nTime.pred*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1),
                              hurdlenet.out$input$K.fit))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z_pred_Kt = array(data = 1e-20,
                        dim = c(hurdlenet.out$input$nMCMC,
                                hurdlenet.out$input$datalist$n, K))
      for(t in 1:nTime.pred){
        
        if(t==1){
          
          h_pred_Kt = hurdlenet.out$MCMCout$mu0[,K] +
            hurdlenet.out$MCMCout$mui[,,K] +
            hurdlenet.out$MCMCout$phi[,,K]*(hurdlenet.out$MCMCout$h[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),K] -
                                              hurdlenet.out$MCMCout$mu0[,K] - hurdlenet.out$MCMCout$mui[,,K]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          for(k in 1:K){
            
            if(k==1){
              
              Z_pred_Kt[,1,1] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                          mean = log(hurdlenet.out$MCMCout$Z[,(hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1,1,K]),
                                          sd = exp(h_pred_Kt[,1]/2)))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 2):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),1,K],
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n]/2))
              
            }else{
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + k):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),k,K],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n]/2))
              
            }
            
          }
          
        }else{
          
          h_pred_Kt = hurdlenet.out$MCMCout$mu0[,K] +
            hurdlenet.out$MCMCout$mui[,,K] +
            hurdlenet.out$MCMCout$phi[,,K]*(h_pred_Kt - hurdlenet.out$MCMCout$mu0[,K] -
                                              hurdlenet.out$MCMCout$mui[,,K]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          for(k in 1:K){
            
            if(k==1){
              
              Z_pred_Kt[,1,1] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                          mean = log(Z_pred[,(t-2)*hurdlenet.out$input$datalist$n + 1,1,K]),
                                          sd = exp(h_pred_Kt[,1]/2)))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = Z_pred[,((t-2)*hurdlenet.out$input$datalist$n + 2):((t-1)*hurdlenet.out$input$datalist$n),1,K],
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n]/2))
              
            }else{
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = Z_pred[,((t-2)*hurdlenet.out$input$datalist$n + k):((t-1)*hurdlenet.out$input$datalist$n),k,K],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n]/2))
              
            }
            
          }
          
        }
        
        Z_pred[,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),1:K,K] = Z_pred_Kt
        normZ_pred_Kt = apply(Z_pred_Kt, 1:2, FUN = function(v){sqrt(sum(v^2))})
        
        pb = txtProgressBar(min = 1, max = hurdlenet.out$input$nMCMC)
        for(r in 1:hurdlenet.out$input$nMCMC){
          
          Lmat_pred_Ktr = scale(x = Matrix::tcrossprod(Z_pred_Kt[r,,]), center = F,
                                scale = normZ_pred_Kt[r,])
          Lmat_pred_Ktr = hurdlenet.out$MCMCout$alpha[r,K]*Lmat_pred_Ktr +
            (1 - hurdlenet.out$MCMCout$alpha[r,K])*t(Lmat_pred_Ktr)
          Lvec_pred[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K] =
            Lmat_pred_Ktr[row(Lmat_pred_Ktr)!=col(Lmat_pred_Ktr)]
          
          if(verbose) setTxtProgressBar(pb, r)
          
        }
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    if(predict.type=='latent'){
      
      return(list("Lvec" = Lvec_pred, "Z" = Z_pred))
      
    }else if(predict.type %in% c('y', 'probit')){
      
      if(predict.type=='y'){
        
        return(list("Lvec" = Lvec_pred, "Z" = Z_pred,
                    "y" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                 FUN = function(k){
                                   
                                   t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_cont[,,k])) +
                                     Lvec_pred[,,k] +
                                     rnorm(n = hurdlenet.out$input$nMCMC,
                                           sd = sqrt(hurdlenet.out$MCMCout$sigma2[,k]))
                                   
                                 }, SIMPLIFY = 'array')))
        
      }else if(predict.type=='probit'){
        
        return(list("Lvec" = Lvec_pred, "Z" = Z_pred,
                    "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                          FUN = function(k){
                                            
                                            (1 + exp(hurdlenet.out$MCMCout$f_shift[,k] -
                                                       hurdlenet.out$MCMCout$f_rate[,k]*(t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_binary[,,k])) +
                                                                                           Lvec_pred[,,k])))^
                                              matrix(data = -1/hurdlenet.out$MCMCout$f_nu[,k],
                                                     nrow = dim(Lvec_pred)[1], ncol = dim(Lvec_pred)[2],
                                                     byrow = F)
                                            
                                          }, SIMPLIFY = 'array')))
        
      }
      
    }
    
  }else if(hurdlenet.out$input$model.choice=='D0dhs'){
    
    # D0dhs ----
    Z_pred = array(dim = c(hurdlenet.out$input$nMCMC,
                           nTime.pred*hurdlenet.out$input$datalist$n,
                           hurdlenet.out$input$K.fit, hurdlenet.out$input$K.fit))
    Lvec_pred = array(dim = c(hurdlenet.out$input$nMCMC,
                              nTime.pred*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1),
                              hurdlenet.out$input$K.fit))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z_pred_Kt = array(data = 1e-20,
                        dim = c(hurdlenet.out$input$nMCMC,
                                hurdlenet.out$input$datalist$n, K))
      for(t in 1:nTime.pred){
        
        if(t==1){
          
          h_pred_Kt = hurdlenet.out$MCMCout$mu0[,K] +
            hurdlenet.out$MCMCout$mui[,,K] +
            hurdlenet.out$MCMCout$phi[,,K]*(hurdlenet.out$MCMCout$h[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),K] -
                                              hurdlenet.out$MCMCout$mu0[,K] - hurdlenet.out$MCMCout$mui[,,K]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          for(k in 1:K){
            
            if(k==1){
              
              Z_pred_Kt[,1,1] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                          mean = 0,
                                          sd = exp(h_pred_Kt[,1]/2)))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = 0,
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n]/2))
              
            }else{
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + k):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),k,K],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n]/2))
              
            }
            
          }
          
        }else{
          
          h_pred_Kt = hurdlenet.out$MCMCout$mu0[,K] +
            hurdlenet.out$MCMCout$mui[,,K] +
            hurdlenet.out$MCMCout$phi[,,K]*(h_pred_Kt - hurdlenet.out$MCMCout$mu0[,K] -
                                              hurdlenet.out$MCMCout$mui[,,K]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          for(k in 1:K){
            
            if(k==1){
              
              Z_pred_Kt[,1,1] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                          mean = 0,
                                          sd = exp(h_pred_Kt[,1]/2)))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = 0,
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n]/2))
              
            }else{
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = 0,
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n]/2))
              
            }
            
          }
          
        }
        
        Z_pred[,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),1:K,K] = Z_pred_Kt
        normZ_pred_Kt = apply(Z_pred_Kt, 1:2, FUN = function(v){sqrt(sum(v^2))})
        
        pb = txtProgressBar(min = 1, max = hurdlenet.out$input$nMCMC)
        for(r in 1:hurdlenet.out$input$nMCMC){
          
          Lmat_pred_Ktr = scale(x = Matrix::tcrossprod(Z_pred_Kt[r,,]), center = F,
                                scale = normZ_pred_Kt[r,])
          Lmat_pred_Ktr = hurdlenet.out$MCMCout$alpha[r,K]*Lmat_pred_Ktr +
            (1 - hurdlenet.out$MCMCout$alpha[r,K])*t(Lmat_pred_Ktr)
          Lvec_pred[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K] =
            Lmat_pred_Ktr[row(Lmat_pred_Ktr)!=col(Lmat_pred_Ktr)]
          
        }
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    if(predict.type=='latent'){
      
      return(list("Lvec" = Lvec_pred, "Z" = Z_pred))
      
    }else if(predict.type %in% c('y', 'probit')){
      
      if(predict.type=='y'){
        
        return(list("Lvec" = Lvec_pred, "Z" = Z_pred,
                    "y" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                 FUN = function(k){
                                   
                                   t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_cont[,,k])) +
                                     Lvec_pred[,,k] +
                                     rnorm(n = hurdlenet.out$input$nMCMC,
                                           sd = sqrt(hurdlenet.out$MCMCout$sigma2[,k]))
                                   
                                 }, SIMPLIFY = 'array')))
        
      }else if(predict.type=='probit'){
        
        return(list("Lvec" = Lvec_pred, "Z" = Z_pred,
                    "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                          FUN = function(k){
                                            
                                            (1 + exp(hurdlenet.out$MCMCout$f_shift[,k] -
                                                       hurdlenet.out$MCMCout$f_rate[,k]*(t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_binary[,,k])) +
                                                                                           Lvec_pred[,,k])))^
                                              matrix(data = -1/hurdlenet.out$MCMCout$f_nu[,k],
                                                     nrow = dim(Lvec_pred)[1], ncol = dim(Lvec_pred)[2],
                                                     byrow = F)
                                            
                                          }, SIMPLIFY = 'array')))
        
      }
      
    }
    
  }else if(hurdlenet.out$input$model.choice=='static'){
    
    # static ----
    Lvec = 
      array(dim = c(hurdlenet.out$input$nMCMC,
                    hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1),
                    hurdlenet.out$input$K.fit))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z = array(dim = c(hurdlenet.out$input$nMCMC,
                        hurdlenet.out$input$datalist$n,
                        K))
      Z[,,1:K] = hurdlenet.out$MCMCout$Z[,,1:K,K]
      normZ_K = apply(Z, 1:2, FUN = function(v){sqrt(sum(v^2))})
      
      pb = txtProgressBar(min = 1, max = hurdlenet.out$input$nMCMC)
      for(r in 1:hurdlenet.out$input$nMCMC){
        
        Lmat_Kr = scale(x = Matrix::tcrossprod(Z[r,,]),
                        center = F,
                        scale = normZ_K[r,])
        Lmat_Kr = hurdlenet.out$MCMCout$alpha[r,K]*Lmat_Kr +
          (1 - hurdlenet.out$MCMCout$alpha[r,K])*t(Lmat_Kr)
        Lvec[r,,K] = Lmat_Kr[row(Lmat_Kr)!=col(Lmat_Kr)]
        
        if(verbose) setTxtProgressBar(pb, r)
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    if(predict.type=='latent'){
      
      return(list("Lvec" = Lvec,
                  "Z" = hurdlenet.out$MCMCout$Z))
      
    }else if(predict.type %in% c('y', 'probit')){
      
      if(predict.type=='y'){
        
        return(list("Lvec" = Lvec,
                    "Z" = hurdlenet.out$MCMCout$Z,
                    "y" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                 FUN = function(k){
                                   
                                   t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_cont[,,k])) +
                                     do.call('cbind',
                                             lapply(1:nTime.pred,
                                                    FUN = function(t){
                                                      
                                                      Lvec[,,k]
                                                      
                                                    })) +
                                     rnorm(n = hurdlenet.out$input$nMCMC,
                                           sd = sqrt(hurdlenet.out$MCMCout$sigma2[,k]))
                                   
                                 }, SIMPLIFY = 'array')))
        
      }else if(predict.type=='probit'){
        
        return(list("Lvec" = Lvec,
                    "Z" = hurdlenet.out$MCMCout$Z,
                    "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                          FUN = function(k){
                                            
                                            (1 + exp(hurdlenet.out$MCMCout$f_shift[,k] -
                                                       hurdlenet.out$MCMCout$f_rate[,k]*(t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_binary[,,k])) +
                                                                                           do.call('cbind',
                                                                                                   lapply(1:nTime.pred,
                                                                                                          FUN = function(t){
                                                                                                            
                                                                                                            Lvec[,,k]
                                                                                                            
                                                                                                          })))))^
                                              matrix(data = -1/hurdlenet.out$MCMCout$f_nu[,k],
                                                     nrow = dim(Lvec)[1], ncol = nTime.pred*dim(Lvec)[2],
                                                     byrow = F)
                                            
                                          }, SIMPLIFY = 'array')))
        
      }
      
    }
    
  }else if(hurdlenet.out$input$model.choice=='indep'){
    
    # indep ----
    Z_pred = array(dim = c(hurdlenet.out$input$nMCMC,
                           nTime.pred*hurdlenet.out$input$datalist$n,
                           hurdlenet.out$input$K.fit, hurdlenet.out$input$K.fit, 2))
    Lvec_pred = array(dim = c(hurdlenet.out$input$nMCMC,
                              nTime.pred*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1),
                              hurdlenet.out$input$K.fit, 2))
    for(K in 1:hurdlenet.out$input$K.fit){
      
      Z_pred_Kt = array(data = 1e-20,
                        dim = c(hurdlenet.out$input$nMCMC,
                                hurdlenet.out$input$datalist$n, K, 2))
      h_pred_Kt = array(dim = c(hurdlenet.out$input$nMCMC,
                                hurdlenet.out$input$datalist$n, 2))
      for(t in 1:nTime.pred){
        
        if(t==1){
          
          h_pred_Kt[,,1] = hurdlenet.out$MCMCout$mu0[,K,1] +
            hurdlenet.out$MCMCout$mui[,,K,1] +
            hurdlenet.out$MCMCout$phi[,,K,1]*(hurdlenet.out$MCMCout$h[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),K,1] -
                                                hurdlenet.out$MCMCout$mu0[,K,1] - hurdlenet.out$MCMCout$mui[,,K,1]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          h_pred_Kt[,,2] = hurdlenet.out$MCMCout$mu0[,K,2] +
            hurdlenet.out$MCMCout$mui[,,K,2] +
            hurdlenet.out$MCMCout$phi[,,K,2]*(hurdlenet.out$MCMCout$h[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),K,2] -
                                                hurdlenet.out$MCMCout$mu0[,K,2] - hurdlenet.out$MCMCout$mui[,,K,2]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          for(k in 1:K){
            
            if(k==1){
              
              Z_pred_Kt[,1,1,1] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                            mean = log(hurdlenet.out$MCMCout$Z[,(hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1,1,K,1]),
                                            sd = exp(h_pred_Kt[,1,1]/2)))
              
              Z_pred_Kt[,1,1,2] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                            mean = log(hurdlenet.out$MCMCout$Z[,(hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 1,1,K,2]),
                                            sd = exp(h_pred_Kt[,1,2]/2)))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 2):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),1,K,1],
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n,1]/2))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1,2] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + 2):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),1,K,2],
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n,2]/2))
              
            }else{
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + k):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),k,K,1],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n,1]/2))
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k,2] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = hurdlenet.out$MCMCout$Z[,((hurdlenet.out$input$datalist$nTime.train-1)*hurdlenet.out$input$datalist$n + k):(hurdlenet.out$input$datalist$nTime.train*hurdlenet.out$input$datalist$n),k,K,2],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n,2]/2))
              
            }
            
          }
          
        }else{
          
          h_pred_Kt[,,1] = hurdlenet.out$MCMCout$mu0[,K,1] +
            hurdlenet.out$MCMCout$mui[,,K,1] +
            hurdlenet.out$MCMCout$phi[,,K,1]*(h_pred_Kt[,,1] - hurdlenet.out$MCMCout$mu0[,K,1] -
                                                hurdlenet.out$MCMCout$mui[,,K,1]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          h_pred_Kt[,,2] = hurdlenet.out$MCMCout$mu0[,K,2] +
            hurdlenet.out$MCMCout$mui[,,K,2] +
            hurdlenet.out$MCMCout$phi[,,K,2]*(h_pred_Kt[,,2] - hurdlenet.out$MCMCout$mu0[,K,2] -
                                                hurdlenet.out$MCMCout$mui[,,K,2]) +
            matrix(data = log(rcauchy(n = hurdlenet.out$input$nMCMC*hurdlenet.out$input$datalist$n)^2),
                   nrow = hurdlenet.out$input$nMCMC,
                   ncol = hurdlenet.out$input$datalist$n)
          
          for(k in 1:K){
            
            if(k==1){
              
              Z_pred_Kt[,1,1,1] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                            mean = log(Z_pred[,(t-2)*hurdlenet.out$input$datalist$n + 1,1,K,1]),
                                            sd = exp(h_pred_Kt[,1,1]/2)))
              
              Z_pred_Kt[,1,1,2] = exp(qnorm(p = runif(n = hurdlenet.out$input$nMCMC),
                                            mean = log(Z_pred[,(t-2)*hurdlenet.out$input$datalist$n + 1,1,K,2]),
                                            sd = exp(h_pred_Kt[,1,2]/2)))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = Z_pred[,((t-2)*hurdlenet.out$input$datalist$n + 2):((t-1)*hurdlenet.out$input$datalist$n),1,K,1],
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n,1]/2))
              
              Z_pred_Kt[,2:hurdlenet.out$input$datalist$n,1,2] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - 1)),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - 1),
                      mean = Z_pred[,((t-2)*hurdlenet.out$input$datalist$n + 2):((t-1)*hurdlenet.out$input$datalist$n),1,K,2],
                      sd = exp(h_pred_Kt[,2:hurdlenet.out$input$datalist$n,2]/2))
              
            }else{
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k,1] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = Z_pred[,((t-2)*hurdlenet.out$input$datalist$n + k):((t-1)*hurdlenet.out$input$datalist$n),k,K,1],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n,1]/2))
              
              Z_pred_Kt[,k:hurdlenet.out$input$datalist$n,k,2] =
                qnorm(p = matrix(data = runif(n = hurdlenet.out$input$nMCMC*(hurdlenet.out$input$datalist$n - (k-1))),
                                 nrow = hurdlenet.out$input$nMCMC,
                                 ncol = hurdlenet.out$input$datalist$n - (k-1)),
                      mean = Z_pred[,((t-2)*hurdlenet.out$input$datalist$n + k):((t-1)*hurdlenet.out$input$datalist$n),k,K,2],
                      sd = exp(h_pred_Kt[,k:hurdlenet.out$input$datalist$n,2]/2))
              
            }
            
          }
          
        }
        
        Z_pred[,((t-1)*hurdlenet.out$input$datalist$n + 1):(t*hurdlenet.out$input$datalist$n),1:K,K,] = Z_pred_Kt
        normZ_pred_Kt = apply(Z_pred_Kt, c(1:2,4), FUN = function(v){sqrt(sum(v^2))})
        Lmat_pred_Ktr = array(dim = c(hurdlenet.out$input$datalist$n, hurdlenet.out$input$datalist$n, 2))
        
        pb = txtProgressBar(min = 1, max = hurdlenet.out$input$nMCMC)
        for(r in 1:hurdlenet.out$input$nMCMC){
          
          Lmat_pred_Ktr[,,1] = scale(x = Matrix::tcrossprod(Z_pred_Kt[r,,,1]), center = F,
                                     scale = normZ_pred_Kt[r,,1])
          Lmat_pred_Ktr[,,1] = hurdlenet.out$MCMCout$alpha[r,K,1]*Lmat_pred_Ktr[,,1] +
            (1 - hurdlenet.out$MCMCout$alpha[r,K,1])*t(Lmat_pred_Ktr[,,1])
          Lvec_pred[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K,1] =
            (Lmat_pred_Ktr[,,1])[row(Lmat_pred_Ktr[,,1])!=col(Lmat_pred_Ktr[,,1])]
          
          Lmat_pred_Ktr[,,2] = scale(x = Matrix::tcrossprod(Z_pred_Kt[r,,,2]), center = F,
                                     scale = normZ_pred_Kt[r,,2])
          Lmat_pred_Ktr[,,2] = hurdlenet.out$MCMCout$alpha[r,K,2]*Lmat_pred_Ktr[,,2] +
            (1 - hurdlenet.out$MCMCout$alpha[r,K,2])*t(Lmat_pred_Ktr[,,2])
          Lvec_pred[r,((t-1)*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1) +1):(t*hurdlenet.out$input$datalist$n*(hurdlenet.out$input$datalist$n-1)),K,2] =
            (Lmat_pred_Ktr[,,2])[row(Lmat_pred_Ktr[,,2])!=col(Lmat_pred_Ktr[,,2])]
          
          if(verbose) setTxtProgressBar(pb, r)
          
        }
        
      }
      
      if(verbose){
        
        cat("\n")
        print(paste0("K = ", K))
        cat("\n")
        
      }
      
    }
    
    
    if(predict.type=='latent'){
      
      return(list("Lvec" = Lvec_pred, "Z" = Z_pred))
      
    }else if(predict.type=='y'){
      
      return(list("Lvec" = Lvec_pred, "Z" = Z_pred,
                  "y" = mapply(k = 1:hurdlenet.out$input$K.fit,
                               FUN = function(k){
                                 
                                 t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg[,,k,1])) +
                                   Lvec_pred[,,k,1] +
                                   rnorm(n = hurdlenet.out$input$nMCMC,
                                         sd = sqrt(hurdlenet.out$MCMCout$sigma2[,k,1]))
                                 
                               }, SIMPLIFY = 'array')))
      
    }else if(predict.type=='probit'){
      
      return(list("Lvec" = Lvec_pred, "Z" = Z_pred,
                  "probitprob" = mapply(k = 1:hurdlenet.out$input$K.fit,
                                        FUN = function(k){
                                          
                                          pnorm(q = t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg[,,k,2])) +
                                                  Lvec_pred[,,k,2])
                                          
                                        }, SIMPLIFY = 'array')))
      
    }
    
  }
  # else if(hurdlenet.out$input$model.choice=='nolatent'){
  #   
  #   # nolatent ----
  #   if(predict.type=='y'){
  #     
  #     return(list("y" = as.numeric(hurdlenet.out$MCMCout$beta0_cont) +
  #                   t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_cont)) +
  #                   rnorm(n = hurdlenet.out$input$nMCMC,
  #                         sd = sqrt(hurdlenet.out$MCMCout$sigma2))))
  #     
  #   }else if(predict.type=='probit'){
  #     
  #     return(list("probitprob" = (1 + exp(as.numeric(hurdlenet.out$MCMCout$freal) - 
  #                                           t(datalist$X.test %*% t(hurdlenet.out$MCMCout$betareg_binary))))^
  #                   matrix(data = -1/as.numeric(hurdlenet.out$MCMCout$fpos2),
  #                          nrow = hurdlenet.out$input$nMCMC, 
  #                          ncol = nrow(datalist$X.test),
  #                          byrow = F)))
  #     
  #   }
  #   
  # }
  
}


# simulating data for hurdlenet ----
#
#
## input
# n : positive integer. number of nodes. typically more than 2.
#
# nTime.train : positive integer. number of time points in the training data
#
# nTime.test : positive integer. number of time points in the test data
#
# pNode : positive integer. number of node-specific covariates corresponding to each edge.
#         p1 as in Pramanik et al. (2025+).
#
# pPair : positive integer. number of node-pair-specific covariates corresponding to each edge.
#         p2 as in Pramanik et al. (2025+).
#
# p : positive integer. number of covariates corresponding to each edge.
#     equals to 2*p1 + p2 where p1 and p2 are node and node-pair specific covariates.
#
# beta0.binary : numeric. intercept for the binary model.
#
# K : positive integer. true latent dimension K in Pramanik et al. (2025+).
#
# alpha.latent : numeric in (0,1). true alpha in Pramanik et al. (2025+).
#
# noise.sd : positive numeric. true sigma in Pramanik et al. (2025+).
#
# nReplicate : positive integer. number of replications.
#
# saveoutput : logical. whether to save output. Default is FALSE.
#
# verbose : logical. whether to print progress. Default is TRUE.
#
# output.dir : character. name of the folder to save output in the working directory. Default 'hurdlenet_output'.
#
# output.filename : character. name of the output file. Default "simdata".
#
#
## output
# named list of following components
#
# nTime : positive integer. total number of time points data simulated for (nTime.train + nTime.test).
#
# group1mean : numeric vector. length K. first group mean for simulating latent variables.
#
# group2mean : numeric vector. length K. second group mean for simulating latent variables.
#
# groupmean : matrix. dimension n X K. node-specific group means at the first time point for simulating latent variables.
#
# changing.nodes : positive integer vector. nodes that are transitioning from group 1 to group 2 over time.
#
# changing.times : positive integer vector. same length as changing.nodes. 
#                  time points where corresponding nodes in changing.nodes started transitioning.
#
# Z : matrix. dimension n*nTime X K. simulated latent positions at all time points.
#
# Lvec : numeric vector. length n*(n-1)*nTime. simulated latent contribution L_{ijt} at all time points.
#
# nzmeanvec : numeric vector. length n*(n-1)*nTime. simulated nonzero edge weight expectations E( y_{ijt} | \delta_{ijt} = 1 ) = x_{ijt}^T \beta_C + L_{ijt} at all time points.
#
# probitprobvec : numeric vector. length n*(n-1)*nTime. simulated edge occurrence probabilities P( \delta_{ijt} = 1 ) = g( x_{ijt}^T \beta_P + L_{ijt} ) at all time points.
#
# Xmatvec : matrix. simulated covariates corresponding to each edge.
#           dimension n*(n-1)*nTime X p.
#
#           rows correspond to different node pair and time point combinations.
#           columns correspond to (node and node-pair-specific) covariates.
#
#           if l corresponds to the edge from i to j at time t, Xmatvec[l,] = c(X.node[(i,t),], X.node[(j,t),], X.nodepair[(i,j,t),]), where
#           with slight abuse of subsetting notation,
#             X.node[(i,t),] are node-i specific covariates (exporters/source/parent) at time t
#             X.node[(j,t),] are node-j specific covariates (importers/receiver/child) at time t
#             X.nodepair[(i,j,t),] are node-pair (i,j) specific covariates at time t
#
#           similar to datalist$X.train in hurdlenet()
#
# yvec : marix. continuous edge weight time series data vectorized across node pairs and time points over different replications.
#        dimension n*(n-1)*nTime X nReplicate.
#
#        let u_ijt denote the edge weight from i to j at time t in replication r.
#        yvec[,r] concatenates the following vector at time t over all time points
#         (u_21t, u_31t, ... , u_n1t,   # j = 1
#          u_12t, u_32t, ... , u_n2t,   # j = 2
#          ... , 
#          u_1nt, u_2nt, ... , u_(n-1)nt)   # j = n
#
#        similarly structured as datalist$y.train in hurdlenet()
#
#        datalist$y.train in hurdlenet() is deltavec*yvec
#
# deltavec : marix. binary edge occurrences time series data vectorized across node pairs and time points over different replications.
#            dimension n*(n-1)*nTime X nReplicate.
#
#            let v_ijt denote the edge occurrence from i to j at time t in replication r.
#            deltavec[,r] concatenates the following vector at time t over all time points
#             (v_21t, v_31t, ... , v_n1t,   # j = 1
#              v_12t, v_32t, ... , v_n2t,   # j = 2
#              ... , 
#              v_1nt, v_2nt, ... , v_(n-1)nt)   # j = n
#
#        datalist$y.train in hurdlenet() is deltavec*yvec
#
# snr : numeric vector. signal to noise ratio in the contiinuous edge weight model for all combinations of node-pair and time. calculated as abs(nzmeanvec)/noise.sd.
#       length n*(n-1)*nTime.
#
#            let v_ijt denote the edge occurrence from i to j at time t in replication r.
#            deltavec[,r] concatenates the following vector at time t over all time points
#             (v_21t, v_31t, ... , v_n1t,   # j = 1
#              v_12t, v_32t, ... , v_n2t,   # j = 2
#              ... , 
#              v_1nt, v_2nt, ... , v_(n-1)nt)   # j = n
#
#        datalist$y.train in hurdlenet() is deltavec*yvec
#
# nonzero.prop : numeric vector. observed proportion of nonzero edge occurrences in deltavec across replications.
#                length nReplicate.
#
# train.id : integer vector. id of deltavec and yvec included in training set. they are node-pair combinations for the first nTime.train time points.
#            length n*(n-1)*nTime.train.
#
# test.id : integer vector. id of deltavec and yvec included in test set. they are node-pair combinations for the last nTime.test time points.
#            length n*(n-1)*nTime.test.

sim.hurdlenet = function(n, nTime.train, nTime.test, 
                         pNode, pPair, beta0.binary,
                         K, alpha.latent, noise.sd, nReplicate,
                         saveoutput = F, verbose = T,
                         output.dir = 'hurdlenet_simdata', output.filename = 'simdata'){
  
  p = 2*pNode + pPair
  betareg.cont = round(sample(x = seq(0, 1, length.out = p),
                              size = p, replace = F)*
                         rep_len(c(-1,1), length.out = p), 1)
  betareg.binary = round(sample(x = seq(0, 1, length.out = p),
                                size = p, replace = F)*
                           rep_len(c(-1,1), length.out = p), 1)
  
  
  input.list = as.list(environment())
  
  
  ## pre-computed quantities ====
  nTime = nTime.train + nTime.test
  group1mean = rep(0, K)
  group2mean = seq(1, .2, length.out = K)*
    (2*rep_len(c(0,1), length.out = K) - 1)
  groupmean = matrix(nrow = n, ncol = K)
  groupmean[1:floor(n/2),] = matrix(data = group1mean, 
                                    nrow = floor(n/2), ncol = K, 
                                    byrow = T)
  groupmean[(floor(n/2) + 1):n,] = matrix(data = group2mean,
                                          nrow = n - floor(n/2), ncol = K,
                                          byrow = T)
  changing.nodes = (floor(n/2) - (.8*floor(n/2) - 1)):floor(n/2)
  wts.changing.nodes_t = rep(NA, length(changing.nodes))
  changing.times = sample(x = 1:(nTime-2), 
                          size = length(changing.nodes),
                          replace = T)
  
  ## storage ====
  Z = matrix(NA, n*nTime, K)
  Lvec = rep(NA, n*(n-1)*nTime)
  temp.mat_t = matrix(0, n, n)
  offdiag_t = (row(temp.mat_t)!=col(temp.mat_t))
  Lvec = nzmeanvec = probitprobvec = numeric(n*(n-1)*nTime)
  Xmatvec = matrix(nrow = n*(n-1)*nTime, ncol = p)
  temp.mat_t = matrix(0, n, n)
  offdiag_t = (row(temp.mat_t)!=col(temp.mat_t))
  
  if(verbose){
    
    cat('\n')
    print('Calculating fixed terms ...')
    
  }
  pb = txtProgressBar(min = 1, max = nTime, style = 3)
  for(t in 1:nTime){
    
    
    ### creating Z ====
    Z[((t-1)*n + 1):(t*n),] = groupmean
    
    wts.changing.nodes_t[changing.times<(t-2)] = 1
    wts.changing.nodes_t[changing.times==(t-2)] = 1/(1 + exp(-2*1))
    wts.changing.nodes_t[changing.times==(t-1)] = 1/(1 + exp(-2*0))
    wts.changing.nodes_t[changing.times==t] = 1/(1 + exp(-2*(-1)))
    wts.changing.nodes_t[changing.times>t] = 0
    Z[(t-1)*n + changing.nodes,] = 
      (1 - wts.changing.nodes_t)*matrix(data = group1mean, 
                                        nrow = length(changing.nodes), ncol = K,
                                        byrow = T) +
      wts.changing.nodes_t*matrix(data = group2mean, 
                                  nrow = length(changing.nodes), ncol = K, 
                                  byrow = T)
    
    Z[((t-1)*n +1):(t*n),] = apply(X = Z[((t-1)*n +1):(t*n),],
                                   MARGIN = 1:2,
                                   FUN = function(x){rnorm(1, x, .1/3)})
    
    
    ### latent matrix ====
    Lmat_t_itoj = scale(x = Matrix::tcrossprod(Z[((t-1)*n +1):(t*n),]),
                        center = F,
                        scale = apply(X = Z[((t-1)*n +1):(t*n),],
                                      MARGIN = 1,
                                      FUN = function(x){sqrt(sum(x^2))}))
    Lmat_t_jtoi = t(Lmat_t_itoj)
    Lmat_t = alpha.latent*Lmat_t_itoj + (1 - alpha.latent)*Lmat_t_jtoi
    
    # storing latent values
    Lvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t)] = Lmat_t[offdiag_t]
    
    
    ### covariate ====
    Xnode_t = mvtnorm::rmvnorm(n = n, mean = numeric(pNode))
    for(j in 1:n){
      
      Xmatvec[n*(n-1)*(t-1) + (((j-1)*(n-1) + 1):(j*(n-1))),] =
        cbind(Xnode_t[-j,],
              matrix(Xnode_t[j,], n-1, pNode, 
                     byrow = T),
              matrix(rnorm((n-1)*(pPair-1)), n-1, pPair-1),
              rbinom(n = n-1, size = 1, prob = .03))
      
    }
    
    
    ### nonzero mean ====
    nzmeanvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t)] =
      c(Xmatvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t),]%*%betareg.cont) + 
      Lvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t)]
    
    # nonzero occurence probability ----
    probitprobvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t)] =
      pnorm(q = beta0.binary + c(Xmatvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t),]%*%betareg.binary) + 
              Lvec[(n*(n-1)*(t-1) + 1):(n*(n-1)*t)])
    
    if(verbose) setTxtProgressBar(pb, t)
    
  }
  
  if(verbose){
    
    cat('\n')
    print('Calculating fixed terms ... Done.')
    cat('\n')
    
  }
  
  if(verbose){
    
    cat('\n')
    print('Simulating data ...')
    
  }
  yvec = deltavec = matrix(nrow = n*(n-1)*nTime, ncol = nReplicate)
  pb = txtProgressBar(min = 1, max = nReplicate, style = 3)
  for(r in 1:nReplicate){
    
    yvec[,r] = rnorm(n = n*(n-1)*nTime,
                     mean = nzmeanvec, sd = noise.sd)
    
    deltavec[,r] = rbinom(n = n*(n-1)*nTime, size = 1,
                          prob = probitprobvec)
    
    if(verbose) setTxtProgressBar(pb, r)
    
  }
  
  if(verbose){
    
    cat('\n')
    print('Simulating data ... Done.')
    cat('\n')
    
  }
  
  output.list = c(input.list,
                  list('nTime' = nTime, 
                       'group1mean' = group1mean, 'group2mean' = group2mean, 'groupmean' = groupmean,
                       'changing.nodes' = changing.nodes, 'changing.times' = changing.times,
                       'Z' = Z, 'Lvec' = Lvec,
                       'nzmeanvec' = nzmeanvec, 'probitprobvec' = probitprobvec,
                       'Xmatvec' = Xmatvec,
                       'yvec' = yvec, 'deltavec' = deltavec,
                       'snr' = abs(nzmeanvec)/noise.sd,
                       'nonzero.prop' = apply(deltavec, 2, mean),
                       'train.id' = 1:(n*(n-1)*nTime.train),
                       'test.id' = (n*(n-1)*nTime.train + 1):(n*(n-1)*nTime)))
  
  if(saveoutput){
    
    if(!dir.exists(output.dir)){
      
      dir.create(output.dir)
      
    }
    
    saveRDS(output.list, file.path(output.dir, output.filename))
    
  }else{
    
    return(output.list)
    
  }
  
}

