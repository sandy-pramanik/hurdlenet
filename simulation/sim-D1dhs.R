

rm(list = ls())


# specify paths to source code and data ====
source.code.path = file.path("/.../sourcecode")  # path to the "sourcecode" folder
data.path = file.path("/.../simdata")  # path to the "simdata" 

output.dir = "sim_output"
if(!dir.exists(output.dir)){
  dir.create(output.dir)
}


# source code ====
source(file.path(source.code.path, 'hurdlenet-functions.R'))


# simulation ====
param.combinations = as.data.frame(tidyr::expand_grid(nNode = c(5, 10, 20),
                                                      t = c(10)))
for(l in 1:nrow(param.combinations)){
  
  # l = 1
  # nNode = 5
  # t = 5
  nNode = param.combinations$nNode[l]
  t = param.combinations$t[l]
  model.data = readRDS(file.path(data.path, paste0('simdata_n', nNode, '_t', t)))
  
  for(r in 1:3){
  # for(r in 1:model.data$nReplicate){
    
    # r = 1
    
    ## model fitting ====
    hurdlenet.out = hurdlenet(datalist = list('n' = model.data$n, 'p' = model.data$p,
                                              'nTime.train' = model.data$nTime.train,
                                              'y.train' = model.data$yvec[model.data$train.id,r],
                                              'X.train' = model.data$Xmatvec[model.data$train.id,],
                                              'delta.train' = model.data$deltavec[model.data$train.id,r],
                                              'nzid.train' = model.data$deltavec[model.data$train.id,r]==1),
                              standadize.y = F, standadize.X = F,
                              model.choice = 'D1dhs', K.fit = 4, nCore.K = 4,
                              nBurn = 5, nMCMC = 5, adapt.delta.stan = .8,
                              # nBurn = 4000, nMCMC = 1000, adapt.delta.stan = .8,
                              verbose = 2,
                              source.code.path = source.code.path,
                              saveoutput = F)
    
    ## fitted quantities ====
    fitted.y = fitted.hurdlenet(hurdlenet.out = hurdlenet.out,
                                fitted.type = 'y', verbose = F)
    
    fitted.probit = fitted.hurdlenet(hurdlenet.out = hurdlenet.out,
                                     fitted.type = 'probit', verbose = F)
    
    ## predictive quantities ====
    pred.y = predict.hurdlenet(datalist = list('X.test' = model.data$Xmatvec[model.data$test.id,]),
                                   nTime.pred = model.data$nTime.test,
                                   hurdlenet.out = hurdlenet.out,
                                   predict.type = 'y', verbose = F)
    
    pred.probit = predict.hurdlenet(datalist = list('X.test' = model.data$Xmatvec[model.data$test.id,]),
                                   nTime.pred = model.data$nTime.test,
                                   hurdlenet.out = hurdlenet.out,
                                   predict.type = 'probit', verbose = F)
    
    print(paste0('n = ', nNode, ', T = ', t, ": Replication ", r))
    
    saveRDS(list("ic" = hurdlenet.out$MCMCout$ic[c("waic_Estimate", "looic_Estimate"),],
                 "betareg_cont" = hurdlenet.out$MCMCout$betareg_cont,
                 "sigma" = sqrt(hurdlenet.out$MCMCout$sigma2),
                 "nzmean_fitted" = apply(fitted.y$EY, 2:3, mean),
                 "betareg_binary" = hurdlenet.out$MCMCout$betareg_binary,
                 "probitprob_fitted" = apply(fitted.probit$probitprob, 2:3, mean),
                 "nzmean_pred" = apply(pred.y$y, 2:3, mean),
                 "probitprob_pred" = apply(pred.probit$probitprob, 2:3, mean)),
            file.path(output.dir, 
                      paste0(hurdlenet.out$input$model.choice,
                             '_n', nNode, '_t', t, '_r', r)))
    
  }
  
}

