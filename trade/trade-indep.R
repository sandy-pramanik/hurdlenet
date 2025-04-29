

rm(list = ls())


# specify paths to source code and data ====
source.code.path = file.path("/.../sourcecode")  # path to the "sourcecode" folder
data.path = "/.../intltrade"  # path to the "tradedata"


# source code and data ====
source(file.path(source.code.path,
                 'hurdlenet-functions.R'))
model.data = readRDS(file.path(data.path, 'model-datan29T20-021525'))


# trade data to fit ====
## number of time points to train and test ====
nTime.train = model.data$nTime-1
train.id = 1:(model.data$n*(model.data$n-1)*nTime.train)

## nonzero trade id ====
deltavec = as.numeric(model.data$lnimports!=0)

## y and X on original scale ====
# y
yvec = model.data$lnimports

# X
Xmatvec = model.data$Xmat
Xmatvec[,model.data$cov_cont] = exp(model.data$Xmat[,model.data$cov_cont])
Xmatvec[,model.data$cov_cont] = 
  scale(x = Xmatvec[,model.data$cov_cont],
        center = colMeans(Xmatvec[train.id,model.data$cov_cont]),
        scale = apply(Xmatvec[train.id,model.data$cov_cont],
                      2, sd))
head(Xmatvec)


# fitting hurdlenet ====
hurdlenet.out = hurdlenet(datalist = list('n' = model.data$n, 'p' = model.data$p,
                                          'nTime.train' = nTime.train,
                                          'y.train' = yvec[train.id],
                                          'X.train' = Xmatvec[train.id,],
                                          'delta.train' = deltavec[train.id],
                                          'nzid.train' = (deltavec==1)[train.id]),
                          standadize.y = F, standadize.X = F,
                          model.choice = 'indep', K.fit = 4, nCore.K = 4, nCore.indep = 2,
                          nBurn = 5, nMCMC = 5, adapt.delta.stan = .8,
                          # nBurn = 4000, nMCMC = 2000, adapt.delta.stan = .8,
                          verbose = 2,
                          source.code.path = source.code.path,
                          saveoutput = T, output.dir = 'trade_output')
