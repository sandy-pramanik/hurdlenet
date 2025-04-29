

# trade data summary ====
## specify paths to output and data ====
rm(list = ls())
data.path = "/.../intltrade"  # path to the "tradedata"

model.data = readRDS(file.path(data.path, 'model-datan29T20-021525'))

# zero vs non-zero proportion
plotdf = data.frame('occurrence' = c('0', '1'),
                    'value' = c(mean(model.data$lnimports==0),
                                mean(model.data$lnimports!=0)))
plotdf$occurrence = factor(x = plotdf$occurrence,
                           levels = c('0', '1'))
occurrence.label = c('0' = 'Did not occur',
                     '1' = 'Occurred')

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,1)) +
  ggplot2::geom_bar(ggplot2::aes(x = occurrence, y = value, fill = site),
                    stat = "identity", fill = "dodgerblue3", alpha = .7,
                    color = 'black', linewidth = .4,
                    # width = .7,
                    position = 'dodge' #ggplot2::position_dodge(width = .1)
  ) +
  ggplot2::scale_x_discrete(labels = occurrence.label) +
  # ggplot2::geom_histogram(ggplot2::aes(x = lny, y = ggplot2::after_stat(density)),
  #                         fill = "dodgerblue3", alpha = .7,
  #                         color = 'black', linewidth = .4) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 14),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 10,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 10,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    # legend.key.size = ggplot2::unit(.5, "cm"),
    legend.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  # ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(x = "Trade occurence", 
                y = 'Observed proportion', fill = NULL) # 4.5 X 5

# histogram of log(nonzero import volume)
ggplot2::ggplot(data = data.frame('lny' = model.data$lnimports[model.data$lnimports!=0])) +
  # ggplot2::coord_cartesian(ylim = c(0,1)) +
  ggplot2::geom_histogram(ggplot2::aes(x = lny, y = ggplot2::after_stat(density)),
                          fill = "dodgerblue3", alpha = .7,
                          color = 'black', linewidth = .4) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 14),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 10,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 10,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    # legend.key.size = ggplot2::unit(.5, "cm"),
    legend.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  # ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(x = "log-transformed nonzero trade volume", 
                y = 'Density', fill = NULL) # 4.5 X 5




# trade data dynamics ====
## specify paths to output and data ====
rm(list = ls())
data.path = "/.../intltrade"  # path to the "tradedata"

model.data = readRDS(file.path(data.path, 'model-datan29T20-021525'))

# zero vs non-zero proportion
plotdf = do.call("cbind",
                 lapply(1:model.data$nTime,
                        FUN = function(t){
                          
                          model.data$lnimports[((t-1)*model.data$n*(model.data$n-1) + 1):(t*model.data$n*(model.data$n-1))]
                          
                        }))
colnames(plotdf) = model.data$years[1:ncol(plotdf)]
rownames(plotdf) = do.call("c",
                           lapply(1:model.data$n,
                                  FUN = function(i){
                                    
                                    paste0(model.data$countries.iso[-i], "_",
                                           model.data$countries.iso[i])
                                    
                                  }))
plotdf[1:5,1:5]
plotdf = plotdf[1:103,]

plotdf_melt = reshape2::melt(plotdf)
plotdf_melt$Var1 = as.character(plotdf_melt$Var1)
plotdf_melt$Var2 = as.character(plotdf_melt$Var2)
head(plotdf_melt)

plotdf_melt$Var1 = factor(x = plotdf_melt$Var1, levels = rev(rownames(plotdf)))
plotdf_melt$Var2 = as.factor(x = plotdf_melt$Var2)

# cause.label = paste0("Cause ", 1:nCause)
# names(cause.label) = allcauses

ggplot2::ggplot(data = plotdf_melt) + 
  ggplot2::geom_tile(
    ggplot2::aes(Var1, Var2, fill = value),
    color="white", linewidth=0
  ) + 
  # ggplot2::geom_text(ggplot2::aes(Var2, Var1, size = (11*value)/100
  # ),
  # # size = 10,
  # label = value.labels, show.legend = F,
  # color = "black", #size = 5,
  # fontface = 'bold'
  # ) +
  ggplot2::scale_fill_gradient(low="white", high="blue3",
                               # breaks = seq(0, 100, 20),
                               # limits = c(0,100),
                               guide = "colourbar",
                               name = 'Log Trade Volume') +
  # ggplot2::scale_x_discrete(labels = cause.label) +
  # ggplot2::scale_y_discrete(labels = cause.label) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size=25, face = "bold",
                                       margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    plot.subtitle = ggplot2::element_text(size=17,
                                          margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 30, hjust = 1, vjust = 1),
    # axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.text.y = ggplot2::element_blank(),
    # axis.ticks.x = ggplot2::element_blank(),
    # axis.ticks.length.x = unit(.2, "cm"),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    # axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.2, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.2, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 14,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 14,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    # legend.key.size = ggplot2::unit(.5, "cm"),
    legend.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=15),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    # title = paste0(rownames(vacod_cacode)[l], ", ", countries[l],
    #                " (n=", sum(vacod_cacode[l,!is.na(vacod_cacode[l,])]), ")"),
    # subtitle = 'Expected Prior Misclassification (CHAMPS)',
    x = 'Year',
    y = 'Country Pair'
  ) # 3 X 7


# head(plotdf)
# plotdf$occurrence = factor(x = plotdf$occurrence,
#                            levels = c('0', '1'))
# occurrence.label = c('0' = 'Did not occur',
#                      '1' = 'Occurred')
# 
# ggplot2::ggplot(data = plotdf) +
#   ggplot2::coord_cartesian(ylim = c(0,1)) +
#   ggplot2::geom_bar(ggplot2::aes(x = occurrence, y = value, fill = site),
#                     stat = "identity", fill = "dodgerblue3", alpha = .7,
#                     color = 'black', linewidth = .4,
#                     # width = .7,
#                     position = 'dodge' #ggplot2::position_dodge(width = .1)
#   ) +
#   ggplot2::scale_x_discrete(labels = occurrence.label) +
#   # ggplot2::geom_histogram(ggplot2::aes(x = lny, y = ggplot2::after_stat(density)),
#   #                         fill = "dodgerblue3", alpha = .7,
#   #                         color = 'black', linewidth = .4) +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_text(size=16,
#                                          margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#     axis.title.y = ggplot2::element_text(size=16,
#                                          margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#     axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                         angle = 0, hjust = .5, vjust = 1),
#     axis.text.y = ggplot2::element_text(color = "black", size = 14),
#     axis.ticks.x = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#     axis.ticks.y = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#     panel.background = ggplot2::element_blank(),
#     panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                          fill = NA, linewidth = 1),
#     panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     strip.text.x = ggplot2::element_text(
#       size = 10,
#       face = "bold"
#     ),
#     strip.text.y = ggplot2::element_text(
#       size = 10,
#       face = "bold"
#     ),
#     strip.background = ggplot2::element_rect(color="black", linewidth=1),
#     legend.title = ggplot2::element_blank(),
#     legend.key.width = ggplot2::unit(.75, "cm"),
#     legend.key.height = ggplot2::unit(.75, "cm"),
#     # legend.key.size = ggplot2::unit(.5, "cm"),
#     legend.spacing.x = ggplot2::unit(.5, 'cm'),
#     legend.text=ggplot2::element_text(size=13),
#     legend.position = 'bottom'
#   ) +
#   # ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
#   ggplot2::labs(x = "Trade Occurence", 
#                 y = 'Observed Proportion', fill = NULL) # 4.5 X 5

# histogram of log(nonzero import volume)
ggplot2::ggplot(data = data.frame('lny' = model.data$lnimports[model.data$lnimports!=0])) +
  # ggplot2::coord_cartesian(ylim = c(0,1)) +
  ggplot2::geom_histogram(ggplot2::aes(x = lny, y = ggplot2::after_stat(density)),
                          fill = "dodgerblue3", alpha = .7,
                          color = 'black', linewidth = .4) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 14),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 10,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 10,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    # legend.key.size = ggplot2::unit(.5, "cm"),
    legend.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  # ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(x = "log (Import Volume)", 
                y = 'Density', fill = NULL) # 4.5 X 5



# trade data analysis ====
## specify paths to output and data ====
rm(list = ls())

# specify paths to output and data ====
source.code.path = file.path("/.../sourcecode")  # path to the "sourcecode" folder
data.path = "/.../intltrade"  # path to the "tradedata"


# sourcing code
source(file.path(source.code.path, "hurdlenet-functions.R"))

## loading outputs ====
outputs.sources = c(
  'D1dhs' = "D1dhs",
  'D0dhs' = "D0dhs",
  'indep' = "indep",
  'shs' = "shs"
)
# outputs.source.path = c(
#   'D1dhs' = 1,
#   # 'D0dhs' = 1,
#   'indep' = 1,
#   'shs' = 1#,
#   # 'nolatent' = "nolatent"
# )
hurdlenet_out_trade = vector(mode = 'list', length = length(outputs.sources))
names(hurdlenet_out_trade) = names(outputs.sources)
for(l in 1:length(outputs.sources)){
  
  # hurdlenet_out_trade[[l]] = readRDS(file.path(output.path[[outputs.source.path[l]]], 
  #                                              outputs.sources[l]))
  hurdlenet_out_trade[[l]] = readRDS(file.path(getwd(), 
                                               "hurdlenet-trade", "hurdlenet_trade_output_longrun",
                                               outputs.sources[l]))
  
  # if(hurdlenet_out_trade[[l]]$input$model.choice %in% c("D1dhs", "D0dhs", "shs")){
  #   
  #   if("h" %in% names(hurdlenet_out_trade[[l]]$MCMCout)) hurdlenet_out_trade[[l]]$MCMCout$h = 2*log(hurdlenet_out_trade[[l]]$input$sd.y) + hurdlenet_out_trade[[l]]$MCMCout$h
  #   
  #   if("mu0" %in% names(hurdlenet_out_trade[[l]]$MCMCout)) hurdlenet_out_trade[[l]]$MCMCout$mu0 = 2*log(hurdlenet_out_trade[[l]]$input$sd.y) + hurdlenet_out_trade[[l]]$MCMCout$mu0
  #   
  # }else if(hurdlenet_out_trade[[l]]$input$model.choice=="indep"){
  #   
  #   if("h" %in% names(hurdlenet_out_trade[[l]]$MCMCout)) hurdlenet_out_trade[[l]]$MCMCout$h[,,,1] = 2*log(hurdlenet_out_trade[[l]]$input$sd.y) + hurdlenet_out_trade[[l]]$MCMCout$h[,,,1]
  #   
  #   if("mu0" %in% names(hurdlenet_out_trade[[l]]$MCMCout)) hurdlenet_out_trade[[l]]$MCMCout$mu0[,,1] = 2*log(hurdlenet_out_trade[[l]]$input$sd.y) + hurdlenet_out_trade[[l]]$MCMCout$mu0[,,1]
  #   
  # }
  
  print(outputs.sources[l])
  
}

identical(names(hurdlenet_out_trade),
          names(outputs.sources))

# par(mfrow=c(2,2))
# for(l in 1:length(hurdlenet_out_trade)){
#   
#   matplot(1:hurdlenet_out_trade[[l]]$input$nMCMC, 
#           apply(hurdlenet_out_trade[[l]]$MCMCout$loglik, 
#                 c(1,3), mean), 
#           type = 'l', lty=1, ylim = c(-4, -1),
#           col=1:hurdlenet_out_trade[[l]]$input$K.fit)
#   
# }
# par(mfrow=c(1,1))



## loading trade data ====
model.data = readRDS(file.path(data.path, 'model-datan29T20-021525'))

nTime.train = model.data$nTime-1
nTime.test = 1
train.id = 1:(model.data$n*(model.data$n-1)*nTime.train)
deltavec = as.numeric(model.data$lnimports!=0)

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

nzid = deltavec==1


### training data ====
yvec.train = yvec[train.id]
Xmatvec.train = Xmatvec[train.id,]
deltavec.train = deltavec[train.id]
nzid.train = nzid[train.id]

### test data ====
test.id = (model.data$n*(model.data$n-1)*nTime.train + 1):(model.data$n*(model.data$n-1)*model.data$nTime)
yvec.test = yvec[test.id]
Xmatvec.test = Xmatvec[test.id,]
deltavec.test = deltavec[test.id]
nzid.test = nzid[test.id]



## ic ====
plotdf = do.call('rbind.data.frame',
                 lapply(X = 1:length(hurdlenet_out_trade),
                        FUN = function(X){
                          
                          print(X)
                          
                          if(hurdlenet_out_trade[[X]]$input$model.choice!='nolatent'){
                            
                            return(rbind.data.frame(data.frame('ic' = hurdlenet_out_trade[[X]]$MCMCout$ic["looic_Estimate",1:hurdlenet_out_trade[[1]]$input$K.fit],
                                                               'k' = 1:hurdlenet_out_trade[[1]]$input$K.fit,
                                                               'ic.type' = 'looic',
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice),
                                                    data.frame('ic' = hurdlenet_out_trade[[X]]$MCMCout$ic["waic_Estimate",1:hurdlenet_out_trade[[1]]$input$K.fit],
                                                               'k' = 1:hurdlenet_out_trade[[1]]$input$K.fit,
                                                               'ic.type' = 'waic',
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice)))
                            
                          }else if(hurdlenet_out_trade[[X]]$input$model.choice=='nolatent'){
                            
                            return(rbind.data.frame(data.frame('ic' = hurdlenet_out_trade[[X]]$MCMCout$ic["looic_Estimate"],
                                                               'k' = 0,
                                                               'ic.type' = 'looic',
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice),
                                                    data.frame('ic' = hurdlenet_out_trade[[X]]$MCMCout$ic["waic_Estimate"],
                                                               'k' = 0,
                                                               'ic.type' = 'waic',
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice)))
                            
                          }
                          
                        }))

plotdf$ic.type = factor(x = plotdf$ic.type, levels = c('looic', 'waic'))
plotdf$method = factor(x = plotdf$method, 
                       levels = c('D1dhs', 'D0dhs', 'indep',
                                  'shs', 'nolatent'))

method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static',
                 'nolatent' = 'No latent')

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

ic.label = c('looic' = 'LOO-IC', 'waic' = 'WAIC')


### waic and loo-ic ====
ggplot2::ggplot(data = plotdf) +
  # ggplot2::coord_cartesian(ylim = c(0,1)) +
  ggplot2::facet_grid(.~ic.type,
                      labeller = ggplot2::labeller(ic.type = ic.label)) +
  # ggplot2::geom_point(data = subset(plotdf, method!='nolatent'),
  #                     shape = 16, size = 8, stroke = 3#,
  #                     # position = ggplot2::position_dodge(width = 1)
  #                     ) +
  # ggplot2::geom_line(data = subset(plotdf, method!='nolatent'),
  #                    linewidth = 2) +
  ggplot2::geom_point(data = subset(plotdf, method!='nolatent'),
                      ggplot2::aes(x = k, y = ic, color = method),
                      shape = 16, size = 5, stroke = 2) +
  ggplot2::geom_line(data = subset(plotdf, method!='nolatent'),
                     ggplot2::aes(x = k, y = ic, color = method),
                     linewidth = 1.25) +
  # ggplot2::geom_hline(data = subset(plotdf, method=='nolatent'),
  #                     ggplot2::aes(yintercept = ic, color = method),
  #                     linewidth = 1) +
  ggplot2::scale_color_manual(values = method.color,
                              labels = method.label) +
  # ggplot2::scale_y_continuous(trans = 'log') +
  ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 20, face = "bold"),
                 plot.subtitle = ggplot2::element_text(color = "black", size = 15),
                 axis.title.x = ggplot2::element_text(size=16,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.title.y = ggplot2::element_text(size=16,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 14,
                                                     angle = 0, hjust = .5, vjust = 1),
                 axis.text.y = ggplot2::element_text(color = "black", size = 14),
                 axis.ticks.x = ggplot2::element_line(linewidth = .5),
                 axis.ticks.length.x = ggplot2::unit(.15, "cm"),
                 axis.ticks.y = ggplot2::element_line(linewidth = .5),
                 axis.ticks.length.y = ggplot2::unit(.15, "cm"),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 1.5),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 15, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 15, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
                 legend.title = ggplot2::element_blank(),
                 # legend.title = ggplot2::element_text(size = 20, face = "bold"),
                 legend.key.width = ggplot2::unit(1.5, "cm"), legend.key.height = ggplot2::unit(1, "cm"),
                 legend.key.size = ggplot2::unit(30, "cm"),
                 legend.key.spacing.x = ggplot2::unit(1, 'cm'), 
                 legend.text=ggplot2::element_text(size=17),
                 legend.position = 'bottom') +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
  ggplot2::labs(
    # title = 'International Trade Data Analysis',
    # subtitle = 'Information Criterion',
    y = 'Information Criterion', 
    x = 'Latent Dimension (k)'
  )   # 6.5 X 12


### loo-ic ====
head(plotdf)
ggplot2::ggplot(data = plotdf[plotdf$ic.type=="looic",]) +
  # ggplot2::coord_cartesian(ylim = c(0,1)) +
  # ggplot2::facet_grid(.~ic.type,
  #                     labeller = ggplot2::labeller(ic.type = ic.label)) +
  # ggplot2::geom_point(data = subset(plotdf, method!='nolatent'),
  #                     shape = 16, size = 8, stroke = 3#,
  #                     # position = ggplot2::position_dodge(width = 1)
  #                     ) +
  # ggplot2::geom_line(data = subset(plotdf, method!='nolatent'),
  #                    linewidth = 2) +
  ggplot2::geom_point(ggplot2::aes(x = k, y = ic, color = method),
                      shape = 16, size = 6, stroke = 2) +
  ggplot2::geom_line(ggplot2::aes(x = k, y = ic, color = method),
                     linewidth = 1.4) +
  # ggplot2::geom_hline(data = subset(plotdf, method=='nolatent'),
  #                     ggplot2::aes(yintercept = ic, color = method),
  #                     linewidth = 1) +
  ggplot2::scale_color_manual(values = method.color,
                              labels = method.label) +
  # ggplot2::scale_y_continuous(trans = 'log') +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size=17,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.title.y = ggplot2::element_text(size=17,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                                     angle = 0, hjust = .5, vjust = 1),
                 axis.text.y = ggplot2::element_text(color = "black", size = 15),
                 axis.ticks.x = ggplot2::element_line(linewidth = .5),
                 axis.ticks.length.x = ggplot2::unit(.15, "cm"),
                 axis.ticks.y = ggplot2::element_line(linewidth = .5),
                 axis.ticks.length.y = ggplot2::unit(.15, "cm"),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 1.5),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 15, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 15, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
                 legend.title = ggplot2::element_blank(),
                 # legend.title = ggplot2::element_text(size = 20, face = "bold"),
                 legend.key.width = ggplot2::unit(1.5, "cm"), legend.key.height = ggplot2::unit(1, "cm"),
                 legend.key.size = ggplot2::unit(30, "cm"),
                 legend.spacing.x = ggplot2::unit(.75, 'cm'), 
                 legend.key.spacing.x = ggplot2::unit(1, 'cm'), 
                 legend.text=ggplot2::element_text(size=16),
                 legend.position = 'bottom') +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
  ggplot2::labs(
    # title = 'International Trade Data Analysis',
    # subtitle = 'Information Criterion',
    y = 'LOO-IC', 
    x = 'Latent Dimension (k)'
  )   # 6.5 X 10



# nonzero trade volume prediction ====
## scatterplot and mspe ====
plotdf_tradevol = NULL
mspe_tradevol = matrix(nrow = length(hurdlenet_out_trade), 
                    ncol = hurdlenet_out_trade[[1]]$input$K.fit + 1)
rownames(mspe_tradevol) = names(hurdlenet_out_trade)
colnames(mspe_tradevol) = paste0('k', 0:hurdlenet_out_trade[[1]]$input$K.fit)
for(l in 1:length(hurdlenet_out_trade)){
  
  # l = 1
  pred.out = predict.hurdlenet(datalist = list('X.test' = Xmatvec.test),
                               nTime.pred = nTime.test,
                               hurdlenet.out = hurdlenet_out_trade[[l]],
                               predict.type = 'y')
  
  if(hurdlenet_out_trade[[l]]$input$model.choice!='nolatent'){
    
    predsumm = apply(pred.out$y, 2:3,
                     FUN = function(v){
                       
                       median(v, na.rm = T)
                       # mean(v, na.rm = T)
                       
                     })
    
    plotdf_tradevol = rbind.data.frame(plotdf_tradevol,
                              data.frame('ynztest' = rep(yvec.test[nzid.test],
                                                         hurdlenet_out_trade[[l]]$input$K.fit),
                                         'est' = as.numeric(predsumm[nzid.test,]),
                                         'k' = rep(1:hurdlenet_out_trade[[l]]$input$K.fit,
                                                   each = sum(nzid.test)),
                                         'method' = hurdlenet_out_trade[[l]]$input$model.choice))
    
    for(k in 1:hurdlenet_out_trade[[l]]$input$K.fit){
      
      mspe_tradevol[l,k+1] = sqrt(sum((yvec.test[nzid.test] - predsumm[nzid.test,k])^2))
      
    }
    
  }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
    
    predsumm = apply(pred.out$y, 2,
                     FUN = function(v){
                       
                       median(v, na.rm = T)
                       # mean(v, na.rm = T)
                       
                     })
    
    plotdf_tradevol = rbind.data.frame(plotdf_tradevol,
                              data.frame('ynztest' = yvec.test[nzid.test],
                                         'est' = predsumm[nzid.test],
                                         'k' = 0,
                                         'method' = hurdlenet_out_trade[[l]]$input$model.choice))
    
    mspe_tradevol[l,1] = sqrt(sum((yvec.test[nzid.test] - predsumm[nzid.test])^2))
    
  }
  
  print(hurdlenet_out_trade[[l]]$input$model.choice)
  
}

plotdf_tradevol.mspe = data.frame('mspe' = as.numeric(mspe_tradevol),
                         'k' = rep(0:hurdlenet_out_trade[[1]]$input$K.fit,
                                   each = length(hurdlenet_out_trade)),
                         'method' = rep(do.call('c',
                                                lapply(1:length(hurdlenet_out_trade),
                                                       FUN = function(l){
                                                         
                                                         hurdlenet_out_trade[[l]]$input$model.choice
                                                         
                                                       })
                         ),
                         hurdlenet_out_trade[[1]]$input$K.fit+1))
plotdf_tradevol.mspe = plotdf_tradevol.mspe[!is.na(plotdf_tradevol.mspe$mspe),]
plotdf_tradevol.mspe$mspe_label = as.character(round(plotdf_tradevol.mspe$mspe, 2))

plotdf_tradevol$k = factor(x = plotdf_tradevol$k)
plotdf_tradevol.mspe$k = factor(x = plotdf_tradevol.mspe$k)
plotdf_tradevol$method = factor(x = plotdf_tradevol$method, 
                       levels = c('D1dhs', 'D0dhs', 'indep',
                                  'shs', 'nolatent'))
plotdf_tradevol.mspe$method = factor(x = plotdf_tradevol.mspe$method,
                            levels = c('D1dhs', 'D0dhs', 'indep',
                                       'shs', 'nolatent'))

k.label = paste0('K=', 0:hurdlenet_out_trade[[1]]$input$K.fit)
names(k.label) = 0:hurdlenet_out_trade[[1]]$input$K.fit

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static',
                 'nolatent' = 'No latent')

ggplot2::ggplot(data = plotdf_tradevol) +
  # ggplot2::coord_cartesian(ylim = c(0,1), xlim = c(1,3)) +
  ggh4x::facet_grid2(method~k, 
                     # nrow = length(hurdlenet_out_trade),
                     # ncol = hurdlenet_out_trade[[1]]$input$K.fit,
                     scales = 'fixed',
                     labeller = ggplot2::labeller(method = method.label,
                                                  k = k.label)) +
  # ggh4x::facet_wrap2(method~k, nrow = length(hurdlenet_out_trade),
  #                    ncol = hurdlenet_out_trade[[1]]$input$K.fit,
  #                    scales = 'free',
  #                     labeller = ggplot2::labeller(method = method.label,
  #                                                  k = k.label)) +
  # ggplot2::facet_grid(method~k,
  #                     labeller = ggplot2::labeller(method = method.label,
  #                                                  k = k.label)) +
  ggplot2::geom_point(ggplot2::aes(x = ynztest, y = est),
                      color = 'dodgerblue2', alpha = .5,
                      shape = 16, size = 1.5, stroke = 1) +
  # ggplot2::geom_errorbar(ggplot2::aes(x = ynztest, y = est,
  #                                     ymin = llim, ymax = ulim, color = method),
  #                        width = .1, linewidth = 1.5, alpha = .5) +
  ggplot2::geom_abline(intercept = 0, slope = 1, 
                       linewidth = 1, linetype = 'dashed') +
  ggplot2::geom_label(
    data = plotdf_tradevol.mspe,
    ggplot2::aes(x = 11.2, y = -100, label = mspe_label), fontface = "bold", size = 6
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=20,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    # axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size=20,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    # axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    # axis.ticks.x = ggplot2::element_line(linewidth = 1),
    axis.ticks.length.x = ggplot2::unit(.15, "cm"),
    axis.ticks.length.y = ggplot2::unit(.15, "cm"),
    # axis.ticks.x = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1.5),
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(size = 17, face = "bold"),
    strip.text.y = ggplot2::element_text(size = 17, face = "bold"),
    strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
    legend.title = ggplot2::element_blank(),
    legend.key.width = ggplot2::unit(3, "cm"), legend.key.height = ggplot2::unit(2, "cm"), 
    legend.key.size = ggplot2::unit(30, "cm"),
    legend.spacing.x = ggplot2::unit(2, 'cm'), legend.text=ggplot2::element_text(size=15),
    legend.position = 'bottom'
  ) +
  # ggplot2::guides(color = ggplot2::guide_legend(
  #   nrow = 2, byrow=FALSE#,
  #   # override.aes = list(
  #   #   linetype = "solid",
  #   #   shape = c(rep(16, length(levels(plotdf_tradevol$method))-1), NA))
  # )) +
  ggplot2::labs(
    # title = 'International Trade Data Analysis',
    # subtitle = 'Nonzero (log) Trade Volume Prediction',
    x = 'Observed',
    y = 'Predictive Median'
  )   # 9.5 X 13

round(mspe_tradevol, 2)


# # K=2 nonzero trade volume prediction ====
# ## scatterplot and mspe ====
# K.best = 2
# plotdf_tradevol_best = NULL
# for(l in 1:length(hurdlenet_out_trade)) {
#   
#   if(hurdlenet_out_trade[[l]]$input$model.choice %in% c('D1dhs', 'D0dhs', 'indep', 'shs')){
#     
#     plotdf_tradevol_best = rbind.data.frame(plotdf_tradevol_best,
#                                 plotdf_tradevol[(plotdf_tradevol$method==hurdlenet_out_trade[[l]]$input$model.choice)&
#                                          (plotdf_tradevol$k==K.best),])
#     
#   }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
#     
#     plotdf_tradevol_best = rbind.data.frame(plotdf_tradevol_best,
#                                 plotdf_tradevol[plotdf_tradevol$method=='nolatent',])
#     
#   }
#   
#   print(hurdlenet_out_trade[[l]]$input$model.choice)
#   
# }
# 
# plotdf_tradevol.mspe_best = plotdf_tradevol.mspe[(plotdf_tradevol.mspe$method=='nolatent')|
#                               (plotdf_tradevol.mspe$k==K.best),]
# 
# method.label = c('D1dhs' = 'Hurdle-Net(1)',
#                  'D0dhs' = 'Hurdle-Net(0)',
#                  'indep' = 'Independent',
#                  'shs' = 'Static',
#                  'nolatent' = 'No latent')
# 
# ggplot2::ggplot(data = plotdf_tradevol_best) +
#   # ggplot2::coord_cartesian(ylim = c(0,1), xlim = c(1,3)) +
#   ggplot2::facet_grid(.~method,
#                       labeller = ggplot2::labeller(method = method.label)) +
#   ggplot2::geom_point(ggplot2::aes(x = ynztest, y = est),
#                       color = 'dodgerblue2', alpha = .7,
#                       shape = 16, size = 1.5, stroke = 1) +
#   # ggplot2::geom_errorbar(ggplot2::aes(x = ynztest, y = est,
#   #                                     ymin = llim, ymax = ulim, color = method),
#   #                        width = .1, linewidth = 1.5, alpha = .5) +
#   ggplot2::geom_abline(intercept = 0, slope = 1, 
#                        linewidth = .5, linetype = 'dashed') +
#   ggplot2::geom_label(
#     data = plotdf_tradevol.mspe_best,
#     ggplot2::aes(x = 25, y = 12, label = mspe_label),
#     fontface = "bold", size = 5
#   ) +
#   ggplot2::theme(plot.title = ggplot2::element_text(size=22, face="bold"),
#                  plot.subtitle = ggplot2::element_text(size=20),
#                  axis.title.x = ggplot2::element_text(size=20,
#                                                       margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#                  # axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size=20,
#                                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#                  axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                                      angle = 0, hjust = .5, vjust = 1),
#                  # axis.text.x = ggplot2::element_blank(),
#                  axis.text.y = ggplot2::element_text(color = "black", size = 15),
#                  # axis.ticks.x = ggplot2::element_line(linewidth = 1),
#                  axis.ticks.length.x = ggplot2::unit(.15, "cm"),
#                  axis.ticks.length.y = ggplot2::unit(.15, "cm"),
#                  # axis.ticks.x = ggplot2::element_blank(),
#                  panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                                       fill = NA, linewidth = 1.5),
#                  panel.background = ggplot2::element_blank(),
#                  panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  strip.text.x = ggplot2::element_text(size = 20, face = "bold"),
#                  strip.text.y = ggplot2::element_text(size = 20, face = "bold"),
#                  strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
#                  legend.title = ggplot2::element_blank(),
#                  legend.key.width = ggplot2::unit(3, "cm"), legend.key.height = ggplot2::unit(2, "cm"), 
#                  legend.key.size = ggplot2::unit(30, "cm"),
#                  legend.spacing.x = ggplot2::unit(2, 'cm'), legend.text=ggplot2::element_text(size=15),
#                  legend.position = 'bottom') +
#   # ggplot2::guides(color = ggplot2::guide_legend(
#   #   nrow = 2, byrow=FALSE#,
#   #   # override.aes = list(
#   #   #   linetype = "solid",
#   #   #   shape = c(rep(16, length(levels(plotdf_tradevol$method))-1), NA))
#   # )) +
#   ggplot2::labs(
#     # title = 'International Trade Data Analysis',
#     # subtitle = 'Nonzero (log) Trade Volume Prediction',
#     x = 'Observed',
#     y = 'Predictive Median'
#   )   # 4 X 16



# trade occurence probability ====
## roc and auc ====
plotdf_roc = NULL
auc_prob = matrix(nrow = length(hurdlenet_out_trade), 
                  ncol = hurdlenet_out_trade[[1]]$input$K.fit + 1)
rownames(auc_prob) = names(hurdlenet_out_trade)
colnames(auc_prob) = paste0('k', 0:hurdlenet_out_trade[[1]]$input$K.fit)
for(l in 1:length(hurdlenet_out_trade)){
  
  pred.out = predict.hurdlenet(datalist = list('X.test' = Xmatvec.test),
                               nTime.pred = nTime.test,
                               hurdlenet.out = hurdlenet_out_trade[[l]],
                               predict.type = 'probit')
  
  if(hurdlenet_out_trade[[l]]$input$model.choice!='nolatent'){
    
    predsumm = apply(pred.out$probitprob, 2:3,
                     FUN = function(v){
                       
                       median(v)
                       
                     })
    
    for(k in 1:hurdlenet_out_trade[[l]]$input$K.fit){
      
      roc_out = pROC::roc(response = factor(x = deltavec.test, levels = c(0,1)),
                          predictor = predsumm[,k],
                          percent = T, quiet = T, smooth = T, auc = T)
      
      plotdf_roc = rbind.data.frame(plotdf_roc,
                                data.frame('roc_sens' = roc_out$sensitivities,
                                           'roc_fp' = 100 - roc_out$specificities,
                                           'k' = k,
                                           'method' = hurdlenet_out_trade[[l]]$input$model.choice))
      
      auc_prob[l,k+1] = roc_out$auc
      
    }
    
  }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
    
    predsumm = apply(pred.out$probitprob, 2,
                     FUN = function(v){
                       
                       median(v)
                       
                     })
    
    roc_out = pROC::roc(response = factor(x = deltavec.test, levels = c(0,1)),
                        predictor = predsumm,
                        percent = T, quiet = T, #smooth = T,
                        auc = T)
    
    plotdf_roc = rbind.data.frame(plotdf_roc,
                              data.frame('roc_sens' = roc_out$sensitivities,
                                         'roc_fp' = 100 - roc_out$specificities,
                                         'k' = 0,
                                         'method' = hurdlenet_out_trade[[l]]$input$model.choice))
    
    auc_prob[l,1] = roc_out$auc
    
  }
  
  print(hurdlenet_out_trade[[l]]$input$model.choice)
  
}

plotdf.auc = data.frame('auc' = as.numeric(auc_prob),
                        'k' = rep(0:hurdlenet_out_trade[[1]]$input$K.fit,
                                  each = length(hurdlenet_out_trade)),
                        'method' = rep(do.call('c',
                                               lapply(1:length(hurdlenet_out_trade),
                                                      FUN = function(l){
                                                        
                                                        hurdlenet_out_trade[[l]]$input$model.choice
                                                        
                                                      })
                        ),
                        hurdlenet_out_trade[[1]]$input$K.fit+1))
plotdf.auc = plotdf.auc[!is.na(plotdf.auc$auc),]
plotdf.auc$auc_label = paste0(round(plotdf.auc$auc), '%')

plotdf_roc$k = factor(x = plotdf_roc$k)
plotdf.auc$k = factor(x = plotdf.auc$k)
plotdf_roc$method = factor(x = plotdf_roc$method, 
                       levels = c('D1dhs', 'D0dhs', 'indep',
                                  'shs', 'nolatent'))
plotdf.auc$method = factor(x = plotdf.auc$method, 
                           levels = c('D1dhs', 'D0dhs', 'indep',
                                      'shs', 'nolatent'))

k.label = paste0('K=', 0:hurdlenet_out_trade[[1]]$input$K.fit)
names(k.label) = 0:hurdlenet_out_trade[[1]]$input$K.fit

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static',
                 'nolatent' = 'No latent')

ggplot2::ggplot(data = plotdf_roc) +
  ggplot2::coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  ggplot2::facet_grid(method~k,
                      labeller = ggplot2::labeller(method = method.label,
                                                   k = k.label)) +
  ggplot2::geom_line(
    ggplot2::aes(x = roc_fp, y = roc_sens),
    linewidth = .7, color = 4
  ) +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  ggplot2::geom_label(
    data = plotdf.auc,
    ggplot2::aes(x = 75, y = 25, label = auc_label), fontface = "bold"
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(size=22, face="bold"),
                 plot.subtitle = ggplot2::element_text(size=20),
                 axis.title.x = ggplot2::element_text(size=20,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 # axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size=20,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                                     angle = 0, hjust = .5, vjust = 1),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(color = "black", size = 15),
                 # axis.ticks.x = ggplot2::element_line(linewidth = 1),
                 axis.ticks.length.x = ggplot2::unit(.2, "cm"),
                 # axis.ticks.x = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 1.5),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 15, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 15, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
                 legend.title = ggplot2::element_blank(),
                 legend.key.width = ggplot2::unit(3, "cm"), legend.key.height = ggplot2::unit(2, "cm"), 
                 legend.key.size = ggplot2::unit(30, "cm"),
                 legend.spacing.x = ggplot2::unit(2, 'cm'), legend.text=ggplot2::element_text(size=15),
                 legend.position = 'bottom') +
  # ggplot2::guides(color = ggplot2::guide_legend(
  #   nrow = 2, byrow=FALSE#,
  #   # override.aes = list(
  #   #   linetype = "solid",
  #   #   shape = c(rep(16, length(levels(plotdf$method))-1), NA))
  # )) +
  ggplot2::labs(
    # title = 'International Trade Data Analysis',
    # subtitle = 'Trade Occurence Prediction',
    x = 'False Positive Rate (FPR)',
    y = 'Sensitivity'
  )   # 10 X 14

round(auc_prob, 2)



## boxplot and mspe ====
plotdf_prob = NULL
mspe_prob = matrix(nrow = length(hurdlenet_out_trade), 
                   ncol = hurdlenet_out_trade[[1]]$input$K.fit + 1)
rownames(mspe_prob) = names(hurdlenet_out_trade)
colnames(mspe_prob) = paste0('k', 0:hurdlenet_out_trade[[1]]$input$K.fit)
for(l in 1:length(hurdlenet_out_trade)){
  
  pred.out = predict.hurdlenet(datalist = list('X.test' = Xmatvec.test),
                               nTime.pred = nTime.test,
                               hurdlenet.out = hurdlenet_out_trade[[l]],
                               predict.type = 'probit')
  
  if(hurdlenet_out_trade[[l]]$input$model.choice!='nolatent'){
    
    predsumm = apply(pred.out$probitprob, 2:3,
                     FUN = function(v){
                       
                       median(v, na.rm = T)
                       
                     })
    
    plotdf_prob = rbind.data.frame(plotdf_prob,
                              data.frame('occurence' = rep(deltavec.test,
                                                           hurdlenet_out_trade[[l]]$input$K.fit),
                                         'est' = as.numeric(predsumm),
                                         'k' = rep(1:hurdlenet_out_trade[[l]]$input$K.fit,
                                                   each = length(test.id)),
                                         'method' = hurdlenet_out_trade[[l]]$input$model.choice))
    
    for(k in 1:hurdlenet_out_trade[[l]]$input$K.fit){
      
      # mspe_prob[l,k+1] = sqrt(sum((deltavec.test - predsumm[,k])^2))
      # mspe_prob[l,k+1] = mean(abs(deltavec.test - predsumm[,k]))
      mspe_prob[l,k+1] = mean(abs(deltavec.test[nzid.test] - predsumm[nzid.test,k])) +
        mean(abs(deltavec.test[!nzid.test] - predsumm[!nzid.test,k]))
      
    }
    
  }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
    
    predsumm = apply(pred.out$probitprob, 2,
                     FUN = function(v){
                       
                       median(v, na.rm = T)
                       
                     })
    
    plotdf_prob = rbind.data.frame(plotdf_prob,
                              data.frame('occurence' = rep(deltavec.test,
                                                           hurdlenet_out_trade[[l]]$input$K.fit),
                                         'est' = predsumm,
                                         'k' = 0,
                                         'method' = hurdlenet_out_trade[[l]]$input$model.choice))
    
    # mspe_prob[l,1] = sqrt(sum((deltavec.test - predsumm)^2))
    # mspe_prob[l,1] = mean(abs(deltavec.test - predsumm))
    mspe_prob[l,1] = mean(abs(deltavec.test[nzid.test] - predsumm[nzid.test])) +
      mean(abs(deltavec.test[!nzid.test] - predsumm[!nzid.test]))
    
  }
  
  print(hurdlenet_out_trade[[l]]$input$model.choice)
  
}

plotdf.mspe = data.frame('mspe' = as.numeric(mspe_prob),
                         'k' = rep(0:hurdlenet_out_trade[[1]]$input$K.fit,
                                   each = length(hurdlenet_out_trade)),
                         'method' = rep(do.call('c',
                                                lapply(1:length(hurdlenet_out_trade),
                                                       FUN = function(l){
                                                         
                                                         hurdlenet_out_trade[[l]]$input$model.choice
                                                         
                                                       })
                         ),
                         hurdlenet_out_trade[[1]]$input$K.fit+1))
plotdf.mspe = plotdf.mspe[!is.na(plotdf.mspe$mspe),]
plotdf.mspe$mspe_label = as.character(round(plotdf.mspe$mspe, 2))

plotdf_prob$occurence = factor(x = plotdf_prob$occurence, levels = c(0,1))
plotdf_prob$k = factor(x = plotdf_prob$k)
plotdf.mspe$k = factor(x = plotdf.mspe$k)
plotdf_prob$method = factor(x = plotdf_prob$method, 
                       levels = c('D1dhs', 'D0dhs', 'indep',
                                  'shs', 'nolatent'))
plotdf.mspe$method = factor(x = plotdf.mspe$method,
                            levels = c('D1dhs', 'D0dhs', 'indep',
                                       'shs', 'nolatent'))

k.label = paste0('K=', 0:hurdlenet_out_trade[[1]]$input$K.fit)
names(k.label) = 0:hurdlenet_out_trade[[1]]$input$K.fit

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static',
                 'nolatent' = 'No latent')

ggplot2::ggplot(data = plotdf_prob) +
  ggplot2::coord_cartesian(ylim = c(0,1), xlim = c(1,3)) +
  ggplot2::facet_grid(method~k,
                      labeller = ggplot2::labeller(method = method.label,
                                                   k = k.label)) +
  ggplot2::geom_boxplot(
    ggplot2::aes(x = occurence, y = est),
    shape = 16, alpha = .7, fill = '#E69F00', linewidth = .8,
    outlier.size = 1.5, outlier.color = "grey40"
  ) +
  # ggplot2::geom_violin(
  #   ggplot2::aes(x = occurence, y = est),
  #   alpha = .7, fill = '#E69F00'#, linewidth = 1, color = 4
  # ) +
  ggplot2::scale_x_discrete(labels = c('0' = 'Did Not Occur',
                                       '1' = 'Occurred')) +
  ggplot2::geom_label(
    data = plotdf.mspe,
    ggplot2::aes(x = 3.1, y = .9, label = mspe_label), fontface = "bold", size = 6
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(size=22, face="bold"),
                 plot.subtitle = ggplot2::element_text(size=20),
                 axis.title.x = ggplot2::element_text(size=20,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 # axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size=20,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                                     angle = 20, hjust = 1, vjust = 1),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(color = "black", size = 15),
                 # axis.ticks.x = ggplot2::element_line(linewidth = 1),
                 axis.ticks.length.x = ggplot2::unit(.15, "cm"),
                 axis.ticks.length.y = ggplot2::unit(.15, "cm"),
                 # axis.ticks.x = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 1.5),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 17, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 17, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
                 legend.title = ggplot2::element_blank(),
                 legend.key.width = ggplot2::unit(3, "cm"), legend.key.height = ggplot2::unit(2, "cm"), 
                 legend.key.size = ggplot2::unit(30, "cm"),
                 legend.spacing.x = ggplot2::unit(2, 'cm'), legend.text=ggplot2::element_text(size=15),
                 legend.position = 'bottom') +
  # ggplot2::guides(color = ggplot2::guide_legend(
  #   nrow = 2, byrow=FALSE#,
  #   # override.aes = list(
  #   #   linetype = "solid",
  #   #   shape = c(rep(16, length(levels(plotdf$method))-1), NA))
  # )) +
  ggplot2::labs(
    # title = 'International Trade Data Analysis',
    # subtitle = 'Trade Occurence Prediction',
    x = 'Observed',
    y = 'Median Predictive Probability'
  )   # 9.5 X 13

round(mspe_prob, 2)


# # K=3 trade occurence probability ====
# K.best = 2
# plotdf_prob_best = NULL
# for(l in 1:length(hurdlenet_out_trade)) {
#   
#   if(hurdlenet_out_trade[[l]]$input$model.choice %in% c('D1dhs', 'D0dhs', 'indep', 'shs')){
#     
#     plotdf_prob_best = rbind.data.frame(plotdf_prob_best,
#                                 plotdf_prob[(plotdf_prob$method==hurdlenet_out_trade[[l]]$input$model.choice)&
#                                          (plotdf_prob$k==K.best),])
#     
#   }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
#     
#     plotdf_prob_best = rbind.data.frame(plotdf_prob_best,
#                                 plotdf_prob[plotdf_prob$method=='nolatent',])
#     
#   }
#   
#   print(hurdlenet_out_trade[[l]]$input$model.choice)
#   
# }
# 
# plotdf.mspe_best = plotdf.mspe[(plotdf.mspe$method=='nolatent')|
#                               (plotdf.mspe$k==K.best),]
# 
# method.label = c('D1dhs' = 'Hurdle-Net(1)',
#                  'D0dhs' = 'Hurdle-Net(0)',
#                  'indep' = 'Independent',
#                  'shs' = 'Static',
#                  'nolatent' = 'No latent')
# 
# ggplot2::ggplot(data = plotdf_prob_best) +
#   ggplot2::coord_cartesian(ylim = c(0,1), xlim = c(1,3)) +
#   ggplot2::facet_grid(.~method,
#                       labeller = ggplot2::labeller(method = method.label)) +
#   ggplot2::geom_boxplot(
#     ggplot2::aes(x = occurence, y = est),
#     shape = 16, alpha = .7, fill = '#E69F00', outlier.size = .7#, linewidth = 1, color = 4
#   ) +
#   # ggplot2::geom_violin(
#   #   ggplot2::aes(x = occurence, y = est),
#   #   alpha = .7, fill = '#E69F00'#, linewidth = 1, color = 4
#   # ) +
#   ggplot2::scale_x_discrete(labels = c('0' = 'Did Not Occur',
#                                        '1' = 'Occurred')) +
#   ggplot2::geom_label(
#     data = plotdf.mspe_best,
#     ggplot2::aes(x = 3.2, y = .9, label = mspe_label), fontface = "bold"
#   ) +
#   ggplot2::theme(plot.title = ggplot2::element_text(size=22, face="bold"),
#                  plot.subtitle = ggplot2::element_text(size=20),
#                  axis.title.x = ggplot2::element_text(size=20,
#                                                       margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#                  # axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size=20,
#                                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#                  axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                                      angle = 30, hjust = 1, vjust = 1),
#                  # axis.text.x = ggplot2::element_blank(),
#                  axis.text.y = ggplot2::element_text(color = "black", size = 15),
#                  # axis.ticks.x = ggplot2::element_line(linewidth = 1),
#                  axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#                  axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#                  # axis.ticks.x = ggplot2::element_blank(),
#                  panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                                       fill = NA, linewidth = 1.5),
#                  panel.background = ggplot2::element_blank(),
#                  panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  strip.text.x = ggplot2::element_text(size = 15, face = "bold"),
#                  strip.text.y = ggplot2::element_text(size = 15, face = "bold"),
#                  strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
#                  legend.title = ggplot2::element_blank(),
#                  legend.key.width = ggplot2::unit(3, "cm"), legend.key.height = ggplot2::unit(2, "cm"), 
#                  legend.key.size = ggplot2::unit(30, "cm"),
#                  legend.spacing.x = ggplot2::unit(2, 'cm'), legend.text=ggplot2::element_text(size=15),
#                  legend.position = 'bottom') +
#   # ggplot2::guides(color = ggplot2::guide_legend(
#   #   nrow = 2, byrow=FALSE#,
#   #   # override.aes = list(
#   #   #   linetype = "solid",
#   #   #   shape = c(rep(16, length(levels(plotdf$method))-1), NA))
#   # )) +
#   ggplot2::labs(
#     # title = 'International Trade Data Analysis',
#     # subtitle = 'Trade Occurence Prediction',
#     x = 'Observed Occurence',
#     y = 'Median Predictive\nProbability'
#   )   # 10 X 14
# 
# round(mspe_prob, 2)



# # mspe ====
# plotdf = rbind.data.frame(data.frame('value' = as.numeric(mspe_tradevol[,(1:hurdlenet_out_trade[[1]]$input$K.fit) + 1]),
#                                      'k' = rep(1:hurdlenet_out_trade[[1]]$input$K.fit,
#                                                each = length(hurdlenet_out_trade)),
#                                      'pred_type' = 'y',
#                                      'method' = rep(rownames(mspe_tradevol), 
#                                                     hurdlenet_out_trade[[1]]$input$K.fit)),
#                           data.frame('value' = as.numeric(mspe_prob[,(1:hurdlenet_out_trade[[1]]$input$K.fit) + 1]),
#                                      'k' = rep(1:hurdlenet_out_trade[[1]]$input$K.fit,
#                                                each = length(hurdlenet_out_trade)),
#                                      'pred_type' = 'probit',
#                                      'method' = rep(rownames(mspe_prob), 
#                                                     hurdlenet_out_trade[[1]]$input$K.fit)))
# 
# plotdf$pred_type = factor(x = plotdf$pred_type, levels = c('y', 'probit'))
# plotdf$method = factor(x = plotdf$method, 
#                        levels = c('D1dhs', 'D0dhs', 'indep',
#                                   'shs', 'nolatent'))
# 
# method.label = c('D1dhs' = 'Hurdle-Net(1)',
#                  'D0dhs' = 'Hurdle-Net(0)',
#                  'indep' = 'Independent',
#                  'shs' = 'Static',
#                  'nolatent' = 'No latent')
# 
# method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
#                  "indep" = 'orchid', "shs" = 'dodgerblue',
#                  "nolatent" = 'slateblue')
# 
# pred.label = c('y' = 'log(Trade volume)', 'probit' = 'Trade occurence')
# 
# ggplot2::ggplot(data = plotdf) +
#   # ggplot2::coord_cartesian(ylim = c(0,1)) +
#   ggh4x::facet_wrap2(.~pred_type, scales = "free_y",
#                       labeller = ggplot2::labeller(pred_type = pred.label)) +
#   # ggplot2::geom_point(data = subset(plotdf, method!='nolatent'),
#   #                     shape = 16, size = 8, stroke = 3#,
#   #                     # position = ggplot2::position_dodge(width = 1)
#   #                     ) +
#   # ggplot2::geom_line(data = subset(plotdf, method!='nolatent'),
#   #                    linewidth = 2) +
#   ggplot2::geom_point(data = subset(plotdf, method!='nolatent'),
#                       ggplot2::aes(x = k, y = value, color = method),
#                       shape = 16, size = 4, stroke = 2) +
#   ggplot2::geom_line(data = subset(plotdf, method!='nolatent'),
#                      ggplot2::aes(x = k, y = value, color = method),
#                      linewidth = 1) +
#   ggplot2::geom_hline(data = subset(plotdf, method=='nolatent'),
#                       ggplot2::aes(yintercept = value, color = method),
#                       linewidth = 1) +
#   ggplot2::scale_color_manual(values = method.color,
#                               labels = method.label) +
#   # ggplot2::scale_y_continuous(trans = 'log') +
#   ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 20, face = "bold"),
#                  plot.subtitle = ggplot2::element_text(color = "black", size = 15),
#                  axis.title.x = ggplot2::element_text(size=15,
#                                                       margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#                  axis.title.y = ggplot2::element_text(size=15,
#                                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#                  axis.text.x = ggplot2::element_text(color = "black", size = 12,
#                                                      angle = 0, hjust = .5, vjust = 1),
#                  axis.text.y = ggplot2::element_text(color = "black", size = 12),
#                  axis.ticks.x = ggplot2::element_line(linewidth = .5),
#                  axis.ticks.length.x = ggplot2::unit(.15, "cm"),
#                  axis.ticks.y = ggplot2::element_line(linewidth = .5),
#                  axis.ticks.length.y = ggplot2::unit(.15, "cm"),
#                  panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                                       fill = NA, linewidth = 1.5),
#                  panel.background = ggplot2::element_blank(),
#                  panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  strip.text.x = ggplot2::element_text(size = 15, face = "bold"),
#                  strip.text.y = ggplot2::element_text(size = 15, face = "bold"),
#                  strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
#                  legend.title = ggplot2::element_blank(),
#                  # legend.title = ggplot2::element_text(size = 20, face = "bold"),
#                  legend.key.width = ggplot2::unit(1.5, "cm"), legend.key.height = ggplot2::unit(1, "cm"),
#                  legend.key.size = ggplot2::unit(30, "cm"),
#                  legend.spacing.x = ggplot2::unit(.75, 'cm'), 
#                  legend.text=ggplot2::element_text(size=15),
#                  legend.position = 'bottom') +
#   ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
#   ggplot2::labs(
#     # title = 'International Trade Data Analysis',
#     # subtitle = 'Information Criterion',
#     y = 'Prediction Error', 
#     x = 'Latent Dimension (k)'
#   )   # 5.5 X 10


# log-likelihood ====
K.plot = hurdlenet_out_trade[[1]]$input$K.fit
plotdf = do.call('rbind.data.frame',
                 lapply(1:length(hurdlenet_out_trade),
                        FUN = function(l){
                          
                          print(hurdlenet_out_trade[[l]]$input$model.choice)
                          
                          if(hurdlenet_out_trade[[l]]$input$model.choice!='nolatent'){
                            
                            return(data.frame('value' = as.numeric(apply(hurdlenet_out_trade[[l]]$MCMCout$loglik,
                                                                         c(1,3), mean)),
                                              'mcmciter' = rep(1:hurdlenet_out_trade[[l]]$input$nMCMC, K.plot),
                                              'k' = rep(1:K.plot, each = hurdlenet_out_trade[[l]]$input$nMCMC),
                                              'method' = hurdlenet_out_trade[[l]]$input$model.choice))
                            
                          }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
                            
                            return(data.frame('value' = rowMeans(hurdlenet_out_trade[[l]]$MCMCout$loglik),
                                              'mcmciter' = 1:hurdlenet_out_trade[[l]]$input$nMCMC,
                                              'k' = 0,
                                              'method' = hurdlenet_out_trade[[l]]$input$model.choice))
                            
                          }
                          
                        }))

plotdf$k = factor(x = plotdf$k)
plotdf$method = factor(x = plotdf$method, 
                       levels = c('D1dhs', 'D0dhs', 'indep',
                                  'shs', 'nolatent'))

method.label = c('D1dhs' = 'Hurdle-Net+DHS(1)',
                 'D0dhs' = 'Hurdle-Net+DHS(0)',
                 'indep' = 'Independent Modeling',
                 'shs' = 'Hurdle-Net+SHS',
                 'nolatent' = 'Hurdle-Net+No-Latent')

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

k.label = c('No latent', paste0('K=', 1:K.plot))
k.color = scales::hue_pal()(K.plot+1)
names(k.label) = names(k.color) = 0:K.plot

# ggplot2::ggplot(data = plotdf) +
#   # ggplot2::facet_grid(.~k,
#   #                     labeller = ggplot2::labeller(k = k.label)) +
#   # ggplot2::geom_line(ggplot2::aes(color = method),
#   #                    linewidth = 1) +
#   # ggplot2::scale_colour_manual(values = method.color,
#   #                              labels = method.label) +
#   ggplot2::facet_grid(.~method,
#                       labeller = ggplot2::labeller(method = method.label)) +
#   ggplot2::geom_line(ggplot2::aes(x = mcmciter, y = value,
#                                   color = k),
#                      linetype = 1, linewidth = 1) +
#   ggplot2::scale_colour_manual(values = k.color,
#                                labels = k.label) +
#   ggplot2::theme(plot.title = ggplot2::element_text(size=22, face="bold"),
#                  plot.subtitle = ggplot2::element_text(size=20),
#                  axis.title.x = ggplot2::element_text(size=20,
#                                                       margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#                  # axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size=20,
#                                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#                  axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                                      angle = 0, hjust = .5, vjust = 1),
#                  # axis.text.x = ggplot2::element_blank(),
#                  axis.text.y = ggplot2::element_text(color = "black", size = 15),
#                  # axis.ticks.x = ggplot2::element_line(linewidth = 1),
#                  axis.ticks.length.x = ggplot2::unit(.15, "cm"),
#                  axis.ticks.length.y = ggplot2::unit(.15, "cm"),
#                  # axis.ticks.x = ggplot2::element_blank(),
#                  panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                                       fill = NA, linewidth = 1.5),
#                  panel.background = ggplot2::element_blank(),
#                  panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                                           colour = "grey90"),
#                  strip.text.x = ggplot2::element_text(size = 17, face = "bold"),
#                  strip.text.y = ggplot2::element_text(size = 17, face = "bold"),
#                  strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
#                  legend.title = ggplot2::element_blank(),
#                  legend.key.width = ggplot2::unit(1, "cm"), 
#                  legend.key.height = ggplot2::unit(2, "cm"), 
#                  legend.key.size = ggplot2::unit(30, "cm"),
#                  legend.key.spacing.x = ggplot2::unit(1, 'cm'),
#                  legend.text=ggplot2::element_text(size=15),
#                  legend.position = 'bottom') +
#   # ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
#   ggplot2::labs(
#     # title = 'International Trade Data Analysis',
#     # subtitle = 'Information Criterion',
#     y = 'Log likelihood', 
#     x = 'MCMC iteration'
#   )   # 12 X 20

ggplot2::ggplot(data = plotdf) +
  ggplot2::facet_grid(.~k,
                      labeller = ggplot2::labeller(k = k.label)) +
  ggplot2::geom_line(ggplot2::aes(x = mcmciter, y = value,
                                  color = method),
                     linetype = 1, linewidth = 1) +
  ggplot2::scale_colour_manual(values = method.color,
                               labels = method.label) +
  # ggplot2::facet_grid(.~method,
  #                     labeller = ggplot2::labeller(method = method.label)) +
  # ggplot2::geom_line(ggplot2::aes(x = mcmciter, y = value,
  #                                 color = k),
  #                    linetype = 1, linewidth = 1) +
  # ggplot2::scale_colour_manual(values = k.color,
  #                              labels = k.label) +
  ggplot2::theme(plot.title = ggplot2::element_text(size=22, face="bold"),
                 plot.subtitle = ggplot2::element_text(size=20),
                 axis.title.x = ggplot2::element_text(size=20,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 # axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size=20,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                                     angle = 0, hjust = .5, vjust = 1),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(color = "black", size = 15),
                 # axis.ticks.x = ggplot2::element_line(linewidth = 1),
                 axis.ticks.length.x = ggplot2::unit(.15, "cm"),
                 axis.ticks.length.y = ggplot2::unit(.15, "cm"),
                 # axis.ticks.x = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 1.5),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 17, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 17, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=1.5),
                 legend.title = ggplot2::element_blank(),
                 legend.key.width = ggplot2::unit(1, "cm"), 
                 legend.key.height = ggplot2::unit(2, "cm"), 
                 legend.key.size = ggplot2::unit(30, "cm"),
                 legend.key.spacing.x = ggplot2::unit(1, 'cm'),
                 legend.text=ggplot2::element_text(size=15),
                 legend.position = 'bottom') +
  # ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
  ggplot2::labs(
    # title = 'International Trade Data Analysis',
    # subtitle = 'Information Criterion',
    y = 'Log likelihood', 
    x = 'MCMC iteration'
  )   # 5 X 13


# regression coefficient ====
covariate.label = c("gdp_new_x" = 'GDP (Exporter)', "pop_new_x" = 'Population (Exporter)',
                    "area_x" = 'Area (Exporter)',
                    "gdp_new_m" = 'GDP (Importer)', "pop_new_m" = 'Population (Importer)',
                    "area_m" = 'Area (Importer)',
                    "distcap" = 'Distance', "lp_labour_stand" = 'Labor Provision',
                    'rta_x' = "Regional Trade Agreement", "LP" = 'Labor Provision')

method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

var_type.label = c('cont' = 'Log Trade Volume',
                   'binary' = 'Trade Occurrence')

dimnames(hurdlenet_out_trade$D1dhs$MCMCout$betareg_cont)[[2]] = 
  dimnames(hurdlenet_out_trade$D1dhs$MCMCout$betareg_binary)[[2]] = 
  colnames(Xmatvec)

# ## k=2 ====
# K.plot = 2  #hurdlenet_out_trade[[1]]$input$K.fit
# betareg_cont.postsumm = apply(hurdlenet_out_trade$D1dhs$MCMCout$betareg_cont[,,K.plot], 2,
#                               FUN = function(v){
#                                 
#                                 c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
#                                 
#                               })
# betareg_binary.postsumm = apply(hurdlenet_out_trade$D1dhs$MCMCout$betareg_binary[,,K.plot], 2,
#                               FUN = function(v){
#                                 
#                                 c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
#                                 
#                               })
# plotdf = rbind.data.frame(data.frame('value' = betareg_cont.postsumm[1,],
#                                      'llim' = betareg_cont.postsumm[2,],
#                                      'ulim' = betareg_cont.postsumm[3,],
#                                      'covariate' = colnames(Xmatvec),
#                                      "var_type" = 'cont'),
#                           data.frame('value' = betareg_binary.postsumm[1,],
#                                      'llim' = betareg_binary.postsumm[2,],
#                                      'ulim' = betareg_binary.postsumm[3,],
#                                      'covariate' = colnames(Xmatvec),
#                                      "var_type" = 'binary'))
# head(plotdf)
# 
# plotdf$covariate = factor(x = plotdf$covariate, levels = colnames(Xmatvec))
# plotdf$var_type = factor(x = plotdf$var_type, levels = names(var_type.label))
# 
# ggplot2::ggplot(data = plotdf) +
#   ggh4x::facet_grid2(.~var_type,
#                      scales = 'free_y', independent = "y", 
#                      labeller = ggplot2::labeller(var_type = var_type.label)) +
#   ggplot2::geom_point(ggplot2::aes(x = covariate, y = value),
#                       color = method.color["D1dhs"], shape = 16, size = 6, stroke = 2) +
#   ggplot2::geom_errorbar(ggplot2::aes(x = covariate, ymin = llim, ymax = ulim),
#                          color = method.color["D1dhs"], width=.4, linewidth = 2, alpha = .4) +
#   ggplot2::scale_x_discrete(labels = covariate.label) +
#   ggplot2::geom_hline(yintercept = 0, color = "black", linetype = 'dashed', linewidth = 1.5) +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_text(size=20,
#                                          margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#     axis.title.y = ggplot2::element_text(size=20,
#                                          margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#     axis.text.x = ggplot2::element_text(color = "black", size = 14,
#                                         angle = 30, hjust = 1, vjust = 1),
#     axis.text.y = ggplot2::element_text(color = "black", size = 14),
#     axis.ticks.x = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#     axis.ticks.y = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#     panel.background = ggplot2::element_blank(),
#     panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                          fill = NA, linewidth = 1),
#     panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     strip.text.x = ggplot2::element_text(
#       size = 22,
#       face = "bold"
#     ),
#     strip.text.y = ggplot2::element_text(
#       size = 22,
#       face = "bold"
#     ),
#     strip.background = ggplot2::element_rect(color="black", linewidth=1),
#     legend.title = ggplot2::element_blank(),
#     # legend.title = ggplot2::element_text(size = 30, face = "bold"),
#     legend.key.width = ggplot2::unit(.75, "cm"),
#     legend.key.height = ggplot2::unit(.75, "cm"),
#     legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
#     legend.text=ggplot2::element_text(size=15),
#     legend.position = 'bottom'
#   ) +
#   ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
#   ggplot2::labs(
#     x = 'Covariates',
#     y = 'Posterior Estimates'
#   )  # 8 X 14


## k=4 ====
K.plot = 4  #hurdlenet_out_trade[[1]]$input$K.fit
dim(hurdlenet_out_trade$D1dhs$MCMCout$betareg_cont)
dim(hurdlenet_out_trade$D1dhs$MCMCout$betareg_binary)
betareg_cont.postsumm = apply(hurdlenet_out_trade$D1dhs$MCMCout$betareg_cont[,,K.plot], 2,
                              FUN = function(v){
                                
                                c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
                                
                              })
betareg_binary.postsumm = apply(hurdlenet_out_trade$D1dhs$MCMCout$betareg_binary[,,K.plot], 2,
                                FUN = function(v){
                                  
                                  c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
                                  
                                })
plotdf = rbind.data.frame(data.frame('value' = betareg_cont.postsumm[1,],
                                     'llim' = betareg_cont.postsumm[2,],
                                     'ulim' = betareg_cont.postsumm[3,],
                                     'covariate' = colnames(Xmatvec),
                                     "var_type" = 'cont'),
                          data.frame('value' = betareg_binary.postsumm[1,],
                                     'llim' = betareg_binary.postsumm[2,],
                                     'ulim' = betareg_binary.postsumm[3,],
                                     'covariate' = colnames(Xmatvec),
                                     "var_type" = 'binary'))
head(plotdf)

plotdf$covariate = factor(x = plotdf$covariate, levels = colnames(Xmatvec))
plotdf$var_type = factor(x = plotdf$var_type, levels = names(var_type.label))

ggplot2::ggplot(data = plotdf) +
  ggh4x::facet_grid2(.~var_type,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(var_type = var_type.label)) +
  ggplot2::geom_point(ggplot2::aes(x = covariate, y = value),
                      color = method.color["D1dhs"], shape = 16, size = 6#, stroke = 2
                      ) +
  ggplot2::geom_errorbar(ggplot2::aes(x = covariate, ymin = llim, ymax = ulim),
                         color = method.color["D1dhs"], width=.4, linewidth = 2, alpha = .6) +
  ggplot2::scale_x_discrete(labels = covariate.label) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linetype = 'dashed', linewidth = 1.5) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=20,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=20,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 14,
                                        angle = 30, hjust = 1, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 14),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 22,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 22,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=15),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Covariates',
    y = 'Posterior Estimates'
  )  # 8 X 14


# ## indep vs D1dhs ====
# K.plot = 4  #hurdlenet_out_trade[[1]]$input$K.fit
# covariate.label = c("gdp_new_x" = 'GDP (Exporter)', "pop_new_x" = 'Population (Exporter)',
#                     "area_x" = 'Area (Exporter)',
#                     "gdp_new_m" = 'GDP (Importer)', "pop_new_m" = 'Population (Importer)',
#                     "area_m" = 'Area (Importer)',
#                     "distcap" = 'Distance', "lp_labour_stand" = 'Labor Provision')
# 
# method.label = c('D1dhs' = 'Hurdle-Net(1)',
#                  'D0dhs' = 'Hurdle-Net(0)',
#                  'indep' = 'Independent',
#                  'shs' = 'Static')
# 
# method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
#                  "indep" = 'orchid', "shs" = 'dodgerblue')
# 
# method.shape = c("D1dhs" = 16, "D0dhs" = 17,
#                  "indep" = 18, "shs" = 19)
# 
# var_type.label = c('cont' = 'Log Trade Volume',
#                    'binary' = 'Trade Occurrence')
# 
# dimnames(hurdlenet_out_trade$D1dhs$MCMCout$betareg_cont)[[2]] = 
#   dimnames(hurdlenet_out_trade$D1dhs$MCMCout$betareg_binary)[[2]] = 
#   dimnames(hurdlenet_out_trade$indep$MCMCout$betareg)[[2]] = 
#   names(covariate.label)
# 
# plotdf = NULL
# for(method in c("D1dhs", "indep")){
#   
#   if(method=="D1dhs"){
#     
#     # cont
#     betareg_cont.postsumm = apply(hurdlenet_out_trade[[method]]$MCMCout$betareg_cont[,,K.plot], 2,
#                                   FUN = function(v){
#                                     
#                                     c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
#                                     
#                                   })
#     
#     # cont
#     betareg_binary.postsumm = apply(hurdlenet_out_trade[[method]]$MCMCout$betareg_binary[,,K.plot], 2,
#                                   FUN = function(v){
#                                     
#                                     c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
#                                     
#                                   })
#     
#     plotdf = rbind.data.frame(plotdf,
#                               rbind.data.frame(data.frame('value' = betareg_cont.postsumm[1,],
#                                                           'llim' = betareg_cont.postsumm[2,],
#                                                           'ulim' = betareg_cont.postsumm[3,],
#                                                           'covariate' = names(covariate.label),
#                                                           'method' = method,
#                                                           "var_type" = 'cont'),
#                                                data.frame('value' = betareg_binary.postsumm[1,],
#                                                           'llim' = betareg_binary.postsumm[2,],
#                                                           'ulim' = betareg_binary.postsumm[3,],
#                                                           'covariate' = names(covariate.label),
#                                                           'method' = method,
#                                                           "var_type" = 'binary')))
#     
#   }else if(method=="indep"){
#     
#     # cont
#     betareg_cont.postsumm = apply(hurdlenet_out_trade[[method]]$MCMCout$betareg[,,K.plot,1], 2,
#                                   FUN = function(v){
#                                     
#                                     c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
#                                     
#                                   })
#     
#     # cont
#     betareg_binary.postsumm = apply(hurdlenet_out_trade[[method]]$MCMCout$betareg[,,K.plot,2], 2,
#                                     FUN = function(v){
#                                       
#                                       c(mean(v), quantile(x = v, probs = c(0.025, 0.975)))
#                                       
#                                     })
#     
#     plotdf = rbind.data.frame(plotdf,
#                               rbind.data.frame(data.frame('value' = betareg_cont.postsumm[1,],
#                                                           'llim' = betareg_cont.postsumm[2,],
#                                                           'ulim' = betareg_cont.postsumm[3,],
#                                                           'covariate' = names(covariate.label),
#                                                           'method' = method,
#                                                           "var_type" = 'cont'),
#                                                data.frame('value' = betareg_binary.postsumm[1,],
#                                                           'llim' = betareg_binary.postsumm[2,],
#                                                           'ulim' = betareg_binary.postsumm[3,],
#                                                           'covariate' = names(covariate.label),
#                                                           'method' = method,
#                                                           "var_type" = 'binary')))
#     
#   }
#   
#   print(method)
#   
# }
# 
# head(plotdf)
# 
# plotdf$covariate = factor(x = plotdf$covariate, levels = names(covariate.label))
# plotdf$method = factor(x = plotdf$method, levels = names(method.label))
# plotdf$var_type = factor(x = plotdf$var_type, levels = names(var_type.label))
# 
# ggplot2::ggplot(data = plotdf) +
#   ggh4x::facet_grid2(.~var_type,
#                      scales = 'free_y', independent = "y", 
#                      labeller = ggplot2::labeller(var_type = var_type.label)) +
#   ggplot2::geom_point(ggplot2::aes(x = covariate, y = value, 
#                                    shape = method, color = method, group = method),
#                       position = ggplot2::position_dodge(.6),
#                       # color = method.color["D1dhs"], shape = 16, 
#                       size = 5, stroke = 1) +
#   ggplot2::geom_errorbar(ggplot2::aes(x = covariate, ymin = llim, ymax = ulim, 
#                                       color = method),
#                          position = ggplot2::position_dodge(.6),
#                          # color = method.color["D1dhs"], 
#                          width=.5, linewidth = 1.5, alpha = .4) +
#   ggplot2::scale_x_discrete(labels = covariate.label) +
#   ggplot2::scale_color_manual(values = method.color, labels = method.label) +
#   ggplot2::scale_shape_manual(values = method.shape, labels = method.label) +
#   ggplot2::geom_hline(yintercept = 0, color = "black", linetype = 'dashed', linewidth = 1.5) +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_text(size=22,
#                                          margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#     axis.title.y = ggplot2::element_text(size=22,
#                                          margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#     axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                         angle = 30, hjust = 1, vjust = 1),
#     axis.text.y = ggplot2::element_text(color = "black", size = 15),
#     axis.ticks.x = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#     axis.ticks.y = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#     panel.background = ggplot2::element_blank(),
#     panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                          fill = NA, linewidth = 1),
#     panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     strip.text.x = ggplot2::element_text(
#       size = 24,
#       face = "bold"
#     ),
#     strip.text.y = ggplot2::element_text(
#       size = 24,
#       face = "bold"
#     ),
#     strip.background = ggplot2::element_rect(color="black", linewidth=1),
#     legend.title = ggplot2::element_blank(),
#     # legend.title = ggplot2::element_text(size = 30, face = "bold"),
#     legend.key.width = ggplot2::unit(2, "cm"),
#     legend.key.height = ggplot2::unit(.75, "cm"),
#     legend.key.spacing.x = ggplot2::unit(1, 'cm'),
#     legend.text=ggplot2::element_text(size=15),
#     legend.position = 'bottom'
#   ) +
#   ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
#   ggplot2::labs(
#     x = 'Covariates',
#     y = 'Posterior Density'
#   )  # 8 X 14


# latent variable dynamics ====
pred.out = predict.hurdlenet(datalist = list('X.test' = Xmatvec.test),
                             nTime.pred = nTime.test,
                             hurdlenet.out = hurdlenet_out_trade$D1dhs,
                             predict.type = 'latent')

head(Xmatvec.test)
gdp = c(Xmatvec.test[1,"gdp_new_m"], Xmatvec.test[1:(model.data$n-1),"gdp_new_x"])
names(gdp) = model.data$countries.iso
sort(gdp, decreasing = T)


# ## k=2 ====
# K.plot = 2  #hurdlenet_out_trade[[1]]$input$K.fit
# 
# Zall = rbind(apply(hurdlenet_out_trade$D1dhs$MCMCout$Z[,,1:K.plot,K.plot], 2:3, mean),
#              apply(pred.out$Z[,,1:K.plot,K.plot], 2:3, median))
# Zall.array = array(dim = c(model.data$nTime, model.data$n, K.plot),
#                    dimnames = list(as.character(model.data$years),
#                                    model.data$countries.iso,
#                                    NULL))
# dimnames(Zall.array)
# for(t in 1:model.data$nTime){
#   
#   Zall.array[t,,] = Zall[((t-1)*model.data$n + 1):(t*model.data$n),]
#   
# }
# 
# plotdf = reshape2::melt(Zall.array)
# head(plotdf)
# 
# plotdf$Var2 = factor(plotdf$Var2, levels = model.data$countries.iso[order(gdp, decreasing = T)])
# plotdf$Var3 = as.factor(plotdf$Var3)
# 
# k.label = paste0('k = ', 1:K.plot)
# names(k.label) = as.character(1:K.plot)
# 
# # time.label = as.character(1:sim.hurdlenet.out$nTime)
# # names(time.label) = time.label
# 
# ggplot2::ggplot(data = plotdf) +
#   # ggplot2::coord_cartesian(ylim = c(0,1), expand = TRUE, default = FALSE, clip = 'on') +
#   ggplot2::facet_grid(.~Var3, scales = 'free_y',
#                       labeller = ggplot2::labeller(Var3 = k.label)) +
#   ggplot2::geom_line(ggplot2::aes(x = Var1, y = value, color = Var2),
#                      linewidth = .6) +
#   # ggplot2::scale_color_discrete(type = "viridis") +
#   # ggplot2::scale_color_viridis_d(palette = "RdPu") +
#   # ggplot2::scale_color_manual(values = paletteer::paletteer_c("grDevices::Turku", model.data$n)) +
#   # ggplot2::scale_color_manual(values = viridisLite::inferno(n = model.data$n)) +
#   # ggthemes::canva_pal() +
#   # ggplot2::scale_colour_hue(l = 45) +
#   ggplot2::scale_color_viridis_d(option="viridis") +
#   # paletteer::scale_color_paletteer_d(dutchmasters, milkmaid) +
#   # ggplot2::scale_color_brewer(palette="Dark2")
#   # ggplot2::scale_x_continuous(n.breaks = 5) +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_text(size=18,
#                                          margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#     axis.title.y = ggplot2::element_text(size=18,
#                                          margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#     axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                         angle = 0, hjust = .5, vjust = 1),
#     axis.text.y = ggplot2::element_text(color = "black", size = 15),
#     axis.ticks.x = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#     axis.ticks.y = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#     panel.background = ggplot2::element_blank(),
#     panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                          fill = NA, linewidth = 1),
#     panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     strip.text.x = ggplot2::element_text(
#       size = 20,
#       face = "bold"
#     ),
#     strip.text.y = ggplot2::element_text(
#       size = 20,
#       face = "bold"
#     ),
#     strip.background = ggplot2::element_rect(color="black", linewidth=1),
#     # legend.title = ggplot2::element_blank(),
#     # legend.title=ggplot2::element_text(size=13),
#     legend.key.width = ggplot2::unit(.75, "cm"),
#     legend.key.height = ggplot2::unit(.75, "cm"),
#     legend.key.spacing.x = ggplot2::unit(.75, "cm"),
#     # legend.key.size = ggplot2::unit(.5, "cm"),
#     legend.spacing.x = ggplot2::unit(.5, 'cm'),
#     legend.text=ggplot2::element_text(size=9),
#     legend.position = 'bottom'
#   ) +
#   ggplot2::guides(color = ggplot2::guide_legend(nrow = 3, byrow=FALSE, 
#                                                 title = NULL,
#                                                 override.aes = list(linewidth = 1.2))) +
#   ggplot2::labs(x = "Year", 
#                 y = 'Latent Values') # 6.5 X 10


## k=4 ====
K.plot = 4  #hurdlenet_out_trade[[1]]$input$K.fit

Zall = rbind(apply(hurdlenet_out_trade$D1dhs$MCMCout$Z[,,1:K.plot,K.plot], 2:3, mean),
             apply(pred.out$Z[,,1:K.plot,K.plot], 2:3, median))
Zall.array = array(dim = c(model.data$nTime, model.data$n, K.plot),
                   dimnames = list(as.character(model.data$years),
                                   model.data$countries.iso,
                                   NULL))
dimnames(Zall.array)
for(t in 1:model.data$nTime){
  
  Zall.array[t,,] = Zall[((t-1)*model.data$n + 1):(t*model.data$n),]
  
}

plotdf = reshape2::melt(Zall.array)
head(plotdf)

plotdf$Var2 = factor(plotdf$Var2, levels = model.data$countries.iso[order(gdp, decreasing = T)])
plotdf$Var3 = as.factor(plotdf$Var3)

k.label = paste0('k = ', 1:K.plot)
names(k.label) = as.character(1:K.plot)

# time.label = as.character(1:sim.hurdlenet.out$nTime)
# names(time.label) = time.label

ggplot2::ggplot(data = plotdf) +
  # ggplot2::coord_cartesian(ylim = c(0,1), expand = TRUE, default = FALSE, clip = 'on') +
  ggplot2::facet_grid(.~Var3, scales = 'free_y',
                      labeller = ggplot2::labeller(Var3 = k.label)) +
  ggplot2::geom_line(ggplot2::aes(x = Var1, y = value, color = Var2),
                     linewidth = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .8) +
  # ggplot2::scale_color_discrete(type = "viridis") +
  # ggplot2::scale_color_viridis_d(palette = "RdPu") +
  # ggplot2::scale_color_manual(values = paletteer::paletteer_c("grDevices::Turku", model.data$n)) +
  # ggplot2::scale_color_manual(values = viridisLite::inferno(n = model.data$n)) +
  # ggthemes::canva_pal() +
  # ggplot2::scale_colour_hue(l = 45) +
  ggplot2::scale_color_viridis_d(option="viridis") +
  # paletteer::scale_color_paletteer_d(dutchmasters, milkmaid) +
  # ggplot2::scale_color_brewer(palette="Dark2")
  # ggplot2::scale_x_continuous(n.breaks = 5) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=18,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=18,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 15),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 22,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 22,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    # legend.title = ggplot2::element_blank(),
    # legend.title=ggplot2::element_text(size=13),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.75, "cm"),
    # legend.key.size = ggplot2::unit(.5, "cm"),
    legend.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=15),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 3, byrow=FALSE, 
                                                title = NULL,
                                                override.aes = list(linewidth = 1.2))) +
  ggplot2::labs(x = "Year", 
                y = expression('Latent Values ('*Z[itk]*')')) # 5.3 X 13


# latent variable norm dynamics ====
pred.out = predict.hurdlenet(datalist = list('X.test' = Xmatvec.test),
                             nTime.pred = nTime.test,
                             hurdlenet.out = hurdlenet_out_trade$D1dhs,
                             predict.type = 'latent')

head(Xmatvec.test)
gdp.end = c(Xmatvec.test[1,"gdp_new_m"], Xmatvec.test[1:(model.data$n-1),"gdp_new_x"])
names(gdp.end) = model.data$countries.iso
sort(gdp.end, decreasing = T)

gdp.start = c(Xmatvec.train[1,"gdp_new_m"], Xmatvec.train[1:(model.data$n-1),"gdp_new_x"])
names(gdp.start) = model.data$countries.iso
sort(gdp.start, decreasing = T)


# ## k=2 ====
# K.plot = 2  #hurdlenet_out_trade[[1]]$input$K.fit
# 
# Zall = rbind(apply(hurdlenet_out_trade$D1dhs$MCMCout$Z[,,1:K.plot,K.plot], 2:3, mean),
#              apply(pred.out$Z[,,1:K.plot,K.plot], 2:3, median))
# Zall.array = array(dim = c(model.data$nTime, model.data$n, K.plot),
#                    dimnames = list(as.character(model.data$years),
#                                    model.data$countries.iso,
#                                    NULL))
# dimnames(Zall.array)
# for(t in 1:model.data$nTime){
#   
#   Zall.array[t,,] = Zall[((t-1)*model.data$n + 1):(t*model.data$n),]
#   
# }
# 
# Zall.norm = apply(Zall.array, 1:2, 
#                   FUN = function(v){
#                     
#                     sqrt(sum(v^2))
#                     
#                   })
# dimnames(Zall.norm)
# 
# plotdf = reshape2::melt(Zall.norm)
# head(plotdf)
# 
# plotdf$Var2 = factor(plotdf$Var2, 
#                      levels = model.data$countries.iso[order(gdp.end, decreasing = T)]#,
#                      # levels = model.data$countries.iso[order(Zall.norm[model.data$nTime,], decreasing = T)]
# )
# 
# ggplot2::ggplot(data = plotdf) +
#   ggplot2::coord_cartesian(xlim = c(1990, 2017), ylim = c(-5, 25), expand = TRUE, default = FALSE, clip = 'on') +
#   ggplot2::geom_line(ggplot2::aes(x = Var1, y = value, color = Var2),
#                      linewidth = .6) +
#   ggplot2::scale_color_viridis_d(option = "viridis") +
#   ggplot2::geom_point(
#     data = plotdf[plotdf$Var1==model.data$years[1],],
#     ggplot2::aes(x = Var1, y = value, color = Var2, 
#                  size = 3*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + .1)
#   ) +
#   ggrepel::geom_text_repel(
#     data = plotdf[plotdf$Var1==model.data$years[1],],
#     ggplot2::aes(x = Var1, y = value, label = Var2, 
#                  size = 6*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + 2),
#     max.overlaps = Inf,
#     force = 2,
#     nudge_y      = 1,
#     direction    = "y",
#     hjust        = 5,
#     segment.size = 0.2,
#     box.padding = 1
#   ) +
#   ggplot2::geom_point(
#     data = plotdf[plotdf$Var1==model.data$years[model.data$nTime],],
#     ggplot2::aes(x = Var1, y = value, color = Var2, 
#                  size = 3*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + .1)
#   ) +
#   ggrepel::geom_text_repel(
#     data = plotdf[plotdf$Var1==model.data$years[model.data$nTime],],
#     ggplot2::aes(x = Var1, y = value, label = Var2,
#                  size = 6*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + 2),
#     max.overlaps = Inf,
#     force = 2,
#     nudge_y      = 1,
#     direction    = "y",
#     hjust        = -3,
#     segment.size = 0.2,
#     box.padding = 1
#   ) +
#   ggplot2::scale_x_continuous(breaks = seq(model.data$years[1],
#                                            model.data$years[model.data$nTime],
#                                            1)) +
#   ggplot2::scale_size_continuous(guide = "none") +
#   # ggrepel::geom_label_repel(
#   #   data = plotdf[plotdf$Var1==model.data$years[model.data$nTime],],
#   #   ggplot2::aes(x = Var1, y = value, label = Var2),
#   #   max.overlaps = Inf,
#   #   hjust = "outward",
#   #   direction = "y",
#   #   seed = 57,
#   #   min.segment.length = 0,
#   #   segment.size = 0.2,
#   #   color = 'black',
#   #   xlim = c(2015,NA)
# # ) +
# ggplot2::theme(
#   axis.title.x = ggplot2::element_text(size=18,
#                                        margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#   axis.title.y = ggplot2::element_text(size=18,
#                                        margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#   axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                       angle = 30, hjust = 1, vjust = 1),
#   axis.text.y = ggplot2::element_text(color = "black", size = 15),
#   axis.ticks.x = ggplot2::element_line(linewidth = .5),
#   axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#   axis.ticks.y = ggplot2::element_line(linewidth = .5),
#   axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#   panel.background = ggplot2::element_blank(),
#   panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                        fill = NA, linewidth = 1),
#   panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                            colour = "grey90"),
#   panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                            colour = "grey90"),
#   strip.text.x = ggplot2::element_text(
#     size = 20,
#     face = "bold"
#   ),
#   strip.text.y = ggplot2::element_text(
#     size = 20,
#     face = "bold"
#   ),
#   strip.background = ggplot2::element_rect(color="black", linewidth=1),
#   # legend.title = ggplot2::element_blank(),
#   # legend.title=ggplot2::element_text(size=13),
#   legend.key.width = ggplot2::unit(.75, "cm"),
#   legend.key.height = ggplot2::unit(.75, "cm"),
#   legend.key.spacing.x = ggplot2::unit(.75, "cm"),
#   # legend.key.size = ggplot2::unit(.5, "cm"),
#   legend.spacing.x = ggplot2::unit(.5, 'cm'),
#   legend.text=ggplot2::element_text(size=15),
#   legend.position = 'bottom'
# ) +
#   ggplot2::guides(color = ggplot2::guide_legend(nrow = 3, byrow=FALSE, 
#                                                 title = NULL)) +
#   ggplot2::labs(x = "Year",
#                 y = "Latent activity of\ncountries as exporters"
#                 # y = expression("L"[2]*"-Norm of latent positions")
#   ) # 6.5 X 13


## k=4 ====
K.plot = 4  #hurdlenet_out_trade[[1]]$input$K.fit

Zall = rbind(apply(hurdlenet_out_trade$D1dhs$MCMCout$Z[,,1:K.plot,K.plot], 2:3, mean),
             apply(pred.out$Z[,,1:K.plot,K.plot], 2:3, median))
Zall.array = array(dim = c(model.data$nTime, model.data$n, K.plot),
                   dimnames = list(as.character(model.data$years),
                                   model.data$countries.iso,
                                   NULL))
dimnames(Zall.array)
for(t in 1:model.data$nTime){
  
  Zall.array[t,,] = Zall[((t-1)*model.data$n + 1):(t*model.data$n),]
  
}

Zall.norm = apply(Zall.array, 1:2, 
                  FUN = function(v){
                    
                    sqrt(sum(v^2))
                    
                  })
dimnames(Zall.norm)

plotdf = reshape2::melt(Zall.norm)
head(plotdf)

plotdf$Var2 = factor(plotdf$Var2, 
                     levels = model.data$countries.iso[order(gdp.end, decreasing = T)]#,
                     # levels = model.data$countries.iso[order(Zall.norm[model.data$nTime,], decreasing = T)]
)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(xlim = c(1990, 2017), #ylim = c(-5, 25),
                           expand = TRUE, default = FALSE, clip = 'on') +
  ggplot2::geom_line(ggplot2::aes(x = Var1, y = value, color = Var2),
                     linewidth = 1) +
  ggplot2::scale_color_viridis_d(option = "viridis") +
  ggplot2::geom_point(
    data = plotdf[plotdf$Var1==model.data$years[1],],
    ggplot2::aes(x = Var1, y = value, color = Var2, 
                 size = 3*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + .1)
  ) +
  ggrepel::geom_text_repel(
    data = plotdf[plotdf$Var1==model.data$years[1],],
    ggplot2::aes(x = Var1, y = value, label = Var2, 
                 size = 6*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + 2),
    max.overlaps = Inf,
    force = 2,
    nudge_y      = 1,
    direction    = "y",
    hjust        = 5,
    segment.size = 0.2,
    box.padding = 1
  ) +
  ggplot2::geom_point(
    data = plotdf[plotdf$Var1==model.data$years[model.data$nTime],],
    ggplot2::aes(x = Var1, y = value, color = Var2, 
                 size = 3*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + .1)
  ) +
  ggrepel::geom_text_repel(
    data = plotdf[plotdf$Var1==model.data$years[model.data$nTime],],
    ggplot2::aes(x = Var1, y = value, label = Var2,
                 size = 6*(gdp.end + sign(min(gdp.end))*min(gdp.end)) + 2),
    max.overlaps = Inf,
    force = 2,
    nudge_y      = 1,
    direction    = "y",
    hjust        = -3,
    segment.size = 0.2,
    box.padding = 1
  ) +
  ggplot2::scale_x_continuous(breaks = seq(model.data$years[1],
                                           model.data$years[model.data$nTime],
                                           1)) +
  ggplot2::scale_size_continuous(guide = "none") +
  # ggrepel::geom_label_repel(
  #   data = plotdf[plotdf$Var1==model.data$years[model.data$nTime],],
  #   ggplot2::aes(x = Var1, y = value, label = Var2),
  #   max.overlaps = Inf,
  #   hjust = "outward",
  #   direction = "y",
  #   seed = 57,
  #   min.segment.length = 0,
  #   segment.size = 0.2,
  #   color = 'black',
  #   xlim = c(2015,NA)
# ) +
ggplot2::theme(
  axis.title.x = ggplot2::element_text(size=18,
                                       margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
  axis.title.y = ggplot2::element_text(size=18,
                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
  axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                      angle = 30, hjust = 1, vjust = 1),
  axis.text.y = ggplot2::element_text(color = "black", size = 15),
  axis.ticks.x = ggplot2::element_line(linewidth = .5),
  axis.ticks.length.x = ggplot2::unit(.2, "cm"),
  axis.ticks.y = ggplot2::element_line(linewidth = .5),
  axis.ticks.length.y = ggplot2::unit(.2, "cm"),
  panel.background = ggplot2::element_blank(),
  panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                       fill = NA, linewidth = 1),
  panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                           colour = "grey90"),
  panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                           colour = "grey90"),
  strip.text.x = ggplot2::element_text(
    size = 20,
    face = "bold"
  ),
  strip.text.y = ggplot2::element_text(
    size = 20,
    face = "bold"
  ),
  strip.background = ggplot2::element_rect(color="black", linewidth=1),
  # legend.title = ggplot2::element_blank(),
  # legend.title=ggplot2::element_text(size=13),
  legend.key.width = ggplot2::unit(.9, "cm"),
  legend.key.height = ggplot2::unit(.75, "cm"),
  legend.key.spacing.x = ggplot2::unit(.85, "cm"),
  # legend.key.size = ggplot2::unit(.5, "cm"),
  legend.spacing.x = ggplot2::unit(.5, 'cm'),
  legend.text=ggplot2::element_text(size=15),
  legend.position = 'bottom'
) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3),
                                                nrow = 3, byrow=FALSE, 
                                                title = NULL),
                  linewidth = ggplot2::guide_legend(override.aes = list(size = 3))) +
  ggplot2::labs(x = "Year",
                y = expression("Latent activity of countries (||"*z[it]*"||)")
                # y = expression("L"[2]*"-Norm of latent positions")
  ) # 6.5 X 13


# latent clustering ====
pred.out = predict.hurdlenet(datalist = list('X.test' = Xmatvec.test),
                             nTime.pred = nTime.test,
                             hurdlenet.out = hurdlenet_out_trade$D1dhs,
                             predict.type = 'latent')

head(Xmatvec.test)
gdp = c(Xmatvec.test[1,"gdp_new_m"], Xmatvec.test[1:(model.data$n-1),"gdp_new_x"])
names(gdp) = model.data$countries.iso
sort(gdp, decreasing = T)


# ## k=2 ====
# K.plot = 2  #hurdlenet_out_trade[[1]]$input$K.fit
# 
# Zall = rbind(apply(hurdlenet_out_trade$D1dhs$MCMCout$Z[,,1:K.plot,K.plot], 2:3, mean),
#              apply(pred.out$Z[,,1:K.plot,K.plot], 2:3, median))
# Zall.array = array(dim = c(model.data$nTime, model.data$n, K.plot),
#                    dimnames = list(as.character(model.data$years),
#                                    model.data$countries.iso,
#                                    NULL))
# dimnames(Zall.array)
# for(t in 1:model.data$nTime){
#   
#   Zall.array[t,,] = Zall[((t-1)*model.data$n + 1):(t*model.data$n),]
#   
# }
# 
# optkmeans = ClusterR::Optimal_Clusters_KMeans(data = Zall.array[t,,], max_clusters = 10,
#                                               criterion = "BIC",
#                                               max_iters = 1000)
# 
# plotdf = reshape2::melt(Zall.array)
# head(plotdf)
# 
# plotdf$Var2 = factor(plotdf$Var2, levels = model.data$countries.iso[order(gdp, decreasing = T)])
# plotdf$Var3 = as.factor(plotdf$Var3)
# 
# k.label = paste0('k = ', 1:K.plot)
# names(k.label) = as.character(1:K.plot)
# 
# # time.label = as.character(1:sim.hurdlenet.out$nTime)
# # names(time.label) = time.label
# 
# ggplot2::ggplot(data = plotdf) +
#   # ggplot2::coord_cartesian(ylim = c(0,1), expand = TRUE, default = FALSE, clip = 'on') +
#   ggplot2::facet_grid(.~Var3, scales = 'free_y',
#                       labeller = ggplot2::labeller(Var3 = k.label)) +
#   ggplot2::geom_line(ggplot2::aes(x = Var1, y = value, color = Var2),
#                      linewidth = .6) +
#   # ggplot2::scale_color_discrete(type = "viridis") +
#   # ggplot2::scale_color_viridis_d(palette = "RdPu") +
#   # ggplot2::scale_color_manual(values = paletteer::paletteer_c("grDevices::Turku", model.data$n)) +
#   # ggplot2::scale_color_manual(values = viridisLite::inferno(n = model.data$n)) +
#   # ggthemes::canva_pal() +
#   # ggplot2::scale_colour_hue(l = 45) +
#   ggplot2::scale_color_viridis_d(option="viridis") +
#   # paletteer::scale_color_paletteer_d(dutchmasters, milkmaid) +
#   # ggplot2::scale_color_brewer(palette="Dark2")
#   # ggplot2::scale_x_continuous(n.breaks = 5) +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_text(size=18,
#                                          margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
#     axis.title.y = ggplot2::element_text(size=18,
#                                          margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
#     axis.text.x = ggplot2::element_text(color = "black", size = 15,
#                                         angle = 0, hjust = .5, vjust = 1),
#     axis.text.y = ggplot2::element_text(color = "black", size = 15),
#     axis.ticks.x = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.x = ggplot2::unit(.2, "cm"),
#     axis.ticks.y = ggplot2::element_line(linewidth = .5),
#     axis.ticks.length.y = ggplot2::unit(.2, "cm"),
#     panel.background = ggplot2::element_blank(),
#     panel.border = ggplot2::element_rect(color='black', linetype = "solid",
#                                          fill = NA, linewidth = 1),
#     panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
#                                              colour = "grey90"),
#     strip.text.x = ggplot2::element_text(
#       size = 20,
#       face = "bold"
#     ),
#     strip.text.y = ggplot2::element_text(
#       size = 20,
#       face = "bold"
#     ),
#     strip.background = ggplot2::element_rect(color="black", linewidth=1),
#     # legend.title = ggplot2::element_blank(),
#     # legend.title=ggplot2::element_text(size=13),
#     legend.key.width = ggplot2::unit(.75, "cm"),
#     legend.key.height = ggplot2::unit(.75, "cm"),
#     legend.key.spacing.x = ggplot2::unit(.75, "cm"),
#     # legend.key.size = ggplot2::unit(.5, "cm"),
#     legend.spacing.x = ggplot2::unit(.5, 'cm'),
#     legend.text=ggplot2::element_text(size=9),
#     legend.position = 'bottom'
#   ) +
#   ggplot2::guides(color = ggplot2::guide_legend(nrow = 3, byrow=FALSE, 
#                                                 title = NULL,
#                                                 override.aes = list(linewidth = 1.2))) +
#   ggplot2::labs(x = "Year", 
#                 y = 'Latent Values') # 6.5 X 10





# sigma ====
latent.label = paste0('K = ', 1:hurdlenet_out_trade[[1]]$input$K.fit)
names(latent.label) = as.character(1:hurdlenet_out_trade[[1]]$input$K.fit)

latent.shape = seq(from = 15, length.out = hurdlenet_out_trade[[1]]$input$K.fit)
names(latent.shape) = names(latent.label)

method.color = c("D1dhs" = 'green4', "D1dhs_phi0" = 'brown4',
                 "D0dhs" = 'chocolate', "D0dhs_phi0" = 'blue',
                 "D1dhs_indep" = 'orchid', "D0shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

method.label = c("D1dhs" = 'Hurdle-Net+A-DHS(1)',
                 "D1dhs_phi0" = 'Hurdle-Net+DHS(1)',
                 "D0dhs" = 'Hurdle-Net+A-DHS(0)',
                 "D0dhs_phi0" = 'Hurdle-Net+DHS(0)',
                 "D1dhs_indep" = 'Independent',
                 "D0shs" = 'Hurdle-Net+SHS',
                 "nolatent" = 'Hurdle-Net+No-Latent')
plotdf = do.call('rbind.data.frame',
                 lapply(X = 1:length(hurdlenet_out_trade),
                        FUN = function(X){
                          
                          if(!hurdlenet_out_trade[[X]]$input$model.choice %in% c('D1dhs_indep', 'nolatent')){
                            
                            postsumm = apply(sqrt(hurdlenet_out_trade[[X]]$MCMCout$sigma2), 1,
                                             FUN = function(v){
                                               
                                               c(mean(v),
                                                 quantile(x = v,
                                                          probs = c(.025, .975)))
                                               
                                             })
                            
                            return(rbind.data.frame(data.frame('value' = as.numeric(postsumm[1,]),
                                                               'llim' = as.numeric(postsumm[2,]),
                                                               'ulim' = as.numeric(postsumm[3,]),
                                                               'k' = 1:hurdlenet_out_trade[[X]]$input$K.fit,
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice)))
                            
                          }else if(hurdlenet_out_trade[[X]]$input$model.choice=='D1dhs_indep'){
                            
                            postsumm = apply(sqrt(hurdlenet_out_trade[[X]]$MCMCout$sigma2[,,1]), 1,
                                             FUN = function(v){
                                               
                                               c(mean(v),
                                                 quantile(x = v,
                                                          probs = c(.025, .975)))
                                               
                                             })
                            
                            return(rbind.data.frame(data.frame('value' = as.numeric(postsumm[1,]),
                                                               'llim' = as.numeric(postsumm[2,]),
                                                               'ulim' = as.numeric(postsumm[3,]),
                                                               'k' = 1:hurdlenet_out_trade[[X]]$input$K.fit,
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice)))
                            
                          }else if(hurdlenet_out_trade[[X]]$input$model.choice=='nolatent'){
                            
                            return(rbind.data.frame(data.frame('value' = mean(sqrt(hurdlenet_out_trade[[X]]$MCMCout$sigma2)),
                                                               'llim' = quantile(x = sqrt(hurdlenet_out_trade[[X]]$MCMCout$sigma2),
                                                                                 probs = .025),
                                                               'ulim' = quantile(x = sqrt(hurdlenet_out_trade[[X]]$MCMCout$sigma2),
                                                                                 probs = .975),
                                                               'k' = 0,
                                                               'method' = hurdlenet_out_trade[[X]]$input$model.choice)))
                            
                          }
                          
                        }))

plotdf$k = factor(x = plotdf$k)
plotdf$method = factor(x = plotdf$method, 
                       levels = names(hurdlenet_out_trade))

ggplot2::ggplot(data = plotdf) +
  ggplot2::facet_grid(.~k,
                      labeller = ggplot2::labeller(k = k.label)) +
  ggplot2::geom_violin(ggplot2::aes(x = method, y = value, fill = method),
                       trim = F, alpha = .75, linewidth = 1.5,
                       position = ggplot2::position_dodge()) +
  ggplot2::scale_x_discrete(labels = method.label) +
  ggplot2::scale_fill_manual(values = method.color, labels = method.label) +
  ggplot2::theme(plot.title = ggplot2::element_text(size=40, face="bold"),
                 plot.subtitle = ggplot2::element_text(size=35),
                 axis.title.x = ggplot2::element_text(size=35,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 # axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size=35,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 35,
                                                     angle = 50, hjust = 1, vjust = 1),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(color = "black", size = 35),
                 axis.ticks.x = ggplot2::element_line(linewidth = 1),
                 axis.ticks.length.x = ggplot2::unit(.5, "cm"),
                 # axis.ticks.x = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 2),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 35, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 35, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=3),
                 legend.title = ggplot2::element_blank(),
                 legend.key.width = ggplot2::unit(3, "cm"), legend.key.height = ggplot2::unit(2, "cm"), 
                 legend.key.size = ggplot2::unit(30, "cm"),
                 legend.spacing.x = ggplot2::unit(2, 'cm'), legend.text=ggplot2::element_text(size=35),
                 legend.position = 'bottom') +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 3, byrow=FALSE,
                                                override.aes = list(
                                                  linetype = "solid",
                                                  shape = c(rep(16, length(unique(plotdf$method))-1), NA)))) +
  ggplot2::labs(title = 'International Trade Data Analysis',
                subtitle = 'Noise Standard Deviation',
                y = 'Value', 
                x = 'Method')   # 15 X 22


# link function ====
fgrid = seq(
  # floor(min(yvec[nzid])), max(yvec[nzid]), 
  -100, 100,
  length.out = 100
)
plotdf = do.call('rbind.data.frame',
                 lapply(1:length(hurdlenet_out_trade),
                        FUN = function(l){
                          
                          if(hurdlenet_out_trade[[l]]$input$model.choic %in% c('D1dhs', 'D0dhs', 'shs')){
                            
                            fpost = mapply(k = 1:hurdlenet_out_trade[[l]]$input$K.fit,
                                           FUN = function(k){
                                             
                                             hurdlenet_out_trade[[l]]$MCMCout$f_lasymptote[,k] +
                                               hurdlenet_out_trade[[l]]$MCMCout$f_increment[,k]*
                                               ((1 + exp(hurdlenet_out_trade[[l]]$MCMCout$f_shift[,k] -
                                                           hurdlenet_out_trade[[l]]$MCMCout$f_rate[,k]*
                                                           matrix(data = fgrid, 
                                                                  nrow = hurdlenet_out_trade[[l]]$input$nMCMC,
                                                                  ncol = length(fgrid), byrow = T)))^
                                                  matrix(data = -1/as.numeric(hurdlenet_out_trade[[l]]$MCMCout$f_nu[,k]),
                                                         nrow = hurdlenet_out_trade[[l]]$input$nMCMC, 
                                                         ncol = length(fgrid),
                                                         byrow = F))
                                             
                                           }, SIMPLIFY = 'array')
                            
                            postsumm = apply(fpost, 2:3,
                                             FUN = function(v){
                                               
                                               c(mean(v),
                                                 quantile(x = v,
                                                          probs = c(.025, .975)))
                                               
                                             })
                            
                            print(hurdlenet_out_trade[[l]]$input$model.choice)
                            
                            return(rbind.data.frame(data.frame('est' = as.numeric(postsumm[1,,]),
                                                               'llim' = as.numeric(postsumm[2,,]),
                                                               'ulim' = as.numeric(postsumm[3,,]),
                                                               'g' = rep(fgrid, hurdlenet_out_trade[[l]]$input$K.fit),
                                                               'k' = rep(1:hurdlenet_out_trade[[l]]$input$K.fit,
                                                                         each = length(fgrid)),
                                                               'method' = hurdlenet_out_trade[[l]]$input$model.choice)))
                            
                          }else if(hurdlenet_out_trade[[l]]$input$model.choice=='nolatent'){
                            
                            fpost = (1 + exp(as.numeric(hurdlenet_out_trade[[l]]$MCMCout$freal) -
                                               matrix(data = fgrid, 
                                                      nrow = hurdlenet_out_trade[[l]]$input$nMCMC,
                                                      ncol = length(fgrid), byrow = T)))^
                              matrix(data = -1/as.numeric(hurdlenet_out_trade[[l]]$MCMCout$fpos2),
                                     nrow = hurdlenet_out_trade[[l]]$input$nMCMC, 
                                     ncol = length(fgrid),
                                     byrow = F)
                            
                            postsumm = apply(fpost, 2,
                                             FUN = function(v){
                                               
                                               c(mean(v),
                                                 quantile(x = v,
                                                          probs = c(.025, .975)))
                                               
                                             })
                            
                            print(hurdlenet_out_trade[[l]]$input$model.choice)
                            
                            return(rbind.data.frame(data.frame('est' = as.numeric(postsumm[1,]),
                                                               'llim' = as.numeric(postsumm[2,]),
                                                               'ulim' = as.numeric(postsumm[3,]),
                                                               'g' = fgrid,
                                                               'k' = 0,
                                                               'method' = hurdlenet_out_trade[[l]]$input$model.choice)))
                            
                          }
                          
                        }))

plotdf$k = factor(x = plotdf$k)
plotdf$method = factor(x = plotdf$method, 
                       levels = c('D1dhs', 'D0dhs',
                                  'shs', 'nolatent'))

k.label = paste0('K=', 0:hurdlenet_out_trade[[1]]$input$K.fit)
names(k.label) = 0:hurdlenet_out_trade[[1]]$input$K.fit

method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "shs" = 'dodgerblue',
                 "nolatent" = 'slateblue')

method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'shs' = 'Static',
                 'nolatent' = 'No latent')

ggplot2::ggplot(data = plotdf) +
  # ggplot2::coord_cartesian(ylim = c(0,1)) +
  # ggplot2::geom_point(
  #   # data = data.frame('trade.volume' = yvec.train[nzid.train],
  #   #                   'trade.occurence' = deltavec.train[nzid.train]),
  #   data = data.frame('trade.volume' = yvec.train,
  #                     'trade.occurence' = deltavec.train),
  #   ggplot2::aes(x = trade.volume,
  #                y = trade.occurence), 
  #   color = 'dodgerblue2', shape = 16, size = 1, alpha = .5
  # ) +
  ggplot2::facet_grid(method~k, scales = 'free_y',
                      labeller = ggplot2::labeller(method = method.label,
                                                   k = k.label)) +
  ggplot2::geom_line(
    ggplot2::aes(x = g, y = est),
    linewidth = .7
  ) +
  ggplot2::scale_colour_manual(values = method.color,
                               labels = method.label) +
  # ggplot2::facet_grid(.~k,
  #                     labeller = ggplot2::labeller(k = k.label)
  #                     ) +
  # ggplot2::geom_line(#data = subset(plotdf, method!='nolatent'),
  #                    ggplot2::aes(x = g, y = est, color = method),
  #                    linewidth = 1.5) +
  # ggplot2::scale_colour_manual(values = method.color,
  #                              labels = method.label) +
  ggplot2::theme(plot.title = ggplot2::element_text(size=20, face="bold"),
                 plot.subtitle = ggplot2::element_text(size=15),
                 axis.title.x = ggplot2::element_text(size=15,
                                                      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
                 # axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size=15,
                                                      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.text.x = ggplot2::element_text(color = "black", size = 10,
                                                     angle = 0, hjust = .5, vjust = 1),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(color = "black", size = 10),
                 # axis.ticks.x = ggplot2::element_line(linewidth = 1),
                 # axis.ticks.length.x = ggplot2::unit(.5, "cm"),
                 # axis.ticks.x = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                                      fill = NA, linewidth = 1),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                                          colour = "grey90"),
                 strip.text.x = ggplot2::element_text(size = 12, face = "bold"),
                 strip.text.y = ggplot2::element_text(size = 12, face = "bold"),
                 strip.background = ggplot2::element_rect(color="black", linewidth=1),
                 legend.title = ggplot2::element_blank(),
                 legend.key.width = ggplot2::unit(1.5, "cm"),
                 legend.key.height = ggplot2::unit(.75, "cm"),
                 # legend.key.size = ggplot2::unit(.5, "cm"),
                 legend.spacing.x = ggplot2::unit(.5, 'cm'),
                 legend.text=ggplot2::element_text(size=15),
                 legend.position = 'bottom') +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(title = 'International Trade Data Analysis',
                subtitle = 'Link Function',
                y = 'Trade Occurence Probability\n(f(x))', 
                x = 'Expected Trade Volume (x)')   # 7 X 9



# simulation: waic, looic ====
rm(list = ls())
sim.output.path = '/.../sim_output'

ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    sim_out = readRDS(file.path(sim.output.path, 
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(sim_out$ic["waic_Estimate",,]),
                                         'k' = rep(1:dim(sim_out$ic)[2], dim(sim_out$ic)[3]),
                                         'ic_type' = 'waic',
                                         'method' = model.choice,
                                         'n' = nNode),
                              data.frame('value' = as.numeric(sim_out$ic["looic_Estimate",,]),
                                         'k' = rep(1:dim(sim_out$ic)[2], dim(sim_out$ic)[3]),
                                         'ic_type' = 'looic',
                                         'method' = model.choice,
                                         'n' = nNode))
    
    rm(sim_out)
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$ic_type = factor(x = plotdf$ic_type, levels = names(ic.label))
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggh4x::facet_grid2(ic_type~n, #nrow = length(ic.label), ncol = length(n.label),
                      scales = 'free_y', independent = "y", 
                      labeller = ggplot2::labeller(ic_type = ic.label,
                                                   n = n.label)) +
  ggplot2::geom_boxplot(
    ggplot2::aes(x = k, y = value, fill = method),
    width = .5,
    alpha = .8,
    linewidth = .3, color = 'black',
    outlier.shape = 19, outlier.color = 'black',
    outlier.size = .5, outlier.stroke = .3
                        ) +
  # ggplot2::geom_violin(
  #   ggplot2::aes(x = k, y = value, fill = method),
  #   scale = 'width', alpha = .8,
  #   linewidth = .3, color = 'black'
  # ) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Information Criterion'
  )  # 5 X 10




# simulation: looic ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n = 5', '10' = 'n = 10', '20' = 'n = 20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    sim_out = readRDS(file.path(sim.output.path, 
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              # data.frame('value' = as.numeric(sim_out$ic["waic_Estimate",,]),
                              #            'k' = rep(1:dim(sim_out$ic)[2], dim(sim_out$ic)[3]),
                              #            'ic_type' = 'waic',
                              #            'method' = model.choice,
                              #            'n' = nNode),
                              data.frame('value' = as.numeric(sim_out$ic["looic_Estimate",,]),
                                         'k' = rep(1:dim(sim_out$ic)[2], dim(sim_out$ic)[3]),
                                         'ic_type' = 'looic',
                                         'method' = model.choice,
                                         'n' = nNode))
    
    rm(sim_out)
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
# plotdf$ic_type = factor(x = plotdf$ic_type, levels = names(ic.label))
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggh4x::facet_grid2(.~n, #nrow = length(ic.label), ncol = length(n.label),
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_boxplot(
    ggplot2::aes(x = k, y = value, fill = method),
    width = .5, alpha = .8, linewidth = .3, color = 'black',
    position = ggplot2::position_dodge(.6),
    outlier.shape = 19, outlier.color = 'grey60',
    outlier.size = .5, outlier.stroke = .3
  ) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=18,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=18,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 16,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 16),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 20,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 20,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(1, 'cm'),
    legend.text=ggplot2::element_text(size=18),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Information Criterion'
  )  # 4.5 X 12




# simulation: dynamics of Z ====
rm(list = ls())
sim.data.path = file.path("/.../simdata")  # path to the "simdata"
simdata = readRDS(file.path(sim.data.path, "simdata_n20_t10"))

plotZ.df = do.call('rbind.data.frame',
                   lapply(1:simdata$K,
                          FUN = function(k){
                            
                            data.frame('t' = rep(1:simdata$nTime, 
                                                 each = simdata$n),
                                       'indiv' = rep(1:simdata$n,
                                                     simdata$nTime),
                                       'k' = k,
                                       'value' = simdata$Z[,k])
                            
                          }))

plotZ.df$indiv = as.factor(plotZ.df$indiv)
plotZ.df$k = as.factor(plotZ.df$k)

k.label = paste0('k = ', 1:simdata$K)
names(k.label) = levels(plotZ.df$k)

ggplot2::ggplot(data = plotZ.df) +
  ggplot2::facet_grid(.~k,
                      labeller = ggplot2::labeller(k = k.label)) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = value, color = indiv),
                     linewidth = .6) +
  ggplot2::scale_colour_hue(l = 45) +
  ggplot2::scale_x_continuous(breaks = 1:simdata$nTime) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=17,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=17,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.1, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.1, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 17,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 17,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    # legend.title = ggplot2::element_blank(),
    legend.title=ggplot2::element_text(size=15),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    # legend.key.size = ggplot2::unit(.5, "cm"),
    legend.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow=FALSE, 
                                                title="Node",
                                                override.aes = list(linewidth = 1.2))) +
  ggplot2::labs(x = "Time", 
                y = expression("Latent Position (z"[itk]*")")) # 5.5 X 9




# simulation: reg coeff continuous ====
## calculating avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  
  simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                  paste0('simdata_n', nNode, '_t', t)))
  
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    
    sim_out = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    postmean = apply(X = sim_out$betareg_cont, 2:4, mean)
    rm(sim_out)
    
    avgloss = apply(X = postmean, 2:3,
                    FUN = function(v){
                      
                      mean((v - simdata_out$betareg.cont)^2)
                      
                    })
    rm(postmean)
    
    saveRDS(avgloss, 
            file.path(sim.output.path, 'hurdlenet_sim_output',
                      paste0('avgloss_betareg_cont_', model.choice, '_n', nNode, '_t', t)))
    
    rm(avgloss)
    gc()
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}


## plotting avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n = 5', '10' = 'n = 10', '20' = 'n = 20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_betareg_cont_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~n,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = k, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = k, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Mean Squared Error'
  )  # 4.5 X 12


## plotting avg loss at K=2 (truth) ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n = 5', '10' = 'n = 10', '20' = 'n = 20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                'simdata_n20_t10'))

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_betareg_cont_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf = plotdf[plotdf$k==simdata_out$K,]

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  # ggh4x::facet_grid2(.~n,
  #                    scales = 'free_y', independent = "y", 
  #                    labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = n, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = n, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=16,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 15,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 15),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(1, 'cm'),
    legend.text=ggplot2::element_text(size=16),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Number Of nodes (n)',
    y = 'Mean Squared Error'
  )  # 5 X 8




# simulation: reg coeff binary ====
## calculating avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  
  simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                  paste0('simdata_n', nNode, '_t', t)))
  
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    
    sim_out = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    postmean = apply(X = sim_out$betareg_binary, 2:4, mean)
    rm(sim_out)
    
    avgloss = apply(X = postmean, 2:3,
                    FUN = function(v){
                      
                      mean((v - simdata_out$betareg.binary)^2)
                      
                    })
    rm(postmean)
    
    saveRDS(avgloss, 
            file.path(sim.output.path, 'hurdlenet_sim_output',
                      paste0('avgloss_betareg_binary_', model.choice, '_n', nNode, '_t', t)))
    
    rm(avgloss)
    gc()
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}


## plotting avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_betareg_binary_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~n,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = k, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = k, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Mean Squared Error'
  )  # 4.5 X 12




# simulation: in-sample continuous ====
## calculating avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  
  simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                  paste0('simdata_n', nNode, '_t', t)))
  
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    
    sim_out = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    postmean = apply(X = sim_out$nzmean_fitted, 2:4, mean)
    rm(sim_out)
    
    avgloss = apply(X = postmean, 2:3,
                    FUN = function(v){
                      
                      mean((v - simdata_out$nzmeanvec[simdata_out$train.id])^2)
                      
                    })
    rm(postmean)
    
    saveRDS(avgloss, 
            file.path(sim.output.path, 'hurdlenet_sim_output',
                      paste0('avgloss_cont_', model.choice, '_n', nNode, '_t', t)))
    
    rm(avgloss)
    gc()
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}


## plotting avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_cont_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~n,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = k, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = k, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Mean Squared Error'
  )  # 4.5 X 12




# simulation: in-sample binary ====
## calculating avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  
  simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                  paste0('simdata_n', nNode, '_t', t)))
  
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    
    sim_out = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    postmean = apply(X = sim_out$probitprob_fitted, 2:4, mean)
    rm(sim_out)
    
    avgloss = apply(X = postmean, 2:3,
                    FUN = function(v){
                      
                      loss = v - simdata_out$probitprobvec[simdata_out$train.id]
                      mean((loss[!(is.na(loss)|is.nan(loss))])^2)
                      
                    })
    rm(postmean)
    
    saveRDS(avgloss, 
            file.path(sim.output.path, 'hurdlenet_sim_output',
                      paste0('avgloss_binary_', model.choice, '_n', nNode, '_t', t)))
    
    rm(avgloss)
    gc()
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}


## plotting avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_binary_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~n,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = k, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = k, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Mean Squared Error'
  )  # 4.5 X 12




# simulation: out-sample continuous ====
## calculating avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  
  simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                  paste0('simdata_n', nNode, '_t', t)))
  
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    
    sim_out = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    postmedian = apply(X = sim_out$nzmean_pred, 2:4, median)
    rm(sim_out)
    
    avgloss = apply(X = postmedian, 2:3,
                    FUN = function(v){
                      
                      loss = v - simdata_out$nzmeanvec[simdata_out$test.id]
                      mean((loss[!(is.na(loss)|is.nan(loss))])^2)
                      
                    })
    rm(postmedian)
    
    saveRDS(avgloss, 
            file.path(sim.output.path, 'hurdlenet_sim_output',
                      paste0('avgpredloss_cont_', model.choice, '_n', nNode, '_t', t)))
    
    rm(avgloss)
    gc()
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}


## plotting avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgpredloss_cont_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~n,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = k, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = k, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Mean Squared Prediction Error'
  )  # 4.5 X 12




# simulation: out-sample binary ====
## calculating avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  
  simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                  paste0('simdata_n', nNode, '_t', t)))
  
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    
    sim_out = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0(model.choice, '_n', nNode, '_t', t)))
    
    postmedian = apply(X = sim_out$probitprob_pred, 2:4, median)
    rm(sim_out)
    
    avgloss = apply(X = postmedian, 2:3,
                    FUN = function(v){
                      
                      loss = v - simdata_out$probitprobvec[simdata_out$test.id]
                      mean((loss[!(is.na(loss)|is.nan(loss))])^2)
                      
                    })
    rm(postmedian)
    
    saveRDS(avgloss, 
            file.path(sim.output.path, 'hurdlenet_sim_output',
                      paste0('avgpredloss_binary_', model.choice, '_n', nNode, '_t', t)))
    
    rm(avgloss)
    gc()
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}


## plotting avg loss ====
rm(list = ls())
sim.output.path = '/.../sim_output'

# ic.label = c('waic' = 'WAIC', 'looic' = 'LOO-IC')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgpredloss_binary_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf$k = as.factor(plotdf$k)
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~n,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(n = n.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = k, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = k, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 13,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 13),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(.5, 'cm'),
    legend.text=ggplot2::element_text(size=13),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Latent dimension (K)',
    y = 'Mean Squared Prediction Error'
  )  # 4.5 X 12




# simulation: in-sample at K=2 (truth)  ====
rm(list = ls())
sim.output.path = '/.../sim_output'

var_type.label = c('cont' = 'In-sample: Continuous',
                   'binary' = 'In-sample: Binary')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                'simdata_n20_t10'))

## cont ====
plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_cont_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'var_type' = 'cont',
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

## binary ====
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgloss_binary_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'var_type' = 'binary',
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf = plotdf[plotdf$k==simdata_out$K,]

plotdf$k = as.factor(plotdf$k)
plotdf$var_type = factor(x = plotdf$var_type, levels = names(var_type.label))
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~var_type,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(var_type = var_type.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = n, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = n, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 14,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 14),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 16,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 16,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(1, 'cm'),
    legend.text=ggplot2::element_text(size=15),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Number Of Nodes (n)',
    y = 'Mean Squared Error'
  )  # 5 X 9




# simulation: out-sample at K=2 (truth)  ====
rm(list = ls())
sim.output.path = '/.../sim_output'

var_type.label = c('cont' = 'Out-sample: Continuous',
                   'binary' = 'Out-sample: Binary')
n.label = c('5' = 'n=5', '10' = 'n=10', '20' = 'n=20')
method.label = c('D1dhs' = 'Hurdle-Net(1)',
                 'D0dhs' = 'Hurdle-Net(0)',
                 'indep' = 'Independent',
                 'shs' = 'Static')
method.color = c("D1dhs" = 'green4', "D0dhs" = 'chocolate',
                 "indep" = 'orchid', "shs" = 'dodgerblue')

simdata_out = readRDS(file.path(sim.output.path, 'hurdlenet_simdata',
                                'simdata_n20_t10'))

## cont ====
plotdf = NULL
t = 10
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgpredloss_cont_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'var_type' = 'cont',
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

## binary ====
for(nNode in c(5, 10, 20)){
  
  # nNode = 5
  # nNode = 10
  # nNode = 20
  for(model.choice in c("D1dhs", "D0dhs", "shs", "indep")){
    
    # model.choice = "D1dhs"
    # model.choice = "D0dhs"
    # model.choice = "shs"
    # model.choice = "indep"
    avgloss = readRDS(file.path(sim.output.path, 'hurdlenet_sim_output',
                                paste0('avgpredloss_binary_', model.choice, '_n', nNode, '_t', t)))
    
    plotdf = rbind.data.frame(plotdf,
                              data.frame('value' = as.numeric(apply(avgloss, 1, mean)),
                                         'value_sd' = as.numeric(apply(avgloss, 1, sd))/sqrt(ncol(avgloss)),
                                         'k' = 1:nrow(avgloss),
                                         'var_type' = 'binary',
                                         'method' = model.choice,
                                         'n' = nNode))
    
    print(paste0('n = ', nNode, ', method = ', model.choice))
    
  }
  
}

plotdf = plotdf[plotdf$k==simdata_out$K,]

plotdf$k = as.factor(plotdf$k)
plotdf$var_type = factor(x = plotdf$var_type, levels = names(var_type.label))
plotdf$method = factor(x = plotdf$method, levels = names(method.label))
plotdf$n = as.factor(plotdf$n)

ggplot2::ggplot(data = plotdf) +
  ggplot2::coord_cartesian(ylim = c(0,NA), expand = TRUE, default = FALSE, clip = 'on') +
  ggh4x::facet_grid2(.~var_type,
                     scales = 'free_y', independent = "y", 
                     labeller = ggplot2::labeller(var_type = var_type.label)) +
  ggplot2::geom_bar(ggplot2::aes(x = n, y = value, fill = method),
                    width = .6,
                    position = ggplot2::position_dodge(.6), stat = "identity",
                    color = "black", linewidth = .5, alpha = .5) +
  ggplot2::geom_errorbar(ggplot2::aes(x = n, 
                                      ymin = value - value_sd,
                                      ymax = value + value_sd,
                                      group = method),
                         width=.3, linewidth = .5, alpha = .4,
                         position = ggplot2::position_dodge(width = .6)) +
  ggplot2::scale_fill_manual(values = method.color,
                             labels = method.label) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.text.x = ggplot2::element_text(color = "black", size = 14,
                                        angle = 0, hjust = .5, vjust = 1),
    axis.text.y = ggplot2::element_text(color = "black", size = 14),
    axis.ticks.x = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.x = ggplot2::unit(.2, "cm"),
    axis.ticks.y = ggplot2::element_line(linewidth = .5),
    axis.ticks.length.y = ggplot2::unit(.2, "cm"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color='black', linetype = "solid",
                                         fill = NA, linewidth = 1),
    panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(linewidth = 0.5, linetype = 'solid',
                                             colour = "grey90"),
    strip.text.x = ggplot2::element_text(
      size = 16,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 16,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    legend.title = ggplot2::element_blank(),
    # legend.title = ggplot2::element_text(size = 30, face = "bold"),
    legend.key.width = ggplot2::unit(.75, "cm"),
    legend.key.height = ggplot2::unit(.75, "cm"),
    legend.key.spacing.x = ggplot2::unit(1, 'cm'),
    legend.text=ggplot2::element_text(size=15),
    legend.position = 'bottom'
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow=FALSE)) +
  ggplot2::labs(
    x = 'Number Of Nodes (n)',
    y = 'Mean Squared Prediction Error'
  )  # 5 X 9





