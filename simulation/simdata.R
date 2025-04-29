
rm(list = ls())

# specify paths to source code and data ====
source.code.path = file.path("/.../sourcecode")  # path to the "sourcecode" folder


# source code ====
source(file.path(source.code.path, "hurdlenet-functions.R"))


# simulate hurdlenet data ====
## check snr and nonzero binary occurence proportions ====
set.seed(1)
sim.hurdlenet.out = sim.hurdlenet(n = 20, nTime.train = 10, nTime.test = 1,
                                  pNode = 3, pPair = 2, beta0.binary = .5,
                                  K = 2, alpha.latent = .9, noise.sd = 1,
                                  nReplicate = 50,
                                  verbose = T, saveoutput = F)

par(mfrow = c(1,2))
hist(sim.hurdlenet.out$snr, freq = F)
hist(sim.hurdlenet.out$nonzero.prop, freq = F)
par(mfrow = c(1,1))

mean(sim.hurdlenet.out$snr>=1)


## dynamics of Z ====
plotZ.df = do.call('rbind.data.frame',
                   lapply(1:sim.hurdlenet.out$K,
                          FUN = function(k){
                            
                            data.frame('t' = rep(1:sim.hurdlenet.out$nTime, 
                                                 each = sim.hurdlenet.out$n),
                                       'indiv' = rep(1:sim.hurdlenet.out$n,
                                                     sim.hurdlenet.out$nTime),
                                       'k' = k,
                                       'value' = sim.hurdlenet.out$Z[,k])
                            
                          }))

plotZ.df$indiv = as.factor(plotZ.df$indiv)
plotZ.df$k = as.factor(plotZ.df$k)

k.label = paste0('k = ', 1:sim.hurdlenet.out$K)
names(k.label) = levels(plotZ.df$k)

time.label = as.character(1:sim.hurdlenet.out$nTime)
names(time.label) = time.label

ggplot2::ggplot(data = plotZ.df) +
  # ggplot2::coord_cartesian(ylim = c(0,1), expand = TRUE, default = FALSE, clip = 'on') +
  ggplot2::facet_grid(.~k, scales = 'free_y',
                      labeller = ggplot2::labeller(k = k.label)) +
  # ggplot2::facet_wrap(.~k, #scales = 'free_y',
  #                     labeller = ggplot2::labeller(k = k.label)) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = value, color = indiv),
                     linewidth = .6) +
  ggplot2::scale_colour_hue(l=45) +
  # ggplot2::scale_color_viridis_d(option="viridis")
  # paletteer::scale_color_paletteer_d(dutchmasters, milkmaid) +
  # ggplot2::scale_color_brewer(palette="Dark2")
  ggplot2::scale_x_continuous(n.breaks = 5) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size=15,
                                         margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size=15,
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
      size = 15,
      face = "bold"
    ),
    strip.text.y = ggplot2::element_text(
      size = 15,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(color="black", linewidth=1),
    # legend.title = ggplot2::element_blank(),
    legend.title=ggplot2::element_text(size=13),
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
                y = 'Latent Values') # 11 X 5



## simulate for different n ====
for(t in c(10)){
  
  for(nNode in c(5, 10, 20)){
    
    set.seed(1)
    sim.hurdlenet.out = sim.hurdlenet(n = nNode, nTime.train = t, nTime.test = 1,
                                      pNode = 3, pPair = 2, beta0.binary = .5,
                                      K = 2, alpha.latent = .9, noise.sd = 1,
                                      nReplicate = 50,
                                      verbose = T, saveoutput = T,
                                      output.dir = 'simdata', 
                                      output.filename = paste0('simdata_n', nNode, '_t', t))
    
    print(paste0('n = ', nNode, ', T = ', t))
    
  }
  
}

