# hurdlenet

## Description

Codes to implement hurdle network model with latent dynamic shrinkage (Hurdle-Net) as proposed in [Pramanik et al. (2025+)](http://arxiv.org/abs/2504.21275) and reproduce figures/results therein. 

## Credit

Sandipan Pramanik developed this repository and wrote the codes included here.

## Required Package Installation

The `sourcecode` folder contains the required functions for implementing the method. Before sourcing the functions therein, we recommend installing the latest versions of `doParallel` and `rstan`.

## Sourcing hurdlenet Functions

Specify the path to the `sourcecode` folder available from this `GitHub` repository. The functions can then be sourced as

``` r
sourcecode.path = ...    # specifies path ".../sourcecode" to the sourcecode folder
source(file.path(source.code.path, 'hurdlenet-functions.R'))    # sources "hurdlenet-functions.R"
```

## Key Functions

In the `sourcecode` folder, `hurdlenet-functions.R` contains all the required functions as listed below:
 
 * `hurdlenet()` for fitting Hurdle-Net(1), Hurdle-Net(0), independent model, and static model (as described in [Pramanik et al. (2025+)](http://arxiv.org/abs/2504.21275)),
 * `fitted.hurdlenet()` for computing model quantities in the training data based on `hurdlenet()` fit,
 * `predict.hurdlenet()` for predicting at future time points based on `hurdlenet()` fit,
 * `sim.hurdlenet()` for generating simulated data.

## Reproducing Results and Figures

* `simulation` folder contains all the files for simulations
  * `simdata` folder contains the simulated data using `sim.hurdlenet()` in `simdata.R`
  * `sim-D1dhs.R`, `sim-D0dhs.R`, `sim-indep.R`, `sim-static.R` conduct simulations for Hurdle-Net(1), Hurdle-Net(0), independent model, and static model, respectively

* `trade` folder contains all the files for real data analysis using bilateral trade flows
  * `trade-D1dhs.R`, `trade-D0dhs.R`, `trade-indep.R`, `trade-static.R` implement Hurdle-Net(1), Hurdle-Net(0), independent model, and static model, respectively

* `plot.R` contains codes used for creating the figures
