mt = readRDS('scratch/ergm_results/tech_poisson.rds')
mt

mc = readRDS('scratch/tech_poisson.rds')
library(ergm.count)
x = 1
test = ergm(technical_networks[[x]] ~ nonzero + sum + nodefactor("Agency") + 
       nodefactor("ONMS_Staff") + mutual(form = "min") + transitiveweights + 
       nodeisqrtcovar + offset(edgecov(no_degree)),
       control = control.ergm(MCMC.interval = 2000,MCMC.samplesize = 10000,init =coef(mt[[x]])))

as.formula(mt[[x]]$formula)
control.ergm
mt[[1]]$formula
mcmc.diagnostics(mt[[2]])
mt[[2]]
sapply(seq_along(mc), function(x) try(mt[[x]]$iterations))

mc = readRDS('scratch/collab_poisson.rds')

sapply(mt,class)
sapply(mc,class)
