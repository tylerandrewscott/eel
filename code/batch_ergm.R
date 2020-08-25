library(statnet)
library(tidyverse)
library(parallel)

network_names <- data.frame(Site = c('Channel Islands','Cordell Bank',
                                     'Fagatele Bay','Florida Keys','Flower Garden Banks',
                                     "Gray's Reef",'Gulf of Farallones','Humpback Whale (Hawaii)',
                                     'Monitor','Monterey Bay','Olympic Coast','Papahanaumokuakea',
                                     'Stellwagen','Thunder Bay'),Network = 1:14,
                            regulated_areas = c(6,5,8,11,11,8,6,6,8,9,8,13,7,3),
                            size_km2 = c(3800 ,3331 ,0.65 ,9600,145.58,57 ,3312 ,3600 ,1.9 ,15783 ,8260 ,360000 ,2190 ,1160),
                            task = c("Ecological","Ecological","Ecological","Ecological and cultural","Ecological","Ecological","Ecological","Ecological",
                                     "Cultural","Ecological","Ecological and cultural","Ecological","Ecological and cultural","Cultural" ))

collaborative_networks <- readRDS('scratch/collaborative_bip_network_list.RDS')
technical_networks <- readRDS('scratch/technical_bip_network_list.RDS')
seed = 24

interval = 2000
sample = 20000
burnin = 5000

cl = makeCluster(cores, type = 'FORK')  
gwb1_alpha = 0.1
gwb2_alpha = 0.5

collab_ergms = mclapply(seq_along(collaborative_networks), function(net) {print(net); try(ergm(collaborative_networks[[net]]~edges + b2degree(0) + 
              gwb1degree(gwb1_alpha ,fixed=T) + gwb2degree(gwb2_alpha ,fixed=T) + 
           b1cov('Role_Count') + b1cov('Time_Involved') + b1factor('On_ONMS_Staff') ,
           eval.loglik = F,verbose=F,constraints = ~edges,
           control = control.ergm(#parallel = 4,parallel.type = 'MPI',
             MCMC.burnin = burnin,MCMC.samplesize = sample,
           MCMC.interval = interval,MCMLE.density.guard.min = length(collaborative_networks[[net]]$mel) * 3,MCMLE.maxit = 40)))},mc.cores = cores,mc.cleanup=T,mc.preschedule=T)

saveRDS(collab_ergms,'scratch/mod_ergm_collab.rds')

tech_ergms = mclapply(seq_along(technical_networks), function(net) try(ergm(technical_networks[[net]]~edges + b2degree(0) + 
                                                                                  gwb1degree(gwb1_alpha ,fixed=T) + gwb2degree(gwb2_alpha ,fixed=T) + 
                                                                                  b1cov('Role_Count') + b1cov('Time_Involved') + b1factor('On_ONMS_Staff') ,
                                                                                eval.loglik = F,verbose=F,constraints = ~edges,
                                                                                control = control.ergm(MCMC.burnin = burnin,MCMC.samplesize = sample,
                                                                                                       MCMC.interval = interval,MCMLE.density.guard.min = length(technical_networks[[net]]$mel) * 3,MCMLE.maxit = 40)))
                    ,mc.cores = cores,mc.cleanup=T,mc.preschedule=T)
saveRDS(tech_ergms,'scratch/mod_ergm_tech.rds')
stopCluster(cl)
