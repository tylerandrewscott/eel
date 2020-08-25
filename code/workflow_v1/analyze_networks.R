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
all_nets <- readRDS('scratch/networks.RDS')


combo_net_models = lapply(seq_along(all_nets$combo),function(x) {print(x);
  tryCatch({ergm(all_nets$combo[[x]]~edges + mutual + 
         gwidegree(decay=.2,fixed=T) + 
             # gwodegree(decay=0,fixed=T) +
         gwesp(decay = 0.1,fixed=T) +
        nodefactor('Agency')+nodefactor('ONMS_Staff')+nodecov('Role_Count'),
       eval.loglik = F,control = 
         control.ergm(seed = 24,MCMC.interval = 1500,
                      MCMC.samplesize = 12000,MCMC.burnin = 7500,
                                              parallel = 1))})})



lapply(seq_along(all_nets$combo),function(x) {print(x);
  summary(all_nets$combo[[x]]~edges + mutual + 
                   gwidegree(decay=0.25,fixed=T) + 
                   # gwodegree(decay=0,fixed=T) +
                   gwesp(decay = 0.25,fixed=T) +
                   nodefactor('Agency')+nodefactor('ONMS_Staff')+nodecov('Role_Count'))})



       
       x = 5
test = ergm(all_nets$combo[[x]]~edges + mutual + 
              isolates + 
              gwidegree(decay=0,fixed=T) + 
             # gwodegree(decay=0,fixed=T) +
              gwesp(decay = 0,fixed=T) +
              nodefactor('Agency')+nodefactor('ONMS_Staff')+nodecov('Role_Count'),
            eval.loglik = F,control = control.ergm(MCMC.interval = 1500,MCMLE.maxit = 40,
                                                   MCMC.samplesize = 1000,MCMC.burnin = 1000,
                                                   parallel = 8))

plot(all_nets$combo[[x]])



combo_ergms <- lapply(seq_along(all_nets$combo),function(x) {
  print(x);
tryCatch({boot.MPLE(all_nets$combo[[x]]~ edges + mutual + gwidegree(decay=0.25,fixed=T) + 
            #gwodegree(decay=0,fixed=T) +
            gwesp(decay = 0.25,fixed=T) +
            nodefactor('Agency')+nodefactor('ONMS_Staff')+nodecov('Role_Count'),
          boot.rep = 1000,calc_loglik = F,number.cores = 4)})})


lapply(combo_ergms ,function(x) x$bootstrapped_MPLE_results)
combo_ergms[[3]]
combo_ergms[[1]]$bootstrapped_MPLE_results



x = 2
test =  
  
  control.ergm


cbind(
  network::get.vertex.attribute(combo_nets[[3]],'Role_Count'),
  sna::degree(combo_nets[[3]],cmode = 'outdegree'))


ergm(combo_nets[[3]]~edges + nodefactor('Agency')+nodefactor('ONMS_Staff'))

as_adjacency_matrix(combo_nets[[3]])








collaborative_networks <- readRDS('scratch/collaborative_bip_network_list.RDS')
technical_networks <- readRDS('scratch/technical_bip_network_list.RDS')
seed = 24;cores = 8

summarize_gwd = function(network_list,sequence,which_gwd = 'gwdb1'){
  df = data.frame(do.call(rbind,lapply(seq_along(network_list),function(i){do.call(rbind,lapply(sequence,function(v) 
    return(cbind(gwd = summary(as.formula(paste0("network_list[[i]] ~ ",
                                                 ifelse(which_gwd == 'gwdb1',"gwb1degree(v,fixed=T)","gwb2degree(v,fixed=T)")))),v,i,which_gwd))))})));df}


summarize_gwd(collaborative_networks,seq(0,3,0.2),'gwb1')
summarize_gwd(collaborative_networks,seq(0,3,0.2),'gwb2')
summarize_gwd(technical_networks,seq(0,3,0.2),'gwb1')
summarize_gwd(technical_networks,seq(0,3,0.2),'gwb2')

tall = do.call(rbind,test)
tall = data.frame(tall)
colnames(tall)[1]<-'gwd'
alphas = tall %>% filter(gwd<250) %>% group_by(i) %>% filter(v == max(v))
gwb1_alpha_collab = alphas$v

test = lapply(seq_along(technical_networks),function(i) {do.call(rbind,lapply(seq(0,4,0.005),
                                                                              function(v) return(cbind(summary(technical_networks[[i]] ~ gwb1degree(v,fixed=T)),v,i))))})
tall = do.call(rbind,test)
tall = data.frame(tall)
colnames(tall)[1]<-'gwd'
alphas = tall %>% filter(gwd<250) %>% group_by(i) %>% filter(v == max(v))
gwb1_alpha = alphas$v

gwb1_alpha_collab

gwb2_alpha = 0.7
interval = 2000
sample = 50000
burnin = 15000
invisible(lapply(seq_along(collaborative_networks), function(x) collaborative_networks[[x]] %v% 'l2_type' <<- ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff' != 1 & collaborative_networks[[x]] %v% 'Agency' != 1, 'None',ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff','Staff','Agency'))))
invisible(lapply(seq_along(technical_networks), function(x) technical_networks[[x]] %v% 'l2_type' <<- ifelse(technical_networks[[x]] %v% 'ONMS_Staff' != 1 & technical_networks[[x]] %v% 'Agency' != 1, 'None',ifelse(technical_networks[[x]] %v% 'ONMS_Staff','Staff','Agency'))))

cl = makeCluster(cores, type = 'FORK')  

collab_mods = parLapply(cl, seq_along(collaborative_networks), function(net) {
  try(ergm(collaborative_networks[[net]]~edges + #cycle(4) + 
             b1cov('Role_Count') + b1cov('Time_Involved') + b1factor('On_ONMS_Staff') +
             gwb1degree(gwb1_alpha ,fixed=T) +
             gwb2degree(gwb2_alpha ,fixed=T) + 
             b2twostar(b1attrname = 'l2_type', b2attrname = 'l2_type',base = 2),eval.loglik = F,
           control = control.ergm(MCMC.burnin = burnin,
                                  MCMC.samplesize = sample,
                                  MCMC.interval = interval)))})
saveRDS(collab_mods,'scratch/ergm_collab2.rds')

tech_mods = parLapply(cl, seq_along(technical_networks), function(net) {
  try(ergm(technical_networks[[net]]~edges + #cycle(4) + 
             b1cov('Role_Count') + b1cov('Time_Involved') + b1factor('On_ONMS_Staff') +
             gwb1degree(gwb1_alpha ,fixed=T) +gwb2degree(gwb2_alpha ,fixed=T) + 
             b2twostar(b1attrname = 'l2_type', b2attrname = 'l2_type',base = 2),eval.loglik = F,
           control = control.ergm(MCMC.burnin = burnin,MCMC.samplesize = sample,MCMC.interval = interval)))})

saveRDS(tech_mods,'scratch/ergm_tech2.rds')

stopCluster(cl)
