
library(texreg)
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
seed = 24;cores = 8
et = readRDS('scratch/mod_ergm_tech.rds')
ec = readRDS('scratch/mod_ergm_collab.rds')

gwb1_alpha = 0.05
gwb2_alpha = 0.25

test = dyad_df[dyad_df$Network==2 & dyad_df$Response%in%2:3,]
dim(test)
head(left_join(test,id_roles))

summary(test)
summary(collaborative_networks[[1]]~)

test = ergm(collaborative_networks[[1]]~edges + b1degree(0) + b2degree(0) +b1degrange(20, to=35) + 
             # gwb1degree(0 ,fixed=T) + 
              gwb2degree(.5 ,fixed=T) + 
              b1cov('Role_Count') + b1cov('Time_Involved'),
            eval.loglik = F,verbose=F,constraints = ~edges)
summary(test)

library(broom)

interval = 750
sample = 12000
burnin = 4000

cl = makeCluster(cores, type = 'FORK')  

test = ergm(collaborative_networks[[1]]~edges + 
             b1cov('Role_Count') + b1cov('Time_Involved') + b1factor('On_ONMS_Staff') +
             gwb1degree(0.1 ,fixed=T) +
             gwb2degree(0.1,fixed=T),eval.loglik = T,verbose=T)

           control = control.ergm(MCMC.burnin = burnin,
                                  MCMC.samplesize = sample,
                                  MCMC.interval = interval),verbose=T)
plot(collaborative_networks[[3]])
summary(test)
plot()
plot(test)
stopCluster(cl)
sapply(collab_mods,class)
summary(collab_mods[[1]])

saveRDS(collab_mods,'scratch/ergm_collab2.rds')



lapply(seq_along(collaborative_networks),function(x) {seq_along(seq(0,4,0.2)),
       function(v) summary(collaborative_networks[[x]] ~ gwb1degree(v,fixed=T))})
seq_along(collaborative_networks)
seq(0,4,0.2)
library(statnet)
library(tidyverse)
library(Bergm)

stats_tech_off = invisible(lapply(1:length(tech_off), function(x) 
  data.frame(coef = tech_off[[x]]$specs,net = x,mod = 'tech_off',
             Bergm::bergm.output(tech_off[[x]])$statistics)))

stats_tech_no_off = invisible(lapply(1:length(tech_no_off), function(x) 
  data.frame(coef = tech_no_off[[x]]$specs,net = x,mod = 'tech_no_off',
             Bergm::bergm.output(tech_no_off[[x]])$statistics)))

stats_collab_off = invisible(lapply(1:length(collab_off), function(x) 
  data.frame(coef = collab_off[[x]]$specs,net = x,mod = 'collab_off',
             Bergm::bergm.output(collab_off[[x]])$statistics)))

stats_colab_no_off = invisible(lapply(1:length(colab_no_off), function(x) 
  data.frame(coef = colab_no_off[[x]]$specs,net = x,mod = 'collab_no_off',
             Bergm::bergm.output(colab_no_off[[x]])$statistics)))

coef_results = do.call(rbind,list(Reduce(full_join,stats_tech_off),
Reduce(full_join,stats_tech_no_off),
Reduce(full_join,stats_collab_off),
Reduce(full_join,stats_colab_no_off)))


ggplot(coef_results[grepl('tech',coef_results$mod)&!grepl('edgecov',coef_results$coef),],aes(y = coef,x = Mean,colour = mod)) + 
  geom_point(pch=21,size = 2) + 
  facet_wrap(~net) + theme(legend.position = c(0.8,0.1))
head(coef_results)
ggplot(coef_results[!grepl('edgecov',coef_results$coef),],aes(y = coef,x = Mean,colour = mod)) + 
  geom_point(pch=21,size = 2) + 
  facet_wrap(~mod) + theme(legend.position = c(0.8,0.1))


p.flo <- bergm(flomarriage ~ edges + kstar(2),
               burn.in = 50,
               aux.iters = 500,
               main.iters = 500,
               gamma = 1)

# Bayesian goodness-of-fit test:
class(p.flo)
p.flo$
bgof(tech_off[[2]],
     aux.iters = 500,
     sample.size = 50,
     n.deg = 10,
     n.dist = 9,
     n.esp = 6)
