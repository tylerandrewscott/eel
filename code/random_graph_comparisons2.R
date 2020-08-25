library(tidyverse)
library(statnet)
library(ergm.count)
library(ggnetwork)
library(ggthemes)
library(viridis)
library(parallel)

nms_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRQWWKfD8Uw1CXwKwAxnTHI5B5AmO78omN1KREAitb03JVT4lxvqcAYxPCUKwpTFi-BznmekC27LUFs/pub?output=csv'
#nms_url = 'input/National Marine Sanctuaries Data.csv'
#Q5 - who are you
#R05 AW1-13, AW35-36
#R05 SO24-34 are nms staff
#ASOWs are drops
df = read_csv(nms_url)
df2 = df
df <- df[!is.na(df$`Record ID`),]
df <- df[,names(df)!='Role']
df$ID = df$`Record ID`
df$Network <- df$`R02 ASW`
df <- df[df$Network>0,]
df <- df[,!names(df) %in% c('R25 1 ASOW','R25 2 ASOW','R27 1 ASOW','R27 2 ASOW')]
df <- df[df$ID %in% df$ID[!sapply(df$ID,function(x) all((gather(df[df$ID==x,grepl('25|26|27',names(df))]))$value %in% c(-2,-5)))],]

role_vectors <- paste0('R05 AW ',c(1:23,35,36))
nms_role_vectors <- paste0('R05 SO ',c(24:30,32:34))
roles_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vShimT8Fb2xCUXjoOaHBCCOaj3UgtHNK5jufXBeltwqnBpqrGd_5aABGQcRAc-kmXOA14LVnpZi1lba/pub?output=csv'
#roles_url = 'input/Roles_Code_Cleaned.csv'
roles_df <- read_csv(roles_url)
id_roles = gather(df[,c('ID','Network',role_vectors,nms_role_vectors)],Role,Response,-ID,-Network)
id_roles <- id_roles[!grepl('Other ONMS staff',id_roles$Role),]
id_roles = id_roles[id_roles$Response>0,]
id_roles$Role_ID <- paste(id_roles$ID,id_roles$Role,sep='_')
id_roles$Self_Role_Name <- roles_df$Role_Name[match(id_roles$Role,roles_df$Role)]
#questions about alters
#Q25 - which ONMS staff do you communicate with: R25‐ASOW.1 - R25‐ASOW.15
#Q26 - which gov folks do you communicate with: R26‐ASOW.1 - R26‐ASOW.10
#Q27 - which non-gov folks do you communicate with: R27‐ASOW.1 - R27‐ASOW.16

actor_details <- data.frame(ID = df$ID,Actor_Type =ifelse(df$`R01 ASW`%in%c(1,2),'Advisory Council',ifelse(df$`R01 ASW`%in%c(5,6),'Staff Member','Working Group')),
                            Time_Involved = df$`R03 ASOW`,Network = df$Network)
alters_df = Reduce(function(u,v) merge(u,v,all=TRUE),list(gather(df[,c('ID','Network',grep('R25',names(df),value=T))],Alter_Role,Response,-ID,-Network),
                                                          gather(df[,c('ID','Network',grep('R26',names(df),value=T))],Alter_Role,Response,-ID,-Network),
                                                          gather(df[,c('ID','Network',grep('R27',names(df),value=T))],Alter_Role,Response,-ID,-Network)))
alters_df$Response[alters_df$Response==-1] <- 0
did_not_complete = alters_df %>% group_by(ID) %>% summarise(no_see = all(Response<0)) %>% filter(no_see) %>% .$ID
df = df[!df$ID %in% did_not_complete,]
id_roles = id_roles[!id_roles$ID %in% did_not_complete,]

dyad_df <- full_join(actor_details,alters_df)
dyad_df$Alter_Role_Name <- as.factor(roles_df$Role_Name[match(dyad_df$Alter_Role,roles_df$Role)])
dyad_df$Bip_Self_Role_Name = as.factor(paste0('Self_',dyad_df$Self_Role_Name))
dyad_df$Bip_Alter_Role_Name = as.factor(paste0('Alter_',dyad_df$Alter_Role_Name))
#dyad_df$ID = as.factor(dyad_df$ID)
dyad_df = dyad_df[dyad_df$Network<=14,]
dyad_df = dyad_df[dyad_df$Response%in%c(1,2,3),]


collab_mats = lapply(seq_along(sort(unique(dyad_df$Network))),function(net) as.matrix(
  table(dyad_df$ID[dyad_df$Network==net&dyad_df$Response%in%c(1,3)],dyad_df$Alter_Role_Name[dyad_df$Network==net&dyad_df$Response%in%c(1,3)])))
collaborative_networks = lapply(collab_mats,function(n) as.network(n,directed=F,matrix.type = 'incidence',bipartite=T))

tech_mats = lapply(seq_along(sort(unique(dyad_df$Network))),function(net) as.matrix(
  table(dyad_df$ID[dyad_df$Network==net&dyad_df$Response%in%c(2,3)],dyad_df$Alter_Role_Name[dyad_df$Network==net&dyad_df$Response%in%c(2,3)])))
technical_networks = lapply(tech_mats,function(n) as.network(n,directed=F,loops=F,matrix.type = 'adjacency',bipartite=T))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(technical_networks[[x]])),1,0)))
invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(technical_networks[[x]])),1,0)))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'On_ONMS_Staff' <<-ifelse(network.vertex.names(collaborative_networks[[x]]) %in% id_roles$ID[grepl('staff',id_roles$Self_Role_Name)],'Is_Staff','Not_Staff')))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'On_ONMS_Staff' <<-ifelse(network.vertex.names(technical_networks[[x]]) %in% id_roles$ID[grepl('staff',id_roles$Self_Role_Name)],'Is_Staff','Not_Staff')))
invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Agency_Rep' <<-ifelse(network.vertex.names(collaborative_networks[[x]]) %in% id_roles$ID[grepl('agency',id_roles$Self_Role_Name)],'Works_For_Agency','Not_Agency')))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Agency_Rep' <<-ifelse(network.vertex.names(technical_networks[[x]]) %in% id_roles$ID[grepl('agency',id_roles$Self_Role_Name)],'Works_For_Agency','Not_Agency')))

role_count = id_roles %>% group_by(ID) %>% summarise(roles = n())
invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Role_Count' <<- role_count$roles[match(network.vertex.names(collaborative_networks[[x]]),role_count$ID)]))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Role_Count' <<-role_count$roles[match(network.vertex.names(technical_networks[[x]]),role_count$ID)]))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Time_Involved' <<- df$`R03 ASOW`[match(network.vertex.names(collaborative_networks[[x]]),df$`Record ID`)]))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Time_Involved' <<-df$`R03 ASOW`[match(network.vertex.names(technical_networks[[x]]),df$`Record ID`)]))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Level' <<- ifelse(1:collaborative_networks[[x]]$gal$n %in% 1:technical_networks[[x]]$gal$bipartite,1,2)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Level' <<- ifelse(1:technical_networks[[x]]$gal$n %in% 1:technical_networks[[x]]$gal$bipartite,1,2)))

network_names <- data.frame(Site = c('Channel Islands','Cordell Bank',
                                     'Fagatele Bay','Florida Keys','Flower Garden Banks',
                                     "Gray's Reef",'Gulf of Farallones','Humpback Whale (Hawaii)',
                                     'Monitor','Monterey Bay','Olympic Coast','Papahanaumokuakea',
                                     'Stellwagen','Thunder Bay','Pacific Island Regional Office',
                                     'West Coast Regional Office','Southeast Atlantic/Gulf of Mexico/Caribbean Regional Office',
                                     'Northeast Regional Office','National Office'),Network = 1:19)
names(collaborative_networks) <- paste0(network_names$Site[1:14],'_collab')
names(technical_networks) <- paste0(network_names$Site[1:14],'_tech')

invisible(lapply(seq_along(collaborative_networks), function(x) collaborative_networks[[x]] %v% 'l2_type' <<- ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff' != 1 & collaborative_networks[[x]] %v% 'Agency' != 1, 'None',ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff','Staff','Agency'))))
invisible(lapply(seq_along(technical_networks), function(x) technical_networks[[x]] %v% 'l2_type' <<- ifelse(technical_networks[[x]] %v% 'ONMS_Staff' != 1 & technical_networks[[x]] %v% 'Agency' != 1, 'None',ifelse(technical_networks[[x]] %v% 'ONMS_Staff','Staff','Agency'))))

sim_n = 1000
seed = 24
cores = 5
cl = makeCluster(cores, type = 'FORK')  
collab_sims = parLapply(cl, seq_along(collaborative_networks), function(net) {
   ne = ergm(collaborative_networks[[net]]~edges,eval.loglik = F)
   sims = simulate(ne,nsim = sim_n,constraints=~edges)
   clustering_coef = sapply(seq_along(sims),function(x) {bipartite::clustering_tm(as.sociomatrix(sims[[x]]))})
   df = data.frame(clustering_coef)
   df})
          
saveRDS(collab_sims,'scratch/collab_simulation_results_cluster.rds')

#cl = makeCluster(cores, type = 'FORK')  
tech_sims = parLapply(cl, seq_along(technical_networks), function(net) {
                        ne = ergm(technical_networks[[net]]~edges,eval.loglik = F)
                        sims = simulate(ne,nsim = sim_n,constraints=~edges)
                        clustering_coef = sapply(seq_along(sims),function(x) {bipartite::clustering_tm(as.sociomatrix(sims[[x]]))})
                        df = data.frame(clustering_coef)
                        df})

saveRDS(tech_sims,'scratch/tech_simulation_results_cluster.rds')
stopCluster(cl)
