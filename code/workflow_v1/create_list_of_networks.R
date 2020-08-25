library(tidyverse)
library(statnet)
library(tidyselect)
library(ggnetwork)
library(ggthemes)
library(parallel)


set = 1:14
cores = 2
seed = 24
n_sims = 1000
cd_max = 100
mc_max = 40
burn = 5000
interval = 1000
samplesize = 5000
#library(devtools)
#install_github('statnet/ergm')

run_collab = TRUE
run_tech = TRUE
run_both = TRUE
sim_collab = FALSE
sim_tech = FALSE
results = FALSE

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
df <- df[df$Network>0&df$Network<15,]
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
id_roles = id_roles %>% select(-Response)
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
role_count = id_roles %>% group_by(Network,Self_Role_Name) %>% summarise(role_count = n())

dyad_df <- full_join(actor_details,alters_df)
dyad_df$Alter_Role_Name <- as.factor(roles_df$Role_Name[match(dyad_df$Alter_Role,roles_df$Role)])
#dyad_df$ID = as.factor(dyad_df$ID)
dyad_df = dyad_df[dyad_df$Network<=14,]

dyad_df$Network = as.factor(dyad_df$Network)
dyad_df = dyad_df[dyad_df$Response %in% c(1:3),]

id_roles$Network = as.factor(id_roles$Network)
dyad_expansion_df = full_join(dyad_df,id_roles)

dyad_expansion_df$Self_Role_Name = as.factor(dyad_expansion_df$Self_Role_Name)
dyad_expansion_df$Self_Role_Name = fct_expand(dyad_expansion_df$Self_Role_Name,union(levels(dyad_expansion_df$Self_Role_Name),levels(dyad_expansion_df$Alter_Role_Name)))
dyad_expansion_df$Alter_Role_Name = fct_expand(dyad_expansion_df$Alter_Role_Name,union(levels(dyad_expansion_df$Self_Role_Name),levels(dyad_expansion_df$Alter_Role_Name)))

collab_mats = lapply(seq_along(sort(unique(dyad_expansion_df$Network))),function(net) as.matrix(
  table(dyad_expansion_df$Self_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(1)],dyad_expansion_df$Alter_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(1)])))
tech_mats = lapply(seq_along(sort(unique(dyad_expansion_df$Network))),function(net) as.matrix(
  table(dyad_expansion_df$Self_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(2)],dyad_expansion_df$Alter_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(2)])))
both_mats = lapply(seq_along(sort(unique(dyad_expansion_df$Network))),function(net) as.matrix(
  table(dyad_expansion_df$Self_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(3)],dyad_expansion_df$Alter_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(3)])))
combo_mats = lapply(seq_along(sort(unique(dyad_expansion_df$Network))),function(net) as.matrix(
  table(dyad_expansion_df$Self_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(1:3)],dyad_expansion_df$Alter_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(1:3)])))


collaborative_networks = lapply(collab_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))
technical_networks = lapply(tech_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))
both_networks = lapply(both_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))
combo_networks = lapply(combo_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(technical_networks[[x]])),1,0)))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(both_networks[[x]])),1,0)))
invisible(lapply(1:length(combo_networks),function(x) combo_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(combo_networks[[x]])),1,0)))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(technical_networks[[x]])),1,0)))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(both_networks[[x]])),1,0)))
invisible(lapply(1:length(combo_networks),function(x) combo_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(combo_networks[[x]])),1,0)))


invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Type' <<-ifelse(collaborative_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Type' <<-ifelse(technical_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(technical_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Type' <<-ifelse(both_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(both_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))
invisible(lapply(1:length(combo_networks),function(x) combo_networks[[x]] %v% 'Type' <<-ifelse(combo_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(combo_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))


invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(collaborative_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(technical_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(both_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))
invisible(lapply(1:length(combo_networks),function(x) combo_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(combo_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(collaborative_networks[[x]] %v% 'Role_Count'),0,technical_networks[[x]] %v% 'Role_Count')))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(technical_networks[[x]] %v% 'Role_Count'),0,collaborative_networks[[x]] %v% 'Role_Count')))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(both_networks[[x]] %v% 'Role_Count'),0,both_networks[[x]] %v% 'Role_Count')))
invisible(lapply(1:length(combo_networks),function(x) combo_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(combo_networks[[x]] %v% 'Role_Count'),0,combo_networks[[x]] %v% 'Role_Count')))


network_names = read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRTNvrIUadJBTyG0qVDYhn-iQ6Ml1yIBnxS0FybjxtvjPKpJkqB3E-qaDjI4zwtCu71buQpM1KV9CAB/pub?output=csv')

names(collaborative_networks) <- paste0(network_names$Site[1:14],'_collab')
names(technical_networks) <- paste0(network_names$Site[1:14],'_tech')
names(both_networks) <- paste0(network_names$Site[1:14],'_both')
names(combo_networks) <- paste0(network_names$Site[1:14],'_combo')

net_list = list(technical_networks,collaborative_networks,both_networks,combo_networks)
names(net_list) <- c('tech','collab','both','combo')
saveRDS(net_list,'scratch/networks.RDS')





























