
#setwd('~/Documents/Github/eel')
#system('ln -s ~/Box/eel/input/ ~/Documents/GitHub/eel')
#system('ln -s ~/Box/eel/output/ ~/Documents/GitHub/eel')

pgks = c('tidyverse','statnet','tidyselect','ggnetwork','ggthemes','data.table','magrittr')
lapply(pgks[!pgks %in% installed.packages()[,'Package']],install.packages)
lapply(pgks,require,character.only = TRUE)
#devtools::install_github("tedhchen/multilayer.ergm")
library(multilayer.ergm)
sheet_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTg6ChPQBBftPdUB-IFq4OQGFyDruWwBBDyJpPmcPSiSOV1Y_bEx2XeF_wR6vkq-rY4TW2-ZAMP4alN/pub?gid=456743467&single=true&output=csv"
df = fread(sheet_url)
df2 = df
df <- df[!is.na(df$`Record ID`),]
setnames(df,c('Role',"R02 ASW","Record ID"),c('Level','Network',"ID"))
df <- df[df$Network>0&df$Network<15,]
df <- df[,!names(df) %in% c('R25 1 ASOW','R25 2 ASOW','R27 1 ASOW','R27 2 ASOW'),with = F]
df <- df[df$ID %in% df$ID[!sapply(df$ID,function(x) all(melt(df[df$ID==x,grepl('R25|R26|R27',names(df)),with = F])$value %in% c(-2,-5)))],]

role_vectors <- paste0('R05 AW ',c(1:23,35,36))
nms_role_vectors <- paste0('R05 SO ',c(24:30,32:34))
role_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vShimT8Fb2xCUXjoOaHBCCOaj3UgtHNK5jufXBeltwqnBpqrGd_5aABGQcRAc-kmXOA14LVnpZi1lba/pub?output=csv'
#roles_url = 'input/Roles_Code_Cleaned.csv'
roles_df <- fread(role_url)


id_roles = melt(df[,c('ID','Network','Level',role_vectors,nms_role_vectors),with = F],id.vars = c('ID','Network','Level'),value.name ='Response',variable.name = 'Role' )
id_roles <- id_roles[!grepl('Other ONMS staff',id_roles$Role),]
id_roles = id_roles[id_roles$Response>0,]
id_roles[,Response:=NULL]
id_roles$Role_ID <- paste(id_roles$ID,id_roles$Role,sep='_')
id_roles$Self_Role_Name <- roles_df$Role_Name[match(id_roles$Role,roles_df$Role)]
#questions about alters
#Q25 - which ONMS staff do you communicate with: R25‐ASOW.1 - R25‐ASOW.15
#Q26 - which gov folks do you communicate with: R26‐ASOW.1 - R26‐ASOW.10
#Q27 - which non-gov folks do you communicate with: R27‐ASOW.1 - R27‐ASOW.16

role_count = id_roles[,.N,by=.(Network,Self_Role_Name)]
setnames(role_count,'N','role_count')

actor_details <- data.table(ID = df$ID,Actor_Type =ifelse(df$`R01 ASW`%in%c(1,2),'Advisory Council',ifelse(df$`R01 ASW`%in%c(5,6),'Staff Member','Working Group')),
                            Time_Involved = df$`R03 ASOW`,Network = df$Network)


alters_df = Reduce(function(u,v) merge(u,v,all=TRUE),list(gather(df[,c('ID','Network',grep('R25',names(df),value=T)),with = F],Alter_Role,Response,-ID,-Network),
                                                          gather(df[,c('ID','Network',grep('R26',names(df),value=T)),with = F],Alter_Role,Response,-ID,-Network),
                                                          gather(df[,c('ID','Network',grep('R27',names(df),value=T)),with = F],Alter_Role,Response,-ID,-Network)))
alters_df  = data.table(alters_df )
alters_df$Response[alters_df$Response==-1] <- 0
did_not_complete = alters_df[,all(Response<0),by=.(ID)][V1==TRUE,]$ID

df = df[!df$ID %in% did_not_complete,]
id_roles = id_roles[!id_roles$ID %in% did_not_complete,]

dyad_df <- merge(actor_details,alters_df)
dyad_df$Alter_Role_Name <- as.factor(roles_df$Role_Name[match(dyad_df$Alter_Role,roles_df$Role)])
#dyad_df$ID = as.factor(dyad_df$ID)
dyad_df = dyad_df[dyad_df$Network<=14,]

dyad_df$Network = as.factor(dyad_df$Network)
dyad_df = dyad_df[dyad_df$Response %in% c(1:3),]
id_roles$Network = as.factor(id_roles$Network)

dyad_expansion_df = merge(dyad_df,id_roles,all = T,allow.cartesian = T,by = c('ID','Network'))
dyad_expansion_df$Self_Role_Name = as.factor(dyad_expansion_df$Self_Role_Name)
dyad_expansion_df$Self_Role_Name = fct_expand(dyad_expansion_df$Self_Role_Name,union(levels(dyad_expansion_df$Self_Role_Name),levels(dyad_expansion_df$Alter_Role_Name)))
dyad_expansion_df$Alter_Role_Name = fct_expand(dyad_expansion_df$Alter_Role_Name,union(levels(dyad_expansion_df$Self_Role_Name),levels(dyad_expansion_df$Alter_Role_Name)))
tdf = dyad_expansion_df
tdf = tdf[tdf$Self_Role_Name!='NGO',]
tdf = tdf[!is.na(tdf$ID),]
tdf$Self_Role_Name = as.character(tdf$Self_Role_Name)
tdf$Level[is.na(tdf$Level)&tdf$Actor_Type=='Advisory Council'] <- 'A'

set = 1:14

df$Group = ifelse(df$`R01 ASW` %in% 1:4, 'Advisory_Council',ifelse(df$`R01 ASW` %in% 5:8,'Staff','Work_Group'))
df$Experience = df$`R03 ASOW`
df$Graduate_Degree = ifelse(df$`R04 ASOW`%in%c(7,8),1,0)
df$Policy_Involvement = df$`R08 W`+df$`R08 S`
df$Science_Involvement = df$`R08 AS`+df$`R08 O`
mean_centered_priority = apply(df[,grepl('R06 ASOW',names(df)),with = F],2,scale,scale = F)
mean_centered_belief = apply(df[,grepl('R28 ASOW',names(df)),with = F],2,scale,scale = F)
rownames(mean_centered_priority) <- df$ID
rownames(mean_centered_belief) <- df$ID
require(lsa)
priority_adjacency_matrix = cosine(t(mean_centered_priority))
belief_adjacency_matrix = cosine(t(mean_centered_belief))

for(n in unique(tdf$Network)){
temp_net = tdf[Network==n]
temp_net$Alter_ID = id_roles$ID[id_roles$Network==n][match(temp_net$Alter_Role_Name,id_roles$Self_Role_Name[id_roles$Network==n])]
net = network::network.initialize(n = length(unique(temp_net$ID)),directed = T,bipartite = F)
net %v% 'vertex.names' <- sort(unique(temp_net$ID))

policy_priority_similarity = priority_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names']
policy_belief_similarity = belief_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names']

cnet = net
inet = net
network::add.edges(x = cnet,tail =match(temp_net[!is.na(Alter_ID)&Response%in%c(1,3),]$ID,network.vertex.names(net)),head = match(temp_net[!is.na(Alter_ID)&Response%in%c(1,3),]$Alter_ID,network.vertex.names(net)))
network::add.edges(x = inet,tail =match(temp_net[!is.na(Alter_ID)&Response%in%c(2,3),]$ID,network.vertex.names(net)),head = match(temp_net[!is.na(Alter_ID)&Response%in%c(2,3),]$Alter_ID,network.vertex.names(net)))

#devtools::install_github("tedhchen/multilayer.ergm")
library(multilayer.ergm)
mnet <- to.multiplex(inet,cnet, output = "network",directed = T)
mnet %v% 'vertex.ID' <- rep(net %v% 'vertex.names',2)
mnet %v% 'Group' <- df$Group[match(mnet %v% 'vertex.ID' ,df$ID)]
mnet %v% 'Experience' <- df$Experience[match(mnet %v% 'vertex.ID' ,df$ID)]
mnet %v% 'Graduate_Degree' <- df$Graduate_Degree[match(mnet %v% 'vertex.ID' ,df$ID)]
mnet %v% 'Policy_Involvement' <- df$Policy_Involvement[match(mnet %v% 'vertex.ID' ,df$ID)]
mnet %v% 'Science_Involvement' <- df$Science_Involvement[match(mnet %v% 'vertex.ID' ,df$ID)]


free <- to.multiplex(matrix(1, ncol = network.size(net), nrow = network.size(net)), 
                     matrix(1, ncol = network.size(net), nrow = network.size(net)), 
                     output = "network", offzeros = TRUE)

mod.within <- ergm(mnet ~ edges_layer(layer = 1) + edges_layer(layer = 2) +  
                   gwidegree(decay = 2,fixed = T,attr = 'layer.mem',levels=T)+
                   gwodegree(decay = 2,fixed = T,attr = 'layer.mem',levels=T)+
                     gwesp_layer(layer = 1)  + gwesp_layer(layer = 2) + 
                     nodefactor_layer('Group',layer = 1) + nodefactor_layer('Group',layer = 2) + 
                     nodecov_layer('Experience',layer = 1) + nodecov_layer('Experience',layer = 2)+
                     nodecov_layer('Policy_Involvement',layer = 1) + nodecov_layer('Policy_Involvement',layer = 2)+
                     nodecov_layer('Science_Involvement',layer = 1) + nodecov_layer('Science_Involvement',layer = 2) +
                     edgecov_layer(priority_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 1)+
                     edgecov_layer(priority_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 2)+
                     edgecov_layer(belief_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 1)+
                     edgecov_layer(belief_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 2))
                     constraints = ~fixallbut(free),
                     eval.loglik = T,control = control.ergm(parallel=8, parallel.type="PSOCK",MCMC.samplesize = 1e4,MCMC.burnin = 1e4))

mod.between <- ergm(mnet ~ edges_layer(layer = 1) + edges_layer(layer = 2) +  
                      gwidegree(decay = 2,fixed = T,attr = 'layer.mem',levels=T)+
                      gwodegree(decay = 2,fixed = T,attr = 'layer.mem',levels=T)+
                      gwesp_layer(layer = 1)  + gwesp_layer(layer = 2) + 
                      duplexdyad(type = c('e','f'),layers = list(1,2)) +
                      nodefactor_layer('Group',layer = 1) + nodefactor_layer('Group',layer = 2) + 
                      nodecov_layer('Experience',layer = 1) + nodecov_layer('Experience',layer = 2)+
                      nodecov_layer('Policy_Involvement',layer = 1) + nodecov_layer('Policy_Involvement',layer = 2)+
                      nodecov_layer('Science_Involvement',layer = 1) + nodecov_layer('Science_Involvement',layer = 2) +
                      edgecov_layer(priority_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 1)+
                      edgecov_layer(priority_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 2)+
                      edgecov_layer(belief_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 1)+
                      edgecov_layer(belief_adjacency_matrix[net %v% 'vertex.names',net %v% 'vertex.names'],layer = 2),
                    constraints = ~fixallbut(free),
                     eval.loglik = T,control = control.ergm(parallel=8, parallel.type="PSOCK",MCMC.samplesize = 1e4,MCMC.burnin = 1e4))
saveRDS(list(mod.within,mod.between),file = paste0('bucket_mount/scratch/network_',n,'.RDS'))
}
#,constraints = ~fixallbut(free))

