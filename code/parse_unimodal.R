library(tidyverse)
library(statnet)
library(tidyselect)
library(ggnetwork)
library(ggthemes)
library(parallel)

library(multinet)


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


role_count = id_roles %>% group_by(Network,Self_Role_Name) %>% summarise(role_count = n())

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

collaborative_networks = lapply(collab_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))
technical_networks = lapply(tech_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))
both_networks = lapply(both_mats,function(n) as.network(n,directed=T,loops=F,matrix.type = 'adjacency',bipartite=F,ignore.eval = F,names.eval = 'count'))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(technical_networks[[x]])),1,0)))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(both_networks[[x]])),1,0)))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(technical_networks[[x]])),1,0)))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(both_networks[[x]])),1,0)))


invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Type' <<-ifelse(collaborative_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Type' <<-ifelse(technical_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(technical_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Type' <<-ifelse(both_networks[[x]] %v% 'Agency' == 1,'Agency',ifelse(both_networks[[x]] %v% 'ONMS_Staff' == 1,'Staff','Other'))))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(collaborative_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(technical_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Role_Count' <<- role_count$role_count[role_count$Network==x][match(network.vertex.names(both_networks[[x]]),role_count$Self_Role_Name[role_count$Network==x])]))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(collaborative_networks[[x]] %v% 'Role_Count'),0,technical_networks[[x]] %v% 'Role_Count')))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(technical_networks[[x]] %v% 'Role_Count'),0,collaborative_networks[[x]] %v% 'Role_Count')))
invisible(lapply(1:length(both_networks),function(x) both_networks[[x]] %v% 'Role_Count' <<- ifelse(is.na(both_networks[[x]] %v% 'Role_Count'),0,both_networks[[x]] %v% 'Role_Count')))


network_names = read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRTNvrIUadJBTyG0qVDYhn-iQ6Ml1yIBnxS0FybjxtvjPKpJkqB3E-qaDjI4zwtCu71buQpM1KV9CAB/pub?output=csv')

names(collaborative_networks) <- paste0(network_names$Site[1:14],'_collab')
names(technical_networks) <- paste0(network_names$Site[1:14],'_tech')
names(both_networks) <- paste0(network_names$Site[1:14],'_tech')





library(multinet)
multinet::
net_list = list(both_networks,collaborative_networks,both_networks)
saveRDS(net_list,'scratch/networks.RDS')
mat_list = list(both_mats,collab_mats,tech_mats)
saveRDS(mat_list,'scratch/matrices.RDS')
#saveRDS(collaborative_networks,'scratch/collaborative_count_network_list.RDS')
#saveRDS(technical_networks,'scratch/technical_count_network_list.RDS')
is_in = which(rownames(no_degree) %in% dyad_expansion_df$Self_Role_Name)
#collaborative_networks = lapply(collaborative_networks,function(x) get.inducedSubgraph(x,is_in))
#technical_networks = lapply(technical_networks,function(x) get.inducedSubgraph(x,is_in))
#both_networks = lapply(both_networks,function(x) get.inducedSubgraph(x,is_in))

no_degree = as.sociomatrix(collaborative_networks[[1]]) * 0
no_degree[{lower.tri(no_degree) & !rownames(no_degree) %in% dyad_expansion_df$Self_Role_Name}] <- 1
no_degree[{upper.tri(no_degree) & !colnames(no_degree) %in% dyad_expansion_df$Alter_Role_Name}] <- 1

net_stats = function(net,stats = c('centralization','betweenness','clustering','closeness','avg_betweenness','sum_betweenness','clustering'),edge_name = 'count',
                     norm = TRUE,scale = TRUE,ign_eval = FALSE,group_value = 'Type'){
  stat_set = stats
  stat_list = list()
  if('centralization' %in% stat_set)
  {stat_list$centralization = sna::centralization(as.sociomatrix(net,attrname = edge_name),degree,ignore.eval = ign_eval,g = 1,mode = 'digraph',normalize = norm)}
  if('betweenness' %in% stat_set)
  {stat_list$betweenness = sna::centralization(as.sociomatrix(net,attrname = edge_name),betweenness,ignore.eval = ign_eval,g = 1,mode = 'digraph',normalize = norm)}
  if('closeness' %in% stat_set)
  {stat_list$closeness = sna::centralization(as.sociomatrix(net,attrname = edge_name),closeness,ignore.eval = ign_eval,g = 1,mode = 'digraph',normalize = norm)}
  if('avg_betweenness' %in% stat_set)
  {
    tdf = data.frame(betweenness = sna::betweenness(as.sociomatrix(net,attrname = edge_name),gmode = 'digraph' ,ignore.eval = ign_eval,rescale = T))
    tdf[group_value] <- net %v% group_value
    call <- substitute(group_by(tdf, group_value), list(group_value = as.name(group_value))) 
    tdf = eval(call)  %>% summarise(avg_betweenness = mean(betweenness,na.rm=T)) %>% spread(group_value,avg_betweenness,sep='_')
    stat_list$avg_betweenness = tdf}
  if('avg_betweenness' %in% stat_set)
  {
    tdf = data.frame(betweenness = sna::betweenness(as.sociomatrix(net,attrname = edge_name),gmode = 'digraph' ,ignore.eval = ign_eval,rescale = T))
    tdf[group_value] <- net %v% group_value
    call <- substitute(group_by(tdf, group_value), list(group_value = as.name(group_value))) 
    tdf = eval(call)  %>% summarise(sum_betweenness = sum(betweenness,na.rm=T)) %>% spread(group_value,sum_betweenness,sep='_')
    stat_list$sum_betweenness = tdf}
  if('clustering' %in% stat_set)
  {stat_list$clustering = unname(tnet::clustering_w(as.sociomatrix(net,attrname = edge_name),'mi'))}
  do.call(cbind,stat_list)
}

#if(any(run_collab,run_tech,sim_collab,sim_tech,run_both))
#{cl = makeCluster(cores, type = 'FORK')}

if(sim_collab){
  collab_sims = parLapply(cl,seq_along(collaborative_networks)[set],function(i) {
    sim_ergm = ergm(collaborative_networks[[i]]~ sum + offset(edgecov(no_degree)),offset.coef = -Inf,reference = ~Poisson,
                    response = 'count',control = control.ergm(MCMLE.trustregion = 1000),eval.loglik = F)
    sims = simulate(sim_ergm,n_sims,reference = ~Poisson,response = 'count')
    slist = lapply(seq_along(sims),function(x) net_stats(sims[[x]]))
    do.call(rbind,slist) %>% mutate(network = i)})
  saveRDS(collab_sims,'scratch/cug_poisson_collab.rds')
}

if(sim_tech){
  tech_sims = parLapply(cl,seq_along(technical_networks)[set],function(i) {
    sim_ergm = ergm(technical_networks[[i]]~ sum + offset(edgecov(no_degree)),offset.coef = -Inf,reference = ~Poisson,
                    response = 'count',control = control.ergm(MCMLE.trustregion = 1000),eval.loglik = F)
    sims = simulate(sim_ergm,n_sims,reference = ~Poisson,response = 'count')
    slist = lapply(seq_along(sims),function(x) net_stats(sims[[x]]))
    do.call(rbind,slist) %>% mutate(network = i)})
  saveRDS(tech_sims,'scratch/cug_poisson_tech_simc.rds')
}

if(run_collab){
  collab_poisson = lapply(seq_along(collaborative_networks)[set],function(x) {
    print(x)
    try(ergm(collaborative_networks[[x]] ~ sum + nodecov('Role_Count') + 
               nodeifactor('Agency') + nodeifactor('ONMS_Staff') + 
               mutual(form = 'min') + transitiveweights + 
               nodeisqrtcovar + 
               edgecov(as.sociomatrix(both_networks[[x]],'count'))+
               edgecov(as.sociomatrix(technical_networks[[x]],'count')),
             response = 'count',reference = ~Poisson,eval.loglik = F,
             control = control.ergm(MCMLE.trustregion = 1000,
                                    MCMC.interval = 1024, MCMLE.maxit = 30,
                                    MCMC.burnin = 1024 * 16, MCMC.samplesize = 1024*24,
                                    parallel = 8)))
  }
  )
  saveRDS(collab_poisson,'collab_poisson_gergms2.rds')  
}

if(run_both){
  both_poisson = lapply(seq_along(both_networks)[set],function(x) {
    print(x)
    try(ergm(both_networks[[x]] ~ sum + nodecov('Role_Count') + 
               nodeifactor('Agency') + nodeifactor('ONMS_Staff') + 
               mutual(form = 'min') + transitiveweights + 
               nodeisqrtcovar + 
               edgecov(as.sociomatrix(collaborative_networks[[x]],'count'))+
               edgecov(as.sociomatrix(technical_networks[[x]],'count')),
             response = 'count',reference = ~Poisson,eval.loglik = F,
             control = control.ergm(MCMLE.trustregion = 1000,
                                    MCMC.interval = 1024, MCMLE.maxit = 30,
                                    MCMC.burnin = 1024 * 16, MCMC.samplesize = 1024*24,
                                    parallel = 8)))
  })
  saveRDS(both_poisson,'both_poisson_gergms2.rds')  
}

# if(run_tech){
#   tech_poisson = parLapply(cl,seq_along(technical_networks)[set],function(x) {
#     print(x)
#     try(ergm(technical_networks[[x]]~ sum + 
#                nodeifactor('Agency') + nodeofactor('ONMS_Staff') + nodeifactor('ONMS_Staff') + 
#                mutual(form = 'min') + transitiveweights + nodeisqrtcovar + nodeosqrtcovar +
#                edgecov(as.sociomatrix(collaborative_networks[[x]],'count'))+
#                edgecov(as.sociomatrix(both_networks[[x]],'count')) + offset(edgecov(no_degree)),offset.coef = -Inf,
#              response = 'count',reference = ~Poisson,eval.loglik = F,
#              control = control.ergm(CD.maxit = cd_max,MCMLE.maxit = mc_max,MCMC.burnin = burn,parallel = 4,
#                                     MCMC.interval = interval,MCMC.samplesize = samplesize)))
#     })
#   saveRDS(tech_poisson,'tech_poisson_gergms.rds')
# }

if(run_tech){
  tech_poisson = lapply(seq_along(technical_networks)[set],function(x) {
    print(x)
    try(ergm(technical_networks[[x]] ~ sum + nodecov('Role_Count') + 
               nodeifactor('Agency') + nodeifactor('ONMS_Staff') + 
               mutual(form = 'min') + transitiveweights + 
               nodeisqrtcovar + 
               edgecov(as.sociomatrix(both_networks[[x]],'count'))+
               edgecov(as.sociomatrix(collaborative_networks[[x]],'count')),
             response = 'count',reference = ~Poisson,eval.loglik = F,
             control = control.ergm(MCMLE.trustregion = 1000,
                                    MCMC.interval = 1024, MCMLE.maxit = 30,
                                    MCMC.burnin = 1024 * 16, MCMC.samplesize = 1024*24,
                                    parallel = 8)))
  })
  saveRDS(tech_poisson,'tech_poisson_gergms2.rds')
}

x = 3
three_tech_fix =  try(ergm(technical_networks[[x]] ~ sum + nodecov('Role_Count') + 
             nodeifactor('Agency') + nodeifactor('ONMS_Staff') +
             #mutual(form = 'min') +
              transitiveweights +
             nodeisqrtcovar +
             edgecov(as.sociomatrix(both_networks[[x]],'count'))+
             edgecov(as.sociomatrix(collaborative_networks[[x]],'count')),
           response = 'count',reference = ~Poisson,eval.loglik = F,
           control = control.ergm(MCMLE.trustregion = 1000,#init = mb[[5]]$coef,
                                  MCMC.interval = 1500, MCMLE.maxit = 40,
                                  MCMC.burnin = 1024 * 16, MCMC.samplesize = 1024*30,
                                  parallel = 8)))
mt[[x]] <- three_tech_fix

x = 5
five_collab_fix =  try(ergm(collaborative_networks[[x]] ~ sum +
                           nodeifactor('Agency') + nodeifactor('ONMS_Staff') +
                           #mutual(form = 'min') +
                            transitiveweights +
                        #  nodeisqrtcovar +
                           edgecov(as.sociomatrix(both_networks[[x]],'count'))+
                           edgecov(as.sociomatrix(technical_networks[[x]],'count')),
                         response = 'count',reference = ~Poisson,eval.loglik = F,
                         control = control.ergm(MCMLE.trustregion = 1000,#init = mb[[5]]$coef,
                                                MCMC.interval = 1500, MCMLE.maxit = 40,
                                                MCMC.burnin = 1024 * 16, MCMC.samplesize = 1024*30,
                                                parallel = 8)))
mc[[x]] <- five_collab_fix

saveRDS(mt,'tech_poisson_gergms2.rds')
saveRDS(mc,'collab_poisson_gergms2.rds')


summary(collaborative_networks[[x]] ~ sum +
          nodeifactor('Agency') + nodeifactor('ONMS_Staff') +
          #mutual(form = 'min') + 
          transitiveweights +
          nodeisqrtcovar +
          edgecov(as.sociomatrix(both_networks[[x]],'count'))+
          edgecov(as.sociomatrix(technical_networks[[x]],'count')),response = 'count',reference = ~Poisson)
#if(results){
##### make plots
collab_nets = do.call(rbind,lapply(seq_along(collaborative_networks), function(net) {
  slist = net_stats(collaborative_networks[[net]])
  slist$network = net
  slist}))

tech_nets = do.call(rbind,lapply(seq_along(technical_networks), function(net) {
  slist = net_stats(technical_networks[[net]])
  slist$network = net
  slist}))

both_nets = do.call(rbind,lapply(seq_along(both_networks), function(net) {
  slist = net_stats(both_networks[[net]])
  slist$network = net
  slist}))


collab_df = do.call(rbind,readRDS('scratch/cug_poisson_collab.rds'))
tech_df = do.call(rbind,readRDS('scratch/cug_poisson_tech.rds'))

temp_collab = full_join(collab_df %>% select(contains('avg_betweenness'),network) %>%  gather(actor,betweenness,-network),
                      collab_nets %>% select(contains('avg_betweenness'),network) %>%  gather(actor,obs_betweenness,-network))
temp_collab = left_join(temp_collab,temp_collab %>% group_by(network,actor) %>% summarise(q = sum(obs_betweenness>betweenness,na.rm=T)/1000))
                      
temp_tech = full_join(tech_df %>% select(contains('avg_betweenness'),network) %>%  gather(actor,betweenness,-network),
                        tech_nets %>% select(contains('avg_betweenness'),network) %>%  gather(actor,obs_betweenness,-network))
temp_tech = left_join(temp_tech,temp_tech %>% group_by(network,actor) %>% summarise(q = sum(obs_betweenness>betweenness,na.rm=T)/1000))

gg1a = ggplot(temp_collab) + 
  geom_boxplot(aes(y = betweenness,fill = actor, colour = actor,x = as.factor(network)),alpha = 0.2) + theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 14),legend.text = element_text(size =12),legend.background = element_rect(fill = 'grey80'),axis.title=element_text(size = 15)) + 
  scale_colour_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +scale_fill_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +
  scale_x_discrete(name = 'NMS network (collaborative ties)') + scale_y_continuous(name = 'Avg. betweenness score by alter role type') + 
  geom_point(aes(y = obs_betweenness,x = as.factor(network),colour = actor,fill=actor,shape = ifelse(q>0.975|q<0.025,TRUE,FALSE)),size=5,alpha = 0.8,position = position_dodge(width = .75)) + coord_flip() + 
  ggtitle('Collaboration graphs') + scale_shape_manual(values = c(0,15)) + guides(colour = FALSE,fill=FALSE,shape = FALSE)

library(gridExtra)
gg1b = ggplot(temp_tech) + 
  geom_boxplot(aes(y = betweenness,fill = actor, colour = actor,x = as.factor(network)),alpha = 0.2) + theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.2),
        legend.title = element_text(size = 14),legend.text = element_text(size =12),
        legend.background = element_rect(fill = alpha('grey80',.8)),axis.title=element_text(size = 15)) + 
  scale_colour_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +scale_fill_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +
  scale_x_discrete(name = 'NMS network (technical ties)') + scale_y_continuous(name = 'Avg. betweenness score by alter role type') + 
  geom_point(aes(y = obs_betweenness,x = as.factor(network),colour = actor,fill=actor,shape = ifelse(q>0.975|q<0.025,TRUE,FALSE)),size=5,alpha = 0.8,position = position_dodge(width = .75)) + coord_flip() + 
  ggtitle('Technical information sharing graphs') + scale_shape_manual(values = c(0,15)) + guides(shape=FALSE)
gg1 = grid.arrange(gg1a,gg1b,ncol=2)
grid.arrange(gg1a,gg1b,ncol=2)

temp_collab = full_join(collab_df %>% select(contains('centralization'),network) %>%  gather(actor,centralization,-network),
                        collab_nets %>% select(contains('centralization'),network) %>%  gather(actor,obs_centralization,-network))
temp_collab = left_join(temp_collab,temp_collab %>% group_by(network,actor) %>% summarise(q = sum(obs_centralization>centralization,na.rm=T)/1000))
temp_collab$tie = 'collab'
temp_tech = full_join(tech_df %>% select(contains('centralization'),network) %>%  gather(actor,centralization,-network),
                        tech_nets %>% select(contains('centralization'),network) %>%  gather(actor,obs_centralization,-network))
temp_tech = left_join(temp_tech,temp_tech %>% group_by(network,actor) %>% summarise(q = sum(obs_centralization>centralization,na.rm=T)/1000))
temp_tech$tie = 'tech'
temp = full_join(temp_collab,temp_tech)
temp = left_join(temp,network_names %>% rename(network = Network) %>% mutate(Age = 2012 - Created))
gg2a = ggplot(data = temp) + geom_smooth(aes(x = Age,y = centralization,colour = tie)) + theme_bw() + 
  scale_colour_colorblind(name = 'Graph type',labels = c('Collaboration','Technical information sharing')) + 
  geom_point(aes(x = Age,y = obs_centralization,colour = tie),shape = 15, size = 3) + 
  theme(legend.position = c(0.8,0.2),legend.background = element_rect(fill = alpha('grey80',.8))) + scale_y_continuous(name = 'Centralization') +
  scale_x_continuous(name = 'Age (year observed - year created)')  + guides(colour = FALSE)

gg2b = ggplot(data = temp) + 
  geom_boxplot(aes(x = as.factor(network),y = centralization,colour = tie),position = position_dodge(width = 0.5)) + theme_bw()+
  scale_colour_colorblind(name = 'Graph type',labels = c('Collaboration','Technical information sharing')) +
  geom_point(aes(x = as.factor(network),y = obs_centralization,colour = tie),shape = 15, size = 3,position = position_dodge(width = 0.5)) +
theme(legend.position = c(0.2,0.8),legend.background = element_rect(fill = alpha('grey80',.8))) + scale_y_continuous(name = 'Centralization') +
  scale_x_discrete(name = 'Network') 

gg2 = grid.arrange(gg2a,gg2b,ncol = 2)



temp_collab = full_join(collab_df %>% select(contains('cluster'),network) %>%  gather(actor,clustering,-network),
                        collab_nets %>% select(contains('cluster'),network) %>%  gather(actor,obs_clustering,-network))
temp_collab = left_join(temp_collab,temp_collab %>% group_by(network,actor) %>% summarise(q = sum(obs_clustering>clustering,na.rm=T)/1000))
temp_collab$tie = 'collab'
temp_tech = full_join(tech_df %>% select(contains('cluster'),network) %>%  gather(actor,clustering,-network),
                      tech_nets %>% select(contains('cluster'),network) %>%  gather(actor,obs_clustering,-network))
temp_tech = left_join(temp_tech,temp_tech %>% group_by(network,actor) %>% summarise(q = sum(obs_clustering>clustering,na.rm=T)/1000))
temp_tech$tie = 'tech'
temp = full_join(temp_collab,temp_tech)
temp = left_join(temp,network_names %>% rename(network = Network))


gg3a = ggplot(data = temp) + geom_smooth(aes(y = clustering,x = log(Size),group = tie,colour = tie),
                                          pch = 21,alpha = 0.25) + theme_bw() + 
  scale_y_continuous(name = 'Clustering coefficient') +   scale_x_continuous(name = 'log(sq. kilometers)')  +
   scale_colour_colorblind(name = 'Graph type',labels = c('Collaboration','Technical information sharing')) + guides(colour = FALSE) + 
  geom_point(aes(y = obs_clustering,x = log(Size),colour = tie,
                 shape = ifelse(q>0.975|q<0.025,TRUE,FALSE)),size=4,alpha = 0.5) + 
  scale_shape_manual(values = c(15)) + guides(shape = FALSE)


temp$tie = ifelse(temp$tie == 'tech','Technical information sharing','Collaboration')
gg3 = ggplot(temp[!duplicated(paste0(temp$network,temp$tie)),]) + 
  geom_point(aes(x = log(Size),y = obs_clustering,size = `# regulated areas`,colour = `Preservation purpose`)) +
  scale_size_continuous(breaks = c(3,6,12)) + facet_wrap(~tie,ncol = 2) +
  scale_color_brewer(type = 'qual',palette = 2) +
  theme_bw() + scale_y_continuous(name = 'Clustering coefficient') + scale_x_continuous(name = 'log(sq. kilometers)') + 
  theme(legend.position = 'bottom')
gg3 


gg3b = ggplot(data = temp) + geom_smooth(aes(y = clustering,x = `# regulated areas`,group = tie,colour = tie),
                                         pch = 21,alpha = 0.25) + theme_bw() + 
  scale_y_continuous(name = 'Clustering coefficient') +   scale_x_continuous(name = 'log(sq. kilometers)')  +
  scale_colour_colorblind(name = 'Graph type',labels = c('Collaboration','Technical information sharing')) + guides(colour = FALSE) + 
  geom_point(aes(y = obs_clustering,x =  `# regulated areas`,colour = tie,
                 shape = ifelse(q>0.975|q<0.025,TRUE,FALSE)),size=4,alpha = 0.5) + 
  scale_shape_manual(values = c(15)) + guides(shape = FALSE)


gg2a = ggplot(data = temp) + geom_smooth(aes(x = Age,y = centralization,colour = tie)) + theme_bw() + 
  scale_colour_colorblind(name = 'Graph type',labels = c('Collaboration','Technical information sharing')) + 
  geom_point(aes(x = Age,y = obs_centralization,colour = tie),shape = 15, size = 3) + 
  theme(legend.position = c(0.8,0.2),legend.background = element_rect(fill = alpha('grey80',.8))) + scale_y_continuous(name = 'Centralization') +
  scale_x_continuous(name = 'Age (year observed - year created)')  + guides(colour = FALSE)

gg2b = ggplot(data = temp) + 
  geom_boxplot(aes(x = as.factor(network),y = centralization,colour = tie),position = position_dodge(width = 0.5)) + theme_bw()+
  scale_colour_colorblind(name = 'Graph type',labels = c('Collaboration','Technical information sharing')) +
  geom_point(aes(x = as.factor(network),y = obs_centralization,colour = tie),shape = 15, size = 3,position = position_dodge(width = 0.5)) +
  theme(legend.position = c(0.2,0.8),legend.background = element_rect(fill = alpha('grey80',.8))) + scale_y_continuous(name = 'Centralization') +
  scale_x_discrete(name = 'Network') 

gg2 = grid.arrange(gg2a,gg2b,ncol = 2)



library(statnet)
library(btergm)
library(ergm.count)
library(ggmcmc)

# mt = readRDS('scratch/ergm_results/tech_poisson_gergms_simc.rds')
# mc = readRDS('scratch/ergm_results/collab_poisson_gergms_simc.rds')
# mb = readRDS('scratch/ergm_results/both_poisson_gergms_simc.rds')


mt = readRDS('tech_poisson_gergms2.rds')
mc = readRDS('collab_poisson_gergms2.rds')
mb = readRDS('both_poisson_gergms2.rds')

sapply(mc,function(x) x$iterations)

mcg = do.call(rbind,lapply(seq_along(mc),function(x) ggs(mc[[x]]$sample) %>% data.frame() %>% mutate(network = x,graph = 'collab')))
mtg = do.call(rbind,lapply(seq_along(mt),function(x) ggs(mt[[x]]$sample) %>% data.frame() %>% mutate(network = x,graph = 'tech')))
mbg = do.call(rbind,lapply(seq_along(mb),function(x) ggs(mb[[x]]$sample) %>% data.frame() %>% mutate(network = x,graph = 'both')))
gof_df = do.call(rbind,list(mcg,mtg,mbg))


ggplot(gof_df %>% filter(graph == 'both'),aes(x = Iteration,y = value,
               color = as.factor(network),group = paste(network,Chain,sep='_'))) + geom_path() + 
  facet_wrap(~Parameter,scales = 'free')

ggplot(mg,aes(x = value,color = as.factor(network),group = as.factor(network))) + geom_density() + 
  facet_wrap(~Parameter,scales = 'free')
ggplot(mbg,aes(x = Iteration,y = value,
               color = as.factor(network),group = as.factor(network))) + geom_path() + 
  facet_wrap(~Parameter,scales = 'free')
ggplot(mtg,aes(x = value,color = as.factor(network),group = as.factor(network))) + geom_density() + 
  facet_wrap(~Parameter,scales = 'free')


sim_coef = replicate(4,coef(mc[[1]]))
sim_coef['nodeisqrtcovar',] <- c(-2,-1,0,1)
sim_sensitivity = lapply(1:ncol(sim_coef),function(i) simulate.ergm(mc[[1]],1,coef = sim_coef[,i], response = 'count',reference = ~Poisson))
ns = sample(1:37,8,replace = F)
sim_subs = lapply(sim_sensitivity,function(n) get.inducedSubgraph(n,v = ns))
sim_layouts = lapply(sim_subs,ggnetwork)


library(ggnetwork)
base_layout = ggnetwork::ggnetwork(sim_subs[[4]])
base_layout %>% filter(!duplicated(vertex.names))
fixed = base_layout[!duplicated(base_layout$vertex.names),c('x','y','vertex.names')]
lapply(seq_along(sim_layouts),function(i) {sim_layouts[[i]]$x <<- base_layout$x[match(sim_layouts[[i]]$vertex.names,base_layout$vertex.names)]
sim_layouts[[i]]$y <<- base_layout$y[match(sim_layouts[[i]]$vertex.names,base_layout$vertex.names)]})

library(gridExtra)
ggplot(sim_layouts[[1]],aes(x = x,y=y,xend = xend,yend=yend)) + geom_edges() + geom_nodes()

unlist(sapply(mc,function(x) x$formula[[2]]))

par(mfrow=c(2,2))
plot(sim_sensitivity[[1]])
plot(sim_sensitivity[[2]])
plot(sim_sensitivity[[3]])
plot(sim_sensitivity[[4]])
sim_sensitivity[[1]]
sim_sensitivity[[4]]



get.inducedSubgraph(sim_sensitivity[[1]],v = ns))
plot(get.inducedSubgraph(sim_sensitivity[[2]],v = ns))
plot(get.inducedSubgraph(sim_sensitivity[[3]],v = ns))
plot(get.inducedSubgraph(sim_sensitivity[[4]],v = ns))


lapply(test,function(x) summary(x~sum + nodefactor("Agency") + nodefactor("ONMS_Staff") + 
                                  mutual(form = "min") + transitiveweights + nodeisqrtcovar ))


library(broom)
mt_coefs = do.call(rbind,lapply(which(sapply(mt,class)=='ergm' & unlist(sapply(mt,function(x) x['iterations'],simplify=T)) < 30),function(x) tidy(mt[[x]],conf.int = T) %>% mutate(network = x,tie = 'tech')))
mc_coefs = do.call(rbind,lapply(which(sapply(mc,class)=='ergm' & unlist(sapply(mc,function(x) x['iterations'],simplify=T)) < 30),function(x) tidy(mc[[x]],conf.int = T) %>% mutate(network = x,tie = 'collab')))
mb_coefs = do.call(rbind,lapply(which(sapply(mb,class)=='ergm' & unlist(sapply(mb,function(x) x['iterations'],simplify=T)) < 30),function(x) tidy(mb[[x]],conf.int = T) %>% mutate(network = x,tie = 'both')))
temp = do.call(rbind,list(mt_coefs,mc_coefs,mb_coefs))
temp = left_join(temp,network_names %>% rename(network = Network))



library(forcats)
temp$term = as.factor(temp$term)
levels(temp$term) <- c("both edge", "collaborative edge","collaborative edge",
                       "technical edge","technical edge","mutuality","agency rep.","ONMS staff",
                       "node indegree sqrt. covar.",'sum','transitive weights')

temp$term = fct_relevel(temp$term,
            c('sum',"mutuality",'transitive weights',"node indegree sqrt. covar.",
              "agency rep.","ONMS staff","collaborative edge",
              "both edge", "technical edge"))

temp$term <- fct_rev(temp$term)

ggplot(temp,aes(y = term,xmin = conf.low, xmax = conf.high, x = estimate,colour = tie)) + 
  scale_color_colorblind(name = 'Ties',labels= c('collaboration','technical','both')) + 
  theme_bw() +
  geom_errorbarh(position = 'dodge') + facet_wrap(~Site,ncol = 5,scale='free_x') + 
  theme(legend.position = c(0.9,0.1),axis.ticks = element_blank(),
  legend.title = element_text(size = 16),strip.text = element_text(size = 14),
  axis.title = element_text(size = 16),legend.text = element_text(size = 14),
  axis.title.y = element_blank())  + 
  geom_vline(xintercept=0,lty=2) +
  scale_y_discrete(name = 'Network') + scale_x_continuous(name = '95% confidence interval') 

seed = 24
set.seed(seed)

gg1a = ggplot(temp %>% filter(term=='transitive weights')) + 
  stat_smooth(se = F,aes(x = log(Size),y = estimate,color=tie),alpha = 0.85,span=1)+
  geom_linerange(aes(x = log(Size),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round') + 
  geom_point(aes(y = estimate,x = log(Size),color=tie,fill=tie))  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_continuous(name= 'log(site area in sq. km)') + ggtitle('Transitivity vs. NMS size') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 18),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))

gg1b = ggplot(temp %>% filter(term=='transitive weights')) + 
  stat_smooth(se = F,aes(x = `# regulated areas`,y = estimate,color=tie),alpha = 0.5,span=0.85)+
  geom_linerange(aes(x = `# regulated areas`,
                   ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',
               position = position_dodge(width=0.5),) + 
  geom_point(aes(y = estimate,x = `# regulated areas`,color=tie,fill=tie), height=0,
             position = position_dodge(width=0.5))  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_continuous(name= '# regulated areas',breaks=c(3,6,9,12)) + ggtitle('Transitivity vs. # regulated areas') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 18),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))


gg1c = ggplot(temp %>% filter(term=='transitive weights')) + 
  geom_linerange(aes(x = `Preservation purpose`,y = estimate,group = paste(tie,network),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',
                 position = position_dodge(width=0.5)) + 
geom_point(aes(y = estimate,x = `Preservation purpose`,color=tie,fill=tie,group = paste(tie,network)),
             position = position_dodge(width=0.5))  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_discrete(name= 'Site purpose(s)',labels = c('Cultural','Ecological',"Both")) + 
  ggtitle('Transitivity vs. purpose') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 14),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))

library(gridExtra)
grid.arrange(gg1,gg2,gg3,ncol=3)



gg2a = ggplot(temp %>% filter(term=='mutuality')) + 
  stat_smooth(se = F,aes(x = log(Size),y = estimate,color=tie),alpha = 0.85,span=1)+
  geom_linerange(aes(x = log(Size),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round') + 
  geom_point(aes(y = estimate,x = log(Size),color=tie,fill=tie))  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_continuous(name= 'log(site area in sq. km)') + ggtitle('Reciprocity vs. NMS size') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 18),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))

gg2b = ggplot(temp %>% filter(term=='mutuality')) + 
  stat_smooth(se = F,aes(x = `# regulated areas`,y = estimate,color=tie),alpha = 0.5,span=0.85)+
  geom_linerange(aes(x = `# regulated areas`,
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',
                 position = position_dodge(width=0.5),) + 
  geom_point(aes(y = estimate,x = `# regulated areas`,color=tie,fill=tie), height=0,
             position = position_dodge(width=0.5))  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_continuous(name= '# regulated areas',breaks=c(3,6,9,12)) + ggtitle('Reciprocity vs. # regulated areas') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 18),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))


gg2c = ggplot(temp %>% filter(term=='mutuality')) + 
  geom_linerange(aes(x = `Preservation purpose`,y = estimate,group = paste(tie,network),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',
                 position = position_dodge(width=0.5)) + 
  geom_point(aes(y = estimate,x = `Preservation purpose`,color=tie,fill=tie,group = paste(tie,network)),
             position = position_dodge(width=0.5))  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_discrete(name= 'Site purpose(s)',labels = c('Cultural','Ecological',"Both")) + 
  ggtitle('Reciprocity vs. purpose') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 14),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))

library(gridExtra)
grid.arrange(gg2a,gg2b,gg2c,ncol=3)



temp$Created

gg4 = ggplot(temp %>% filter(term=="node indegree sqrt. covar.",estimate>(-20))) + 
  geom_linerange(aes(x = Created,y = estimate,group = paste(tie,network),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',lwd=1.5,
                 position = position_dodge(width=1)) + 
  geom_point(aes(y = estimate,x = Created,color=tie,fill=tie,group = paste(tie,network)),
             position = position_dodge(width=1),size = 3)  + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_continuous(name= 'Year created') + 
  ggtitle('Centralization (nodeisqrtcovar) vs. year created') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 14),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))
gg4
     
temp$tie <- as.factor(temp$tie)
temp$tie <- fct_relevel(temp$tie,c("collab","both","tech"))

gg5 = ggplot(temp %>% filter(term == "node indegree sqrt. covar.",estimate>(-20))) + 
  geom_linerange(aes(x = tie,y = estimate,group = paste(tie,network),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',lwd=1.5,
                 position = position_dodge(width=1)) +
  geom_point(aes(y = estimate,x = tie,color=tie,fill=tie,group = paste(tie,network)),
             position = position_dodge(width=1),size = 3)   +
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_discrete(name= 'Graph type') + 
  ggtitle('Centralization (nodeisqrtcovar) vs. graph type') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 14),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))+ guides(fill=FALSE,colour=FALSE)
gg5
gg6 = ggplot(temp %>% filter(term == "transitive weights",estimate>(-20))) + 
  geom_linerange(aes(x = tie,y = estimate,group = paste(tie,network),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',lwd=1.5,
                 position = position_dodge(width=1)) +
  geom_point(aes(y = estimate,x = tie,color=tie,fill=tie,group = paste(tie,network)),
             position = position_dodge(width=1),size = 3)   +
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_discrete(name= 'Graph type') + 
  ggtitle('Transitivity vs. graph type') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 14),
        title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8)) + guides(fill=FALSE,colour=FALSE)
grid.arrange(gg5,gg6,ncol=2)


ggplot(temp %>% filter(term%in%c('ONMS staff','agency rep.'))) + 
  geom_linerange(aes(x = tie,y = estimate,group = paste(tie,network),
                     ymin = conf.low,ymax = conf.high,color = tie,fill=tie),lineend = 'round',
                 position = position_dodge(width=0.5)) +
  geom_point(aes(y = estimate,x = tie,color=tie,fill=tie,group = paste(tie,network)),
             position = position_dodge(width=0.5))  + 
  facet_wrap(~term) + 
  #stat_smooth(aes(y = estimate,x = log(Size),color=tie)) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_tufte(ticks=F) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% confidence interval') + 
  scale_x_discrete(name= 'Site purpose(s)',labels = c('Cultural','Ecological',"Both")) + 
  ggtitle('Activity by actor type, graph type, and site purpose(s)') +
  theme(axis.title = element_text(size =18),axis.text = element_text(size = 14),
        title = element_text(size = 18),strip.text = element_text(size = 14),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        legend.position = c(0.8,0.8))


nba = read.csv("http://datasets.flowingdata.com/ppg2008.csv")

library(GGally)
ggcorr(nba)


ggplot(temp,
       aes(y = as.factor(network),x = estimate,
           xmin= conf.low,xmax = conf.high,colour = tie)) + 
  geom_errorbarh(height = 0.4,position = position_dodge(width = 1),alpha = 0.75) +  
  facet_wrap(~term,ncol = 2,scale = 'free_x') + 
  geom_point(position = position_dodge(width = 1)) +
  scale_color_colorblind(name = 'Ties',labels= c('collaboration','technical information sharing')) + 
  theme_bw() +
  theme(legend.position = c(0.74,0.2),axis.ticks = element_blank(),
        legend.title = element_text(size = 16),strip.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14))  + geom_vline(xintercept=0,lty=2) +
  scale_y_discrete(name = 'Network') + scale_x_continuous(name = '95% confidence interval') 



ggplot(temp[temp$term %in% 'nodeisqrtcovar'),],
       aes(y = as.factor(network),x = estimate,
           xmin= conf.low,xmax = conf.high,colour = tie)) + 
  geom_errorbarh(height = 0.4,position = position_dodge(width = 1),alpha = 0.75) +  
  facet_wrap(~term,ncol = 2,scale = 'free_x') + 
  geom_point(position = position_dodge(width = 1)) +
  scale_color_colorblind(name = 'Ties',labels= c('collaboration','technical information sharing')) + 
  theme_bw() +
  theme(legend.position = c(0.74,0.2),axis.ticks = element_blank(),
        legend.title = element_text(size = 16),strip.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14))  + geom_vline(xintercept=0,lty=2) +
  scale_y_discrete(name = 'Network') + scale_x_continuous(name = '95% confidence interval') 


#,-tie,-Site,-Created,-Size))
wide_coefs = temp %>% select(-p.value,-conf.low,-conf.high,-mcmc.error,-std.error) %>%
       spread(term,estimate)

ggcorr(data = wide_coefs[,colnames(wide_coefs) %in% levels(temp$term)]) 


cdt = cor(wide_coefs[,colnames(wide_coefs) %in% levels(temp$term)],use = 'pairwise.complete.obs')
corrplot::corrplot(cdt,tl.col = 'black',type = 'lower',title = 'all graphs')

cdt1 = cor(wide_coefs[wide_coefs$tie == 'collab',colnames(wide_coefs) %in% levels(temp$term)],use = 'pairwise.complete.obs')
corrplot::corrplot(cdt1,tl.col = 'black',type = 'lower',na.label = 'NA')
cdt2 = cor(wide_coefs[wide_coefs$tie == 'both',colnames(wide_coefs) %in% levels(temp$term)],use = 'pairwise.complete.obs')
corrplot::corrplot(cdt2,tl.col = 'black',type = 'lower',na.label = 'NA')
cdt3 = cor(wide_coefs[wide_coefs$tie == 'tech',colnames(wide_coefs) %in% levels(temp$term)],use = 'pairwise.complete.obs')
corrplot::corrplot(cdt3,tl.col = 'black',type = 'lower',na.label = 'NA')


ergm(technical_networks[[1]]~sum + isolates, response = 'count',reference = ~Poisson)
library(statnet)

data.frame(role_response = 
             colSums(sapply(technical_networks,function(x) degree(x,cmode = 'outdegree')) > 0))




library(btergm)



