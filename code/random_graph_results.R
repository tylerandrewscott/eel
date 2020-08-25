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
                                     'Northeast Regional Office','National Office'),Network = 1:19,Created = c(1980,1989,1986,1990,1991,1981,1981,1992,1975,1992,1994,2006,1992,2000,rep(NA,5)))
names(collaborative_networks) <- paste0(network_names$Site[1:14],'_collab')
names(technical_networks) <- paste0(network_names$Site[1:14],'_tech')

invisible(lapply(seq_along(collaborative_networks), function(x) collaborative_networks[[x]] %v% 'l2_type' <<- ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff' != 1 & collaborative_networks[[x]] %v% 'Agency' != 1, 'None',ifelse(collaborative_networks[[x]] %v% 'ONMS_Staff','Staff','Agency'))))
invisible(lapply(seq_along(technical_networks), function(x) technical_networks[[x]] %v% 'l2_type' <<- ifelse(technical_networks[[x]] %v% 'ONMS_Staff' != 1 & technical_networks[[x]] %v% 'Agency' != 1, 'None',ifelse(technical_networks[[x]] %v% 'ONMS_Staff','Staff','Agency'))))

collab_nc = readRDS('scratch/collab_simulation_results_nocluster.rds')
invisible(lapply(seq_along(collab_nc),function(x) collab_nc[[x]]$Network <<- x))
collab_nc = do.call(rbind,collab_nc)

tech_nc = readRDS('scratch/tech_simulation_results_nocluster.rds')
invisible(lapply(seq_along(tech_nc),function(x) tech_nc[[x]]$Network <<- x))
tech_nc = do.call(rbind,tech_nc)


collab_nets = lapply(seq_along(collaborative_networks), function(net) {
  #clustering_coef = sapply(seq_along(sims),function(x) {bipartite::clustering_tm(as.sociomatrix(sims[[x]]))})
  centralization = sna::centralization(collaborative_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = (collaborative_networks[[net]]$gal$bipartite+1):collaborative_networks[[net]]$gal$n)
  centralization_staff = sna::centralization(collaborative_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = which(collaborative_networks[[net]] %v% 'l2_type'=='Staff'))
  centralization_agency = sna::centralization(collaborative_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = which(collaborative_networks[[net]]%v% 'l2_type'=='Agency'))
  centralization_other = sna::centralization(collaborative_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = which(collaborative_networks[[net]] %v% 'l2_type'=='None' & !grepl('[0-9]',network.vertex.names(collaborative_networks[[net]]))))
  one_mode_prj = bipartite::projecting_tm(t(as.sociomatrix(collaborative_networks[[net]])),method = 'Newman')
  avg_betweenness = bipartite::betweenness_w(one_mode_prj) %>% as.data.frame() %>% 
      tidyr::complete(node = 1:(collaborative_networks[[net]]$gal$n-collaborative_networks[[net]]$gal$bipartite),fill = list(node = 0)) %>%
      mutate(l2_type = {(collaborative_networks[[net]] %v% 'l2_type')[(collaborative_networks[[net]]$gal$bipartite+1):collaborative_networks[[net]]$gal$n]}) %>%
      group_by(l2_type) %>% summarise(avg_betweenness = mean(betweenness,na.rm=T)) %>% spread(l2_type,avg_betweenness) %>% 
      rename(Agency_betweenness = Agency,None_betweenness = None,Staff_betweenness = Staff)
  df = data.frame(#clustering_coef,
    centralization,centralization_staff,centralization_agency,centralization_other,avg_betweenness,Network = net)
  df})
  
collab_obs = do.call(rbind,collab_nets)


tech_nets = lapply(seq_along(technical_networks), function(net) {
  #clustering_coef = sapply(seq_along(sims),function(x) {bipartite::clustering_tm(as.sociomatrix(sims[[x]]))})
  centralization = sna::centralization(technical_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = (technical_networks[[net]]$gal$bipartite+1):technical_networks[[net]]$gal$n)
  centralization_staff = sna::centralization(technical_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = which(technical_networks[[net]] %v% 'l2_type'=='Staff'))
  centralization_agency = sna::centralization(technical_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = which(technical_networks[[net]]%v% 'l2_type'=='Agency'))
  centralization_other = sna::centralization(technical_networks[[net]],'degree',g = 1,normalize = TRUE, nodes = which(technical_networks[[net]] %v% 'l2_type'=='None' & !grepl('[0-9]',network.vertex.names(technical_networks[[net]]))))
  one_mode_prj = bipartite::projecting_tm(t(as.sociomatrix(technical_networks[[net]])),method = 'Newman')
  avg_betweenness = bipartite::betweenness_w(one_mode_prj) %>% as.data.frame() %>% 
    tidyr::complete(node = 1:(technical_networks[[net]]$gal$n-technical_networks[[net]]$gal$bipartite),fill = list(node = 0)) %>%
    mutate(l2_type = {(technical_networks[[net]] %v% 'l2_type')[(technical_networks[[net]]$gal$bipartite+1):technical_networks[[net]]$gal$n]}) %>%
    group_by(l2_type) %>% summarise(avg_betweenness = mean(betweenness,na.rm=T)) %>% spread(l2_type,avg_betweenness) %>% 
    rename(Agency_betweenness = Agency,None_betweenness = None,Staff_betweenness = Staff)
  df = data.frame(#clustering_coef,
    centralization,centralization_staff,centralization_agency,centralization_other,avg_betweenness,Network = net)
  df})

tech_obs = do.call(rbind,tech_nets)

temp = collab_nc %>% select(Agency_betweenness,Staff_betweenness,None_betweenness,Network) %>% gather(actor,betweenness,-Network)
temp = left_join(temp,collab_obs %>%  select(Agency_betweenness,Staff_betweenness,None_betweenness,Network) %>% gather(actor,betweenness,-Network) %>% rename(obs_betweenness = betweenness))
temp = left_join(temp,temp %>% group_by(Network,actor) %>% summarise(q = sum(obs_betweenness>betweenness,na.rm=T)/1000))

gg1a = ggplot(temp) + 
  geom_boxplot(aes(y = betweenness,fill = actor, colour = actor,x = as.factor(Network)),alpha = 0.2) + theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 14),legend.text = element_text(size =12),legend.background = element_rect(fill = 'grey80'),axis.title=element_text(size = 15)) + 
  scale_colour_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +scale_fill_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +
  scale_x_discrete(name = 'NMS network (collaborative ties)') + scale_y_continuous(name = 'Avg. betweenness score by alter role type') + 
  geom_point(aes(y = obs_betweenness,x = as.factor(Network),colour = actor,fill=actor,shape = ifelse(q>0.975|q<0.025,TRUE,FALSE)),size=5,alpha = 0.8,position = position_dodge(width = .75)) + coord_flip() + 
  ggtitle('Collaboration graphs') + scale_shape_manual(values = c(0,15))

temp2 = tech_nc %>% select(Agency_betweenness,Staff_betweenness,None_betweenness,Network) %>% gather(actor,betweenness,-Network)
temp2 = left_join(temp2,tech_obs %>%  select(Agency_betweenness,Staff_betweenness,None_betweenness,Network) %>% gather(actor,betweenness,-Network) %>% rename(obs_betweenness = betweenness))
temp2 = left_join(temp2,temp2 %>% group_by(Network,actor) %>% summarise(q = sum(obs_betweenness>betweenness,na.rm=T)/1000))

gg2a = ggplot(temp2) + 
  geom_boxplot(aes(y = betweenness,fill = actor, colour = actor,x = as.factor(Network)),alpha = 0.2) + theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 14),legend.text = element_text(size =12),legend.background = element_rect(fill = 'grey80'),axis.title=element_text(size = 15)) + 
  scale_colour_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +scale_fill_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +
  scale_x_discrete(name = 'NMS network (technical ties)') + scale_y_continuous(name = 'Avg. betweenness score by alter role type') + 
  geom_point(aes(y = obs_betweenness,x = as.factor(Network),colour = actor,fill=actor,shape = ifelse(q>0.975|q<0.025,TRUE,FALSE)),size=5,alpha = 0.8,position = position_dodge(width = .75)) + coord_flip() + 
  ggtitle('Technical information sharing graphs') + scale_shape_manual(values = c(0,15)) + guides(shape=FALSE)
gg1 = grid.arrange(gg1,gg2,ncol=2)
library(ggplot2)
library(ggridges)
library(ggthemes)

library(gridExtra)


temp = full_join(collab_nc %>% select(centralization,Network) %>% mutate(tie = 'collab'),tech_nc %>% select(centralization,Network) %>% mutate(tie = 'tech'))
tobs = full_join(collab_obs %>% select(centralization,Network) %>% mutate(tie = 'collab') %>% rename(obs_centralization = centralization),
              tech_obs %>% select(centralization,Network) %>% mutate(tie = 'tech') %>% rename(obs_centralization = centralization))
tobs = left_join(tobs,tobs %>%group_by(Network,tie) %>% summarise(q = sum(obs_centralization>centralization,na.rm=T)/1000))
temp = left_join(temp,tobs)
temp = left_join(temp,network_names)
temp$Age = 2012 - temp$Created
temp$Network = as.factor(as.character(temp$Network))
temp$tie2 = temp$tie

gg2 = ggplot(data = temp) + geom_point(aes(y = obs_centralization,x = Age,fill = tie,colour = tie),pch=21,size = 3,alpha = 0.7) +
  geom_smooth(aes(y = centralization,x = Age,colour = tie),method = 'loess') + theme_bw() + 
  theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 14),legend.text = element_text(size =12),legend.background = element_rect(fill = alpha('grey90',0.8)),axis.title=element_text(size = 15)) + 
  scale_color_colorblind("Smoothed simulation results",labels = c('Collaboration ties','Technical ties'),
                     guide = guide_legend(override.aes = list(fill = NA,pch = NA))) +
scale_fill_colorblind("Observed centralization values",labels = c('Collaboration ties','Technical ties')) +
  scale_y_continuous(name = 'Centralization (alter-role level)') + scale_x_continuous(name = 'Years since creation')

gg2


ggplot(data = temp,aes(y = fct_reorder(temp$Network,temp$Created),x = centralization, fill = tie,colour = tie),alpha = 0.5) + geom_density_ridges2() + facet_wrap(~tie) +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + theme_bw() +


gg2 = ggplot() + 
  geom_boxplot(data = temp2,aes(y = betweenness,fill = actor, colour = actor,x = as.factor(Network)),alpha = 0.2) + theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 14),legend.text = element_text(size =12),legend.background = element_rect(fill = 'grey80'),axis.title=element_text(size = 15)) + 
  scale_colour_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +scale_fill_colorblind(name = 'Avg. betweenness score',labels = c('Agency','Other','ONMS staff')) +
  scale_x_discrete(name = 'NMS network (technical ties)') + scale_y_continuous(name = 'Avg. betweenness score by alter role type') + 
  geom_point(data = tech_obs %>%  select(Agency_betweenness,Staff_betweenness,None_betweenness,Network) %>% gather(actor,betweenness,-Network),
             aes(y = betweenness,x = as.factor(Network),colour = actor,fill=actor),shape=22,size=5,alpha = 0.8,position = position_dodge(width = .75)) + coord_flip() + 
  ggtitle('Technical information sharing graphs')



avg_staff_degree = sapply(seq_along(collaborative_networks),function(x) mean(degree(collaborative_networks[[x]],gmode = 'graph',nodes = which({collaborative_networks[[x]]  %v% 'l2_type' }== 'Staff'))))
plot(collab_obs$Staff_betweenness ~ avg_staff_degree)

