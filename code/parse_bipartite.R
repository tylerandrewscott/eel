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

all_dyads <- full_join(actor_details,alters_df)
all_dyads$Alter_Role_Name <- as.factor(roles_df$Role_Name[match(all_dyads$Alter_Role,roles_df$Role)])
all_dyads$Bip_Self_Role_Name = as.factor(paste0('Self_',all_dyads$Self_Role_Name))
all_dyads$Bip_Alter_Role_Name = as.factor(paste0('Alter_',all_dyads$Alter_Role_Name))
#all_dyads$ID = as.factor(all_dyads$ID)
all_dyads$Network = as.factor(all_dyads$Network)

collab_mats = lapply(seq_along(sort(unique(dyad_df$Network))),function(net) as.matrix(
  table(dyad_df$ID[dyad_df$Network==net&dyad_df$Response%in%c(1,3)],dyad_df$Alter_Role_Name[dyad_df$Network==net&dyad_df$Response%in%c(1,3)])))
tech_mats = lapply(seq_along(sort(unique(dyad_df$Network))),function(net) as.matrix(
  table(dyad_df$ID[dyad_df$Network==net&dyad_df$Response%in%c(2,3)],dyad_df$Alter_Role_Name[dyad_df$Network==net&dyad_df$Response%in%c(2,3)])))

collaborative_networks = lapply(collab_mats,function(n) as.network(n,directed=F,matrix.type = 'adjacency',bipartite=T))
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

library(ggnetwork)
library(GGally)
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

saveRDS(collaborative_networks,'scratch/collaborative_bip_network_list.RDS')
saveRDS(technical_networks,'scratch/technical_bip_network_list.RDS')


gcoords = lapply(1:length(collaborative_networks), function(x) ggnetwork(collaborative_networks[[x]]))
for (site in 1:length(gcoords)){gcoords[[site]]$Site = network_names$Site[match(site,network_names$Network)]}
gcdf = do.call(rbind,gcoords)

library(ggthemes)
gcdf$Staff_All = (gcdf$On_ONMS_Staff=='Is_Staff'|gcdf$ONMS_Staff==1) + 0

ggplot(gcdf,aes(x = x,xend=xend,y = y,yend=yend,colour = paste0(Level,as.factor(Staff_All)),shape = paste0(Level,as.factor(Staff_All)))) +  geom_edges(colour = 'grey50',alpha =0.5) + geom_nodes() + 
  facet_wrap(~Site) + theme_map() + scale_shape_manual(labels=c('Respondent not on staff','Respondent on staff','Alter not on staff','Alter on staff'),values = c(19,19,21,21)) +
  scale_colour_manual(labels=c('Respondent not on staff','Respondent on staff','Alter not on staff','Alter on staff'),values = c(1,3,1,3)) + theme(legend.title = element_blank(),legend.position = c(0.7,0.05)) 




fruch_layout <- data.frame(layout_with_fr(intergraph::asIgraph(all_observed_edges)))
rownames(fruch_layout) = rownames(circle_layout) = network.vertex.names(all_observed_edges)

edges_df <- all_dyads[all_dyads$Response>0&all_dyads$Network<=14,] 

edges_df$Site <- as.character(network_names$Site[match(edges_df$Network,network_names$Network)])

edges_df %>% filter(Network==1) %>% select(ID,Network,Role,Response) %>% filter(Response%in%2:3) %>% 
  filter(!duplicated(Role))
edges_df %>% filter(Network==1) %>% select(ID,Network,Role,Response) %>% filter(Response%in%c(1,3)) %>% 
  filter(!duplicated(Role))

num_response = id_roles %>% filter(!duplicated(paste0(ID,Role_ID))) %>%
  group_by(Network,Self_Role_Name) %>% summarise(num_response = n())
edges <- left_join(edges_df,num_response)
collaborative_edges <- edges %>% filter(Response == 1|Response == 3) %>% select(-ID,-Response,-Role_ID) %>%
  group_by(Network,Role,Self_Role_Name,Alter_Role,Alter_Role_Name,Site,num_response) %>% summarise(edge_value = n())
technical_edges <- edges %>% filter(Response == 2|Response == 3) %>% select(-ID,-Response,-Role_ID) %>%
  group_by(Network,Role,Self_Role_Name,Alter_Role,Alter_Role_Name,Site,num_response) %>% summarise(edge_value = n())

layout = 'circle'
if(layout == 'circle')
{collaborative_edges[,c('x','y')] <- circle_layout[match(collaborative_edges$Self_Role_Name,rownames(circle_layout)),]
collaborative_edges[,c('xend','yend')] <- circle_layout[match(collaborative_edges$Alter_Role_Name,rownames(circle_layout)),]
technical_edges[,c('x','y')] <- circle_layout[match(technical_edges$Self_Role_Name,rownames(circle_layout)),]
technical_edges[,c('xend','yend')] <- circle_layout[match(technical_edges$Alter_Role_Name,rownames(circle_layout)),]}
if(layout == 'fr')
{collaborative_edges[,c('x','y')] <- fruch_layout[match(collaborative_edges$Self_Role_Name,rownames(fruch_layout)),]
collaborative_edges[,c('xend','yend')] <- fruch_layout[match(collaborative_edges$Alter_Role_Name,rownames(fruch_layout)),]
technical_edges[,c('x','y')] <- fruch_layout[match(technical_edges$Self_Role_Name,rownames(fruch_layout)),]
technical_edges[,c('xend','yend')] <- fruch_layout[match(technical_edges$Alter_Role_Name,rownames(fruch_layout)),]}

summary_stats <- full_join(id_roles %>% group_by(Network) %>% filter(!duplicated(ID)) %>% summarise(respondents=n()),
id_roles %>% group_by(Network) %>% filter(!duplicated(Role_ID)) %>% summarise(roles=n()))
summary_stats <- left_join(summary_stats,network_names)
library(stargazer)
library(htmlTable)
htmlTable(summary_stats[,c('Site','respondents','roles')],)

gg_tech <- ggplot() + ggtitle('Information exchange') + 
  geom_edges(data = technical_edges,alpha = 0.1,curvature = 0.2,aes(x = x,y = y, yend = yend,xend=xend,size = log(edge_value+0.1) ),colour = 'grey40') + 
  #geom_edges(data = collaborative_edges,alpha = 0.1,curvature = 0.2,aes(x = x,y = y, yend = yend,xend=xend,size = log(edge_value+0.1) ),colour = 'red') + 
  geom_nodes(data = technical_edges,pch=21,aes(x = xend,y=yend,size = 0.25),fill = 'black') +
  geom_nodes(data = technical_edges,pch=21,aes(x = x,y=y,size = num_response/3),fill = 'white') +  
  #geom_nodetext(aes(x = x,y=y,label = Self_Role_Name)) +
  facet_wrap(~Site,ncol=3) + theme_map() + scale_colour_viridis(direction = -1,option = 'C') + 
  guides(size = FALSE)  + theme(strip.text = element_text(size = 14),title = element_text(size = 18))

gg_collab <- ggplot() + ggtitle('Stakeholder collaboration') + 
  geom_edges(data = collaborative_edges,alpha = 0.1,curvature = 0.2,aes(x = x,y = y, yend = yend,xend=xend,size = log(edge_value+0.1) ),colour = 'grey40') + 
  #geom_edges(data = collaborative_edges,alpha = 0.1,curvature = 0.2,aes(x = x,y = y, yend = yend,xend=xend,size = log(edge_value+0.1) ),colour = 'red') + 
  geom_nodes(data = collaborative_edges,pch=21,aes(x = xend,y=yend,size = 0.25),fill = 'black') +
  geom_nodes(data = collaborative_edges,pch=21,aes(x = x,y=y,size = num_response/3),fill = 'white') +  
  #geom_nodetext(aes(x = x,y=y,label = Self_Role_Name)) +
  facet_wrap(~Site,ncol=3) + theme_map() + scale_colour_viridis(direction = -1,option = 'C') + 
  guides(size = FALSE)  + theme(strip.text = element_text(size = 14),title = element_text(size = 18))

ggplot(technical_edges,aes(x = Self_Role_Name,y = Alter_Role_Name,fill = edge_value)) + 
  geom_tile() + facet_wrap(~Network,ncol=3) + scale_fill_viridis() + 
  scale_x_discrete(expand=c(0,0)) +  scale_y_discrete(expand=c(0,0)) + theme_map() + 
  theme(axis.text = element_text(size=8)) 

offset_mat_list <- lapply(1:dim(na_array)[3],function(i) na_array[,,i])
invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'ONMS_Staff' <<-ifelse(grepl('staff',network.vertex.names(technical_networks[[x]])),1,0)))
invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(collaborative_networks[[x]])),1,0)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'Agency' <<-ifelse(grepl('agency',network.vertex.names(technical_networks[[x]])),1,0)))

invisible(lapply(1:length(collaborative_networks),function(x) collaborative_networks[[x]] %v% 'No_Response' <<-ifelse(rowSums(offset_mat_list[[x]]) == 0,0,1)))
invisible(lapply(1:length(technical_networks),function(x) technical_networks[[x]] %v% 'No_Response' <<-ifelse(rowSums(offset_mat_list[[x]]) == 0,0,1)))


saveRDS(technical_networks,file = 'scratch/technical_bip_networks.rds')
saveRDS(collaborative_networks,file = 'scratch/collaborative_bip_networks.rds')
saveRDS(offset_mat_list,file = 'scratch/offset_matrix_list.rds')

library(intergraph)
technical_graphs <- lapply(technical_networks,asIgraph)
collaborative_graphs <- lapply(collaborative_networks,asIgraph)

technical_summary_stats = as.data.frame(do.call(cbind,list(
  'density' = sapply(technical_networks,network.density),
  'mean distance' = sapply(technical_networks,function(x) mean_distance(intergraph::asIgraph(x))),
  'reciprocity' = sapply(technical_graphs,function(x) igraph::reciprocity(x)),
  'clustering' = sapply(technical_graphs,function(x) igraph::transitivity(x)),
  'avg. degree' = sapply(technical_networks,function(x) mean(sna::degree(x,gmode = 'digraph' ,cmode = 'outdegree'))),
  'sd in-degree' = sapply(technical_networks,function(x) sd(sna::degree(x,gmode = 'digraph' ,cmode = 'indegree'))),
  'sd out-degree' = sapply(technical_networks,function(x) sd(sna::degree(x,gmode = 'digraph' ,cmode = 'outdegree'))))))
technical_summary_stats <- round(technical_summary_stats,2)
technical_summary_stats$Site = network_names$Site[1:14]

collaborative_summary_stats = as.data.frame(do.call(cbind,list(
  'density' = sapply(collaborative_networks,network.density),
  'mean distance' = sapply(collaborative_networks,function(x) mean_distance(intergraph::asIgraph(x))),
  'reciprocity' = sapply(collaborative_graphs,function(x) igraph::reciprocity(x)),
  'clustering' = sapply(collaborative_graphs,function(x) igraph::transitivity(x)),
  'avg. degree' = sapply(collaborative_networks,function(x) mean(sna::degree(x,gmode = 'digraph' ,cmode = 'outdegree'))),
  'sd in-degree' = sapply(collaborative_networks,function(x) sd(sna::degree(x,gmode = 'digraph' ,cmode = 'indegree'))),
  'sd out-degree' = sapply(collaborative_networks,function(x) sd(sna::degree(x,gmode = 'digraph' ,cmode = 'outdegree'))))))
collaborative_summary_stats <- round(collaborative_summary_stats,2)
collaborative_summary_stats$Site = network_names$Site[1:14]

all_summary_stats = data.frame(do.call(cbind,sapply(1:ncol(collaborative_summary_stats),function(i){
  if (colnames(collaborative_summary_stats)[i]!='Site')
  {
    paste(collaborative_summary_stats[,i],
          technical_summary_stats[,colnames(technical_summary_stats) == colnames(collaborative_summary_stats)[i]],sep=' | ')
  }})))
all_summary_stats$Site = collaborative_summary_stats$Site
colnames(all_summary_stats) <- colnames(collaborative_summary_stats)

htmlTable(all_summary_stats)



