
#setwd('~/Documents/Github/eel')
#system('ln -s ~/Box/eel/input/ ~/Documents/GitHub/eel')
#system('ln -s ~/Box/eel/output/ ~/Documents/GitHub/eel')

pgks = c('tidyverse','statnet','tidyselect','ggnetwork','ggthemes','fergm','data.table','magrittr')
lapply(pgks[!pgks %in% installed.packages()[,'Package']],install.packages)
lapply(pgks,require,character.only = TRUE)

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

testn = lapply(1:14,function(n) {
print(n)
temp_df = tdf %>% filter(Network==n)
temp_df$Alter_ID = id_roles$ID[id_roles$Network==n][match(temp_df$Alter_Role_Name,id_roles$Self_Role_Name[id_roles$Network==n])]
#temp_df = temp_df[!is.na(temp_df$Alter_ID),]
mnet = multinet::ml.empty()
add.layers.ml(mnet,as.character(unique(temp_df$Level)),directed = rep(TRUE,length(unique(temp_df$Level))))
add.actors.ml(mnet,as.character(unique(temp_df$ID)))
add.actors.ml(mnet,as.character(unique(temp_df$Alter_ID)))
n1 = data.frame(grep('^W',temp_df$ID,value=T),rep('W',sum(grepl('^W',temp_df$ID))),stringsAsFactors = F);n1 = n1[!duplicated(n1),]
if(nrow(n1)>0){add.vertices.ml(mnet,n1)}
n2 = data.frame(grep('^A',temp_df$ID,value=T),rep('A',sum(grepl('^A',temp_df$ID))),stringsAsFactors = F);n2 = n2[!duplicated(n2),]
if(nrow(n2)>0){add.vertices.ml(mnet,n2)}
n3 = data.frame(grep('^S',temp_df$ID,value=T),rep('S',sum(grepl('^S',temp_df$ID))),stringsAsFactors = F);n3 = n3[!duplicated(n3),]
if(nrow(n3)>0){add.vertices.ml(mnet,n3)}
edges = data.frame(ID = temp_df$ID,Level = temp_df$Level,Alter_ID = temp_df$Alter_ID,Alter_Level = str_extract(temp_df$Alter_ID,'^[A-Z]{1}'),stringsAsFactors = F)
isols = edges[is.na(edges$Alter_Level),]
edges = edges[!is.na(edges$Alter_Level),]
print(nrow(isols)/(nrow(edges)+nrow(isols)))
#isols <- isols[!isols$temp_df.ID %in% edges$temp_df.ID,]
add.edges.ml(mnet,edges)
mnet})

n = 1
temp_net = tdf[Network==n]
temp_net$Alter_ID = id_roles$ID[id_roles$Network==n][match(temp_net$Alter_Role_Name,id_roles$Self_Role_Name[id_roles$Network==n])]


net = network::network.initialize(n = length(unique(temp_net$ID)),directed = T,bipartite = F)
net %v% 'vertex.names' <- sort(unique(temp_net$ID))
cnet = net
inet = net
network::add.edges(x = cnet,tail =match(temp_net[!is.na(Alter_ID)&Response%in%c(1,3),]$ID,network.vertex.names(net)),head = match(temp_net[!is.na(Alter_ID)&Response%in%c(1,3),]$Alter_ID,network.vertex.names(net)))
network::add.edges(x = inet,tail =match(temp_net[!is.na(Alter_ID)&Response%in%c(2,3),]$ID,network.vertex.names(net)),head = match(temp_net[!is.na(Alter_ID)&Response%in%c(2,3),]$Alter_ID,network.vertex.names(net)))

#devtools::install_github("tedhchen/multilayer.ergm")
library(multilayer.ergm)
mnet <- to.multiplex(inet,cnet, output = "network",directed = T)
mod.within <- ergm(mnet ~ edges_layer(layer = 1) + 
                     #gwesp_layer(0,fixed = T,layer = 1) +
                     edges_layer(layer = 2) + 
                     #gwesp_layer(0,fixed = T,layer = 2) + 
                     mutual("layer.mem", diff = T) +
                     duplexdyad(c("e", "f"), layers = list(1, 2)),
                    eval.loglik = F,
                   control = control.ergm(parallel=4,MCMC.samplesize = 1e3))



test = fergm(mnet,form = "edges + gwesp(0,fixed = T) + gwdegree(0,fixed = T)",seed = 24,warmup = 1e3,chains = 4,iter = 1e4,cores = 4)




summary(mod.within)


summary(mod.within)
                   mutual("layer.mem", diff = T),
                   control = control.ergm(seed = 24),
                   constraints = ~fixallbut(free))

summary(mod.within)

mnet %v% 'layer.mem'

test
summary(test)
mnet
add.edge.network(net,tail = temp_net$ID[!is.na(temp_net$Alter_ID)],head = temp_net$Alter_ID[!is.na(temp_net$Alter_ID)])




temp_net[!duplicated(ID)]

table(df2$`R02 ASW`)
temp_net$ID

unique(tdf$ID[tdf$Network==1])



df2$`Record ID`[df2$`R02 2 ASOW 1`==1]
unique(temp_net$ID)
names(df2)
df2$`R02 ASW`


df2$`R02 1 ASOW`
test = df2[df2$`Record ID`=="A035",grepl('^R02',names(df2)),with = F]
test

intersect(length(unique(temp_net$ID)),unique(test$`Record ID`))


unique()


test[`Record ID`=='A112',grepl('R2[5-7]',colnames(test)), with]
test$`Record ID`
unique(temp_net$ID)

dim(test)

temp_net[is.na(Alter_ID)]

plot(testn[[1]])

fergm(net = )


plot(testn[[8]])
testn[[8]]
igraph::triangles(testn[[8]])

library(plotly)
n = 7
vs <- multinet::nodes.ml(testn[[n]])
es <- multinet::edges.ml(testn[[n]])
L <- layout.multiforce.ml(testn[[n]])
Xn <- L$x
Yn <- L$y
Zn <- L$z
Ln <- L$layer
network <- plot_ly(x = ~Xn, y = ~Yn, z = ~Zn, color = ~Ln,type = "scatter3d",mode = 'markers')

edge_shapes <- list()
for(i in 1:nrow(es)) {
  v0 <- es$from_actor[i]
  v1 <- es$to_actor[i]
  edge_shape = list(
    type = "line",
    line = list(width = 0.3),
    x = list(Xn[v0],Xn[v1]),
    y = list(Yn[v0],Yn[v1]),
    z = list(Zn[v0],Zn[v1]))
  edge_shapes[[i]] <- edge_shape
}
  p <- layout(
    network,
    shapes = edge_shapes
  )
  
network %>%
plotly::add_lines(edge_shapes)

edge_shapes[[2]]
library(rgl)



network

add_segments(x = edge_shapes[[1]]$x0,
            xend = edge_shapes[[1]]$x1,
          y = edge_shapes[[1]]$y0,
                  yend = edge_shapes[[1]]$y1,
          z = edge_shapes[[1]]$z0,
          zend = edge_shapes[[1]]$z1,mode="lines") %>%
  layout(showlegend=F)
network
?add_segments

network
y= c(min(mtcars$disp), max(mtcars$disp)), mode = "lines")
c(min(mtcars$disp), max(mtcars$disp))
c(edge_shapes[[1]]$y0,edge_shapes[[1]]$y1)
  

# Make the graph
plot_ly(mtcars, x = ~mpg, y = ~disp, type="scatter", mode = "markers" , marker=list( color=ifelse(mtcars$mpg>20,"red","blue") , opacity=0.5 , size=30) ) %>%
  
  #Add the segment with add_trace
  add_trace(x = c(20, 20), y= c(min(mtcars$disp), max(mtcars$disp)), mode = "lines") %>%
  
  #Layout
  layout(showlegend=F)


p <- layout(
  network,
  title = 'Karate Network',
  shapes = edge_shapes
)
p

)



Xe <- L$x[match(es$to_actor,L$actor)]
Ye <- L$y[match(es$to_actor,L$actor)]
Ze <- L$z[match(es$to_actor,L$actor)]



Nv <- length(vs)
Ne <- nrow(es)

Ln <- L$layer

Xn=[layt[k][0] for k in range(N)]# x-coordinates of nodes
Yn=[layt[k][1] for k in range(N)]# y-coordinates
Zn=[layt[k][2] for k in range(N)]# z-coordinates
Xe=[]
Ye=[]
Ze=[]
for e in Edges:
  Xe+=[layt[e[0]][0],layt[e[1]][0], None]# x-coordinates of edge ends
Ye+=[layt[e[0]][1],layt[e[1]][1], None]  
Ze+=[layt[e[0]][2],layt[e[1]][2], None]  



nkk1 <- plot_ly(x = ~Xn, y = Yn,z = ~Zn,color = ~Ln,mode = "lines",type = 'scatter3d' )


nkk2 <- plot_ly(x = ~Xn, y = Yn,z = ~Zn,color = ~Ln,mode = "lines",type = 'scatter3d' )


trace1=go.Scatter3d(x=Xe,
                    y=Ye,
                    z=Ze,
                    mode='lines',
                    line=dict(color='rgb(125,125,125)', width=1),
                    hoverinfo='none'
)

trace2=go.Scatter3d(x=Xn,
                    y=Yn,
                    z=Zn,
                    mode='markers',
                    name='actors',
                    marker=dict(symbol='circle',
                                size=6,
                                color=group,
                                colorscale='Viridis',
                                line=dict(color='rgb(50,50,50)', width=0.5)
                    ),
                    text=labels,
                    hoverinfo='text'
)


edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es$from_actor[i]
  v1 <- es$to_actor[i]
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    z0 = Zn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1],
    z1 = Zn[v1]
  )
  edge_shapes[[i]] <- edge_shape
}


ggplot() + 
  geom_point(data = lay %>% filter(layer=='S'),aes(x = x,y=y)) +
  geom_point(data = lay %>% filter(layer=='W'),aes(x = x,y=y)) +
  geom_point(data = lay %>% filter(layer=='A'),aes(x = x,y=y)) 

library(plotly)


str(lay)
p <- plot_ly(lay %>% filter(layer!='flattening'), x = ~x, y = ~y, z = ~z, color = ~layer) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')))

p


library(plotly)
library(igraph)

data(karate, package="igraphdata")
G <- upgrade_graph()
L <- layout.circle(G)



nkk <- plot_ly(x = ~L$x, y = ~L$y,z = ~L$z,color = ~L$layer,mode = "markers")
nkk



p <- layout(
  nkk,
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
)

edge_shapes


v0 <- match(es$from_actor,vs$actor)
v1 <- match(es$to_actor,vs$actor)
edge_df = data.frame(
x0 = Xn[v0],
y0 = Yn[v0],
z0 = Zn[v0],
x1 = Xn[v1],
y1 = Yn[v1],



stringsAsFactors = F)


edge_shape = list(
  type = "line",
  line = list(color = "#030303", width = 0.3),
  x0 = Xn[v0],
  y0 = Yn[v0],
  x1 = Xn[v1],
  y1 = Yn[v1]
  




edge_shapes[[2]]


axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

p <- layout(
  nkk,
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
)

p








flatten.ml(testn[[7]])
plot(testn[[7]])
summary(testn[[7]])

tlist = as.list(testn[[7]])
library(GGally)
ggnet2(tlist$flattening,)
?graph.isomorphic
# Load igraph
library(igraph)

library(networkD3)

# Use igraph to make the graph and find membership
karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
members <- membership(wc)

# Convert to object suitable for networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)
karate_d3
# Create force directed network plot
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')



# Use igraph to make the graph and find membership
karate <- as.igraph(testn[[7]])
members <- str_extract(names(V(karate)),'^[A-Z]{1}')

# Convert to object suitable for networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)
karate_d3$links$source = as.numeric(str_extract(karate_d3$links$source,'[0-9]{1,}'))


simpleNetwork(Data = karate_d3$links)
# Create force directed network plot
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes,
             Target = 'target',NodeID= 'name', Group = 'group')
            
 
karate_d3$links
karate_d3$nodes
Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')

networkD3::forceNetwork(in)

plot
plot(testn[[7]],fill = c('blue','brown','grey50'))

is_multilevel(as.igraph(testn[[2]]))
n2 = as.igraph(testn[[2]])
mutli

citation('multinet')
install.packages('multiplex')
library(multiplex)
library(multinets)
library(multinets)
class(testn[[2]])
plot(testn[[2]])




multinet::add
isols
sort(unique(edges$temp_df.ID))

tdf[tdf$ID=='A402',]
n2


unique(temp_df$ID)
data.frame(grep('^A',temp_df$ID,value=T),'A',stringsAsFactors = F)



nodes.ml(test)
sort(actors.ml(test))
as.character(unique(temp_df$ID))
as.character(unique(temp_df$Alter_ID))


tdf[tdf$ID=='A042',]

unique(edges$temp_df.ID)
unique(edges$temp_df.Alter_ID)


head(edges)
edges

flatten.ml(test, new.layer="flattening",
                method="weighted", force.directed=TRUE, all.actors=FALSE)

ig = as.igraph(test,'flattening',merge.actors = FALSE)
tn = intergraph::asNetwork(ig)

summary(test)
igraph::transitivity(ig)
summary(tn ~ transitive + twopath + triangle)
sna::gtrans(tn)


8765/17689

multinet::flatten.ml(test)
summary(test)

multinet::layers.ml(test)




?multinet::summary.Rcpp_RMLNetwork

multinet::layout.circular.ml(test)


tdf[tdf$Level=='W',]

vertices
actors.ml(test)
summary(test)

tdf[tdf$Level=='W',]

summary(test)
edges.ml(test)


unique(tdf$Alter_ID)
unique(tdf$ID)



vertices_df = data.frame(ID = tdf$ID[!duplicated(tdf$ID)],Level = tdf$Level[!duplicated(tdf$ID)])
add.vertices.ml(test,vertices_df)
add.nodes.ml(test,vertices_df)

edf = tdf %>% select(ID,Level,Alter_ID) %>% mutate(Alter_Level = str_extract(Alter_ID,'^[A-Z]{1}'))
add.edges.ml(test,edf)

nodes.ml(test) %>% arrange(actor)

vertices <- data.frame(
  c("A1","A2","A3","A1","A2","A3"),
  c("l1","l1","l1","l2","l2","l2"))
vertices_df

multinet::layer.summary.ml(test,'A')

collab_mats = lapply(seq_along(sort(unique(dyad_expansion_df$Network))),function(net) as.matrix(
  table(dyad_expansion_df$Self_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(1,3)],dyad_expansion_df$Alter_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(1,3)])))
tech_mats = lapply(seq_along(sort(unique(dyad_expansion_df$Network))),function(net) as.matrix(
  table(dyad_expansion_df$Self_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(2,3)],dyad_expansion_df$Alter_Role_Name[dyad_expansion_df$Network==net&dyad_expansion_df$Response%in%c(2,3)])))
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

igraph::transitivity(intergraph::asIgraph(technical_networks[[1]]))
sna::gtrans(technical_networks[[1]],mode = 'digraph',measure = 'weak')
#weak, directed transitivity statistic = transitive/twopath






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



