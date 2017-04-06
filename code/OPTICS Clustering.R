library(dbscan)
library(ggplot2)
library(ggmap)
library(data.table)

funPath='D:/Git/Rprojects/ECA/function.R'
funPath2='D:/Git/Rprojects/ShipEmissions/code/ShipTrajectoryAnalysisFunctions.R'
source(funPath)
source(funPath2)

plotsp<-function(x,zoomSize=8){
  dev.new()
  zoomSize=8
  temp=dt
  centerX=0.5*(max(temp$lon)+min(temp$lon))
  centerY=0.5*(max(temp$lat)+min(temp$lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google',maptype = 'satellite'))
  p=p+geom_point(data=temp,aes(x=lon,y=lat,col=as.factor(stayid)),size=1,alpha=0.75)
  p=p+geom_text(data=temp[,.SD[1],stayid],nudge_x = 0.05,nudge_y = 0.05,aes(x=lon,y=lat,label=stayid),color='black',size=2)
  p=p+labs(x="Longtitude",y="Latitude")+theme(legend.position='none')
  p
}


dt0=fread('D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/candidates_berth.csv');
dt=dt0[lon<125&lon>119&lat>29&lat<32.5]
plotsp(dt,8)
### run OPTICS
x=as.matrix(dt[,list(lon,lat)])
res <- optics(x, eps = 0.01,  minPts = 10)
res

### get order
res$order


### plot produces a reachability plot
plot(res)


### identify clusters by cutting the reachability plot (black is noise)
res <- optics_cut(res, eps_cl = 0.5)
res
dt=data.table(dt,c=res$cluster)[c>0,list(lon,lat,stayid=c)];head(dt);dim(dt[,.N,stayid])
plotsp(dt,8)
plot(res)
plot(x, col = res$cluster+1L)

### re-cutting at a higher eps threshold
res <- optics_cut(res, eps_cl = .65)
res
plot(res)
plot(x, col = res$cluster+1L)

### identify clusters of varying density hierarchically using the Xi method
res <- opticsXi(res, xi = 0.7)
res

plot(res)
plot(x, col = res$cluster+1L)
# better visualization of the nested structure using convex hulls
hullplot(x, res)

# Xi cluster structure
res$clusters_xi

### use OPTICS on a precomputed distance matrix
d <- dist(x)
res <- optics(x, eps = 1, minPts = 10)
plot(res)