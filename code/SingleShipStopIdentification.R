#long trajectory process
#memsi=219134000 or 215896000 both
library('data.table')
library('ggplot2')
library('ggmap')
library('dplyr')
library('dbscan')
source('D:/Git/Rprojects/ShipEmissions/code/Function.R')
source('D:/Git/Rprojects/ShipEmissions/code/FunctionForRealShip.R')
funPath='D:/Git/Rprojects/ECA/function.R'
funPath2='D:/Git/Rprojects/ShipEmissions/code/ShipTrajectoryAnalysisFunctions.R'
source(funPath)
source(funPath2)
filePath='D:/Git/data/container/215896000.txt'

p0=fread(filePath);
p0=setNames(p0,c('mmsi','time','sog','lon','lat','status'));
setkey(p0,mmsi,time)
p0=data.table(p0,pid=seq(1,nrow(p0)))
head(p0);dim(p0);

timeCluster<-function(dt,eps=3600*2,minp=5){
  #dt=tt
  #dt=dt[,id:=0]
  temp0=dt[status==5&sog==0]#point speed==0
  if(nrow(temp0)>minp){
    setkey(temp0,mmsi,time)
    dm <- as.matrix(temp0[,list(time)])#get time series
    cl <- dbscan(dm, eps = eps, minPts =minp)
    temp1=data.table(temp0,id=cl$cluster);
    temp2=temp1[id>0,list(startpid=.SD[1]$pid,endpid=.SD[nrow(.SD)]$pid,.N),list(mmsi,id)]#only get the id >0
    n=nrow(temp2)
    dt=dt[,stayid:=0]
    if(n>0){
      for( i in seq(1,n)){
        r=temp2[i]
        dt=dt[(pid>=r$startpid)&(pid<=r$endpid),stayid:=r$id]
      }
      return(dt) 
    }
  }
}

tt0=p0[5000:6500]
tt=p0[5000:6500][status==5&sog==0]
plot((tt0$time-min(tt0$time))/3600,tt0$sog/10)
plot(tt0$sog/10)
plot(tt0$lon,tt0$lat)
plot(tt0[sog<10]$sog)
plot(tt$sog,(tt$time-min(tt$time))/3600)
staydt=timeCluster(p0,eps=3600*2,minp=5);head(staydt);nrow(staydt);staydt[,.N,stayid]
plot((staydt[stayid==1]$time-min(staydt[stayid==1]$time)),staydt[stayid==1]$sog)
plot(staydt[stayid==1]$lon,staydt[stayid==1]$lat)



spaceCluster<-function(staydt,eps=0.00005,minp=5){
  setkey(staydt,mmsi,time)
  n=nrow(staydt)
  if(n>=2){
    dm <- as.matrix(staydt[,list(lon,lat)])#get time series
    cl <- dbscan(dm, eps = eps, minPts =minp)
    temp=data.table(staydt,stayid2=cl$cluster); 
    return(temp)
  }
}

stopdt=spaceCluster(staydt[stayid==1],eps=0.00005,minp=3);stopdt[,.N,stopid]

plot(stopdt[stopid>0]$lon,stopdt[stopid>0]$lat)
###-------------only for track analysis in arcgis---------------
library('xts')
staydt=staydt[,datetime:=format(as.POSIXct(time,origin='1970-01-01'),"%Y%m%d%H%M%OS")];head(staydt)

library(rgdal)
library('sp')
coordinates(staydt)<-c('lon','lat') # whatever the equivalent is in your 
proj4string(staydt) = CRS("+proj=longlat +datum=WGS84") 
writeOGR(staydt, 'D:/Git/Rprojects/ShipEmissions/data/ship215896000/ship215896000.shp', "layer name", driver = "ESRI Shapefile")


write.csv(staydt,'D:/Git/Rprojects/ShipEmissions/data/ship215896000/ship215896000.csv')
data2shp(staydt,'D:/Git/Rprojects/ShipEmissions/data/ship215896000/ship215896000.shp')
