#construct route between two ports
library(data.table)
library(dplyr)
library(pracma)
shipfile='D:/Git/Rprojects/basedata/ships.csv'
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi)

r2=fread('D:/data/route/top10route.csv')#
dim(r2);head(r2)

library(ggplot2)
library(ggmap)
library(grid)
library(gridExtra)
for(i in seq(1,10)){
  r=r2[rid==i]
  temp=sample_n(r,30000)
  #dev.new();
  ggplotMap(temp,5)
  #p1=ggplot()+geom_point(data=atrip,aes(x=lon1,y=lat1))+geom_point(data = atrip[sog1<10],aes(x=lon1,y=lat1,col='red'))+ggtitle(tripids[i])
  #p2=ggplot()+geom_point(data = atrip,aes(x=time1,y=sog1*10,col='red'))+geom_point(data = atrip,aes(x=time1,y=cog1,col='blue'))+ggtitle('sog+cog');
  #grid.arrange(p1, p2, ncol =2)
  
}

ggplotMap<-function(temp,zoomSize){
  centerX=0.5*(max(temp$lon)+min(temp$lon))
  centerY=0.5*(max(temp$lat)+min(temp$lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google',maptype = 'satellite'))
  p=p+geom_point(data=temp[1],aes(x=olon,y=olat,col='red'),size=4,alpha=0.5)
  p=p+geom_point(data=temp[1],aes(x=dlon,y=dlat,col='blue'),size=4,alpha=0.5)
  p=p+geom_point(data=temp,aes(x=lon,y=lat),size=1,alpha=0.5)
  #p=p+geom_text(data=temp[1],nudge_x = 0.0002,nudge_y = 0.0002,aes(x=lon,y=lat,label=bsid),color='black',size=3)
  #p=p+labs(x="Longtitude",y="Latitude")+theme(legend.position=c(0.9,0.9))
  p=p+labs(x="Longtitude",y="Latitude")+theme(legend.position='none')+ggtitle(temp$rid)
  ggsave(paste('D:/data/route/',temp[1]$rid,'.png',sep=''),width = 6,height = 6)
}

#----计算多个轨迹之间的hausdorff 距离--------
trips=r2[rid==3]
tripdist=data.table(id1=0,id2=0,hdist=0)[hdist<0]
#hausdorff_dist
tripids=unique(trips$tripid)
n=length(tripids);n
for(i in seq(1,1)){
  print(i)
  trip1=trips[tripid==tripids[i]]
  
  for(j in seq(i+1,n)){
    print(j)
    trip2=trips[tripid==tripids[j]]
    #plot(temp[tripid%in%tripids[3:4]]$lon,temp[tripid%in%tripids[3:4]]$lat)
    #dim(trip1);dim(trip2)
    t= data.table(id1=tripids[i],id2=tripids[j],hdist=hausdorff_dist(as.matrix(trip1[,list(lon,lat)]),as.matrix(trip2[,list(lon,lat)])))
    #setnames(temp,'hdist')
    tripdist=rbind(tripdist,t)
    
  }
}

plot(tripdist$hdist)

##-----

setkey(trips,mmsi,time)

#plot(r2$lon,r2$lat)
#calculate each trip line segment.
tripln=setTripLine(trips)
#tripln=merge(tripln0,berthStats[,list(bsid,lon.f=lon,lat.f=lat)],by.x='bsid.f',by.y='bsid')
#tripln=merge(tripln,berthStats[,list(bsid,lon.t=lon,lat.t=lat)],by.x='bsid.t',by.y='bsid')


setTripLine<-function(trippoints){
  setkey(trippoints,mmsi,time)
  n=nrow(trippoints)
  l1=trippoints[1:(n-1),list(mmsi,bsid1,bsid2,olon,olat,dlon,dlat,time1=time,status1=status,sog1=sog,cog1=cog,lon1=lon,lat1=lat,pid1=pid,tripid1=tripid)]
  l2=trippoints[2:(n),list(time2=time,status2=status,sog2=sog,cog2=cog,lon2=lon,lat2=lat,pid2=pid,tripid2=tripid)]
  ln=cbind(l1,l2)[tripid1==tripid2]
  ln=ln[,dist:=round(distance(lon1,lat1,lon2,lat2))][,dur:=abs(time2-time1)][,diffcog:=abs(cog2-cog1)][,meansog:=round((sog1+sog2)/2,1)]
  ln=ln[,avgsog:=round((dist/1852)/(dur/3600),1)]
  return(ln)
}


#判断每个line segment 是否为大范围缺失，这里只用距离
tripln=tripln[,ismiss:=0][dist>5*1000,ismiss:=1]
#calculate the statistics of each trip as a whole 
tripStats=tripln[,list(N=(.N+1),dur=sum(.SD$dur),oddist=round(distance(olon,olat,dlon,dlat)),
                       dist=sum(.SD$dist),Nmiss=sum(.SD$ismiss),missdist=sum(.SD[ismiss>0]$dist)),
                 list(mmsi,tripid=tripid1,bsid1,bsid2,olon,olat,dlon,dlat)][,distRate:=round(dist/oddist,2)]
#选择合理的trip，并计算每个点点到起始点途经的距离，然后每个trip采集200个点
#-----selected trips for testing, calculate the odist of each point-----
distLimit=quantile(tripStats$dist,1)[[1]];distLimit
durLimit=quantile(tripStats$dur,1)[[1]];durLimit
missLimit=quantile(tripStats$Nmiss,1)[[1]];missLimit
tripStats1=tripStats[Nmiss<missLimit&dur<durLimit&dist<distLimit];nrow(tripStats1)
trips1=trips[tripid%in%tripStats1$tripid];dev.new();plot(trips1$lon,trips1$lat)

setkey(trips1,tripid,mmsi)
tripids=tripStats1$tripid
n=length(tripStats1$tripid);n
ps0=data.table(r2[1])[,odist:=0][,ratio:=0][,group:=0][mmsi<0]
for(i in seq(1,(n))){#for each trip
  print(i)
  id=tripids[i]
  ps=trips1[tripid==id]
  m=nrow(ps)
  for(j in seq(1,m)){
    if(j==m){
      ps[j,odist:=tripStats1[tripid==id]$dist]
    }else{
      ls=tripln[tripid1==id]#lines
      ps[j,odist:=sum(ls[1:j]$dist)]
    }
  }
  ps=ps[,ratio:=(odist/ps[nrow(ps)]$odist)]#比例
  ps=ps[,group:=floor(ratio*200)]#2000个点
  ps0=rbind(ps0,ps)
}

mmsis=ps0[,.N,mmsi]$mmsi;length(mmsis)
ships[mmsi%in%mmsis]

route=ps0[,list(.N,
                lon=median(.SD[,list(lon=median(lon)),tripid]$lon),
                lat=median(.SD[,list(lat=median(lat)),tripid]$lat),
                sog=round(median(.SD[,list(sog=median(sog)),tripid]$sog)),
                cog=round(median(.SD[,list(cog=median(cog)),tripid]$cog))),(group)]
triprt=ps0[,list(.N,
                 lon=median(.SD$lon),
                 lat=median(.SD$lat),
                 sog=round(median(.SD$sog)),
                 cog=round(median(.SD$cog))),list(tripid,group)]
tr1=triprt[tripid==148331]
gs=tr1$group
temp0=triprt[tripid<0][,d1:=0][,c1:=0];temp0
for(i in seq(1,length(gs))){
  print(i)
  grp=gs[i]
  temp=triprt[group==grp&tripid!=148331][,d1:=distance(lon,lat,tr1[group==grp]$lon,tr1[group==grp]$lat)]
  temp=temp[,c1:=abs(cog-tr1[group==grp]$cog)]
  temp0=rbind(temp0,temp)
}
ag=temp0
dev.new();plot(ag$group,ag$d1)
dev.new();plot(ag$group,(ag$c1))
dev.new();plot(tr1$lon,tr1$lat)
tripdiff=ag[,list(distdiff=median(d1),diffcog=median(c1)),tripid]
dev.new();plot(tripdiff$distdiff)
dev.new();plot(tripdiff$diffcog)
dev.new();plot(tripdiff$distdiff/1000,tripdiff$diffcog)
#---将多个ggplot图片画在一个page上-----
library(ggplot2)
library(grid)
library(gridExtra)
for(i in seq(11,70)){
  atrip=tripln[tripid1==tripids[i]]
  dev.new();
  p1=ggplot()+geom_point(data=atrip,aes(x=lon1,y=lat1))+geom_point(data = atrip[sog1<10],aes(x=lon1,y=lat1,col='red'))+ggtitle(tripids[i])
  p2=ggplot()+geom_point(data = atrip,aes(x=time1,y=sog1*10,col='red'))+geom_point(data = atrip,aes(x=time1,y=cog1,col='blue'))+ggtitle('sog+cog');
  grid.arrange(p1, p2, ncol =2)
  
}

#-----------

p1=ggplot()+geom_point(data=ps0[,list(lon=median(lon),lat=median(lat)),list(tripid,group)],
                       aes(x=lon,y=lat,group=group,col=as.factor(tripid)))
p1
agroup=(ps0[group==100])
plot(agroup[,list(mediancog=median(cog)),tripid])
tmp=r2[tripid%in%tripids]
p <- ggplot() +
  # blue plot
  geom_point(data=route, aes(x=lon, y=lat)) + 
  geom_smooth(data=tmp, aes(x=lon, y=lat), fill="blue",
              colour="darkblue", size=1)

ggplot(ps0,aes(x=lon,y=lat,group=group))+geom_boxplot()+facet_wrap(~tripid)


jtrips=data.table(left_join(tripln,tripStats[,list(tripid1=tripid,N,dist,oddist,distRate,dur,Nmiss,missdist)],'tripid1'))

#medianTripDist=median(trips$dist)
#medianTrip=trips[which.min(abs(medianTripDist-dist))]
mtrip=jtrips[tripid1==medianTrip$tripid]
plot(mtrip$lon,mtrip$lat)
startPort=berthCities[bsid==mtrip$bsid1.f]



plot(temp$lon,temp$lat)
plot(trips$dur,trips$dist)
temp1=sample_n(temp,10000)
kk=kNNdist(scale(temp1[,list(lon,lat,sog,cog)]),10)
x=setorder(data.table(d=array(kk)),d)
plot(x[98000:nrow(x)][d<30]$d,type='l')


#----计算多个轨迹之间的hausdorff 距离--------
tripdist=data.table(id1=0,id2=0,hdist=0)[hdist<0]

#hausdorff_dist
tripids=trips$tripid
n=nrow(trips);n
for(i in seq(1,n)){
  print(i)
  trip1=temp[tripid==tripids[i]]
  
  for(j in seq(i+1,n)){
    trip2=temp[tripid==tripids[j]]
    #plot(temp[tripid%in%tripids[3:4]]$lon,temp[tripid%in%tripids[3:4]]$lat)
    #dim(trip1);dim(trip2)
    t= data.table(id1=tripids[i],id2=tripids[j],hdist=hausdorff_dist(as.matrix(trip1[,list(lon,lat)]),as.matrix(trip2[,list(lon,lat)])))
    #setnames(temp,'hdist')
    tripdist=rbind(tripdist,t)
    
  }
}

#-----计算每个trip的途经距离-----
#tra_dist
tradist=data.table(id1=0,id2=0,sumdist=0,meandist=0)[id1<0]
tripids=unique(triprt$tripid)
n=length(tripids);n

trip1=triprt[tripid==tripids[1]]

for(j in seq(2,n)){
  print(j)
  trip2=triprt[tripid==tripids[j]]
  if(nrow(trip1)==nrow(trip2)){
    
    #plot(temp[tripid%in%tripids[3:4]]$lon,temp[tripid%in%tripids[3:4]]$lat)
    #dim(trip1);dim(trip2)
    ln=cbind(trip1[,list(lon1=lon,lat1=lat)],trip2[,list(lon2=lon,lat2=lat)])
    ln=ln[,dist:=distance(lon1,lat1,lon2,lat2)]
    t1=data.table(id1=trip1[1]$tripid,id2=trip2[1]$tripid,sumdist=sum(ln$dist),meandist=mean(ln$dist))
    #setnames(temp,'hdist')
    tradist=rbind(tradist,t1)
    
  }
  
}


# trip1=temp[tripid==tripids[3]]
# trip2=temp[tripid==tripids[4]]
# plot(temp[tripid%in%tripids[3:4]]$lon,temp[tripid%in%tripids[3:4]]$lat)
# dim(trip1);dim(trip2)
# hausdorff_dist(as.matrix(trip1[,list(lon,lat)]),as.matrix(trip2[,list(lon,lat)]))


kk=kNNdist(tripdist[,list(hdist)],10)
x=setorder(data.table(d=array(kk)),d)
plot(x$d,type='l')
cl=dbscan(tripdist[2:71,list(hdist)],eps=0.001,minPts = 2)
tripdist1=cbind(tripdist[2:71],c=cl$cluster)
cl1=temp[tripid%in%tripdist1[c==1]$id1]
cl2=temp[tripid%in%tripdist1[c==2]$id1]
plot(cl2$lon,cl2$lat)

unique(tripdist1[c==2]$id1)

#-----map output------
data2shp(temp1,"D:/data/route/routes.shp")
coordinates(temp1) <- c("lon", "lat")
proj4string(temp1) <-  CRS("+proj=longlat +datum=WGS84") 
stops_ll <- spTransform(temp1, CRS("+proj=longlat +datum=WGS84"))
#writeOGR(stops_ll, "D:/data/global/gridBased/berths.kml", layer="berth", driver="KML")
kmlPoints(stops_ll, "D:/data/route/routes.kml", name=stops_ll$tripid)


temp1=temp1[cog<3600];dim(temp1)
plot(temp1$lon,temp1$lat)
cl=dbscan(scale(temp1[,list(lon,lat,sog,cog)]),eps=0.2,minPts =10)
temp2=cbind(temp1,c=cl$cluster)[c>0]
#plot(temp2$lon,temp2$lat)
dev.new()
p=ggplot(data=temp2,aes(x=lon,y=lat,col=as.factor(c)))+geom_point();p
