#construct route between two ports
library(pracma)
shipfile='D:/Git/Rprojects/basedata/ships.csv'
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi)

r1=fread('D:/data/Global/gridBased/SHtoUSAdt.csv')
r2=fread('D:/data/Global/gridBased/SHtoNBdt.csv')#洋山到大榭
berthStats=fread('D:/data/Global/gridBased/berthStats.csv')
dim(r2);head(r2)
setkey(r2,mmsi,time)

#plot(r2$lon,r2$lat)
#calculate each trip line segment.
tripln0=setTripLine(r2)
tripln=merge(tripln0,berthStats[,list(bsid,lon.f=lon,lat.f=lat)],by.x='bsid.f',by.y='bsid')
tripln=merge(tripln,berthStats[,list(bsid,lon.t=lon,lat.t=lat)],by.x='bsid.t',by.y='bsid')

#判断每个line segment 是否为大范围缺失，这里只用距离
tripln=tripln[,ismiss:=0][dist>5*1000,ismiss:=1]
#calculate the statistics of each trip as a whole 
tripStats=tripln[,list(N=(.N+1),dur=sum(.SD$dur),oddist=round(distance(lon.f,lat.f,lon.t,lat.t)),
                       dist=sum(.SD$dist),Nmiss=sum(.SD$ismiss),missdist=sum(.SD[ismiss>0]$dist)),list(mmsi,tripid=tripid1,bsid.f,bsid.t,lon.f,lat.f,lon.t,lat.t)][,distRate:=round(dist/oddist,2)]
#选择合理的trip，并计算每个点点到起始点途经的距离，然后每个trip采集200个点
#-----selected trips for testing, calculate the odist of each point-----
trips=tripStats[distRate<3&Nmiss==0&dur<3600*20&dist>200*1000];nrow(trips)
setkey(trips,tripid,mmsi)
tripids=trips$tripid
n=length(tripids);n
ps0=data.table(r2[1])[,odist:=0][,ratio:=0][,group:=0][mmsi<0]
for(i in seq(1,(n))){#for each trip
  print(i)
  id=tripids[i]
  ps=r2[tripid==id]
  m=nrow(ps)
  for(j in seq(1,m)){
    if(j==m){
      ps[j,odist:=trips[tripid==id]$dist]
    }else{
      ls=tripln[tripid1==id]#lines
      ps[j,odist:=sum(ls[1:j]$dist)]
    }
  }
  ps=ps[,ratio:=(odist/ps[nrow(ps)]$odist)]#比例
  ps=ps[,group:=floor(ratio*2000)]#2000个点
  ps0=rbind(ps0,ps)
}

mmsis=ps0[,.N,mmsi]$mmsi;length(mmsis)
ships[mmsi%in%mmsis]

route=ps0[,list(.N,
                lon=median(.SD[,list(lon=median(lon)),tripid]$lon),
                lat=median(.SD[,list(lat=median(lat)),tripid]$lat),
                sog=round(median(.SD[,list(sog=median(sog)),tripid]$sog)),
                cog=round(median(.SD[,list(cog=median(cog)),tripid]$cog))),(group)]
#有些trip在某些点位是没有数据的，因此不同tripid在不同的group中是不相同的。
triprt=ps0[,list(.N,
                lon=median(.SD$lon),
                lat=median(.SD$lat),
                sog=round(median(.SD$sog)),
                cog=round(median(.SD$cog))),list(tripid,group)]

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
trips
tripdist=data.table(id1=0,id2=0,hdist=0)[hdist<0]

#hausdorff_dist
tripids=trips$tripid
n=nrow(trips);n
for(i in seq(1,1)){
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
setTripLine<-function(trippoints){
  setkey(trippoints,mmsi,time)
  n=nrow(trippoints)
  l1=trippoints[1:(n-1),list(mmsi,bsid.f=bsid1,bsid.t=bsid2,time1=time,status1=status,sog1=sog,cog1=cog,lon1=lon,lat1=lat,pid1=pid,tripid1=tripid)]
  l2=trippoints[2:(n),list(time2=time,status2=status,sog2=sog,cog2=cog,lon2=lon,lat2=lat,pid2=pid,tripid2=tripid)]
  ln=cbind(l1,l2)[tripid1==tripid2]
  ln=ln[,dist:=round(distance(lon1,lat1,lon2,lat2))][,dur:=abs(time2-time1)][,diffcog:=abs(cog2-cog1)][,meansog:=round((sog1+sog2)/2,1)]
  ln=ln[,avgsog:=round((dist/1852)/(dur/3600),1)]
  return(ln)
}
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
