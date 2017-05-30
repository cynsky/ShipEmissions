#input: s, sid location, r 
library('swfscMisc')
library('sp')
#
shipdt=fread('D:/data/Global/gridBased/ashipdt.csv')
tm=berthCities
n=nrow(tm);n
temp0=shipdt[mmsi<0];temp0=temp0[,bsid:=0][,dist:=0][,UID:=0][,ISO:=''][,NAME_0:=''][,NAME_1:=''][,NAME_2:=''];temp0
for(i in seq(1,n)){
  t=tm[i]
  pl1 <- circle.polygon(t$lon, t$lat,5,units = 'nm',poly.type = "cart.earth")
  idx=point.in.polygon(shipdt$lon,shipdt$lat,pl1[,'x'],pl1[,'y'])
  temp=data.table(cbind(shipdt,idx))[idx>0];
  temp[,idx:=NULL]
  if(nrow(temp)>0){
    print(paste(i,nrow(temp)))
    temp=temp[,bsid:=t$bsid][,UID:=t$UID][,ISO:=t$ISO][,NAME_0:=t$NAME_0][,NAME_1:=t$NAME_1][,NAME_2:=t$NAME_2]
    temp=temp[,dist:=distance(lon,lat,t$lon,t$lat)]
    temp0=rbind(temp0,temp)
  }
}

temp0[,.N,list(bsid,NAME_0,NAME_1,NAME_2)]
setkey(temp0,pid,bsid,dist);
temp0=temp0[,.SD[1],list(mmsi,pid)]#在港口范围内的轨迹点集合

#-----对每个bsid中的轨迹点进行分割-----
setorder(temp0,mmsi,time)
n=nrow(temp0)
temp1=temp0[1:(n-1),list(mmsi,time1=time,pid1=pid,lon1=lon,lat1=lat,bsid1=bsid,
                         UID.f=UID,ISO.f=ISO,NAME_0.f=NAME_0,NAME_1.f=NAME_1,NAME_2.f=NAME_2)]
temp2=temp0[2:(n),list(time2=time,pid2=pid,lon2=lon,lat2=lat,bsid2=bsid,
                       UID.t=UID,ISO.t=ISO,NAME_0.t=NAME_0,NAME_1.t=NAME_1,NAME_2.t=NAME_2)]
templn=cbind(temp1,temp2)
tripln=templn[bsid1!=bsid2];#相邻两个点的停留点bsid不同表明是一个trip
dim(tripln);head(tripln)
#----统计trip信息-----
tripln=tripln[,tripid:=seq(1,nrow(tripln))]
tripln=tripln[,dur:=abs(time2-time1)]
#另个港口或者码头(tm:terminal)之间两端距离，这里的点并不是用港口和码头的坐标，而是用trip两个端点的坐标
tripln=tripln[,portdist:=distance(lon1,lat1,lon2,lat2)]
tripln=tripln[,dist:=0]
tripln=tripln[,N:=0]
for (i in seq(1,nrow(tripln))){
  l=tripln[i]
  atrip=shipdt[pid%between%c(l$pid1,l$pid2)]
  setkey(atrip,mmsi,time)
  n=nrow(atrip)
  atrip1=atrip[1:(n-1),list(lon1=lon,lat1=lat)]
  atrip2=atrip[2:n,list(lon2=lon,lat2=lat)]
  ln=cbind(atrip1,atrip2)
  ln=ln[,d:=distance(lon1,lat1,lon2,lat2)]
  tripln[i,dist:=sum(ln$d)]
  tripln=tripln[i,N:=n]
}

tripln=tripln[,distRate:=round(dist/portdist,2)]
tripln=tripln[,list(mmsi,tripid,bsid1,bsid2,N,dur,dist,portdist,distRate,pid1,pid2,time1,time2,sog1,sog2,cog1,cog2,
                    UID.f,UID.t,ISO.f,ISO.t,NAME_0.f,NAME_0.t,NAME_1.f,NAME_1.t,NAME_2.f,NAME_2.t)]
head(tripln);dim(tripln)
tripln[UID.f!=UID.t]



#-----plot trip statistics------

plot(tripln$distrate)
tripln[distrate>1.5]$tripid2
#tripln[distrate>1.5]
(tripln[sid1==2&sid2==3])
trips23=s[tripid2%in%tripln[sid1==2&sid2==3]$tripid2]
plot(trips23[tripid2==3]$time-min(trips23[tripid2==3]$time),trips23[tripid2==3]$sog)
plot(trips23[tripid2==10]$time-min(trips23[tripid2==10]$time),trips23[tripid2==10]$sog)
plot(trips23[tripid2==22]$time-min(trips23[tripid2==22]$time),trips23[tripid2==22]$sog)
trips23[tripid2==10&stayid>0]

#----画出speed profile
slon=tm[sid==2]$lon;slat=tm[sid==2]$lat# lon and lat of start terminal
trips=s[tripid2%in%tripln[sid1==2&sid2==3]$tripid2] # trips from terminal 2 to terminal 3 
#add sdist : distance of point to terminal
trips=trips[,sdist:=distance(lon,lat,slon,slat)]
trips=trips[,seg:=floor(sdist/2/1852)]#以100米进行分段
segStats=trips[,list(msog=round(median(.SD$sog))),seg];setorder(segStats,seg)
p=ggplot(data=segStats,aes(x=seg,y=msog))+geom_point()+geom_path(col='green');p











