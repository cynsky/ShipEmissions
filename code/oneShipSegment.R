#test the geo distance matrix, added in the function files
#weighted cluster method
library('data.table')
library('ggplot2')
library('ggmap')
library('dbscan')
library('dplyr')
library('xts')

#funPath='D:/share/Git/Rprojects/ECA/function.R'
funPath='D:/Git/Rprojects/ECA/function.R';source(funPath)
funPath2='D:/Git/Rprojects/ShipEmissions/code/ShipTrajectoryAnalysisFunctions.R';source(funPath2)

dataPath2='D:/Git/data/container/215896000.txt'
s=fread(dataPath2);dim(s);setnames(s,c('mmsi','time','sog','lon','lat','status'));
s=cbind(s,pid=seq(1,nrow(s)));head(s,3);
s=s[1:100000]
#plot(s$sog/10)
#----
ts=timeCluster(s,eps=3600*2,minp=3);head(ts);dim(ts[,.N,stayid]);dim(ts)#
ss=spaceCluster(ts,eps=0.002,minp=3);head(ss);dim(ss[stayid>0&stayid2>0,.N,list(stayid,stayid2)]);dim(ss)
ss[stayid==0&stayid2>0]

#single ship statistics:duration,avgspeed,zerorate (round(#zerospeed/#total,2)
head(ss);setkey(ss,pid)
stayStat=ss[stayid>0&stayid2>0,list(startpid=first(.SD)$pid,endpid=last(.SD)$pid,stime=first(.SD)$time,etime=last(.SD)$time,.N,duration=(last(.SD)$time-first(.SD)$time),
                                    lon=mean(.SD$lon),lat=mean(.SD$lat),asog=mean(.SD$sog),zerorate=round(nrow(.SD[sog==0])/.N,2)),list(mmsi,stayid,stayid2)]

ms=mergeStayPoint(stayStat,eps=0.01,minp=1);sids=ms[,.N,sid]$sid;
#---将多个ggplot图片画在一个page上-----
library(ggplot2)
library(grid)
library(gridExtra)
for(i in seq(1,1)){
  temp=ms[sid==sids[i]]
  dev.new();
  plot(ggplotMap(temp,16))
  # p1=ggplot()+geom_point(data=atrip[1],aes(x=lon,y=lat))+geom_point(data = atrip[sog1<10],aes(x=lon1,y=lat1,col='red'))+ggtitle(tripids[i])
  # p2=ggplot()+geom_point(data = atrip,aes(x=time1,y=sog1*10,col='red'))+geom_point(data = atrip,aes(x=time1,y=cog1,col='blue'))+ggtitle('sog+cog');
  # grid.arrange(p1, p2, ncol =2)
  
}
#---------------
ggplotMap<-function(temp,zoomSize){
  centerX=0.5*(max(temp$lon)+min(temp$lon))
  centerY=0.5*(max(temp$lat)+min(temp$lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google',maptype = 'satellite'))
  #p=p+geom_point(data=temp[1],aes(x=olon,y=olat,col='red'),size=4,alpha=0.5)
  #p=p+geom_point(data=temp[1],aes(x=dlon,y=dlat,col='blue'),size=4,alpha=0.5)
  p=p+geom_point(data=temp,aes(x=lon,y=lat),col='red',size=4,alpha=0.5)
  p=p+geom_text(data=temp[1],nudge_x = 0.0002,nudge_y = 0.0002,aes(x=lon,y=lat,label=sid),color='black',size=3)
  #p=p+labs(x="Longtitude",y="Latitude")+theme(legend.position=c(0.9,0.9))
  p=p+labs(x="Longtitude",y="Latitude")+theme(legend.position='none')+ggtitle(temp[1]$sid)
  return(p)#ggsave(paste('D:/data/route/',temp[1]$sid,'.png',sep=''),width = 6,height = 6)
}
#----------
setkey(ms,mmsi,startpid);dim(ms)

ms[zerorate<0.5]
plot(stayStat$duration)
plot(stayStat$asog)
#plot(stayStat$zerorate)
s3=setStayId(ss,ms);head(s3[sid>0]);dim(s3)
s4=setTripId(s3,ms);head(s4[tripid>0],3)
trips=s4[tripid>0,.N,list(mmsi,tripid)];dim(trips);head(trips)
trips=addTripStats(trips,s4);
#s[,.N,stayid]

#input: s, sid location, r 
library('swfscMisc')
library('sp')
tm=ms[asog<3,list(.N,lon=mean(.SD$lon),lat=mean(.SD$lat)),list(sid)];tm#其中有一个点的asog太高

temp0=s[mmsi<0][,sid:=0];temp0# points inside the 5 nm circle
for(i in seq(1,4)){
  t=tm[i]
  pl1 <- circle.polygon(t$lon, t$lat,5,poly.type = "cart.earth")
  idx=point.in.polygon(s$lon,s$lat,pl1[,'x'],pl1[,'y'])
  temp=data.table(cbind(s,idx))[idx>0];temp=temp[,sid:=t$sid]
  temp[,idx:=NULL]
  print(nrow(temp))
  if(nrow(temp>0)){
    temp0=rbind(temp0,temp)
  }
}

head(temp0);dim(temp0)
head(temp0[,.N,list(time)][N>1])

head(s)
setorder(temp0,mmsi,time)
n=nrow(temp0)
temp1=temp0[1:(n-1),list(time1=time,sog1=sog,lon1=lon,lat1=lat,status1=status,pid1=pid,startstayid=stayid,startstayid2=stayid2,sid1=sid)]
temp2=temp0[2:(n),list(time2=time,sog2=sog,lon2=lon,lat2=lat,status2=status,pid2=pid,endstayid=stayid,endstayid2=stayid2,sid2=sid)]
templn=cbind(temp1,temp2)
tripln=templn[sid1!=sid2];dim(tripln);head(tripln)

m=nrow(tripln)
s=s[,tripid:=0]
s=s[,sid1:=0]
s=s[,sid2:=0]
for(i in seq(1,m)){
  
  ln=tripln[i]
  s[pid%between%c(ln$pid1,ln$pid2),tripid:=i]
  s[pid%between%c(ln$pid1,ln$pid2),sid1:=ln$sid1]
  s[pid%between%c(ln$pid1,ln$pid2),sid2:=ln$sid2]
  
}
head(s[tripid>0])
dim(trips[sid1!=sid2])

#统计trip信息
tripln=tripln[,tripid2:=seq(1,nrow(tripln))]
tripln=tripln[,dur:=abs(time2-time1)]
tripln=tripln[,N:=(abs(pid2-pid1)+1)]
tripln=tripln[,enddist:=distance(lon1,lat1,lon2,lat2)]#两端直线距离
tripln=tripln[,dist:=0]
for (i in seq(1,nrow(tripln))){
  l=tripln[i]
  atrip=s[pid%between%c(l$pid1,l$pid2)]
  n=nrow(atrip)
  atrip1=atrip[1:(n-1),list(lon1=lon,lat1=lat)]
  atrip2=atrip[2:n,list(lon2=lon,lat2=lat)]
  ln=cbind(atrip1,atrip2)
  ln=ln[,d:=distance(lon1,lat1,lon2,lat2)]
  sum(ln$d)
  tripln[i,dist:=sum(ln$d)]
}

tripln=tripln[,distrate:=round(dist/enddist,2)]
tripln=tripln[,list(tripid2,sid1,sid2,N,dur,dist,enddist,distrate,pid1,pid2,time1,time2,sog1,sog2)]
head(tripln)

plot(tripln$distrate)
tripln[distrate>1.5]$tripid2
#tripln[distrate>1.5]
(tripln[sid1==2&sid2==3])
trips23=s[tripid2%in%tripln[sid1==2&sid2==3]$tripid2]
plot(trips23[tripid2==3]$time-min(trips23[tripid2==3]$time),trips23[tripid2==3]$sog)
plot(trips23[tripid2==10]$time-min(trips23[tripid2==10]$time),trips23[tripid2==10]$sog)
plot(trips23[tripid2==22]$time-min(trips23[tripid2==22]$time),trips23[tripid2==22]$sog)
trips23[tripid2==10&stayid>0]

slon=tm[sid==2]$lon;slat=tm[sid==2]$lat# lon and lat of start terminal
trips=s[tripid2%in%tripln[sid1==2&sid2==3]$tripid2] # trips from terminal 2 to terminal 3 
#add sdist : distance of point to terminal
trips=trips[,sdist:=distance(lon,lat,slon,slat)]
trips=trips[,seg:=floor(sdist/2/1852)]#以100米进行分段
segStats=trips[,list(msog=round(median(.SD$sog))),seg];setorder(segStats,seg)
p=ggplot(data=segStats,aes(x=seg,y=msog))+geom_point()+geom_path(col='green');p

tripids=trips[,.N,tripid2]$tripid2
trippoints0=trips[,list(mmsi,time,status,sog,lon,lat,pid,sid1,sid2,tripid=tripid2)]
trippoints1=data.table(left_join(trippoints0,tm[,list(sid1=sid,olon=lon,olat=lat)],'sid1'))
trippoints=data.table(left_join(trippoints1,tm[,list(sid2=sid,dlon=lon,dlat=lat)],'sid2'))

setTripLine<-function(trippoints){
  setkey(trippoints,mmsi,time)
  n=nrow(trippoints)
  l1=trippoints[1:(n-1),list(mmsi,sid1,sid2,olon,olat,dlon,dlat,time1=time,status1=status,sog1=sog,lon1=lon,lat1=lat,pid1=pid,tripid1=tripid)]
  l2=trippoints[2:(n),list(time2=time,status2=status,sog2=sog,lon2=lon,lat2=lat,pid2=pid,tripid2=tripid)]
  ln=cbind(l1,l2)[tripid1==tripid2]
  ln=ln[,dist:=round(distance(lon1,lat1,lon2,lat2))][,dur:=abs(time2-time1)][,meansog:=round((sog1+sog2)/2,1)]
  ln=ln[,avgsog:=round((dist/1852)/(dur/3600),1)]
  return(ln)
}

tripln=setTripLine(trippoints)

# main route and speed profile for trips-----

#判断每个line segment 是否为大范围缺失，这里只用距离
tripln=tripln[,ismiss:=0][dist>5*1000,ismiss:=1]
#calculate the statistics of each trip as a whole 
tripStats=tripln[,list(N=(.N+1),dur=sum(.SD$dur),oddist=round(distance(olon,olat,dlon,dlat)),
                       dist=sum(.SD$dist),Nmiss=sum(.SD$ismiss),missdist=sum(.SD[ismiss>0]$dist)),
                 list(mmsi,tripid=tripid1,sid1,sid2,olon,olat,dlon,dlat)][,distRate:=round(dist/oddist,2)]
#选择合理的trip，并计算每个点点到起始点途经的距离，然后每个trip采集200个点
#-----trip 选择-----
distLimit=quantile(tripStats$dist,1)[[1]];distLimit
durLimit=quantile(tripStats$dur,1)[[1]];durLimit
missLimit=quantile(tripStats$Nmiss,1)[[1]];missLimit
tripStats1=tripStats[Nmiss<=missLimit&dur<=durLimit&dist<=distLimit];nrow(tripStats1)
trips1=trippoints[tripid%in%tripStats1$tripid];dev.new();plot(trips1$lon,trips1$lat)

setkey(trips1,tripid,mmsi)
tripids=tripStats1$tripid
n=length(tripStats1$tripid);n
ps0=data.table(trippoints[1])[,odist:=0][,ratio:=0][,group:=0][mmsi<0]
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
                sog=round(median(.SD[,list(sog=median(sog)),tripid]$sog))),(group)]
triprt=ps0[,list(.N,
                 lon=median(.SD$lon),
                 lat=median(.SD$lat),
                 sog=round(median(.SD$sog))),list(tripid,group)]
setkey(route,group)
plot(route$lon,route$lat)
plot(route$sog,type='l')

tr1=triprt[tripid==3]
gs=tr1$group
temp0=triprt[tripid<0][,d1:=0];temp0
for(i in seq(1,length(gs))){
  print(i)
  grp=gs[i]
  temp=triprt[group==grp&tripid!=3][,d1:=distance(lon,lat,tr1[group==grp]$lon,tr1[group==grp]$lat)]
  #temp=temp[,c1:=abs(cog-tr1[group==grp]$cog)]
  temp0=rbind(temp0,temp)
}
ag=temp0
dev.new();plot(ag$group,ag$d1)
#dev.new();plot(ag$group,(ag$c1))
dev.new();plot(tr1$lon,tr1$lat)
#tripdiff=ag[,list(distdiff=median(d1),diffcog=median(c1)),tripid]
dev.new();plot(tripdiff$distdiff)
#dev.new();plot(tripdiff$diffcog)
#dev.new();plot(tripdiff$distdiff/1000,tripdiff$diffcog)

# 补充缺失轨迹--------------

scale=1000
trips=trippoints[,glon:=floor(lon*scale)/scale+0.5/scale][,glat:=floor(lat*scale)/scale+0.5/scale][,gid:=paste(glon,glat,sep='')][,.SD[1],list(gid,mmsi,tripid)]
setkey(trips,mmsi,tripid,time)

n = nrow(trips)
l1 = trips[1:(n - 1),list(mmsi1=mmsi,time1=time,sog1=sog,lon1=lon,lat1=lat,glon1=glon,glat1=glat,pid1=pid,gid1=gid,tripid1=tripid)]
l2 = trips[2:n,list(mmsi2=mmsi,time2=time,sog2=sog,lon2=lon,lat2=lat,glon2=glon,glat2=glat,pid2=pid,gid2=gid,tripid2=tripid)]
trips.l=cbind(l1,l2)[mmsi1==mmsi2&tripid1==tripid2]
trips.l=trips.l[,lid:=seq(1,nrow(trips.l))][,dist:=distance(lon1,lat1,lon2,lat2)][,dur:=abs(time2-time1)][,meansog:=(sog1+sog2)/2][,avgsog:=round(dist*3600*10/dur/1852,1)]
trips.l=trips.l[,ismiss:=0][dist>10*1000,ismiss:=1]
miss.l=trips.l[ismiss==1]
n=nrow(miss.l);n
miss.l[,missid:=seq(1,n)]
#4 get reference trips-----半径为1公里,整个搜索半径为40个0.001的网格，大约4个多公里

dradium=4000 #meters
#cogdiff=200 #30度
rangegrid=100 #50 grids
reftrips0=data.table(mmsi=0,tripid=0,spid=0,slon=0,slat=0,slid=0,missid=0,misslid=0,epid=0,elon=0,elat=0,elid=0,d0=0)[mmsi<0]
for(i in seq(1,n)){
  print(paste('miss',i))
  aln=miss.l[i]
  range1=trips.l[tripid1!=aln$tripid1&abs(glon1-aln$glon1)<(rangegrid/scale)&abs(glon2-aln$glon1)<(rangegrid/scale)][abs(glat1-aln$glat1)<(rangegrid/scale)&abs(glat2-aln$glat1)<(rangegrid/scale)]
  range2=trips.l[tripid1!=aln$tripid1&abs(glon1-aln$glon2)<(rangegrid/scale)&abs(glon2-aln$glon2)<(rangegrid/scale)][abs(glat1-aln$glat2)<(rangegrid/scale)&abs(glat2-aln$glat2)<(rangegrid/scale)]
  if(nrow(range1)>0&nrow(range2)>0){
    
    #第一个点
    lns1=data.table(missid=0,misslid=0,mmsi=0,tripid=0,spid=0,slon=0,slat=0,slid=0,d0=0)[spid<0]#lid代表miss.l中的一个点，其他为合格line中一个离该店距离最近的点。
    for(j in seq(1,nrow(range1))){
      #print(j)
      temp=range1[j]
      d=dist2Line(c(aln$lon1,aln$lat1),rbind(c(temp$lon1,temp$lat1),c(temp$lon2,temp$lat2)))
      d0=data.table(d)$distance
      if(d0<dradium){
        d1=distance(aln$lon2,aln$lat2,temp$lon1,temp$lat1)
        d2=distance(aln$lon2,aln$lat2,temp$lon2,temp$lat2)#选择离第二个点更近的点
        if(d2>=d1){#取第一个点
          tmp=data.table(missid=aln$missid,misslid=aln$lid,temp[,list(mmsi=mmsi1,tripid=tripid1,spid=pid1,slon=lon1,slat=lat1,slid=lid)],d0)
        }else{#取第一个点
          tmp=data.table(missid=aln$missid,misslid=aln$lid,temp[,list(mmsi=mmsi1,tripid=tripid1,spid=pid2,slon=lon2,slat=lat2,slid=lid)],d0)
        }
        lns1=rbind(lns1,tmp)
      }
    }
    reftrips1=lns1[,.SD[which.min(d0)],list(mmsi,tripid)]
    #第二个点
    lns2=data.table(missid=0,misslid=0,mmsi=0,tripid=0,epid=0,elon=0,elat=0,elid=0,d0=0)[epid<0]#lid代表miss.l中的一个点，其他为合格line中一个离该店距离最近的点。
    for(j in seq(1,nrow(range2))){
      #print(j)
      temp=range2[j]
      d=dist2Line(c(aln$lon2,aln$lat2),rbind(c(temp$lon1,temp$lat1),c(temp$lon2,temp$lat2)))
      d0=data.table(d)$distance
      if(d0<dradium){
        d1=distance(aln$lon1,aln$lat1,temp$lon1,temp$lat1)
        d2=distance(aln$lon1,aln$lat1,temp$lon2,temp$lat2)#选择离第二个点更近的点
        if(d2>=d1){#取第一个点，哪个小取哪个
          tmp=data.table(missid=aln$missid,misslid=aln$lid,temp[,list(mmsi=mmsi1,tripid=tripid1,epid=pid1,elon=lon1,elat=lat1,elid=lid)],d0)
        }else{#取第一个点
          tmp=data.table(missid=aln$missid,misslid=aln$lid,temp[,list(mmsi=mmsi1,tripid=tripid1,epid=pid2,elon=lon2,elat=lat2,elid=lid)],d0)
        }
        lns2=rbind(lns2,tmp)
      }
    }
    reftrips2=lns2[,.SD[which.min(d0)],list(mmsi,tripid)]
    reftrips=inner_join(reftrips1[,list(mmsi,tripid,spid,slon,slat,slid)],reftrips2,c('mmsi','tripid'))
    reftrips0=rbind(reftrips0,reftrips)
    
  }
}



#进行插值------------------------
#总共有136个miss segment
miss.l 
reftrips0;n=nrow(reftrips0)
refpoints0=data.table( mmsi=0,time=0,sog=0,lon=0,lat=0,pid=0,gid=0,glon=0,glat=0,missid=0,tripid=0)[mmsi<0]
for(i in seq(1,n)){
  if(i%%100==0){print(i)}
  l=reftrips0[i]
  p=trips[pid%between%c(l$spid,l$epid),list(mmsi,time,sog,lon,lat,pid,gid,glon,glat)][,mmsi:=l$mmsi][,missid:=l$missid][,tripid:=l$tripid]
  
  refpoints0=rbind(refpoints0,p)
}
#sid1,sid2 分别为开始和结束点
refpoints=data.table(left_join(refpoints0,miss.l[,list(missid,sid1=pid1,olon=lon1,olat=lat1,osog=sog1,otime=time1,
                                                      sid2=pid2,dlon=lon2,dlat=lat2,dsog=sog2,dtime=time2,oddist=dist,oddur=dur)],'missid'))
setkey(refpoints,mmsi,missid,tripid,time)

tripln=setTripLine2(refpoints)

setTripLine2<-function(trippoints){
  #setkey(trippoints,missid,tripid,mmsi,time)
  n=nrow(trippoints)
  l1=trippoints[1:(n-1),list(mmsi,missid1=missid,sid1,sid2,olon,olat,dlon,dlat,time1=time,sog1=sog,lon1=lon,lat1=lat,pid1=pid,tripid1=tripid)]
  l2=trippoints[2:(n),list(missid2=missid,time2=time,sog2=sog,lon2=lon,lat2=lat,pid2=pid,tripid2=tripid)]
  ln=cbind(l1,l2)[missid1==missid2][tripid1==tripid2]
  ln=ln[,dist:=round(distance(lon1,lat1,lon2,lat2))][,dur:=abs(time2-time1)][,meansog:=round((sog1+sog2)/2,1)]
  
  ln=ln[,avgsog:=round((dist/1852)/(dur/3600),1)]
  return(ln)
}

#built mainroute and speed profile for a miss line-----------------

#判断每个line segment 是否为大范围缺失，这里只用距离
# tripln=tripln[,ismiss:=0][dist>5*1000,ismiss:=1]
# #calculate the statistics of each trip as a whole 
tripStats=tripln[,list(N=(.N+1),dur=sum(.SD$dur),oddist=round(distance(olon,olat,dlon,dlat)),
                       dist=sum(.SD$dist),Nmiss=sum(.SD$ismiss),missdist=sum(.SD[ismiss>0]$dist)),
                 list(mmsi,missid=missid1,tripid=tripid1,sid1,sid2,olon,olat,dlon,dlat)][,distRate:=round(dist/oddist,2)]
#选择合理的trip，并计算每个点点到起始点途经的距离，然后每个trip采集200个点
#-----selected trips for testing, calculate the odist of each point-----
distLimit=quantile(tripStats$dist,1)[[1]];distLimit
durLimit=quantile(tripStats$dur,1)[[1]];durLimit
missLimit=quantile(tripStats$Nmiss,1)[[1]];missLimit
tripStats1=tripStats[Nmiss<=missLimit&dur<=durLimit&dist<=distLimit];nrow(tripStats1)
trips1=refpoints[tripid%in%tripStats1$tripid];dev.new();plot(trips1$lon,trips1$lat)

setkey(trips1,mmsi,missid,tripid,time)
missids=trips1[,.N,missid]$missid
nm=length(missids)
ps0=data.table( lon=0,lat=0,sog=0,tripid=0,missid=0,odist=0,ratio=0,group=0,milespot=0)[missid<0] 
for(k in seq(1,nm)){
  print(paste('miss',k))
  
  amissid=missids[k]
  misstripid=miss.l[missid==amissid]$tripid1
  amissdt=trips1[missid==amissid];plot(amissdt$lon,amissdt$lat)
  #确定分割成多少个group，基本上根据miss line 两个端点之间的距离，以1km左右为一个点
  kgroup=floor(amissdt[1]$oddist/1000) #分割成k个段，k groups
  tripids=unique(amissdt$tripid);n=length(tripids);n
  for(i in seq(1,(n))){#for each trip
    #i=1
    #print(i)
    id=tripids[i]
    ps=amissdt[tripid==id]
    #----
    
    ps=rbind(amissdt[1][,list(lon=olon,lat=olat,sog=osog,tripid=misstripid,missid)],
             ps[,list(lon,lat,sog,tripid,missid)],amissdt[1][,list(lon=dlon,lat=dlat,sog=dsog,tripid=misstripid,missid)])
    m=nrow(ps)
    l1=ps[1:(m-1),list(lon1=lon,lat1=lat)]
    l2=ps[2:m,list(lon2=lon,lat2=lat)]
    ln=cbind(l1,l2)[,dist:=round(distance(lon1,lat1,lon2,lat2))]
    ps[1,odist:=0]
    for(j in seq(2,m)){
      ps[j,odist:=sum(ln[1:(j-1)]$dist)]
    }
    
    #-----
    ps=ps[,ratio:=(odist/ps[nrow(ps)]$odist)]#比例
    ps=ps[,milespot:=round(ratio*ps[nrow(ps)]$odist)]#比例
    ps=ps[,group:=floor(ratio*kgroup)]#2000个点k
    ps0=rbind(ps0,ps)
  }
}


route=ps0[,list(.N,
                lon=median(.SD[,list(lon=median(lon)),tripid]$lon),
                lat=median(.SD[,list(lat=median(lat)),tripid]$lat),
                sog=round(median(.SD[,list(sog=median(sog)),tripid]$sog))),list(missid,group)]
triprt=ps0[,list(.N,
                 lon=median(.SD$lon),
                 lat=median(.SD$lat),
                 sog=round(median(.SD$sog))),list(missid,tripid,group)]

setkey(route,group)
plot(route$lon,route$lat)
plot(route$sog,type='l')

#补充轨迹点-----
missids=unique(ps0$missid)
nr=length(missids);nr
route0=data.table(missid=0,group=0,N=0,lon=0,lat=0,sog=0,time=0,mmsi=0,tripid=0)
for(k in seq(1,nr)){
  print(paste('miss',k))
  amissid=missids[k]
  ap=ps0[missid==amissid]
  setkey(ap,missid,group)
  route=ap[,list(.N,
                 lon=median(.SD[,list(lon=median(lon)),tripid]$lon),
                 lat=median(.SD[,list(lat=median(lat)),tripid]$lat),
                 sog=round(median(.SD[,list(sog=median(sog)),tripid]$sog))),list(missid,group)]
  
  n=nrow(route);n
  l1=route[1:(n-1),list(missid,group,lon1=lon,lat1=lat,sog1=sog)]
  l2=route[2:n,list(lon2=lon,lat2=lat,sog2=sog)]
  ln=cbind(l1,l2)[,meansog:=(sog1+sog2)/2][,timegap:=round((distance(lon1,lat1,lon2,lat2)/1852)/(meansog/10)*3600)]
  amissln=miss.l[missid==amissid,list(mmsi=mmsi1,tripid=tripid1,time1,time2)];
  compdur=sum(ln$timegap)
  realdur=abs(amissln$time2-amissln$time1);
  rate=realdur/compdur
  # computer route time
  n=nrow(route);n
  route[1,time:=amissln$time1]
  
  for(i in seq(2,(n-1))){
    route[i,time:=amissln$time1+round(sum(ln[1:(i-1)]$timegap)*rate)]
    
  }
  route[n,time:=amissln$time2]
  route[,mmsi:=amissln$mmsi][,tripid:=amissln$tripid]
  setkey(route,missid,time)
  route0=rbind(route0,route)

}

dev.new();plot(route$group,route$sog,type='l')
plot(route$time,route$sog,type='l')

#----plot---------------
ggthemr_reset()
ggthemr('earth')
#36条轨迹中的每条轨迹
#plot each trip between sid2 and sid3
tripids=trippoints[,.N,tripid]$tripid;zoomSize=7
n=length(tripids);n
for(i in seq(1,n)){
  temp=trippoints[tripid==tripids[i]]
  centerX=0.5*(max(temp$lon)+min(temp$lon))
  centerY=0.5*(max(temp$lat)+min(temp$lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google',maptype = 'hybrid'))
  p = p + geom_point(data=tm[sid==2],aes(x = lon,y = lat),col='red',size=3,alpha=0.95);
  p = p + geom_point(data=tm[sid==3],aes(x = lon,y = lat),col='red',size=3,alpha=0.95);
  p=p+geom_text(data=tm[sid==2],nudge_x = 0.002,nudge_y = -0.05,aes(x=lon,y=lat,label=paste('Lzmir Port')),color='red',size=4)
  p=p+geom_text(data=tm[sid==3],nudge_x = 0.002,nudge_y = -0.05,aes(x=lon,y=lat,label=paste('Pireaus Port')),color='red',size=4)
  p=p+scale_x_continuous(limits = c(23.5, 27.5))+scale_y_continuous(limits = c(37.5, 39));
  p=p+labs(x="Longtitude",y="Latitude")+theme(legend.position='bottom')+ggtitle(paste('trip id =',temp[1]$tripid))
  #p=p+geom_text(data=sample_n(temp,5),nudge_x = 0.0005,nudge_y = 0.0005,aes(x=lon,y=lat,label=pid),color='black',size=2)
  p1 = p + geom_point(data=temp,aes(x = lon,y = lat),col='yellow',size=1);
  ggsave(paste('D:/data/route/',temp[1]$tripid,'points','.png',sep=''),width = 10,height = 5)
  p2 = p1 + geom_path(data=temp,aes(x = lon,y = lat),col='green',size=0.3);
  ggsave(paste('D:/data/route/',temp[1]$tripid,'path','.png',sep=''),width = 10,height = 5)
  
}

# trips.l 计算缺失情况
p=ggplot(trips.l, aes(x=log10(dist)))+geom_histogram(binwidth=.05,colour="black",fill="white");p # Overlay wit

p=ggplot(tripStats, aes(x=dist))+geom_histogram(binwidth=1000,colour="black",fill="white");p # Overlay wit



