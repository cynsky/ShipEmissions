library(geosphere)
line <- rbind(c(0,0), c(2,2))
pnts <- rbind(c(3,3))
d = dist2Line(pnts,line);d
library(data.table)
library(ggplot2)
library(sp)
library(rgeos)
library(dplyr)
library(dbscan)
library(igraph)
#library(DSpat)
#1.read traj file
routes=fread('D:/data/route/top10route.csv')[cog>0][cog<3600][sog>0][sog<300]#
trips=routes[rid==3][,list(mmsi,time,status,sog,lon,lat,cog,pid,tripid)];dim(trips)
scale=1000
trips=trips[,glon:=floor(lon*scale)/scale+0.5/scale][,glat:=floor(lat*scale)/scale+0.5/scale][,gid:=paste(glon,glat,sep='')][,.SD[1],list(gid,mmsi,tripid)]
setkey(trips,mmsi,tripid,time)

n = nrow(trips)
l1 = trips[1:(n - 1),list(mmsi1=mmsi,time1=time,sog1=sog,lon1=lon,lat1=lat,glon1=glon,glat1=glat,cog1=cog,pid1=pid,gid1=gid,tripid1=tripid)]
l2 = trips[2:n,list(mmsi2=mmsi,time2=time,sog2=sog,lon2=lon,lat2=lat,glon2=glon,glat2=glat,cog2=cog,pid2=pid,gid2=gid,tripid2=tripid)]
trips.l=cbind(l1,l2)[mmsi1==mmsi2&tripid1==tripid2]
trips.l=trips.l[,lid:=seq(1,nrow(trips.l))][,dist:=distance(lon1,lat1,lon2,lat2)][,dur:=abs(time2-time1)][,meansog:=(sog1+sog2)/2][,avgsog:=round(dist*3600*10/dur/1852,1)]
trips.l=trips.l[,ismiss:=0][dist>50*1000,ismiss:=1]
miss.l=trips.l[ismiss==1]
n=nrow(miss.l);n
miss.l[,missid:=seq(1,n)]
#4 get reference trips-----半径为1公里,整个搜索半径为40个0.001的网格，大约4个多公里

dradium=2000 #meters
cogdiff=200 #30度
rangegrid=100 #50 grids
reftrips0=data.table(mmsi=0,tripid=0,spid=0,slon=0,slat=0,slid=0,missid=0,misslid=0,epid=0,elon=0,elat=0,elid=0,d0=0)[mmsi<0]
for(i in seq(1,10)){
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
      if(d0<dradium&(abs((temp$cog1+temp$cog2)/2-aln$cog1)<cogdiff)){
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
      if(d0<dradium&(abs((temp$cog1+temp$cog2)/2-aln$cog2)<cogdiff)){
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

p=ggplot()+geom_point(data = sample_n(trips,30000),aes(x=lon,y = lat))+
  geom_point(data=aln,aes(x=lon1,y=lat1),size=4,col='red')+
  geom_point(data=aln,aes(x=lon2,y=lat2),size=4,col='blue')+
  geom_point(data = range2,aes(x=lon1,y=lat1),col='yellow')+
  geom_point(data = range2,aes(x=lon2,y=lat2),col='yellow')
  
p




#开始补充轨迹--------
#有共同航次要判断航次是否符合时间差限制deltatThreshold=0.1
#但是如果中间有锚地停留等情况将使时间差非常不准确

deltatThreshold=0.2
refers=referlist[time.diff<=deltatThreshold]
#添加每个参考轨迹的参考距离
for(i in (1:nrow(lines.miss))){
  line=lines.miss[i]
  refer=refers[tripid.miss==line$tripid&lid.miss==line$lid]
  if(nrow(refer)>0){
    for(j in (1:nrow(refer))){
      refer1=refer[j]
      ln.add = lines[tripid == refer1$tripid.ref & time1 > refer1$time.from &time2 < refer1$time.to]
      #不包括距离大于35000的线段
      len = sum(ln.add[distance < missThreshold,distance])
      refers[tripid.miss==refer1$tripid.miss&lid.miss==refer1$lid.miss&tripid.ref==refer1$tripid.ref,sumDist:=len]
    }
  }
}


#插值
interpolatedPoints=data.table(tripid=0,lon=0,lat=0,time=0,sog=0)
interpolatedPoints=interpolatedPoints[tripid<0]

##----方法1：距离最大法------------------
for(m in (1:nrow(lines.miss))){
  line=lines.miss[m]
  line=lines.miss[tripid==26&lid==74]
  refer=refers[tripid.miss==line$tripid&lid.miss==line$lid]
  if(nrow(refer)>0){
    #距离最大航次
    referTrip=refer[sumDist==max(sumDist)]
    #拟添加的轨迹段
    ln.add = lines[tripid == referTrip$tripid.ref & time1 > referTrip$time.from &time2 < referTrip$time.to]
    if(nrow(ln.add)>0){
      #待加入数据的时间和sog
      setkey(ln.add,lid)
      #直线到两个端点的垂直距离
      d1=distance(line$lon1,line$lat1,referTrip$lon.from,referTrip$lat.from)
      d2=distance(line$lon2,line$lat2,referTrip$lon.to,referTrip$lat.to)
      #两个垂直点到靠近端点的距离
      d.first=distance(line$lon1,line$lat1,ln.add[1,lon1],ln.add[1,lat1])
      d.last=distance(line$lon2,line$lat2,ln.add[nrow(ln.add),lon2],ln.add[nrow(ln.add),lat2])
      #总距离
      d=sum(ln.add$distance)
      #缺失线段时间间隔
      t1=line$timespan
      avgSpeed=(d+d1+d2+d.first+d.last)/t1
      tt=data.table(deltat=c(d1,d.first,ln.add$distance,d.last)/avgSpeed)
      t2=tt
      for(k in (1:nrow(tt))){
        t2[k]$deltat=line$time1+sum(tt[1:k,deltat])
      }
      t2=t2[,sog:=avgSpeed]
      #get interpolated points
      p1=referTrip[,list(lon=lon.from,lat=lat.from)]
      pMiddle1=ln.add[,list(lon=lon1,lat=lat1)]
      pMiddle2=ln.add[nrow(ln.add),list(lon=lon2,lat=lat2)]
      p2=referTrip[,list(lon=lon.to,lat=lat.to)]
      temp=cbind(rbind(p1,pMiddle1,pMiddle2,p2),t2)
      temp=temp[,tripid:=referTrip$tripid.miss]
      setnames(temp,'deltat','time')
      interpolatedPoints=rbind(interpolatedPoints,temp)
    }
  }
  
}

results=rbind(lines[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)],
              lines[nrow(lines),list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)],interpolatedPoints)
setkey(results,tripid,time)


##----方法2：最小时间差方法------------------   

#插值
interpolatedPoints=data.table(tripid=0,lon=0,lat=0,time=0,sog=0)
interpolatedPoints=interpolatedPoints[tripid<0]

for(m in (1:nrow(lines.miss))){
  line=lines.miss[m]
  # line=lines.miss[tripid==26&lid==74]
  refer=refers[tripid.miss==line$tripid&lid.miss==line$lid]
  if(nrow(refer)>0){
    #距离最大航次
    referTrip=refer[time.diff==min(time.diff)]
    #拟添加的轨迹段
    ln.add = lines[tripid == referTrip$tripid.ref & time1 > referTrip$time.from &time2 < referTrip$time.to]
    if(nrow(ln.add)>0){
      #待加入数据的时间和sog
      setkey(ln.add,lid)
      #直线到两个端点的垂直距离
      d1=distance(line$lon1,line$lat1,referTrip$lon.from,referTrip$lat.from)
      d2=distance(line$lon2,line$lat2,referTrip$lon.to,referTrip$lat.to)
      #两个垂直点到靠近端点的距离
      d.first=distance(line$lon1,line$lat1,ln.add[1,lon1],ln.add[1,lat1])
      d.last=distance(line$lon2,line$lat2,ln.add[nrow(ln.add),lon2],ln.add[nrow(ln.add),lat2])
      #总距离
      d=sum(ln.add$distance)
      #缺失线段时间间隔
      t1=line$timespan
      avgSpeed=(d+d1+d2+d.first+d.last)/t1
      tt=data.table(deltat=c(d1,d.first,ln.add$distance,d.last)/avgSpeed)
      t2=tt
      for(k in (1:nrow(tt))){
        t2[k]$deltat=line$time1+sum(tt[1:k,deltat])
      }
      t2=t2[,sog:=avgSpeed]
      #get interpolated points
      p1=referTrip[,list(lon=lon.from,lat=lat.from)]
      pMiddle1=ln.add[,list(lon=lon1,lat=lat1)]
      pMiddle2=ln.add[nrow(ln.add),list(lon=lon2,lat=lat2)]
      p2=referTrip[,list(lon=lon.to,lat=lat.to)]
      temp=cbind(rbind(p1,pMiddle1,pMiddle2,p2),t2)
      temp=temp[,tripid:=referTrip$tripid.miss]
      setnames(temp,'deltat','time')
      interpolatedPoints=rbind(interpolatedPoints,temp)
    }
  }
  
}

results=rbind(lines[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)],
              lines[nrow(lines),list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)],interpolatedPoints)
setkey(results,tripid,time)

##----方法3：回归插值方法-----

#插值
interpolatedPoints=data.table(tripid=0,lon=0,lat=0,time=0,sog=0)
interpolatedPoints=interpolatedPoints[tripid<0]

##----方法1：距离最大法------------------
for(m in (1:nrow(lines.miss))){
  line=lines.miss[m]
  #line=lines.miss[tripid==26&lid==74]
  refer=refers[tripid.miss==line$tripid&lid.miss==line$lid]
  if(nrow(refer)>0){
    #提取每个航次的可参考直线
    res=data.table(tripid=0,lon=0,lat=0,time=0,sog=0)[tripid<0]
    for(k in (1:nrow(refer))){
      
      referTrip=refer[k]
      ln.add = lines[tripid == referTrip$tripid.ref & time1 > referTrip$time.from &time2 < referTrip$time.to]
      if(nrow(ln.add)>0){
        res=rbind(res,ln.add[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)],
                  ln.add[nrow(ln.add),list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)])
      }
      
    }
    
    p1=line[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)]
    p2=line[,list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)]
    refPoints=rbind(p1,res,p2)
    setkey(refPoints,lon)
    out1=data.table(lon=seq(min(line$g.lon1,line$g.lon2)+0.01,max(line$g.lon1,line$g.lon2)-0.01,by=0.01))
    setkey(out1,lon)
    #smooth.spline
    spl=smooth.spline(x=refPoints$lon,y=refPoints$lat)
    res1=predict(spl,x=out1$lon)
    #需新加点
    res1=data.table(res1$x,res1$y)
    setnames(res1,c('lon','lat'))
    #draw picture
    plot(x=refPoints$lon,y=refPoints$lat,main="regression and interpolation")
    lines(spl,col='blue')
    points(x=res1$lon,y=res1$lat,col='red')
    ###-----res1中lon需要单调-----
    #待加入数据的时间和sog
    setkey(res1,lon)
    d1=line$lon1-res1[1,lon]
    d2=line$lon1-res1[nrow(res1),lon]
    #调整次序
    if(d2<d1){
      setorder(res1,-lon)
    }
    #res1之间距离
    
    r1=res1[1:(nrow(res1)-1),list(lon1=lon,lat1=lat)]
    r2=res1[2:(nrow(res1)),list(lon2=lon,lat2=lat)]
    ln.add=cbind(r1,r2)
    ln.add=ln.add[,distance:=distance(lon1,lat1,lon2,lat2)]
    
    #line第一个点到res1第一个点的距离
    d.first=distance(line$lon1,line$lat1,res1[1,lon],res1[1,lat])
    d.last=distance(line$lon2,line$lat2,res1[nrow(res1),lon],res1[nrow(res1),lat])
    
    #总距离
    d=sum(ln.add$distance)
    #缺失线段时间间隔
    t1=line$timespan
    avgSpeed=(d+d.first+d.last)/t1
    tt=data.table(deltat=c(d.first,ln.add$distance)/avgSpeed)
    t2=tt
    for(k in (1:nrow(tt))){
      t2[k]$deltat=line$time1+sum(tt[1:k,deltat])
    }
    t2=t2[,sog:=avgSpeed]
    #get interpolated points
    pMiddle1=ln.add[,list(lon=lon1,lat=lat1)]
    pMiddle2=ln.add[nrow(ln.add),list(lon=lon2,lat=lat2)]
    temp=cbind(rbind(pMiddle1,pMiddle2),t2)
    temp=temp[,tripid:=referTrip$tripid.miss]
    setnames(temp,'deltat','time')
    interpolatedPoints=rbind(interpolatedPoints,temp) 
    
  }
  
}

results=rbind(lines[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)],
              lines[nrow(lines),list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)],interpolatedPoints)
setkey(results,tripid,time)

###--------------------method4: median point ------------------------------
interpolatedPoints=data.table(tripid=0,lon=0,lat=0,time=0,sog=0)
interpolatedPoints=interpolatedPoints[tripid<0]

for(m in (1:nrow(lines.miss))){
  line=lines.miss[m]
  #line=lines.miss[tripid==26&lid==74]
  dt1=refers[tripid.miss==line$tripid&lid.miss==line$lid]
  #dt1=refers[tripid.miss==26&lid.miss==74&tripid.ref>4]
  #line=lines[tripid==dt1[1]$tripid.miss&lid==dt1[1]$lid.miss]
  mediantime=abs(line$time2-line$time1)
  point.nm=data.table(tripid=0,lon=0,lat=0,time=0,sog=0,ntime=0)[tripid<0]
  if(nrow(dt1)>0){
    for(i in (1:nrow(dt1))){
      trip=dt1[i]
      projp1=trip[,list(lon=lon.from,lat=lat.from,time=time.from,sog=sog.from)]
      projp2=trip[,list(lon=lon.to,lat=lat.to,time=time.to,sog=sog.to)]
      ln.add=lines[tripid==trip$tripid.ref&time1>=projp1$time&time2<=projp2$time]
      point.add=rbind(ln.add[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)],ln.add[nrow(ln.add),list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)])
      #get normalized time
      setkey(add1,time)
      temp=point.add[,ntime:=(time-projp1$time)/(projp2$time-projp1$time)*mediantime]
      #加上两个端点，注意时间信息已经改变
      p1=line[,list(tripid=trip$tripid.ref,lon=lon1,lat=lat1,time=trip$time.from,sog=sog1,ntime=0)]
      p2=line[,list(tripid=trip$tripid.ref,lon=lon2,lat=lat2,time=trip$time.to,sog=sog2,ntime=mediantime)]
      point.nm=rbind(point.nm,p1,temp,p2)
      
    }
    
    nt=point.nm$ntime # the time to be interpolated with
    ntrips=nrow(dt1) # number of distinct subtrajs
    vpoints=point.nm[tripid<0,list(tripid,lon,lat,sog,ntime)]
    
    # linear interpolate
    for (i in (1:ntrips)){
      
      id=dt1[i,tripid.ref]
      trip=point.nm[tripid==id]
      alon=approx(x=trip$ntime,y=trip$lon,xout=nt)$y
      alat=approx(x=trip$ntime,y=trip$lat,xout=nt)$y
      asog=approx(x=trip$ntime,y=trip$sog,xout=nt)$y
      atrip=data.table(cbind(tripid=id,lon=alon,lat=alat,sog=asog,ntime=nt))
      vpoints=rbind(vpoints,atrip)
    }
    vpoints=distinct(vpoints)
    setkey(vpoints,ntime,tripid)
    
    mainRoute=vpoints[,list(lon=median(lon),lat=median(lat)),by=ntime]
    r1=mainRoute[1:(nrow(mainRoute)-1),list(lon1=lon,lat1=lat)]
    r2=mainRoute[2:(nrow(mainRoute)),list(lon2=lon,lat2=lat)]
    ln.add=cbind(r1,r2)
    ln.add=ln.add[,distance:=distance(lon1,lat1,lon2,lat2)]
    #总距离
    d=sum(ln.add$distance)
    #缺失线段时间间隔
    t1=line$timespan
    avgSpeed=d/t1*3600*10/1852
    mainRoute=mainRoute[,tripid:=line$tripid]
    mainRoute=mainRoute[,sog:=avgSpeed]
    mainRoute=mainRoute[,time:=ntime+line$time1]
    temp=mainRoute[2:(nrow(mainRoute)-1),list(tripid,lon,lat,time,sog)]
    interpolatedPoints=rbind(interpolatedPoints,temp)
  }
}

results=rbind(lines[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)],
              lines[nrow(lines),list(tripid,lon=lon2,lat=lat2,time=time2,sog=sog2)],interpolatedPoints)
setkey(results,tripid,time)


##-------------------------------------------------------------
dev.new()
p=ggplot()+geom_point(data=p1,aes(x=lon,y=lat))
p=p+geom_point(data=p2,aes(x=lon,y=lat))
p=p+geom_point(data=tmp,aes(x=lon,y=lat),color='red')
p=p+geom_point(data=point.nm,aes(x=lon,y=lat),color='green')
p



#------------------------

dev.new()
p = ggplot(data = lines[,list(tripid,lon=lon1,lat=lat1,time=time1,sog=sog1)])
p = p + geom_point(aes(x = lon,y = lat)) + facet_wrap(~ tripid)
p = p + geom_path(aes(x = lon,y = lat)) + facet_wrap(~ tripid)
p
##--------------------------
dev.new()
p = ggplot(data =results)
p = p + geom_point(aes(x = lon,y = lat)) + facet_wrap(~ tripid)
p = p + geom_path(aes(x = lon,y = lat)) + facet_wrap(~ tripid)

p

p = ggplot()
p = p + geom_path(data = points.new,aes(
  x = lon,y = lat,xend = lon,yend = lat,color = factor(tripid)
))
p = p + geom_segment(data = lines,aes(
  x = lon1,y = lat1,xend = lon2,yend = lat2,color = factor(tripid)
))
p = p + geom_point(data = lines[tripid %in% c(8,9,10) &
                                  sog2 > 30,],aes(
                                    x = lon1,y = lat1,color = factor(tripid)
                                  ))
p = p + geom_point(data = lines.miss,aes(
  x = lon1,y = lat1,color = factor(tripid)
))
p = p + geom_point(data = lines.miss,aes(
  x = lon2,y = lat2,color = factor(tripid)
))
p = p + geom_text(data = lines.miss,aes(
  x = lon2,y = lat2,label = factor(tripid)
),size = 4)
p = p + geom_text(data = lines.miss,aes(
  x = lon1,y = lat1,label = factor(tripid)
),size = 4)
p


#区别是新加的轨迹点还是原来的
#查看每条轨迹的插补情况
#利用多条轨迹推测两点间的距离
#利用referlist可以聚类分析轨迹的相似性，可以认为referlist为距离矩阵，
