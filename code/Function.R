
# The following two functions have already been tested
#经纬度转墨卡???
lonlat2mercator<-function(lon,lat){
  
  mercater.lon=lon*20037508.34/180;
  mercater.lat=log(tan((90+lat)*pi/360))/(pi/180);
  mercater.lat=mercater.lat*20037508.34/180;
  mercater=cbind(mercater.lon,mercater.lat);
  return(mercater);
  
}


#墨卡托转经纬???
mercator2lonlat<-function(mlon,mlat){
  
  lon=mlon/20037508.34*180;
  lat=mlat/20037508.34*180;
  lat=180/pi*(2*atan(exp(lat*pi/180))-pi/2);
  
  return (lonlat=cbind(lon,lat));
  
}
#default scale is 10
setPoints <- function(points,scale = 10) {
  n = nrow(points)
  
  points[,pid:= seq(1,n,1)]
  points[,g.lon:= floor(lon * scale) / scale]
  points[,g.lat:= floor(lat * scale) / scale]
  points[,gid:= paste(floor(lon * scale) / scale,floor(lat * scale) / scale,sep =
                         '_')]
  
  #setkey(points,time)
  
  return(points)
  
}

# points:pid,gid,lon,lat,time,...
# just add two points together


setLines <- function(points) {
  n = nrow(points)
  setkey(points,time)
  dt1 = points[1:(n - 1),list(mmsi,lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  dt2 = points[2:n,list(lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  lines = cbind(dt1,dt2)
  setnames(
    lines,c('mmsi','lon1','lat1','time1','sog1','pid1','gid1','g.lon1','g.lat1','lon2','lat2','time2','sog2','pid2','gid2','g.lon2','g.lat2'
    )
  )
  lines[,lid:= seq(1,n - 1)]
  
  return(lines)
  
}

#lines:data.table
#speed unit:knot(nm/h)

addLineSpeed <- function(lines,time_threshold = 600,dist_threshold = 2) {
  lines = lines[,timespan:= abs(time2 - time1) * 1.0]
  lines = lines[,distance:= distance(lon1,lat1,lon2,lat2)]
  lines[,avgspeed1:= round((sog1 + sog2) / 2,1)]
  lines[,avgspeed2:= round((distance / 1852) * 10 / (timespan / 3600))]# lavgspeed 与 sog 单位相同 海里/小时*10
  lines[,avgspeed:= avgspeed1];
  lines[(distance / 1852 > dist_threshold) |
          (timespan > time_threshold),avgspeed:= avgspeed2]
  return(lines)
  
}
#distance of two points,单位米
distance <- function(lon1,lat1,lon2,lat2) {
  radlat1 = rad(lat1);
  radlat2 = rad(lat2);
  delta_lon = rad(lon2 - lon1);
  top1 = cos(radlat2) * sin(delta_lon);
  top2 = cos(radlat1) * sin(radlat2) - sin(radlat1) * cos(radlat2) * cos(delta_lon);
  top = sqrt(top1 * top1 + top2 * top2);
  bottom = sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(delta_lon);
  delta_sigma = atan2(top,bottom);
  distance = delta_sigma * 6378137.0;
  return (distance);
  
}

#计算弧度
rad <- function(d) {
  return (d * pi / 180);
  
}

#time_threshold 位两相邻轨迹点之间的时间间隔，单位为天，默认值为3天 72 小时
#返回值为加入了tripid的lines

segTra <- function(l,time_threshold = 3) {
  setkey(l,lid)
  bl = l[timespan / 3600 / 24 >= time_threshold,list(lid,time1,time2,timespan)]
  lids = bl$lid
  if (nrow(bl) > 1) {
    l[,tripid:= 0]#分割的line的tripid=0
    for (i in (1:(nrow(bl) - 1))) {
      l[(lid > lids[i]) & (lid < lids[i + 1]),tripid:= (i + 1)]
    }
    
    #第一个和最后一个tripid
    l[(lid < lids[1]),tripid:= 1]
    l[(lid > lids[nrow(bl)]),tripid:= (nrow(bl) + 1)]
   
  }else if (nrow(bl) == 1) {
    l[,tripid:= 0]#分割的line的tripid=0
    #第一个和最后一个tripid
    l[(lid < lids[1]),tripid:= 1]
    l[(lid > lids[nrow(bl)]),tripid:= (nrow(bl) + 1)]

  }else{
    l[,tripid:= 1]
    
  }
  return(l)
}

getships <- function(shipfile) {
  #ships=fread('D:/Rprojects/ships/ships.txt',sep=',') # build year should be include
  ships = fread(shipfile,sep = ',')
  ships = ships[!is.na(mmsi)]
  # ships$mmsi<-as.character(ships$mmsi)
  setkey(ships,mmsi)
  return (ships)

}

getMap<-function(dt,zoomsize){
  

  lon=dt$lon
  lat=dt$lat
  
  centerX=0.5*(max(lon)+min(lon))
  centerY=0.5*(max(lat)+min(lat))
  
  p<-ggmap(get_googlemap(center=c(centerX,centerY),zoom=zoomsize))
  
  return(p)

}

gridALine<-function(line,scale=10){
  
  #   scale=10
  #   line=lines[2,]
  # line=lines[761,]
  timespan=abs(line$time2-line$time1)
  lid=line$lid
  
  #interpolate at grid.lon and grid.lat
  
  if(ceiling(min(line$lon1,line$lon2)*scale)/scale<floor(max(line$lon1,line$lon2)*scale)/scale){
    
    seq.lon=seq(ceiling(min(line$lon1,line$lon2)*scale)/scale,floor(max(line$lon1,line$lon2)*scale)/scale,by=1/scale)
    lon.interp.lat=approx(c(line$lon1,line$lon2),c(line$lat1,line$lat2),xout=seq.lon)$y
    lon.interp.time=approx(c(line$lon1,line$lon2),c(line$time1,line$time2),xout=seq.lon)$y
    lon.dt=data.table(cbind(seq.lon,lon.interp.lat,lon.interp.time))
  }else{
    lon.dt=line[lon1=='na',list(lon1,lat1,time1)] #get an empty data.table with 3
  }
  
  if(ceiling(min(line$lat1,line$lat2)*scale)/scale<floor(max(line$lat1,line$lat2)*scale)/scale){
    
    seq.lat=seq(ceiling(min(line$lat1,line$lat2)*scale)/scale,floor(max(line$lat1,line$lat2)*scale)/scale,by=1/scale)
    lat.interp.lon=approx(c(line$lat1,line$lat2),c(line$lon1,line$lon2),xout=seq.lat)$y
    lat.interp.time=approx(c(line$lat1,line$lat2),c(line$time1,line$time2),xout=seq.lat)$y
    lat.dt=data.table(cbind(lat.interp.lon,seq.lat,lat.interp.time))
    
  }else{
    lat.dt=line[lon1=='na',list(lon1,lat1,time1)] #get an empty data.table with 3
  }
  
  #remove duplicate points
  
  setnames(lon.dt,c('lon','lat','time'))
  setnames(lat.dt,c('lon','lat','time'))
  
  dt.inline=distinct(rbind(lon.dt,lat.dt))
  
  # add start and end point of the original line
  
  dt1=line[,list(lon1,lat1,time1)]
  dt2=line[,list(lon2,lat2,time2)]
  setnames(dt.inline,c('lon','lat','time'))
  setnames(dt1,c('lon','lat','time'))
  setnames(dt2,c('lon','lat','time'))
  points=rbind(dt1,dt.inline,dt2)
  setkey(points,time)
  
  #here the lines is grided line, each line only belong to an individual grid.
  
  points[,sog:=0]
  lines=setLines(setPoints(points,scale))
  
  # use average value to determine the grid where the line belongs to
  
  lines[,grid.x:=floor((lon1+lon2)/2*scale)/scale]
  lines[,grid.y:=floor((lat1+lat2)/2*scale)/scale]
  lines[,gid:=paste(grid.x,grid.y,sep='_')]
  
  lines[,percent:=abs(time2-time1)/timespan]
  lines[,lid:=line$lid]
  
  gridPercent=lines[,list(gid,grid.x,grid.y,percent,time1,lid)]
  
  #gridPercent:gid,grid.x,grid.y,percent
  
  return(gridPercent)
  
}


# get the percentage of line in each grid


gridLines<-function(lines,scale=10){
  
  gridPercent=data.table()
  
  for (i in (1:nrow(lines))) {
    if(i%%1000==0){
      print(i)
    }
    
    
    line=lines[i,]
    dt=gridALine(line,scale)
    gridPercent=rbind(dt,gridPercent)
    
    
  }
  
  return(gridPercent)
  
}

#emission of a ship

shipEmission<-function(ship,lines,mBaseEF,auxEF,boiEF){
  auxPowerdt = fread('data/auxpw.csv',sep=',',header = TRUE)
  boiPowerdt = fread('data/boilerpw.csv',sep=',',header = TRUE)
  #low load adjustment multipler
  llaFactordt = fread('data/LowLoadAdjustmentFactors.csv',sep=',',header = TRUE)
  sSpeed=ship$speed*10#service speed
  pw=ship$powerkw
  MCR=round(pw/0.9)
  DWT=ship$dwt
  #计算船舶在每种航速下的能耗
  em=lines[,list(.N,duration=sum(timespan)),list(speed=round(avgspeed))]
  em[,load.main:=round((speed*0.94/sSpeed)^3,2)]#load.main=main engine load factor
  # plot(em$load.main)
  #operation modes:1 at berth, 2 anchored, 3 manoeuvering, 4 slow-steaming, 5 normal cruising
  #imo 2014,p122
  em[,mode:=0]
  em[speed<10,mode:=1]
  em[speed>=1&speed<=30,mode:=2]
  em[speed>30&load.main<0.2,mode:=3]
  em[load.main>=0.2&load.main<=0.65,mode:=4]
  em[load.main>0.65,mode:=5]
  em[mode==3&load.main<0.02,load.main:=0.02]
  #e[mode==3&load.main*100<19.5&load.main*100>1.5,load.main:=0.2]
  
  em[,loadId:=100*load.main] # to join with low load factor table
  em[load.main>0.195|load.main<0.015,loadId:=20]#only load with in (0.02,0.2) need adject
  
  #----------------calculate emission factors------------------
  
  llaFactordt[,loadId:=Load]
  setkey(llaFactordt,loadId)
  setkey(em,loadId)
  em=data.table(left_join(em,llaFactordt[,list(loadId,CO2,PM2.5,SOx,NOx)],by='loadId'))
  setnames(em,c('loadId','speedid', 'segments','duration','load.main','mode','llaCO2','llaPM2.5','llaSOx','llaNOx'))
  
  #main engine emission:kw*n*g/kwh*n*s/3600/1000/1000: tons
  em[,meCO2:=MCR*load.main*mBaseEF$CO2*llaCO2*duration/3600/1000/1000]
  em[,mePM2.5:=MCR*load.main*mBaseEF$PM2.5*llaPM2.5*duration/3600/1000/1000]
  em[,meSOx:=MCR*load.main*mBaseEF$SOx*llaSOx*duration/3600/1000/1000]
  em[,meNOx:=MCR*load.main*mBaseEF$NOx*llaNOx*duration/3600/1000/1000]
  
  #-----IMO 2014 中辅机功率没有分SRZ和SEA两种模式，只是提供了一种在海模式的功率-----
  #-----如果要分这两种模式，可以参考port 2009中的处理方式---------------------------
  #------------aux engine-----------
  
  auxPower=auxPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  
  em[,aePM2.5:=0]
  em[,aeNOx:=0]
  em[,aeSOx:=0]
  em[,aeCO2:=0]
  
  em[mode==1,aePM2.5:=auxPower$Berth*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==1,aeNOx:=auxPower$Berth*auxEF$NOx*duration/3600/1000/1000]
  em[mode==1,aeSOx:=auxPower$Berth*auxEF$SOx*duration/3600/1000/1000]
  em[mode==1,aeCO2:=auxPower$Berth*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==2,aePM2.5:=auxPower$Anchorage*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==2,aeNOx:=auxPower$Anchorage*auxEF$NOx*duration/3600/1000/1000]
  em[mode==2,aeSOx:=auxPower$Anchorage*auxEF$SOx*duration/3600/1000/1000]
  em[mode==2,aeCO2:=auxPower$Anchorage*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==3,aePM2.5:=auxPower$Maneuvering*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==3,aeNOx:=auxPower$Maneuvering*auxEF$NOx*duration/3600/1000/1000]
  em[mode==3,aeSOx:=auxPower$Maneuvering*auxEF$SOx*duration/3600/1000/1000]
  em[mode==3,aeCO2:=auxPower$Maneuvering*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==5,aePM2.5:=auxPower$Sea*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==5,aeNOx:=auxPower$Sea*auxEF$NOx*duration/3600/1000/1000]
  em[mode==5,aeSOx:=auxPower$Sea*auxEF$SOx*duration/3600/1000/1000]
  em[mode==5,aeCO2:=auxPower$Sea*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==4,aePM2.5:=auxPower$Sea*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==4,aeNOx:=auxPower$Sea*auxEF$NOx*duration/3600/1000/1000]
  em[mode==4,aeSOx:=auxPower$Sea*auxEF$SOx*duration/3600/1000/1000]
  em[mode==4,aeCO2:=auxPower$Sea*auxEF$CO2*duration/3600/1000/1000]
  
  #------------boiler engine-----------
  
  boiPower=boiPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  
  em[,boPM2.5:=0]
  em[,boNOx:=0]
  em[,boSOx:=0]
  em[,boCO2:=0]
  
  em[mode==1,boPM2.5:=boiPower$Berth*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==1,boNOx:=boiPower$Berth*boiEF$NOx*duration/3600/1000/1000]
  em[mode==1,boSOx:=boiPower$Berth*boiEF$SOx*duration/3600/1000/1000]
  em[mode==1,boCO2:=boiPower$Berth*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==2,boPM2.5:=boiPower$Anchorage*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==2,boNOx:=boiPower$Anchorage*boiEF$NOx*duration/3600/1000/1000]
  em[mode==2,boSOx:=boiPower$Anchorage*boiEF$SOx*duration/3600/1000/1000]
  em[mode==2,boCO2:=boiPower$Anchorage*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==3,boPM2.5:=boiPower$Maneuvering*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==3,boNOx:=boiPower$Maneuvering*boiEF$NOx*duration/3600/1000/1000]
  em[mode==3,boSOx:=boiPower$Maneuvering*boiEF$SOx*duration/3600/1000/1000]
  em[mode==3,boCO2:=boiPower$Maneuvering*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==5,boPM2.5:=boiPower$Sea*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==5,boNOx:=boiPower$Sea*boiEF$NOx*duration/3600/1000/1000]
  em[mode==5,boSOx:=boiPower$Sea*boiEF$SOx*duration/3600/1000/1000]
  em[mode==5,boCO2:=boiPower$Sea*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==4,boPM2.5:=boiPower$Sea*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==4,boNOx:=boiPower$Sea*boiEF$NOx*duration/3600/1000/1000]
  em[mode==4,boSOx:=boiPower$Sea*boiEF$SOx*duration/3600/1000/1000]
  em[mode==4,boCO2:=boiPower$Sea*boiEF$CO2*duration/3600/1000/1000]
  em=em[,mmsi:=ship$mmsi]
  setkey(em,speedid)
  return(em)
  
}

shipProxy<-function(ship,points){
  
  sSpeed=ship$speed*10#service speed
  pw=ship$powerkw
  MCR=round(pw/0.9)
  DWT=ship$dwt
  auxPowerdt = fread('data/auxpw.csv',sep=',',header = TRUE)
  boiPowerdt = fread('data/boilerpw.csv',sep=',',header = TRUE)
  auxPower=auxPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  boiPower=boiPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  dt2=points#points为已经网格化的轨迹点
  
  dt2[,mp:=0]
  dt2[,ap:=0]
  dt2[,bp:=0]
  dt2[,load.main:=round((sog*0.94/sSpeed)^3,2)]
  dt2[,mp:=round((sog*0.94/sSpeed)^3*MCR)]
  #set ship status: 1 for berth,2for anchor,3for maneuvering,4for lowCruise,5for highCruise
  dt2[,mode:=0]
  dt2[sog<10,mode:=1]
  dt2[sog>=10&sog<=30,mode:=2]
  dt2[sog>30&load.main<0.2,mode:=3]
  dt2[load.main>=0.2&load.main<=0.65,mode:=4]
  dt2[load.main>0.65,mode:=5]
  
  dt2[mode==1,ap:=auxPower$Berth]
  dt2[mode==2,ap:=auxPower$Anchorage]
  dt2[mode==3,ap:=auxPower$Maneuvering ]
  dt2[mode==4,ap:=auxPower$Sea]
  dt2[mode==5,ap:=auxPower$Sea]
  
  dt2[mode==1,bp:=boiPower$Berth]
  dt2[mode==2,bp:=boiPower$Anchorage]
  dt2[mode==3,bp:=boiPower$Maneuvering ]
  dt2[mode==4,bp:=boiPower$Sea]
  dt2[mode==5,bp:=boiPower$Sea]
  
  dt2[mode==3&load.main<0.02,load.main:=0.02]
  dt2[,tp:=(mp+ap+bp)]
  proxy=dt2[,list(idx=sum(tp)/sum(dt2$tp)),list(gid,g.lon,g.lat)]
  return(proxy)
 
}

#GeoDistanceInMetresMatrix 
ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}

#from long tra compression

#note:no solution to the case that a ship could not find a refer ship btn (-250,+250)
getSpeedandPower<-function(ships.global,ships.local){
  ships1=data.table(mmsi=0,speed=0,powerkw=0)[mmsi<0]
  for (i in (1:nrow(ships.local))){
    
    ship=ships.local[i]
    
    referships=ships.global[type_en=='Container'&dwt<=ship$CARRYING_CAPACITY+250&dwt>=ship$CARRYING_CAPACITY-250]
    referships=referships[!is.na(speed)&!is.na(powerkw)]
    print(nrow(referships))
    temp=data.table(mmsi=ship$mmsi,speed=round(median(referships$speed)),powerkw=round(median(referships$powerkw)))
    ships1=rbind(ships1,temp)
  
  }
  
  return(ships1)
  
}
#use boxplot to remove trips with abnormal trip duration
getNormalTrips<-function(trips){
  
  res1=quantile(log10(trips$duration),probs = c(0.25,0.75),na.rm = TRUE)
  Qlow=res1[[1]]
  Qhigh=res1[[2]]
  IQR=Qhigh-Qlow
  max=Qhigh+1.5*IQR
  min=Qlow-1.5*IQR
  res=trips[log10(duration)<=max&log10(duration)>=min]
  return(res)
  
}


#get lines of multiple ships
getShipsLines<-function(points,gridscale=1000){
  
  #prepare ais msgs, mainly lines
  # points=ais.allships
  setkey(points,mmsi,time)
  mmsis=points[,.N,by=mmsi]$mmsi
  res=data.table( lon1=0,lat1=0,time1=0,sog1=0,pid1=0,gid1=0,g.lon1=0,g.lat1=0,
                  lon2=0,lat2=0,time2=0,sog2=0,pid2=0,gid2=0,g.lon2=0,g.lat2=0,
                  lid=0,timespan=0,distance=0,avgspeed1=0,avgspeed2=0,avgspeed=0,mmsi=0)[lid<0]
  for (mm in mmsis){
    
    points1=points[mmsi==mm,list(lon,lat,time,sog)]
    scale=gridscale #0.001*0.001 grids
    points1=setPoints(points1,scale)
    lines=setLines(points1)
    lines=addLineSpeed(lines,150,0.5)
    
    res=rbind(res,lines[,mmsi:=mm])
  }
  return(res)
}

#segment ships lines

breakTrajs<-function(shipsLines,timespan_threshold=12*3600){
  
  shipsLines[,tpid:=1]
  shipsLines[timespan>=timespan_threshold,tpid:=0]
  breakLines=shipsLines[tpid==0]
  mmsis=breakLines[,.N,by=mmsi]$mmsi
  
  for(id in mmsis){
    shipbreaks=breakLines[mmsi==id]
    for (i in (1:(nrow(shipbreaks)-1))){
      line1=shipbreaks[i]
      line2=shipbreaks[i+1]
      shipsLines[mmsi==id&lid>line1$lid&lid<line2$lid,tpid:=(i+1)]
    }
    shipsLines[mmsi==id&lid>line2$lid,tpid:=(i+2)]
  }
  return(shipsLines[tpid>0])
}

getRealTrips<-function(shipsTrips,goodTrips){
  
  trippoint=goodTrips[,list(from=min(time),to=max(time)),list(mmsi,tripid)]
  res=shipsTrips[mmsi<0]
  
  #res=data.table(mmsi=0,tpid=0,tripid=0)[mmsi<0]
  for (i in (1:nrow(trippoint))){
    print(i)
    
    tp=trippoint[i]
    #res1=shipsTrips[mmsi==tp$mmsi&time1==tp$time,list(mmsi,tpid)]
    res1=shipsTrips[mmsi==tp$mmsi&time1>=tp$from&time1<=tp$to,.N,by=list(mmsi,tpid)]
    if(nrow(res1)>0){
      for(j in (1:nrow(res1))){
        
        temp=shipsTrips[mmsi==res1[j]$mmsi&tpid==res1[j]$tpid]
        res=rbind(res,temp)
        
      }
      
    }
  }
  setkey(res,mmsi,tpid,lid)
  return(res)

}

lineIntersection<-function(line1,lon){
  
  #Ax+BY+C=0
  A=line1$lat2-line1$lat1
  B=line1$lon1-line1$lon2
  C=line1$lon2*line1$lat1-line1$lon1*line1$lat2
  
  lat=(-A*lon-C)/B
  
  return(data.table(lon=lon,lat=lat))
  
}

#boundaryPoint of a trip
#here trips are lines, 如果输入时points也是可以通过适当调整程序完成
boundaryPoint<-function(trips){
  
  leftlon=118.90
  rightlon=119.15
  res=data.table(lon=0,lat=0,time=0,sog=0,tripid=0,mmsi=0,into=0,out=0)[mmsi<0]
  tripids=trips[,.N,tripid]$tripid
  #1 for left and 2 for right
  infrom=1
  outfrom=2
  for(id in tripids){
    
    print(id)
    
    atrip=trips[tripid==id]
    #当firstline和lastline两个点的位置相同时，系统自动寻找到两个位置点不同的直线段进行计算
    firstline=atrip[distance>0][1]
    f2l=abs(mean(firstline$lon1,firstline$lon2)-leftlon)
    f2r=abs(mean(firstline$lon1,firstline$lon2)-rightlon)
    lastline=atrip[distance>0][nrow(atrip[distance>0])]
    l2l=abs(mean(lastline$lon1,lastline$lon2)-leftlon)
    l2r=abs(mean(lastline$lon1,lastline$lon2)-rightlon)
    
    if(firstline$sog1>=20){
      
      if(f2l>=f2r){
        firstpoint=lineIntersection(firstline,rightlon)
        #偏的太大需要纠正
        firstpoint[lat<32.245|lat>32.255,lat:=32.250]
        infrom=2
        
      }else{
        firstpoint=lineIntersection(firstline,leftlon)
        firstpoint[lat<32.180|lat>32.190,lat:=32.185]
        infrom=1
      }
      firstpoint[,sog:=firstline$sog1]
      #如果sog=0怎么办？另外，如果最后一个点不在边界周边在泊位处怎么办
      #1. 要限制点不在泊位处，2 限制航速至少>20
      deltat1=distance(firstpoint$lon,firstpoint$lat,firstline$lon1,firstline$lat1)*3600*10/(firstline$sog1*1852)
      firstpoint[,time:=firstline$time1-round(deltat1)]
      
    }
    
    #for last line
    if(lastline$sog2>=20){
      
      if(l2l>=l2r){
        lastpoint=lineIntersection(lastline,rightlon)
        lastpoint[lat<32.245|lat>32.255,lat:=32.250]
        outfrom=2
      }else{
        lastpoint=lineIntersection(lastline,leftlon)
        lastpoint[lat<32.180|lat>32.190,lat:=32.185]
        outfrom=1
      }
      lastpoint[,sog:=lastline$sog2]
      deltat2=distance(lastpoint$lon,lastpoint$lat,lastline$lon2,lastline$lat2)*3600*10/(lastline$sog2*1852)
      lastpoint[,time:=lastline$time2+round(deltat2)]
      
      
    }
    
    res1=rbind(firstpoint[,list(lon,lat,time,sog)],atrip[,list(lon=lon1,lat=lat1,time=time1,sog=sog1)],
               atrip[nrow(atrip),list(lon=lon2,lat=lat2,time=time2,sog=sog2)],lastpoint[,list(lon,lat,time,sog)])
    res1=res1[,tripid:=atrip[1]$tripid]
    res1=res1[,mmsi:=atrip[1]$mmsi]
    res1=res1[,into:=infrom]
    res1=res1[,out:=outfrom]
    res=rbind(res,res1)
  }
  
  return(res)
}

