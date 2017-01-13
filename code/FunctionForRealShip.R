
#functons for calculate a real ship's oil consumption and emissions

# The following two functions have already been tested
#经纬度转墨卡托
lonlat2mercator<-function(lon,lat){
  
  mercater.lon=lon*20037508.34/180;
  mercater.lat=log(tan((90+lat)*pi/360))/(pi/180);
  mercater.lat=mercater.lat*20037508.34/180;
  mercater=cbind(mercater.lon,mercater.lat);
  return(mercater);
  
}

#墨卡托转经纬度
mercator2lonlat<-function(mlon,mlat){
  
  lon=mlon/20037508.34*180;
  lat=mlat/20037508.34*180;
  lat=180/pi*(2*atan(exp(lat*pi/180))-pi/2);
  
  return (lonlat=cbind(lon,lat));
  
}

#计算两点之间的球面距离-郑宇提供
# The code of GeoDistance function:
# Input: Two coordination {lon1, lat1, lon2, lat2 } 
# Output: Distance(Unit: m) 输出单位为米

distance<-function(lon1,lat1,lon2,lat2){
  
  radlat1=rad(lat1);
  radlat2=rad(lat2);
  delta_lon=rad(lon2-lon1);
  top1=cos(radlat2)*sin(delta_lon);
  top2=cos(radlat1)*sin(radlat2)-sin(radlat1)*cos(radlat2)*cos(delta_lon);
  top=sqrt(top1*top1+top2*top2);
  bottom=sin(radlat1)*sin(radlat2)+cos(radlat1)*cos(radlat2)*cos(delta_lon);
  delta_sigma=atan2(top,bottom);
  distance=delta_sigma*6378137.0;
  return (distance);
  
}

#计算弧度
rad<-function(d){
  
  return (d*pi/180);
  
}
#change timestamp to string date YYYY-MM-DD HH:MM:SS
timestamp2date<-function(timestamp){
  date=as.character(as.POSIXct(timestamp,origin="1970-01-01"));
  return(date);
}

#input 参数df必须为四列time,sog,lat,lon;
#output:"t1","sog1","lat1","lon1","t2","sog2","lat2","lon2","distance","duration",
#"avgspeed","mlon1","mlat1","mlon2","mlat2","date1","date2"

#time_threshold(s)[1200] and dist_threshold(nm)[4] for calculating average speed

getlines<-function(df,time_threshold,dist_threshold){ 
  
  df=data.table(df);
  setkey(df,time)
  rownum=nrow(df);
  df=df[,lid:=seq(1:rownum)]
  df=df[,lavgspeed:=0]
  df=df[,avgspd1:=0]
  df=df[,avgspd2:=0]
  df=df[,ltimespan:=0]
  df=df[,ldistance:=0]
  df=df[,lon2:=0]
  df=df[,lat2:=0]
  
  if (rownum>1){
    
    df1=df[1:(rownum-1)];
    df2=df[2:rownum];
    distance=distance(df1$lon,df1$lat,df2$lon,df2$lat);
    timespan=abs(df2$time-df1$time)*1.0;
    
    df[1:(rownum-1),lon2:=df2$lon]
    df[1:(rownum-1),lat2:=df2$lat]
    
    df[1:(rownum-1),ltimespan:=timespan]
    df[1:(rownum-1),ldistance:=distance]
    
    aspeed1=round((df1$sog+df2$sog)/2,1)  
    df[1:(rownum-1),lavgspeed:=aspeed1];
    
    aspeed2=round((distance/1852)*10/(timespan/3600),1)# lavgspeed 与 sog 单位相同
    
    df[1:(rownum-1),avgspd1:=aspeed1]
    df[1:(rownum-1),avgspd2:=aspeed2]
    
    index=df[ldistance/1852>dist_threshold|ltimespan>time_threshold,which=TRUE] 
    df[index,lavgspeed:=aspeed2[index]]
    
    
  }
  return(df)
}

# 主机载荷保存小数点后两位。
mloadfactor<-function(aspeed,dspeed){
  load=(aspeed/dspeed)^3;
  load=round(load,2);
  return(load);
}

#default value:loadPercent=90%, weatherFactor=1.1,fullFactor=1.09
mLoadFactor2<-function(aSpeed,sSpeed,loadPercent,weatherFactor,hullFactor){
  load=(aSpeed*0.95/sSpeed)^3*loadPercent^(2/3)*weatherFactor*hullFactor
  load=round(load,2)
  return(load)
}


co2llafactor<-function(mloadfactor,llatable){
  rownum=length(mloadfactor);
  ef=0;
  if (rownum>1){
    for (i in 1:rownum){
      ef[i]=llatable[which(llatable$load==mloadfactor[i]),]$CO2;
      
    }
  }else{
    
    ef=llatable[which(llatable$load==mloadfactor),]$CO2;
    
  }
  
  return(ef);
  
}

#油耗调整系数

BSFCllafactor<-function(mloadfactor,llatable){
  
  rownum=length(mloadfactor);
  ef=0;
  
  if (rownum>1){
    for (i in 1:rownum){
      ef[i]=llatable[which(llatable$load==mloadfactor[i]),]$BSFC;
      
    }
  }else{
    
    ef=llatable[which(llatable$load==mloadfactor),]$BSFC;
    
  }
  
  return(ef);
  
}

# 将直线分割到网格中

gridline<-function(lon1,lat1,lon2,lat2){
  
  grid=data.frame(0,0);# 存放每个网格的内容，percentage 和 gridid（lon_lat)
  colnames(grid)=c("percent","gridid");
  scale=10#0.01X0.01 grid;
  
  lon1=lon1*scale;
  lon2=lon2*scale;
  lat1=lat1*scale;
  lat2=lat2*scale;
  
  maxlat=max(lat1,lat2);
  minlat=min(lat1,lat2);
  maxlon=max(lon1,lon2);
  minlon=min(lon1,lon2);
  
  latspan=floor(maxlat)-floor(minlat);
  lonspan=floor(maxlon)-floor(minlon);
  
  
  if (lon1==lon2){
    
    
    distance=abs(lat1-lat2);
    
    if (floor(lat1)==floor(lat2)){
      
      percent=1;
      gridid=paste(floor(lon1),floor(lat1),sep="_");
      grid=rbind(grid,c(percent,gridid));
      
      
    }else {
      
      
      distance1=maxlat-floor(maxlat);
      gridid1=paste(floor(lon1),floor(maxlat),sep="_");
      percent1=distance1/distance;
      
      distance2=ceiling(minlat)-minlat;
      print(distance2);
      gridid2=paste(floor(lon1),floor(minlat),sep="_");
      percent2=distance2/distance;
      
      if (distance1!=0){
        
        grid=rbind(grid,c(percent1,gridid1));
        
      }
      if (distance2!=0){
        
        grid=rbind(grid,c(percent2,gridid2));
        
      }
      
      
      bottom=ceiling(minlat);
      top=floor(maxlat);
      count=top-bottom;
      
      
      if (count>0){
        
        for (i in 1:(count)){
          
          
          percent=1/distance;
          gridid=paste(floor(lon1),(floor(minlat)+i),sep="_");
          grid=rbind(grid,c(percent,gridid));
          
        }
        
      }
      
    }
    
    
  }
  
  if(lon1!=lon2){
    
    
    
    #get the line equation y=kx+b
    k = (lat1-lat2)/(lon1-lon2);
    b = lat1-k*lon1;
    lats=0;
    lons=0;
    flag=FALSE;#是否有跨网格
    
    #get the lat direct crosses
    
    if (latspan > 0) {
      flag=TRUE;
      for (j in 1:latspan) {
        
        lats[j]=floor(minlat)+j;
        lons[j]=(lats[j]-b)/k; 
      }
    }
    
    #get the lon direct crosses
    if (lonspan> 0) {
      flag=TRUE;
      for (j in 1:lonspan) {
        
        lons[j+latspan] = floor(minlon)+j;
        lats[j+latspan] = k*lons[j+latspan]+b;
      }
    }
    
    if(flag){
      
      points=cbind(lons,lats);
      points=rbind(points,c(lon1,lat1),c(lon2,lat2));#需要将两个端点的数据也加入
      
    }else{
      
      points=rbind(c(lon1,lat1),c(lon2,lat2));#需要将两个端点的数据也加入
      
    }
    
    points=points[order(points[,1],decreasing=FALSE),];
    
    # create gridIds
    rownum=nrow(points);
    for (i in 1:rownum) {
      
      midPointLon = 0.5 * (points[,1][i] + points[,1][i + 1]);
      midPointLat = 0.5 * (points[,2][i] + points[,2][i + 1]);
      percent =abs(points[,1][i + 1] - points[,1][i])/abs(lon1-lon2);
      gridid=paste(floor(midPointLon),floor(midPointLat),sep="_");
      grid=rbind(grid,c(percent,gridid));
      
      
    }
    
  }
  
  return(grid[which(grid$percent>0),]);
  
  
}

getgrids<-function(lines){
  
  rownum=nrow(lines);
  
  grids=0;
  
  for (i in 1:rownum){
    line=lines[i,];
    grid=gridline(line$lon1,line$lat1,line$lon2,line$lat2);
    grid$FC=as.numeric(grid$percent)*line$totalFC;
    grid$co2E=as.numeric(grid$percent)*line$totalco2E;
    grids=rbind(grids,grid);  
    
  }
  
  grids=grids[which(grids$percent>0),];
  
  #将相同网格中的数据合并
  
  grids=aggregate(grids[,3:4], by=list(grids$gridid), FUN=sum);# 分grid 求和
  colnames(grids)=c("gridid","FC","co2E");
  
  return(grids);
}

#copy from another document-------------------------------------
#compute the distance of two points based on algorithm of google map
#results in meters

earthR=6378137; #meters
rad<-function(d){  
  return (d * pi/180);   
}   

getDist<-function(lon1,lat1,lon2,lat2){
  
  radLat1=rad(lat1);   
  radLat2 = rad(lat2);   
  a = radLat1-radLat2;   
  b = rad(lon1) - rad(lon2);   
  s = 2*asin(sqrt((sin(a/2)^2) + cos(radLat1)*cos(radLat2)*(sin(b/2)^2)));   
  s = s*earthR;   
  s = round(s*10000)/10000;   
  return(s);   
  
}


#------------------------------------------------------------------
#CO2 emissions

getMBaseFactor<-function(df,engineType,oil,airType){
  
  eFactor=subset(df,df$engine==engineType&df$fuel==oil);
  return(eFactor[,airType]);
  
}

getMainLoad<-function(aSpeed,sSpeed){
  
  load=(aSpeed/(sSpeed/0.95))^3;
  return(load) 
}

getLLACoefs<-function(df,airType){
  
  a=subset(df,df$engine=="Main_Coefficient_a");
  x=subset(df,df$engine=="Main_Coefficient_x");
  b=subset(df,df$engine=="Main_Coefficient_b");
  
  results=rbind(a,x);
  results=rbind(results,b);
  return(results[,airType]);
  
}

getMFactor<-function(df,engineType,oil,airType,load){
  
  bf=getMBaseFactor(df,engineType,oil,airType);
  coef=getLLACoefs(df,airType);
  a=coef[1];
  x=coef[2];
  b=coef[3];
  
  lla=load;
  
  for( i in 1:length(load)){
    
    
    if (load[i]<=0.2&&load[i]>=0.02){	
      lla[i]=(a*(load[i])^(-x)+b)/(a*(0.2)^(-x)+b);
    }else {
      lla[i]=1;
    }
    
  }
  
  return(bf*lla);
}


getMainEmission<-function(power,load,time,ef){
  
  e=load*time*ef*power
  return(e)
  
}

getAuxFactor<-function(df,engineType,oil,airType){
  
  aFactors=subset(df,df$engine==engineType&df$fuel==oil);
  return(aFactors[,airType]);
  
}

getAuxLoad<-function(df,vesselType,aSpeed){
  
  load=subset(df,df$shipType==vesselType);
  if (nrow(load)<1){
    
    load[1,]=c(rep(0,length(load)));
    
  }
  
  #----------------------------------------
  aload=aSpeed;
  cruise=load$cruise;
  rsz=load$RSZ;
  maneuver=load$maneuver;
  hotel=load$hotel;
  
  for (i in 1:length(aSpeed)){
    
    if (aSpeed[i]<=1){
      aload[i]=hotel;
    } else if (aSpeed[i]>1&&aSpeed[i]<=8){
      
      aload[i]=maneuver;
      
    }else if (aSpeed[i]>8&&aSpeed[i]<=12){
      
      aload[i]=rsz;
    }else {
      aload[i]=cruise;
    }
    
  }
  
  return(aload);
}


getAuxPower<-function(df,vesselType,mainPower){
  
  load=subset(df,df$shipType==vesselType);
  power=0;
  if (nrow(load)>0){
    
    power=load$powerRatio*mainPower;
    
  }
  return(power);
}

getAuxEmission<-function(power,load,time,ef){
  
  e=power*load*time*ef;
  
  return(e);
  
}


#----------------------------------------------------------------------------
#calculate emissions, currently only for CO2
#

mLoad<-function(aSpeed,sSpeed){
  load=(aSpeed/(sSpeed/0.95))^3;
  return(load)
}

#baseFactor,a,b,x

mFactor<-function(load){
  
  x=1;
  a=44.1;
  b=648.6;
  baseFactor=677.91;
  lla=load;
  
  for( i in 1:length(load)){
    
    
    if (load[i]<=0.2&&load[i]>=0.02){	
      lla[i]=(a*(load[i])^(-x)+b)/(a*(0.2)^(-x)+b);
    }else {
      lla[i]=1;
    }
    
  }
  
  return(baseFactor*lla);
}

mEmission<-function(hours,aSpeed,sSpeed,mPower){
  
  mload=mLoad(aSpeed,sSpeed);
  mfactor=mFactor(mload);
  
  return(hours*mPower*mload*mfactor)
}

aLoad<-function(aSpeed){
  
  aload=aSpeed;
  
  for (i in 1:length(aSpeed)){
    
    if (aSpeed[i]<=1){
      aload[i]=0.13;
    } else if (aSpeed[i]>1&&aSpeed[i]<=8){
      
      aload[i]=0.25;
      
    }else if (aSpeed[i]>8&&aSpeed[i]<=12){
      
      aload[i]=0.48;
    }else {
      aload[i]=0.19;
    }
    
  }
  
  
  
  return(aload);
  
}

#aFactor,powerRatio

aEmission<-function(hours,aSpeed,mPower){
  
  powerRatio=0.22;
  aFactor=722.54;
  load=aLoad(aSpeed);
  e=hours*load*mPower*powerRatio*aFactor;
  
  return(e)
  
}

getEmission<-function(hours,aSpeed,sSpeed,mPower){
  
  me=mEmission(hours,aSpeed,sSpeed,mPower);
  ae=aEmission(hours,aSpeed,mPower);
  return(me+ae);
  
}

#-------------------------------------------------------------------------------------------------------
# show trajectory on map

#df: dataframe data, lat: label of latitude in df, lon: lable of longitude in df ,zoomSize:zoom size of map

plotTraj<-function(df,lon,lat,zoomSize){
  
  library(ggmap,ggplot2)
  centerX=0.5*(max(as.numeric(df$lon))+min(as.numeric(df$lon)))
  centerY=0.5*(max(as.numeric(df$lat))+min(as.numeric(df$lat)))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google'))
  
  p<-p+geom_path(data=df,aes(x=lon,y=lat),size=0.7,color='green', alpha=1)
  
  p<-p+geom_point(data=df,aes(x=lon,y=lat),size=1,color='red', alpha=0.7)
  
  p
  
  
}


plotTraj2<-function(dt,lon,lat,zoomSize){
  
  library(ggmap,ggplot2)
  centerX=0.5*(max(as.numeric(dt$lon))+min(as.numeric(dt$lon)))
  centerY=0.5*(max(as.numeric(dt$lat))+min(as.numeric(dt$lat)))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google'))
  
  #p<-p+geom_path(data=dt,aes(x=lon,y=lat),size=0.7,color='green', alpha=1)
  
  p<-p+geom_point(data=dt,aes(x=lon,y=lat),size=1,color='red', alpha=0.7)
  p
  
  
}


#plot multiple ships trajectroy

plotMultiTraj<-function(df,lon,lat,zoomSize,shipList){
  
  library(ggmap,ggplot2)
  centerX=0.5*(max(as.numeric(df$lon))+min(as.numeric(df$lon)))
  centerY=0.5*(max(as.numeric(df$lat))+min(as.numeric(df$lat)))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google'))
  
  for(i in 1:nrow(shipList)){
    
    ship=shipList[i,]
    points=subset(df,df$mmsi==ship$mmsi)
    rownum=nrow(points)
    
    if (rownum>2){
      
      p<-p+geom_path(data=points,aes(x=lon,y=lat),size=1.2,color='green', alpha=0.5);
      
    }
    
  }
  
  p<-p+geom_point(data=df,aes(x=lon,y=lat),size=1,color='red', alpha=0.5)
  p
  
  
}


# 在题图上画轨迹能耗和排放的空间分布
#enlarge 为网格放大倍数 enlarge=100时，网格为 0.01X0.01,
#df colnames：lon,lat,fuel,emission.lon,lat都是已经被放大的值,为网格的周下角坐标,fillcol表示要显示颜色的值

spatialDistribution<-function(df,scale,zoomsize){ 
  
  library(ggmap,ggplot2)
  centerX=0.5*(max(df$lon)+min(df$lon))/scale
  centerY=0.5*(max(df$lat)+min(df$lat))/scale
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomsize,source='google'))
  p<-p+ geom_rect(data=df, aes(xmin =lon/10, xmax =(lon+1)/10, ymin = lat/10, ymax = (lat+1)/10,fill=fuel))
  p<-p+scale_fill_gradient(low="yellow", high="red")
  p 
  
}

#ggplot(df, aes(xmin = lon10/10, xmax = lon10/10+0.1, ymin = lat10/10, ymax = lat10/10+0.1,fill=mainEmission)) + geom_rect()+scale_fill_gradient(low="red", high="yellow")

#--------------------------------------------------------------------------------
#plot a ship's trajectory and ports,save the plot picture as well 
#df is a traject of a ship or ships, it must include a lon and lat column

drawTrajPort<-function(df,lon,lat,zoomSize,ports,mmsi,savePath){
  
  library('dplyr','ggmap','ggplot2')
  
  lonMax=max(df$lon);lonMin=min(df$lon);
  latMax=max(df$lat);latMin=min(df$lat);
  ports=subset(ports,ports$lon<lonMax+0.01&ports$lon>lonMin-0.01&ports$lat<latMax+0.01&ports$lat>latMin-0.01)
  df=distinct(df,lon,lat) # remove overlapped points
  # df.berth=subset(df,df$status==5&df$speed==0);#points at berthes
  
  df.berth=subset(df,df$speed==0);#points at berthes
  
  #plot points on maps
  centerX=0.5*(max(df$lon)+min(df$lon))
  centerY=0.5*(max(df$lat)+min(df$lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google'))
  p<-p+geom_path(data=df,aes(x=lon,y=lat),size=0.7,color='green', alpha=1)
  p<-p+geom_point(data=df,aes(x=lon,y=lat),size=0.7,color="yellow",alpha=0.7)
  p<-p+geom_point(data=ports,aes(x=lon,y=lat),size=1.5,color="blue",alpha=1)
  p<-p+geom_point(data=df.berth,aes(x=lon,y=lat),size=4,color="red",alpha=0.5,shape=2)
  p
  
  ggsave(filename=paste(mmsi,'.png',sep=''),scale=3,path=savePath)
  
}


#-------------------------------------------------------------------------------
# calcultate geo matrx dist

#Here is my program to compute geodetic inter-site distance matrix. Input requires *vectors* in degrees. 

geodetic.distance.matrix <- function (long,lat) {  	
  
  NSITES <- length(lat)
  R <- 6378137.0
  
  latitude <- lat	
  longitude <- long	
  latlong <- cbind(latitude, longitude)*pi/180	
  
  d <- matrix(nrow=NSITES, ncol=NSITES)	
  
  for(i in 1:(NSITES-1)) {	
    d[i,i] <- 1.0		
    for (j in (i+1):NSITES) {					
      d[i,j] <- sin(latlong[i,1]) * sin(latlong[j,1]) + 			cos(latlong[i,1]) * cos(latlong[j,1])	*				cos(abs(latlong[i,2] - latlong[j,2]))
      d[j,i] <- d[i,j]
    }	
  }
  d[NSITES, NSITES] <- 1.0
  d <- R*acos(d)
  d
  
}

#Here is another version of the above program - with a dataframe as input, having lat and lon as components.
geodetic.distance.dataframe <- function(dataframe, lat, lon) {	
  NSITES <- nrow(dataframe)
  
  latitude <- dataframe$lat	
  longitude <- dataframe$lon	
  
  latlong <- cbind(latitude, longitude)*pi/180	
  
  d <- matrix(nrow=NSITES, ncol=NSITES)
  
  for(i in 1:(NSITES-1)) {		
    d[i,i] <- 1.0
    for (j in (i+1):NSITES) {			
      d[i,j] <- sin(latlong[i,1])*sin(latlong[j,1]) + cos(latlong[i,1]) * cos(latlong[j,1])*cos(abs(latlong[i,2] - latlong[j,2]))			
      #d[j,i] <- d[i,j]		
    }	
  }
  
  d[NSITES, NSITES] <- 1.0
  d <- acos(d)
  
  for (i in 1: NSITES) { 	
    for (j in i:NSITES) { 		
      if (d[i,j] < 0.00000000001) {d[i,j] <- 0}
      d[j,i] <- d[i,j]	 	
    }
  }
  d <- ifelse (d < 0.000000000001, 0.0, d)
  R <- 6378137.0
  d <- R*d
  d
  
}

#dt must have a lon and lat column

getMap<-function(dt,zoomsize){
  library('ggmap')
  
  dt=dt
  lon=dt$lon
  lat=dt$lat
  
  centerX=0.5*(max(lon)+min(lon))
  centerY=0.5*(max(lat)+min(lat))
  
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomsize,source='google'))
  
  return(p)
  
  
  
}

dtplot<-function(dt){
  library('ggplot2')
  p=ggplot(data=dt,aes(x=lon,y=lat))
  p<-p+geom_point(data=dt,aes(x=lon,y=lat),size=2,color="red",alpha=0.7)
  p<-p+geom_path(data=dt,aes(x=lon,y=lat),size=0.5,color="green",alpha=0.7)
  return(p) 
}

# dt must include a mmsi,time,lon, lat and sog columns
#timespan(default 3600): combine two points (sog==0) whose timespan is less than timespan (s) 
# distspan(default 10000): combine two points(sog==0) whose distance is less than disttime (m)

getstops<-function(df,timespan,distspan){
  
  df.len=nrow(df)
  df.stops=df[sog==0]
  df.stops.len=nrow(df.stops)
  rownumber=which(df$sog==0)
  rowtype=rownumber*0 #a flag of the row, o for candidate stops and 1 for stops
  if (df.stops.len>1){
    timesp=df.stops[2:df.stops.len]$time-df.stops[1:df.stops.len-1]$time
    distsp=distance(df.stops[2:df.stops.len]$lon,df.stops[2:df.stops.len]$lat,df.stops[1:df.stops.len-1]$lon,df.stops[1:df.stops.len-1]$lat)
    # time and distance btm two stops should bigger than 3600(s) and 5000(m)
    timeflag=timesp>timespan
    distflag=distsp>distspan
    #issamemmsi=df.stops[2:df.stops.len]$mmsi==df.stops[1:df.stops.len-1]$mmsi 
    rowtype[which(distflag&timeflag)]=1 
    
    stoprows=data.table(cbind(rownumber,rowtype))[rowtype==1,rownumber]
    
    #classified the df with stoppoint and not
    
    df[stoprows,isstop:=1]
    
  }
  
  return(df)
  
}

getsubtraj<-function(df){
  len=nrow(df[isstop==1])
  stop.rownum=df[isstop==1,which=TRUE]
  
  if (len>1){
    
    for (i in 1:(len-1)){
      df[stop.rownum[i]:(stop.rownum[i+1]-1),subtraj:=i]
    } 
  }
  
  return(df)
}

# use stay points
getsubtraj2<-function(df){
  len=nrow(df[stayflag>0])
  df.len=nrow(df)
  stop.rownum=df[stayflag>0,which=TRUE]
  df[,startclst:=0]
  df[,endclst:=0]
  if (len>1){
    
    for (i in 1:(len-1)){
      
      
      df[stop.rownum[i]:(stop.rownum[i+1]-1),subtraj:=i]
      df[stop.rownum[i]:(stop.rownum[i+1]-1),startstay:=df[stop.rownum[i]]$stayflag]
      df[stop.rownum[i]:(stop.rownum[i+1]-1),endstay:=df[stop.rownum[i+1]]$stayflag]
      df[stop.rownum[i]:(stop.rownum[i+1]-1),startclst:=df[stop.rownum[i]]$cluster]
      df[stop.rownum[i]:(stop.rownum[i+1]-1),endclst:=df[stop.rownum[i+1]]$cluster]
      
    } 
  }
  
  return(df)
}
#calculate the distance of two sequent points for a single ship and identify the miss traj

getmisstraj<-function(df,threshold){
  df.len=nrow(df)
  setkey(df,time)
  
  if (df.len>1){
    
    df[ldistance>threshold,ismissing:=1]       
  }
  
  return (df)
  
}

getclusters<-function(stops,ln,lt,e){
  
  distmatrix=geodetic.distance.dataframe(stops,lt,ln)
  model <- dbscan(as.dist(distmatrix),eps=e,MinPts=1)  # 50000 for archor places5
  stops=cbind(stops,cluster=model$cluster)
  return(stops)
  
}

getships<-function(shipfile){
  
  #ships=fread('D:/Rprojects/ships/ships.txt',sep=',') # build year should be include
  ships=fread(shipfile,sep=',')
  ships=ships[!is.na(mmsi)]
  # ships$mmsi<-as.character(ships$mmsi)
  setkey(ships,mmsi)
  return (ships)
  
}

getaispoints<-function(aisfilepath,startfilenum,endfilenum){ 
  
  colnames=c("mmsi","time","status","sog","lon","lat","src_id")
  filenames=list.files(aisfilepath)
  dt=data.table()
  dt=data.table(t(rep(NA,7)))
  setnames(dt,colnames)
  dt=dt[!is.na(mmsi)]
  
  for (i in startfilenum:endfilenum){
    
    print(i)
    tm=fread(paste(aisfilepath,i,'.txt',sep=""),sep='@',select=c(1:3,5,7:8,14))
    setnames(tm,colnames)
    dt=rbind(dt,tm)
    
  }
  
  #   tm[lon>110&lon<130&lat>20&lat<45]
  return (dt)
  
}

getstartendclust<-function(x){ #get start and end clusters
  
  x[,startclst:=0]
  x[,endclst:=0]
  
  stoprow=x[isstop==1,which=TRUE]
  startrow=stoprow[1:length(stoprow)-1]
  endrow=stoprow[2:length(stoprow)]
  startcluster=x[startrow,cluster]
  endcluster=x[endrow,cluster]
  
  for (i in (1:length(startrow))){ 
    
    mmsinum=length(unique(x[stoprow[i]:stoprow[i+1],mmsi]))# number of ships btw 2 stops
    
    if(mmsinum<2){
      x[stoprow[i]:stoprow[i+1]-1,startclst:=startcluster[i]]
      x[stoprow[i]:stoprow[i+1]-1,endclst:=endcluster[i]]  
    }
  }
  
  return(x)  
  
}

#too slow to wait
getstartendclust2<-function(x){ #get start and end clusters
  
  x[,startclst:=0]
  x[,endclst:=0]
  x[,startclst:=.SD[1]$cluster,by=list(mmsi,subtraj)]
  x[subtraj>0,endclst:=x[pid==.SD[.N]$pid+1,cluster],by=list(mmsi,subtraj)]
  return(x)  
  
}


getlvclusters<-function(stops,level,e){ #1000000 for port group level cluster
  
  
  if (level==1){
    
    lstops=stops[,list(lonmean=mean(lon),latmean=mean(lat)),by=list(level1lon,level1lat)]
    lstops=getclusters(lstops,lonmean,latmean,e)
    jstops=inner_join(stops,lstops,by=c('level1lon','level1lat')) 
    setkey(jstops,mmsi,time)
    
    
  } else if(level==2){
    
    lstops=stops[,list(lonmean=mean(lon),latmean=mean(lat)),by=list(level2lon,level2lat)]
    lstops=getclusters(lstops,lonmean,latmean,e)
    jstops=inner_join(stops,lstops,by=c('level2lon','level2lat')) 
    setkey(jstops,mmsi,time)
    
    
  } else if(level==3){
    
    lstops=stops[,list(lonmean=mean(lon),latmean=mean(lat)),by=list(level3lon,level3lat)]
    lstops=getclusters(lstops,lonmean,latmean,e)
    jstops=inner_join(stops,lstops,by=c('level3lon','level3lat')) 
    setkey(jstops,mmsi,time)
    
  } else if(level==4){
    
    lstops=stops[,list(lonmean=mean(lon),latmean=mean(lat)),by=list(level4lon,level4lat)]
    lstops=getclusters(lstops,lonmean,latmean,e)
    jstops=inner_join(stops,lstops,by=c('level4lon','level4lat')) 
    setkey(jstops,mmsi,time)
    
  }
  
  return(jstops)
  
}


#---------------------multiplot--------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#get projected point, point to line
dist2<-function(lon1,lat1,lon2,lat2){return (sqrt((lon2-lon1)^2+(lat2-lat1)^2)) }

point2line<-function(plon,plat,lon1,lat1,lon2,lat2){
  
  d=dist2(lon1,lat1,lon2,lat2)
  if(d==0){return(data.table(lon=lon1,lat=lat1))}
  
  t = ((plon - lon1) * (lon2 - lon1) + (plat - lat1) * (lat2 - lat1)) / d;
  
  lon=lon1 + t * (lon2 - lon1)
  lat=lat1 + t * (lat2 - lat1)
  
  return(data.table(lon,lat))
}

getinterpolate<-function(p1,p2,nt){
  
  ntime=nt
  vt=(p2$sog-p1$sog)/(p2$ntime-p1$ntime)*(nt-p1$ntime)+p1$sog
  sog=vt
  
  dlon=p2$lon-p1$lon
  dlat=p2$lat-p1$lat
  
  le=(vt+p1$sog)/2*(nt-p1$ntime)
  tl=sqrt(dlon^2+dlat^2)
  
  lon=(p2$lon-p1$lon)*le/tl+p1$lon
  lat=(p2$lat-p1$lat)*le/tl+p1$lat
  
  return(data.table(lon,lat,sog,ntime))
  
}

#get the angel and distance 
getangel2<-function(a,b,c){
  
  mlon1=a$mlon;mlat1=a$mlat
  mlon2=c$mlon;mlat2=c$mlat
  alon=b$alon;alat=b$alat
  
  ab=sqrt((alon-mlon1)^2+(alat-mlat1)^2)
  ac=sqrt((mlon1-mlon2)^2+(mlat1-mlat2)^2)
  bc=sqrt((alon-mlon2)^2+(alat-mlat2)^2)
  
  angel=acos((ab^2 + ac^2 - bc^2) / (2 * ab * ac))/pi*180
  
  
  return (data.table(cbind(ab,angel)))
}

#based on vector 
getangel<-function(a,b,c){
  
  va = as.vector(c(x=(b$alon-a$mlon), y=(b$alat-a$mlat)))
  vb = as.vector(c(x=(c$mlon-a$mlon), y=(c$mlat-a$mlat)))
  angel=acos(sum(va*vb)/(sqrt(sum(va*va))*sqrt(sum(vb*vb))))/pi*180
  ab=sqrt(sum(va*va))
  return (data.table(cbind(ab,angel)))
}

getangel3<-function(x,y){
  
  xrow=length(x)
  yrow=length(y)
  theta=x
  
  
  for (i in (1:xrow)){
    if(x[i]==0){
      if (y[i]>0){
        theta[i]=90
      }else if(y[i]==0){
        
        theta[i]=0
        
      }else{
        theta[i]=-90
      }
      
      
      
    }else if(x[i]>0){
      
      theta[i]=atan(y[i]/x[i])/pi*180
      
      if(theta[i]>0|theta[i]==0){
        
        theta[i]=theta[i]
        
      }else{
        
        theta[i]=360+theta[i]
        
      }
      
    }else {
      
      theta[i]=atan(y[i]/x[i])/pi*180
      
      theta[i]=180+theta[i]
      
    } 
    
  }
  return(theta)
  
}


point2linedist<-function(plon,plat,lon1,lat1,lon2,lat2){ 
  
  d=dist2(lon1,lat1,lon2,lat2)
  
  
  t = ((plon - lon1) * (lon2 - lon1) + (plat - lat1) * (lat2 - lat1)) / d;
  
  lon=lon1 + t * (lon2 - lon1)
  lat=lat1 + t * (lat2 - lat1)
  
  return(distance(plon,plat,lon,lat))
}

# idea from zhengyu's paper:'Mining User Similarity Based on Location History'
# however this algrathem may not suitable for two points with large gaps,maybe density based is better
# Input: A traject traj with lon,lat column, a distance threshold ddthreh
# and time span threshold tthreh
# suggest dthred=200,tthead=360

staypointdetect<-function(traj,dthred,tthred){
  
  setkey(traj,time)
  len=nrow(traj)
  i=1
  traj[,stayflag:=1]# the last few point is also a stay 
  stays=2
  
  while (i<len){
    ps=traj[i]
    j=i+1
    flag=0
    while (j<len+1){
      pe=traj[j]
      deltad=distance(ps$lon,ps$lat,pe$lon,pe$lat)
      if (deltad>dthred){
        deltat=abs(pe$time-ps$time)
        if(deltat>tthred){
          # virtual point
          #         vlon=mean(traj[i:(j-1)]$lon)
          #         vlat=mean(traj[i:(j-1)]$lat)
          #         vsog=mean(traj[i:(j-1)]$sog)
          #         arvt=traj[i]$time
          #         levt=traj[j-1]$time
          
          traj[i:(j-1),stayflag:=stays]
          i=j
          flag=1
          stays=stays+1
        }
        break
      }
      j=j+1
    }
    if (flag!=1){i=i+1}
  }
  
}

cellmerge<-function(r,i){
  
  print(i)
  
  if (nrow(r[region==0])>0){
    
    
    r[1,region:=i]
    gids.array=unique(as.vector(as.matrix(r[(region<(i+1))&(region>(i-1)),list(gid1,gid2)])))   
    
    while((nrow(r[region==0][gid1==gids.array])+nrow(r[region==0][gid2==gids.array]))>0){
      
      r[gid1==gids.array,region:=i]
      r[gid2==gids.array,region:=i]
      
      setkey(r,region)
      gids.array=unique(as.vector(as.matrix(r[(region<(i+1))&(region>(i-1)),list(gid1,gid2)]))) 
      
      
    }
    
    print(nrow(r[(region<(i+1))&(region>(i-1))]))
    setkey(r,region)
    r=cellmerge(r,i+1)
    #     r=rbind(r1,r2)
    
  }
  
  return(r)
  
  
  
}

clstplot<-function(dt){
  
  p <-dtplot(dt)
  # p=getMap(x[cluster>0],5)
  p=p+geom_point(data=dt,size=2, aes(colour=factor(cluster)),shape=3)
  p=p+geom_text(data=dt,size=2,col='green',aes(x=lon+0.05,y=lat+0.05,label=cluster))
  p
  
  
}





