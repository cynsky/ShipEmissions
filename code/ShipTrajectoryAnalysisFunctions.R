

##---------functions for a ship---------------------------------------


#get stay points based data with status=5 and sog=0, mainly berth area
#also work for database with more then one mmsi
getStayPoint<-function(dt,eps=3600*2,minp=5){
  setkey(dt,mmsi,time)
  temp3=dt[,.N,list(mmsi)]
  nr=nrow(temp3)#how many ships
  r0=data.table(mmsi=0,stayid=0,startpid=0,endpid=0,duration=0,lon=0,lat=0)[mmsi<0]
  for (i in seq(1,nr)){
    r=temp3[i]
    cship=dt[mmsi==r$mmsi]
    cship2 <- as.matrix(cship[,list(time)])#get time series
    cl2 <- dbscan(cship2, eps = eps, minPts =minp)
    cship3=data.table(cbind(cship,stayid=cl2$cluster));
    
    temp=cship3[,list(startpid=first(.SD)$pid,endpid=last(.SD)$pid,duration=(last(.SD)$time-first(.SD)$time),lon=mean(.SD$lon),lat=mean(.SD$lat)),list(mmsi,stayid)]
    
    r0=rbind(r0,temp)      
  }
  return(r0)
}

#combine stay points based on dbscan
#the function will add a sid column
#for stayid not in a cluster, just set sid=-stayid
mergeStayPoint<-function(sp,eps=0.02,minp=2){
  cship2 <- as.matrix(sp[,list(lon,lat)])#get time series
  cl2 <- dbscan(cship2, eps = eps, minPts =minp)
  cship3=data.table(cbind(sp,sid=cl2$cluster))
  cship3=cship3[sid==0,sid:=(100000+stayid)]#pay attention on this line
  return(cship3) 
}


#add stayid and sid to the original ship AIS records
setStayId<-function(s,sp){
  s=s[,stayid:=0]
  s=s[,sid:=0]
  n=nrow(sp)
  for(i in seq(1,n)){
    l=sp[i]
    s[pid<=l$endpid&pid>=l$startpid,stayid:=l$stayid]
    s[pid<=l$endpid&pid>=l$startpid,sid:=l$sid]
  }
  return(s)
  
}


#set tripid to the original one
setTripId<-function(s,sp){
  
  n=nrow(sp)
  s=s[,tripid:=0]
  if(n>1){
    sp1=sp[1:(n-1),list(mmsi,startpid1=startpid,endpid1=endpid)]
    sp2=sp[2:n,list(startpid2=startpid,endpid2=endpid)]
    spln=cbind(sp1,sp2)
    for(i in seq(1,nrow(spln))){
      l=spln[i]
      s[pid>=l$endpid1&pid<=l$startpid2,tripid:=i]    
    }
  }
  return(s) 
}


#add trips distance,duration, stayid,sid
#all of the functions are for a single ship
addTripStats<-function(trips,s){
  trips=trips[,dist:=0]
  trips=trips[,dur:=0]
  n=nrow(trips)
  for (i in (seq(1,n))){
    id=trips[i]$tripid
    ammsi=trips[i]$mmsi
    trip=s[mmsi==ammsi&tripid==id]
    m=nrow(trip)
    
    if(m>1){
      trip1=trip[1:(m-1),list(mmsi,time1=time,lon1=lon,lat1=lat,sid1=sid)]
      trip2=trip[2:m,list(tripid2=tripid,time2=time,lon2=lon,lat2=lat,sid2=sid)] 
      tripln=cbind(trip1,trip2)
      
      tripln=tripln[,dist:=distance(lon1,lat1,lon2,lat2)]
      tripln=tripln[,dur:=(time2-time1)]
      totalDist=sum(tripln$dist)
      totalDur=sum(tripln$dur)
      trips[tripid==id,dist:=totalDist]
      trips[tripid==id,dur:=totalDur]
      trips[tripid==id,stayid1:=first(trip)$stayid]
      trips[tripid==id,stayid2:=last(trip)$stayid]
      trips[tripid==id,sid1:=first(trip)$sid]
      trips[tripid==id,sid2:=last(trip)$sid]
    }
  }
  
}

#calculate trip statistics from original ais records 
#trips columns include:mmsi,tripid,N,dist,dur,stayid1,stayid2,sid1,sid2
getShipTripStats<-function(s){
  #individual ship
  dt=s[sog==0&status==5];
  sp=getStayPoint(dt,eps=3600*2,minp=5);sp=sp[stayid>0]
  sp=mergeStayPoint(sp,eps=0.02,minp=2)
  s=setStayId(s,sp)
  s=setTripId(s,sp)
  trips=s[tripid>0,.N,list(mmsi,tripid)];
  addTripStats(trips,s)
  return(trips)
}

##----------------for each trip --------------------------------

#get stay points based data with status!=5 and sog<5, mainly berth area
#also work for database with more then one mmsi
#sogLimit to set the speed limit of the select point
getTripStayPoint<-function(trip,soglimit=5,eps=0.002,minp=5){
  #add the first and last staypoint to the trip stay points
  dt=trip[sog<soglimit&status!=5]#very important
  if(nrow(dt)>0){
    cship2 <- as.matrix(dt[,list(lon,lat)])#get time series
    cl2 <- dbscan(cship2, eps = eps, minPts =minp)
    cship3=cbind(dt[,list(mmsi,time,sog,lon,lat,status,pid,stayid,sid,tripid)],tripstayid=cl2$cluster);
    tripStayPoint=cship3[tripstayid>0,list(startpid=first(.SD)$pid,endpid=last(.SD)$pid,duration=(last(.SD)$time-first(.SD)$time),lon=mean(.SD$lon),lat=mean(.SD$lat)),list(mmsi,tripid,tripstayid)] 
    tripStayPoint=tripStayPoint[,tripstayid:=(tripstayid+1)];
    firstStay=first(trip);firstStay=data.table(firstStay[,list(mmsi,tripid,tripstayid=1,startpid=pid,endpid=pid,duration=0,lon,lat)]);
    n=nrow(tripStayPoint)
    lastStay=last(trip);lastStay=data.table(lastStay[,list(mmsi,tripid,tripstayid=(n+2),startpid=pid,endpid=pid,duration=0,lon,lat)]);
    tripStayPoint=rbind(firstStay,tripStayPoint,lastStay);
  } 
  if(nrow(dt)==0){
    firstStay=first(trip);firstStay=data.table(firstStay[,list(mmsi,tripid,tripstayid=1,startpid=pid,endpid=pid,duration=0,lon,lat)]);
    lastStay=last(trip);lastStay=data.table(lastStay[,list(mmsi,tripid,tripstayid=2,startpid=pid,endpid=pid,duration=0,lon,lat)]);
    tripStayPoint=rbind(firstStay,lastStay);
    
  }
  
  return(tripStayPoint)
  
  
}


#set tripstayid within an individual trip
# add a column called 'tripstayid '
#mainly for the anchor places 
setTripStayId<-function(trip,tripStayPoint){
  
  trip=trip[,tripstayid:=0]
  n=nrow(tripStayPoint);n
  for(i in seq(1,n)){
    l=tripStayPoint[i];
    trip[pid<=l$endpid&pid>=l$startpid,tripstayid:=l$tripstayid]
  }
  return(trip)
}


#set subtripid for a trip
#add an column called subtripid

setTripSubTripId<-function(trip,tripStayPoint){
  n=nrow(tripStayPoint)
  trip=trip[,subtripid:=0]
  sp1=tripStayPoint[1:(n-1),list(mmsi,tripid,startpid1=startpid,endpid1=endpid)]
  sp2=tripStayPoint[2:n,list(startpid2=startpid,endpid2=endpid)]
  spln=cbind(sp1,sp2)
  for(i in seq(1,nrow(spln))){
    l=spln[i]
    trip[pid>=l$endpid1&pid<=l$startpid2,subtripid:=i]    
  } 
  return(trip)
}


#add distance, duration,tripstayid1 and tripstayid2 to subtrips
addSubTripStats<-function(subtrips,trip){
  
  subtrips=subtrips[,dist:=0]
  subtrips=subtrips[,dur:=0]
  n=nrow(subtrips)
  for (i in (seq(1,n))){
    id=subtrips[i]$subtripid
    subtrip=trip[subtripid==id]
    m=nrow(subtrip)
    
    if(m>1){
      trip1=subtrip[1:(m-1),list(mmsi,time1=time,lon1=lon,lat1=lat)]
      trip2=subtrip[2:m,list(tripid2=tripid,time2=time,lon2=lon,lat2=lat)] 
      tripln=cbind(trip1,trip2)
      
      tripln=tripln[,dist:=distance(lon1,lat1,lon2,lat2)]
      tripln=tripln[,dur:=(time2-time1)]
      totalDist=sum(tripln$dist)
      totalDur=sum(tripln$dur)
      subtrips[subtripid==id,dist:=totalDist]
      subtrips[subtripid==id,dur:=totalDur]
      subtrips[subtripid==id,tripstayid1:=first(subtrip)$tripstayid]
      subtrips[subtripid==id,tripstayid2:=last(subtrip)$tripstayid]
      
    }
  }
  
  return(subtrips) 
}


###------------combine all together-----------------------------------
#input: s[mmsi,time,sog,lon,lat,status] only for one ship
shipTraSegment<-function(s){
  
  #individual ship
  dt=s[sog==0&status==5];
  sp=getStayPoint(dt,eps=3600*2,minp=5);sp=sp[stayid>0]
  sp=mergeStayPoint(sp,eps=0.02,minp=2)
  s=setStayId(s,sp)
  s=setTripId(s,sp)
  trips=s[tripid>0,.N,list(mmsi,tripid)];
  addTripStats(trips,s)
  
  #individual trip------
  
  res=data.table(s[1],tripstayid=0,subtripid=0)[mmsi<0];
  
  n=nrow(trips)
  for(i in seq(1,n)){
    trip=s[tripid==trips[i]$tripid]
    setkey(trip,mmsi,time)
    #get trip stay point
    tripStayPoint=getTripStayPoint(trip,soglimit=5,eps=0.002,minp=5);
    #set trip stay id 
    trip=setTripStayId(trip,tripStayPoint)
    #set trip subtripid
    trip=setTripSubTripId(trip,tripStayPoint);
    res=rbind(res,trip)
    
  }
  #---------add subtripid and tripstayid to records with a tripid ==0, for example the stay points 
  temp=s[tripid==0]
  temp=temp[,tripstayid:=0]
  temp=temp[,subtripid:=0]
  res=rbind(res,temp)
  #--------------
  setkey(res,mmsi,time)
  return(res)
}
