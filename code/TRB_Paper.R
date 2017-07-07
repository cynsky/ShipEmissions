berthcityfile='D:/data/Global/gridBased/TRB_paper/berthCities.csv'
stopfile='D:/data/Global/gridBased/TRB_paper/berthstops.csv'

stays=fread(berthcityfile);stays #stay with GADM cities
stops=fread(stopfile)#with city
wpiports=fread('D:/data/Global/gridBased/TRB_paper/WPIportCities.csv')#with city
##----add wpi port for each bsid:calculate the distance,and map styaid to wpi-----
#stays1 with wpi ports
stays1=data.table(stays[1,list(label2,label,bsid,lon.stay=lon,lat.stay=lat,shipN,stopN,UID,ID_0, ID_1, ID_2, ID_3,ISO,NAME_0,NAME_1,NAME_2,NAME_3)],
                  wpiports[1,list(FID,INDEX_NO,REGION_NO,PORT_NAME,COUNTRY,lon.wpiport=lon,lat.wpiport=lat,
                                       NAME_0.port=NAME_0,NAME_1.port=NAME_1,NAME_2.port=NAME_2,NAME_3.port=NAME_3)],dist=0)[bsid<0]
n=nrow(stays);n

for(i in seq(1,n)){
  
  if(i%%100==0){
    print(i)
    flush.console()
  }
  
  t=stays[i,list(label2,label,bsid,lon.stay=lon,lat.stay=lat,shipN,stopN,UID,ID_0, ID_1, ID_2, ID_3,ISO,NAME_0,NAME_1,NAME_2,NAME_3)]
  r=wpiports[,dist:=distance(t$lon.stay,t$lat.stay,lon,lat)][dist==min(dist),]#use berth location
  l=cbind(t,r[,list(FID,INDEX_NO,REGION_NO,PORT_NAME,COUNTRY,lon.wpiport=lon,lat.wpiport=lat,NAME_0.port=NAME_0,NAME_1.port=NAME_1,NAME_2.port=NAME_2,NAME_3.port=NAME_3,dist)])
  stays1=rbind(stays1,l)
  
}

##----add LloydList port----
# city= d1[,.N,list(ISO,NAME_0,NAME_1,NAME_2,NAME_3)];
# fwrite(city,'D:/data/Global/gridBased/TRB_paper/city.csv') # map each port of lloyd top 100 manually
#topports=fread('D:/data/Global/gridBased/TRB_paper/2016top100port_desc.csv')
city_port=fread('D:/data/Global/gridBased/TRB_paper/city_LloydList_port.csv')
stays2=data.table(left_join(stays1,city_port[port!='',list(ISO,NAME_0,NAME_1,NAME_2,NAME_3,lloyd_port=port)],c('ISO','NAME_0','NAME_1','NAME_2','NAME_3')))


d0=data.table(inner_join(stops[,list(sid,mmsi,tsid,startpid,endpid,stime,etime,lon.stop=lon,lat.stop=lat,bsid)],stays2,by='bsid'));

fwrite(d0,'D:/data/Global/gridBased/TRB_paper/labelled_stop_berth_city_wpiport_lloydlist100port.csv')#
d1=d0[label2 %in% c(0,1,2)];d1 #remove channal and anchor places, stops with cities


#-------add ship information-----------------------
#-----ships-------------
mmsis=unique(d0$mmsi)
IHSimo=unique(IHSships$imo)
IHSships=fread('D:/data/IHS/IHS_container_ship_all.csv')
blmships=fread('D:/Git/Rprojects/basedata/ships.csv')
blmships[imo%in%IHSimo][type_en=='Container']
blmships[type_en=='Container'&is.na(mmsi)][imo%in%IHSimo]
crawlships=fread('D:/data/python_crawl_mmsi/containerShipsAfterMMSICrawl.csv')

mmsis2=crawlships[,.N,mmsi][N>1]$mmsi
ships=crawlships[!mmsi%in%mmsis2]#去掉重复MMSI的船舶22艘，但是有十艘在IHS中有记录

mmsis3=unique(ships[mmsi%in%mmsis]$mmsi)#出现在ships中船舶
mmsis4=unique(d0[!mmsi%in%mmsis3]$mmsi)#未出现在ships中的船舶
IHSships[mmsi%in%mmsis4]#未出现在ships中却，m出现在IHS中的船舶

d2=data.table(left_join(d1,ships[,list(mmsi,imo,speed,powerkw,dwt,grosston)],'mmsi'))
d2=d2[,port:=lloyd_port]
d2=d2[is.na(port),port:=PORT_NAME]#final port equal to 

#-----统计两个停留点之间的航次和路径-----
#以bsid为停留点，bsid1和bsid2不同，同时要统计距离
setkey(d2,mmsi,stime)
mmsis=d2[,.N,mmsi][N>1]$mmsi;length(mmsis)

trips0=data.table(mmsi=0,imo=0,dwt=0,speed=0,powerkw=0,label.f=0,bsid1=0,startpid1=0,endpid1=0,stime1=0,etime1=0,lon1=0,lat1=0,shipN1=0,stopN1=0,
                  UID.f=0,ID_0.f=0,ID_1.f=0,ID_2.f=0,ID_3.f=0,ISO.f='',NAME_0.f='',NAME_1.f='',NAME_2.f='',NAME_3.f='',wpi_port.f='',lloyd_port.f='',port.f='',
                  label.t=0,bsid2=0,startpid2=0,endpid2=0,stime2=0,etime2=0,lon2=0,lat2=0,shipN2=0,stopN2=0,
                  UID.t=0,ID_0.t=0,ID_1.t=0,ID_2.t=0,ID_3.t=0,ISO.t='',NAME_0.t='',NAME_1.t='',NAME_2.t='',NAME_3.t='',wpi_port.t='',lloyd_port.t='',port.t='')[mmsi<0]
                  
n=length(mmsis);n
for(i in seq(1,n)){
  if(i%%100==0){print(i)}
  ammsi=mmsis[i]
  mmsistops=d2[mmsi==ammsi];
  setkey(mmsistops,mmsi,stime)
  m=nrow(mmsistops);m
  l1=mmsistops[1:(m-1),list(mmsi,imo,dwt,speed,powerkw,label.f=label2,bsid1=bsid,startpid1=startpid,endpid1=endpid,stime1=stime,etime1=etime,lon1=lon.stop,lat1=lat.stop,shipN1=shipN,stopN1=stopN,
                            UID.f=UID,ID_0.f=ID_0,ID_1.f=ID_1,ID_2.f=ID_2,ID_3.f=ID_3,ISO.f=ISO,NAME_0.f=NAME_0,NAME_1.f=NAME_1,
                            NAME_2.f=NAME_2,NAME_3.f=NAME_3,wpi_port.f=PORT_NAME,lloyd_port.f=lloyd_port,port.f=port)]
  l2=mmsistops[2:m,list(label.t=label2,bsid2=bsid,startpid2=startpid,endpid2=endpid,stime2=stime,etime2=etime,lon2=lon.stop,lat2=lat.stop,shipN2=shipN,stopN2=stopN,
                        UID.t=UID,ID_0.t=ID_0,ID_1.t=ID_1,ID_2.t=ID_2,ID_3.t=ID_3,ISO.t=ISO,NAME_0.t=NAME_0,NAME_1.t=NAME_1,
                        NAME_2.t=NAME_2,NAME_3.t=NAME_3,wpi_port.t=PORT_NAME,lloyd_port.t=lloyd_port,port.t=port)]
  ln=cbind(l1,l2)
  trips0=rbind(trips0,ln)
  
}
fwrite(trips0,'D:/data/Global/gridBased/TRB_paper/trips0.csv')#
gc()

#-----travel sequence and network-----
trips=trips0[label.f==1&label.t==1];trips #去掉到修船或者其他港口的trip
trips=trips[,tripid:=seq(1,nrow(trips))][,dist:=round(distance(lon1,lat1,lon2,lat2),0)][,dur:=round(abs(stime2-etime1)/3600,2)]
fwrite(tripln,'D:/data/Global/gridBased/TRB_paper/trips.csv')#container port to container port
#
portlinks=trips[port.f!=port.t,list(.N,meandwt=mean(dwt,na.rm=TRUE),weight=.N*mean(dwt,na.rm=TRUE)),list(port.f,port.t,ISO.f,ISO.t,NAME_0.f,NAME_0.t)];portlinks
countrylinks=trips[ISO.f!=ISO.t][,list(.N,meandwt=mean(dwt,na.rm=TRUE),weight=.N*mean(dwt,na.rm=TRUE)),list(ISO.f,ISO.t,NAME_0.f,NAME_0.t)];countrylinks
setorder(countrylinks,-weight);countrylinks[!is.nan(weight)][1:50]
setorder(portlinks,-weight);portlinks[!is.nan(weight)][1:50]

