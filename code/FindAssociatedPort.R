#find the associated port/city and contury
#svm
library('data.table')
library('e1071')
library('dplyr')
library('ggmap')
#file paths
wpiPath='D:/Git/Rprojects/ShipEmissions/data/worldport.csv'
countryPath='D:/Git/Rprojects/ShipEmissions/data/CountryListFromWorldPortIndex.txt'
MaritiemSilkLoadPath='D:/Git/Rprojects/ShipEmissions/data/MaritimeSilkLoadCitiesandCountries.txt'
berthPath='D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/candidates_berth.csv'
terminalPath='D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/candidates_terminals_label.csv'

terminals0=fread(terminalPath);head(terminals0)
country=fread(countryPath,header=FALSE,sep = '@');setnames(country,c('country','country_name'));head(country)
MaritimeSilk=fread(MaritiemSilkLoadPath,header = FALSE);setnames(MaritimeSilk,c('country','country_name','city','note','port_name'));dim(MaritmeSilk);head(MaritimeSilk)
#inputs selected terminal and world port index
ports0=fread(wpiPath)
berth0=fread(berthPath)
dim(ports0);dim(terminals0);head(berth0)
ports=ports0[,list(fid=FID,index_no=INDEX_NO,region_no=REGION_NO,port_name=PORT_NAME,country=COUNTRY,lon,lat)]
berths=berth0[,list(mmsi,stayid,startpid,endpid,duration,lon,lat,speed,grosston,sid)]
terminals=terminals0[,V15:=NULL]
head(terminals);head(berths)
#add country name to the ports
ports=data.table(inner_join(ports,country,'country'));head(ports)
terminals=data.table(inner_join(berths,terminals[label==1],'sid'));dim(terminals);head(terminals)

##----calculate the distance,and map styaid to wpi-----
r0=data.table(terminals[1],ports[1,list(fid,index_no,region_no,port_name,country,country_name,lon2=lon,lat2=lat)],dist=0)[sid<0]
n=nrow(terminals)

for(i in seq(1,n)){
  
  if(i%%10000==0){
    print(i)
    flush.console()
  }

  t=terminals[i]
  r=ports[,dist:=distance(t$lon.x,t$lat.x,lon,lat)][dist==min(dist),]#use berth location
  l=cbind(t,r[,list(fid,index_no,region_no,port_name,country,country_name,lon2=lon,lat2=lat,dist)])
  r0=rbind(r0,l)

}
##----end-----

dt=r0
rank2=fread('D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/portRank_statsLabelled.csv',header = TRUE)
addLabels=rank2[,.N,list(sid,port_name,statsLabel)]
#dt=data.table(left_join(r0,country,'country'));head(dt)

#get trips
setkey(dt,mmsi,stayid)
dt=dt[,grosston:=as.integer(grosston)]
dt=data.table(left_join(dt,addLabels[,list(sid,port_name,statsLabel)],c('sid','port_name')));head(dt)
dt1=dt[,list(mmsi,stayid,sid,index_no,region_no,country_name,port_name,grosston,duration,portlon=lon2,portlat=lat2,clusterlon=lon.y,clusterlat=lat.y,statsLabel)];dt1
#get trips
p1=dt1[1:(n-1),list(mmsi1=mmsi,stayid1=stayid,sid1=sid,index_no1=index_no,region_no1=region_no,country1=country_name,port_name1=port_name,
                    grosston1=grosston,duration1=duration,portlon1=portlon,portlat1=portlat,statsLabel1=statsLabel)]
p2=dt1[2:n,list(mmsi2=mmsi,stayid2=stayid,sid2=sid,index_no2=index_no,region_no2=region_no,country2=country_name,port_name2=port_name,
                grosston2=grosston,duration2=duration,portlon2=portlon,portlat2=portlat,statsLabel2=statsLabel)]
ln0=cbind(p1,p2);dim(ln0)
ln0=ln0[mmsi1==mmsi2];dim(ln0)
ln=data.table(ln0,tripid=seq(1,nrow(ln0)));dim(ln);head(ln)

#ln1=ln[statsLabel1!=statsLabel2];dim(ln1)
#ln1=ln[sid1!=sid2];dim(ln1)
ln1=ln

lnStats=ln1[sid1!=sid2,list(tripNum=.N,shipNum=length(unique(.SD$mmsi1)),meanGrosston=mean(.SD$grosston1),throughput=sum(.SD$grosston1)),list(statsLabel1,statsLabel2,sid1,sid2,port_name1,port_name2,country2,country1,portlon1,portlat1,portlon2,portlat2)];dim(lnStats);head(lnStats)

write.csv(lnStats,'D:/Git/Rprojects/ShipEmissions/data/lnStats.csv')

rank3=lnStats[,list(tripNum=sum(tripNum),shipNum=sum(shipNum),meanGrosston=mean(meanGrosston),throughput=sum(meanGrosston*tripNum)),list(sid1,country1,statsLabel1)]

dim(rank3);head(rank3);setorder(rank3,-throughput,-tripNum);
head(rank3,20)
temp3=rank3[,list(tripNum=sum(tripNum),shipNum=sum(shipNum),meanGrosston=mean(meanGrosston),throughput=sum(throughput)),list(country1,statsLabel1)];
dim(rank3);head(temp3);setorder(temp3,-throughput,-tripNum);
head(temp3,30)
head(temp3[country1=='China'|statsLabel1=='HONG KONG'],50)
##----------china--------------------------------------------------------------



chinalnStats=lnStats[(country1=='China'|country1=='HONG KONG')&(country2=='China'|country2=='HONG KONG')]
dim(chinalnStats);head(chinalnStats)


write.csv(chinalnStats,'D:/Git/Rprojects/ShipEmissions/shipNetworkAnalysis/chinalnStats.csv')




##------------start maritime silk road------------------------------
portnames=MaritimeSilk[,.N,port_name]$port_name

silkdt=dt1[port_name%in%portnames];dim(silkdt);head(silkdt);dim(silkdt)
silkdt=data.table(inner_join(dt1,MaritimeSilk[,list(city,port_name)],'port_name'))
n=nrow(silkdt)
silkp1=silkdt[1:(n-1),list(mmsi1=mmsi,stayid1=stayid,sid1=sid,index_no1=index_no,region_no1=region_no,country1=country_name,city1=city,port_name1=port_name,
                    grosston1=grosston,duration1=duration,portlon1=portlon,portlat1=portlat,statsLabel1=statsLabel)]
silkp2=silkdt[2:n,list(mmsi2=mmsi,stayid2=stayid,sid2=sid,index_no2=index_no,region_no2=region_no,country2=country_name,city2=city,port_name2=port_name,
                grosston2=grosston,duration2=duration,portlon2=portlon,portlat2=portlat,statsLabel2=statsLabel)]
silkln0=cbind(silkp1,silkp2);dim(silkln0)
silkln0=silkln0[port_name1%in%portnames&port_name2%in%portnames&mmsi1==mmsi2];dim(silkln0)
silkln=data.table(silkln0,tripid=seq(1,nrow(silkln0)));dim(silkln);head(silkln)

#ln1=ln[statsLabel1!=statsLabel2];dim(ln1)
#ln1=ln[sid1!=sid2];dim(ln1)
silkln1=silkln[statsLabel1!=statsLabel2];dim(silkln1);dim(silkln1)
#silkln1=silkln;dim(silkln)

silklnStats=silkln1[,list(tripNum=.N,shipNum=length(unique(.SD$mmsi1)),meanGrosston=mean(.SD$grosston1)),list(statsLabel1,statsLabel2,sid1,sid2,port_name1,port_name2,city1,city2,country2,country1,portlon1,portlat1,portlon2,portlat2)];dim(silklnStats);head(silklnStats)

#write.csv(lnStats,'D:/Git/Rprojects/ShipEmissions/data/lnStats.csv')
#should use statsLabel but not the port_name
silklnStats2=silklnStats[statsLabel1%in%portnames&statsLabel2%in%portnames|(port_name1%in%portnames&port_name2%in%portnames)];silklnStats2
silkrank=silklnStats2[,list(tripNum=sum(tripNum),shipNum=sum(shipNum),throughput=sum(meanGrosston*tripNum)),list(sid1,city1,country1,statsLabel1)]

write.csv(silklnStats2,'D:/Git/Rprojects/ShipEmissions/data/silklnStats2.csv')

dim(silkrank);head(silkrank);setorder(silkrank,-throughput,-tripNum);
head(silkrank,20)
silkrank=silkrank[,country_name:=country1]
temp2=data.table(left_join(silkrank,MaritimeSilk,'country_name'));dim(temp2);head(temp2,20)

temp3=temp2[,list(tripNum=sum(tripNum),shipNum=sum(shipNum),throughput=sum(throughput)),list(country1,statsLabel1)];
dim(temp3);head(temp3);setorder(temp3,-throughput,-tripNum);
head(temp3,30)

silklnStats3=data.table(left_join(silklnStats2,MaritimeSilk[,list(city1=city,city_cn1=note,country_name)],c('country1'='country_name')))
silklnStats4=data.table(left_join(silklnStats3,MaritimeSilk[,list(city2=city,city_cn2=note,country_name)],c('country2'='country_name')))

write.csv(silklnStats4,'D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/silklnStats4.csv')


##-----end of maritime silk load------------------

##-----how many ports and countries------------

temp=r0[,.N,(index_no)];dim(temp)
ppp=ports0[,list(fid=FID,index_no=INDEX_NO,region_no=REGION_NO,port_name=PORT_NAME,country=COUNTRY,lon,lat)]
temp2=data.table(inner_join(ppp,temp,'index_no'));head(temp2);dim(temp2)
dim(temp2[,.N,country])
write.csv(temp2,'D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/selected_ports.csv')
data2shp(temp2,'D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/selected_ports.shp')


###----------------ranking--------------------------------------------------------
rank=dt1[,list(tripNum=.N,shipNum=length(unique(.SD$mmsi)),meanGrosston=mean(.SD$grosston),throughput=sum(.SD$grosston)),list(sid,index_no,region_no,port_name,country_name,lon=clusterlon,lat=clusterlat)];
#rank=rank[,putThrough:=meanGrosston*tripNum];

setorder(rank,-throughput);dim(rank)
head(rank,20)
write.csv(rank,'D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/portRank.csv')
data2shp(rank,'D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/portRank.shp')

rank2=fread('D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/portRank_statsLabelled.csv',header = TRUE)
addLabels=rank2[,.N,list(sid,port_name,statsLabel)]

myrank=rank2[,list(putThrough=sum(.SD$putThrough),tripNum=sum(.SD$tripNum)),list(statsLabel,country_name)];
setorder(myrank,-putThrough);head(myrank[country_name=='China'],30)


write.csv(MaritimeSilk,'D:/Git/Rprojects/ShipEmissions/data/MaritimeSilkWithPorts.csv')

##-------------fuzhou--------------------------------

lnFuzhouStats=lnStats[statsLabel1!=statsLabel2&(statsLabel2=='FUZHOU'|statsLabel2=='FUZHOU')]
dim(lnFuzhouStats);head(lnFuzhouStats)







