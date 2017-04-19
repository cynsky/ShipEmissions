library('data.table')
library('dbscan')
library('dplyr')
library('ggplot2')
library('ggthemr')
library('ggedit')
library('ggmap')
library('xts')
library('sp')

funPath='D:/share/Git/Rprojects/ECA/function.R';source(funPath)
funPath2='../code/ShipTrajectoryAnalysisFunctions.R';source(funPath2)
#funPath='D:/Git/Rprojects/ECA/function.R'
ggthemr('greyscale')

dt=fread('D:/share/AIS/2015_ECA_container_with_COG/2015_ECA_container_with_COG2.csv',sep=',')
ships=fread('d:/share/ships/containerShipsAfterMMSICrawl.csv');ships=ships[,V1:=NULL]
duplicatedmmsis=ships[duplicated(ships$mmsi)]$mmsi
ships=ships[!mmsi%in%duplicatedmmsis];dim(ships)#remove ships with same mmsi
dt=dt[!mmsi%in%duplicatedmmsis]
setkey(dt,mmsi,time)
dt=data.table(dt,pid=seq(1,nrow(dt)))
dt=dt[,list(mmsi,time,status,sog,lon,lat,cog,pid)]
mmsis=dt[,.N,mmsi]$mmsi
count=dt[,.N,mmsi]
ships=data.table(inner_join(ships,count,'mmsi'));dim(ships)#ships with ais points
p=ggplot(data=ships[N>=100])+geom_histogram(aes(x=log10(N)))+ggtitle("Number of points");p#轨迹点分布
p=ggplot(data=ships[N>=100])+geom_histogram(aes(x=log10(dwt)))+ggtitle("Ship Dead Weight Tons Distribution");p#船舶吨位
#航速存在问题，有一部分航速太高，需要查看有没有那条船的航速一直都是一场
p=ggplot(data=sample_n(dt,100000))+geom_histogram(aes(x=sog))+ggtitle("Ship speed Distribution");p
#靠泊和航速小于1节的点
mooredPnt=dt[sog>0&status==5];dim(mooredPnt);
mooredPnt=dt[sog==0&status==5];dim(mooredPnt);
mooredPnt=dt[sog<=10&status==5];dim(mooredPnt);#input data for stay point










