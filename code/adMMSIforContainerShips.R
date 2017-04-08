#combile ship file with crawlled mmsis
#mainly for container ships
library('rjson')
library('data.table')
library('dplyr')
#input files
shipPath='C:/GitProjects/data/ships.txt'
ships=fread(shipPath);head(ships)
shipNAmmsiPath='C:/GitProjects/data/shipWithoutMMSI.csv'
naShips=fread(shipNAmmsiPath);head(naShips)
# 有些爬回来的mmsi不是数字需要去掉
nammsis=fread('C:/GitProjects/tutorial/mmsis.csv')[!is.na(imo)];

nammsis=nammsis[,imo:=as.integer(imo)]
#remove mmsis which are not a number
nammsis=nammsis[,mmsi:=as.integer(mmsi)][!is.na(mmsi)];dim(nammsis)
class(ships$imo)
#combine container ships and save to csvfile
containers=ships[type_en=='Container'];head(containers);dim(containers)
ship1=data.table(inner_join(naShips,nammsis[,list(imo,mmsi)],'imo'));dim(ship1);dim(ship1)
ship2=ship1[,list(mmsi=mmsi.y,imo,callsign,speed,powerkw,dwt,grosston,mlength,beam,draft,type_en)]
conShips=rbind(containers[!is.na(mmsi)],ship2);dim(conShips);conShips[is.na(mmsi)]
write.csv(conShips,'C:/GitProjects/data/containerships.csv')