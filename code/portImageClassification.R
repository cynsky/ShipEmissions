#produce port image for cnn classification
#eps=0.002 and minp=5
library(ggplot2)
library(data.table)
library('ggmap')
tm=fread('D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/candidates_terminals_label.csv')[sid>0]
n=nrow(tm);n
for (i in seq(1357,n)){
  zoomSize=16
  temp=tm[i]
  sid=temp$sid
  centerX=0.5*(max(temp$lon)+min(temp$lon))
  centerY=0.5*(max(temp$lat)+min(temp$lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomSize,source='google',maptype = 'satellite'))
  #p=ggplot()
  #p=p+geom_point(data=temp,aes(x=lon,y=lat,),size=0.1,alpha=0.1)
  #p=p+geom_text(data=temp[,.SD[1],list(glon,glat,c)],nudge_x = 0.005,nudge_y = 0.005,aes(x=lon,y=lat,label=c),color='black',size=4)
  p=p+theme(legend.position='none')
  #去除刻度线和标签
  p=p + theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
  p=p + theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
  ggsave(paste('D:/Git/data/portClusterPictures/individual_ship_based/eps0.002minp5/portImagesForKe/',sid,'.png',sep=''),width = 6,height = 6)
  
}


#----最新labelled数据---

tm.labelled=fread('D:/data/Global/gridBased/berthStats_labelled.csv')
berthstops=fread('D:/data/Global/gridBased/berthstops.csv')
head(tm.labelled);dim(tm.labelled)
head(berthstops);dim(berthstops)
berthstops=berthstops[,glon:=floor(lon*scale)/scale][,glat:=floor(lat*scale)][,gid:=paste(glon,glat,sep = '_')]
stops_labelled=data.table(left_join(berthstops[bsid>0],tm.labelled[,V1:=NULL],'bsid'))
scale=1000
#stops_labelled_grid=stops_labelled[,glon:=floor(lon.x*scale)/scale][,glat:=floor(lat.x*scale)][,gid:=paste(glon,glat,sep = '_')]
stops.tm=stops_labelled[label==1][shipN>20&stopN>50]
stops.tm[,.SD[1],gid]
#再通过网格化，从每个网格提取一个terminal stops

stops.notm1=stops_labelled[label==0]
#网格化后再取
stops.notm1[,.SD[1],gid]
#
stops.notm2=berthstops[bsid==0]
stops.notm2[,.SD[1],gid]



