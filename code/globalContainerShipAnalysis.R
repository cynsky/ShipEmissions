#global container ship analysis
library(data.table)
path='D:/share/AIS/2015_global_container_with_COG/'
dt=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0,cog=0,head=0)[mmsi<0]
for(i in seq(201501,201512)){
  
  filepath=paste(path,i,'.csv',sep='')
  temp=fread(filepath)[,list(mmsi,time,status,sog,lon,lat,cog,head)];
  dt=rbind(dt,temp)
}

#-----提取在区域内的轨迹点-------------
library('doParallel')
library('foreach')
cl=makeCluster(12)#利用12个处理器
registerDoParallel(cl)
getDoParWorkers()
path='D:/share/AIS/2015_global_container_with_COG/'
dt=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0,cog=0)[mmsi<0]
filepaths=list.files(path,full.names = TRUE)#博懋东部沿海集装箱船数据
gc()
system.time(dt<-foreach(i=seq(1,12),.combine='rbind',.packages=c('data.table')) %dopar% { 
  
  fread(filepaths[i],sep='@')
 
})
stopCluster(cl)







