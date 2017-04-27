# find the closest city from world ploygons which is publically available
# worldPolygon is the world polygon downloaded from Global administrative area.
# dt, must include lon,lat columns

findCity<-function(dt,worldPolygon){
  library('gdistance')
  library('rgeos')
  library('data.table')
  library('sp')
  library('rgdal')
  spdf <- SpatialPointsDataFrame(coords = dt[,list(lon,lat)], data = pp,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  system.time(d<-gDistance(spdf,worldPolygon,byid = TRUE))
  n=nrow(spdf@data);n
  cities=data.table(OBJECTID=0,UID=0,ID_0=0,ISO='',NAME_0='',NAME_1='',NAME_2='',NAME_3='')[UID<0]# same to columns in world polygon
  for(i in seq(1,n)){
    d1=data.table(d)
    city=data.table(worldPolygon@data)[which.min(d1[[i]]),list(OBJECTID,UID,ID_0,ISO,NAME_0,NAME_1,NAME_2,NAME_3)]
    cities=rbind(cities,city)
    print(i)
    flush.console()
  }
  newdt=cbind(dt,cities);
  return(newdt)
}

pp=data.table(lon=c(121.6,23),lat=c(31.05,111))
system.time(worldPolygon<-readOGR(dsn="D:/Git/Rprojects/basedata/Global administrative area/gadm28.shp",layer="gadm28"));
system.time(findCity(pp,borderdt))


