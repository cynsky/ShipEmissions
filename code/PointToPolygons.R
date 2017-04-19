library('gdistance')
library('rgeos')
library('data.table')

rds_path='D:/Git/Rprojects/basedata/Global administrative area/'
borderdt=readRDS(paste(rds_path,"CHN_adm2.rds",sep=''));dim(borderdt);

pt1 = readWKT("POINT(0.5 0.5)")
pt2 = readWKT("POINT(2 2)")
p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
p2 = readWKT("POLYGON((2 0,3 1,4 0,2 0))")
gDistance(pt1,pt2)
gDistance(p1,pt1)
gDistance(p1,pt2)
gDistance(p1,p2)

gDistance(pt1,borderdt)
pp=data.table(lon=c(0,10),lat=c(0,10))
spdf <- SpatialPointsDataFrame(coords = pp, data = pp,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
gDistance(spdf,borderdt,byid = TRUE)
cities=c(166,175,174,170,167,163,171,169,261,336,335,334,341,339,344,342)
#data.table(borderdt@data)
head(data.table(borderdt@data)[ID_2%in%cities])
head(borderdt@polygons[[1]]@Polygons[[1]]@coords)




