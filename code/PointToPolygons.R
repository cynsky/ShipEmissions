library('gdistance')
library('rgeos')
library('data.table')
library('sp')
library('rgdal')

#rds_path='D:/Git/Rprojects/basedata/Global administrative area/'
#borderdt=readRDS(paste(rds_path,"CHN_adm2.rds",sep=''));dim(borderdt);
#borderdt<-readOGR(dsn="D:/Git/Rprojects/basedata/Global administrative area/CHN_adm_shp",layer="CHN_adm1");
#borderdt<-readOGR(dsn="D:/Git/Rprojects/basedata/Global administrative area/CHN_adm_shp",layer="CHN_adm1");
system.time(borderdt<-readOGR(dsn="D:/Git/Rprojects/basedata/Global administrative area/gadm28.shp",layer="gadm28"));

dim(borderdt);

# pt1 = readWKT("POINT(29.00867 41.00485)")
# pt2 = readWKT("POINT(2 2)")
# p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
# p2 = readWKT("POLYGON((2 0,3 1,4 0,2 0))")
# gDistance(pt1,pt2)
# gDistance(p1,pt1)
# gDistance(p1,pt2)
# gDistance(p1,p2)

# gDistance(pt1,borderdt)
pp=data.table(lon=c(121.6,23),lat=c(31.05,111))
spdf <- SpatialPointsDataFrame(coords = pp[,list(lon,lat)], data = pp,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
system.time(d<-gDistance(spdf,borderdt,byid = TRUE))
# cities=c(166,175,174,170,167,163,171,169,261,336,335,334,341,339,344,342)
#data.table(borderdt@data)
#head(data.table(borderdt@data)[ID_2%in%cities])
#head(borderdt@polygons[[31]]@Polygons[[1]]@coords)

n=nrow(spdf@data);n
cities=data.table(OBJECTID=0,UID=0,ID_0=0,ISO='',NAME_0='',NAME_1='',NAME_2='',NAME_3='')[UID<0]# same to columns in world polygon
for(i in seq(1,n)){
  d1=data.table(d)
  city=data.table(borderdt@data)[which.min(d1[[i]]),list(OBJECTID,UID,ID_0,ISO,NAME_0,NAME_1,NAME_2,NAME_3)]
  cities=rbind(cities,city)
}
newdt=cbind(pp,cities);head(newdt)


#-----circle.polygon,point ot point distance with geophere package-----
#just for test
list1 <- data.frame(longitude = c(80.15998, 72.89125, 77.65032, 77.60599, 
                                  72.88120, 76.65460, 72.88232, 77.49186, 
                                  72.82228, 72.88871), 
                    latitude = c(12.90524, 19.08120, 12.97238, 12.90927, 
                                 19.08225, 12.81447, 19.08241, 13.00984,
                                 18.99347, 19.07990))
list2 <- data.frame(longitude = c(72.89537, 77.65094, 73.95325, 72.96746, 
                                  77.65058, 77.66715, 77.64214, 77.58415,
                                  77.76180, 76.65460), 
                    latitude = c(19.07726, 13.03902, 18.50330, 19.16764, 
                                 12.90871, 13.01693, 13.00954, 12.92079,
                                 13.02212, 12.81447), 
                    locality = c("A", "A", "B", "B", "C", "C", "C", "D", "D", "E"))

#To calculate the geographic distance between two points with latitude/longitude coordinates, you can use several formula's. The package geosphere has the distCosine, distHaversine, distVincentySphere and distVincentyEllipsoid for calculating the distance. Of these, the distVincentyEllipsoid is considered the most accurate one, but is computationally more intensive than the other ones.
#With one of these functions, you can make a distance matrix. Based on that matrix you can then assign locality names based on shortest distance with which.min and the corresponding distance with min (see for this the last part of the answer) like this:

library('geosphere')

# create distance matrix
mat <- distm(list1[,c('longitude','latitude')], list2[,c('longitude','latitude')], fun=distVincentyEllipsoid)

# assign the name to the point in list1 based on shortest distance in the matrix
list1$locality <- list2$locality[max.col(-mat)]


library('swfscMisc')

cart.earth <- circle.polygon(0, 0, 10,units='km',poly.type = "cart.earth",dist.method = 'haversine')
plot(cart.earth)


#use gBuffer this function will return ploggons
