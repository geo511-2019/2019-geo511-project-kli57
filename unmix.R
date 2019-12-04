library(googledrive)
library(raster)
library(rgdal)
library(sf)
library(foreach)
library(doParallel)
library(tidyverse)
library(RStoolbox)
buffalo<-st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/buffalo/buffalo_shp.shp")
repro_buffalo=st_transform(buffalo,crs=crs(unmix1988))
buffalo_lonlat<-as.data.frame(st_coordinates(repro_buffalo$geometry))[,c(1,2)]
id=data.frame(name="buffalo")
poly=Polygons(list(Polygon(buffalo_lonlat)),1)
buffalo_poly<-SpatialPolygons(list(poly))
buffalo_frame<-SpatialPolygonsDataFrame(buffalo_poly,id)
crs(buffalo_frame)<-crs(unmix1988)

img2018=stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/ImageBuffalo2017-2019.tif")
img2008=stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/ImageBuffalo2007-2009.tif")
img1998=stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/ImageBuffalo1997-1999.tif")
img1988=stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/ImageBuffalo1987-1989.tif")
img1988_water=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1988water.shp")
img1988_veg=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1988veg.shp")
img1988_urban=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1988urban.shp")
img1988_soil=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1988soil.shp")

img1998_water=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1998water.shp")
img1998_veg=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1998veg.shp")
img1998_urban=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1998urban.shp")
img1998_soil=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/1998soil.shp")

img2008_water=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2008water.shp")
img2008_veg=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2008veg.shp")
img2008_urban=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2008urban.shp")
img2008_soil=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2008soil.shp")

img2018_water=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2018water.shp")
img2018_veg=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2018veg.shp")
img2018_urban=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2018urban.shp")
img2018_soil=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/2018soil.shp")


extract_water_1988=raster::extract(img1988,img1988_water)
extract_veg_1988=raster::extract(img1988,img1988_veg)
extract_urban_1988=raster::extract(img1988,img1988_urban)
extract_soil_1988=raster::extract(img1988,img1988_soil)

extract_water_1998=raster::extract(img1988,img1998_water)
extract_veg_1998=raster::extract(img1988,img1998_veg)
extract_urban_1998=raster::extract(img1988,img1998_urban)
extract_soil_1998=raster::extract(img1988,img1998_soil)

extract_water_2008=raster::extract(img2008,img2008_water)
extract_veg_2008=raster::extract(img2008,img2008_veg)
extract_urban_2008=raster::extract(img2008,img2008_urban)
extract_soil_2008=raster::extract(img2008,img2008_soil)

extract_water_2018=raster::extract(img2018,img2018_water)
extract_veg_2018=raster::extract(img2018,img2018_veg)
extract_urban_2018=raster::extract(img2018,img2018_urban)
extract_soil_2018=raster::extract(img2018,img2018_soil)

water_1988<-colMeans(foreach(i=1:NROW(extract_water_1988),.combine = "rbind")%do%
                       unlist(extract_water_1988[[i]]))
soil_1988<-colMeans(foreach(i=1:NROW(extract_soil_1988),.combine = "rbind")%do%
                      unlist(extract_soil_1988[[i]]))
urban_1988<-colMeans(foreach(i=1:NROW(extract_urban_1988),.combine = "rbind")%do%
                       unlist(extract_urban_1988[[i]]))
veg_1988<-colMeans(foreach(i=1:NROW(extract_veg_1988),.combine = "rbind")%do%
                     unlist(extract_veg_1988[[i]]))

water_1998<-colMeans(foreach(i=1:NROW(extract_water_1998),.combine = "rbind")%do%
                       unlist(extract_water_1998[[i]]))
soil_1998<-colMeans(foreach(i=1:NROW(extract_soil_1998),.combine = "rbind")%do%
                      unlist(extract_soil_1998[[i]]))
urban_1998<-colMeans(foreach(i=1:NROW(extract_urban_1998),.combine = "rbind")%do%
                       unlist(extract_urban_1998[[i]]))
veg_1998<-colMeans(foreach(i=1:NROW(extract_veg_1998),.combine = "rbind")%do%
                     unlist(extract_veg_1998[[i]]))

water_2008<-colMeans(foreach(i=1:NROW(extract_water_2008),.combine = "rbind")%do%
                       unlist(extract_water_2008[[i]]))
soil_2008<-colMeans(foreach(i=1:NROW(extract_soil_2008),.combine = "rbind")%do%
                      unlist(extract_soil_2008[[i]]))
urban_2008<-colMeans(foreach(i=1:NROW(extract_urban_2008),.combine = "rbind")%do%
                       unlist(extract_urban_2008[[i]]))
veg_2008<-colMeans(foreach(i=1:NROW(extract_veg_2008),.combine = "rbind")%do%
                     unlist(extract_veg_2008[[i]]))

water_2018<-colMeans(foreach(i=1:NROW(extract_water_2018),.combine = "rbind")%do%
                       unlist(extract_water_2018[[i]]))
soil_2018<-colMeans(foreach(i=1:NROW(extract_soil_2018),.combine = "rbind")%do%
                      unlist(extract_soil_2018[[i]]))
urban_2018<-colMeans(foreach(i=1:NROW(extract_urban_2018),.combine = "rbind")%do%
                       unlist(extract_urban_2018[[i]]))
veg_2018<-colMeans(foreach(i=1:NROW(extract_veg_2018),.combine = "rbind")%do%
                     unlist(extract_veg_2018[[i]]))

end_1988<-rbind(water_1988,soil_1988,urban_1988,veg_1988)
rownames(end_1988)<-c('water',"soil",'urban','veg')
end_1998<-rbind(water_1998,soil_1998,urban_1998,veg_1998)
rownames(end_1998)<-c('water',"soil",'urban','veg')

end_2008<-rbind(water_2008,soil_2008,urban_2008,veg_2008)
rownames(end_2008)<-c('water',"soil",'urban','veg')

end_2018<-rbind(water_2018,soil_2018,urban_2018,veg_2018)
rownames(end_2018)<-c('water',"soil",'urban','veg')

probs <- mesma(img1988,end_1988, method = "NNLS")
probs1998 <- mesma(img1998,end_1998, method = "NNLS")
probs2008 <- mesma(img2008,end_2008, method = "NNLS")
probs2018 <- mesma(img2018,end_2018, method = "NNLS")

unmix1988<-writeRaster(probs,filename = 
                         file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'unmix1988.grd'),
                       bandorder="BIL",overwrite=TRUE)
unmix1998<-writeRaster(probs1998,filename = 
                         file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'unmix1998.grd'),
                       bandorder="BIL",overwrite=TRUE)
unmix2008<-writeRaster(probs2008,filename = 
                         file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'unmix2008.grd'),
                       bandorder="BIL",overwrite=TRUE)
unmix2018<-writeRaster(probs2018,filename = 
                         file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'unmix2018.grd'),
                       bandorder="BIL",overwrite=TRUE)

unmix1988<-stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/unmix1988.grd")
unmix1998<-stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/unmix1998.grd")
unmix2008<-stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/unmix2008.grd")
unmix2018<-stack("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/unmix2018.grd")

change_1998<- (unmix1998-unmix1988)[[1:4]]
change_2008<- (unmix2008-unmix1998)[[1:4]]
change_2018<- (unmix2018-unmix1998)[[1:4]]


vegincrease1988=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/1988vegincrease.shp")
vegdecrease1988=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/1988vegdecrease.shp")
other1988=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/1988other.shp")
vegnochange1988=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/1988vegnochange.shp")

vegincrease2008=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2008change/2008vegincrease.shp")
vegdecrease2008=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2008change/2008vegdecrease.shp")
other2008=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2008change/2008other.shp")
vegnochange2008=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2008change/2008vegnochange.shp")

vegincrease2018=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2018change/2018vegincrease.shp")
vegdecrease2018=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2018change/2018vegdecrease.shp")
other2018=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2018change/2018other.shp")
vegnochange2018=st_read("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/change/2018change/2018vegnochange.shp")


class_1988=data.frame(class=c(rep('vegincrease1988',length(vegincrease1988$ID)),
                   rep('vegdecrease1988',length(vegdecrease1988$ID)),
                   rep('vegnochange1988',length(vegnochange1988$ID)),
                   rep('other1988',length(other1988$ID))))
class_2008=data.frame(class=c(rep('vegincrease2008',length(vegincrease2008$ID)),
                              rep('vegdecrease2008',length(vegdecrease2008$ID)),
                              rep('vegnochange2008',length(vegnochange2008$ID)),
                              rep('other2008',length(other2008$ID))))
class_2018=data.frame(class=c(rep('vegincrease2018',length(vegincrease2018$ID)),
                              rep('vegdecrease2018',length(vegdecrease2018$ID)),
                              rep('vegnochange2018',length(vegnochange2018$ID)),
                              rep('other2018',length(other2018$ID))))

vegin_lonlat<-as.data.frame(st_coordinates(vegincrease1988$geometry))
vegde_lonlat<-as.data.frame(st_coordinates(vegdecrease1988$geometry))
vegno_lonlat<-as.data.frame(st_coordinates(vegnochange1988$geometry))
other_lonlat<-as.data.frame(st_coordinates(other1988$geometry))

vegin_lonlat2008<-as.data.frame(st_coordinates(vegincrease2008$geometry))
vegde_lonlat2008<-as.data.frame(st_coordinates(vegdecrease2008$geometry))
vegno_lonlat2008<-as.data.frame(st_coordinates(vegnochange2008$geometry))
other_lonlat2008<-as.data.frame(st_coordinates(other2008$geometry))

vegin_lonlat2018<-as.data.frame(st_coordinates(vegincrease2018$geometry))
vegde_lonlat2018<-as.data.frame(st_coordinates(vegdecrease2018$geometry))
vegno_lonlat2018<-as.data.frame(st_coordinates(vegnochange2018$geometry))
other_lonlat2018<-as.data.frame(st_coordinates(other2018$geometry))

lon_lat_1988<-rbind(vegin_lonlat,vegde_lonlat,vegno_lonlat,other_lonlat)
training1988<-SpatialPointsDataFrame(lon_lat_1988,class_1988)
crs(training1988)<-crs(change_1998)
mlh_1988<-superClass(change_1998, trainData = training1988, responseCol = "class", 
                                 model="mlc", tuneLength = 1, trainPartition = 0.7)

lon_lat_2008<-rbind(vegin_lonlat2008,vegde_lonlat2008,vegno_lonlat2008,other_lonlat2008)
training2008<-SpatialPointsDataFrame(lon_lat_2008,class_2008)
crs(training2008)<-crs(change_2008)
mlh_2008<-superClass(change_2008, trainData = training2008, responseCol = "class", 
                     model="mlc", tuneLength = 1, trainPartition = 0.5)


lon_lat_2018<-rbind(vegin_lonlat2018,vegde_lonlat2018,vegno_lonlat2018,other_lonlat2018)
training2018<-SpatialPointsDataFrame(lon_lat_2018,class_2018)
crs(training2018)<-crs(change_2018)
mlh_2018<-superClass(change_2018, trainData = training2018, responseCol = "class", 
                     model="mlc", tuneLength = 1, trainPartition = 0.5)
writeRaster(mlh_1988$map,filename = 
                         file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'mlh1998.grd'),
                       bandorder="BIL",overwrite=TRUE)
writeRaster(mlh_2008$map,filename = 
              file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'mlh2008.grd'),
            bandorder="BIL",overwrite=TRUE)
writeRaster(mlh_2018$map,filename = 
              file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/",'mlh2018.grd'),
            bandorder="BIL",overwrite=TRUE)

a1<-raster("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/mlh1998.grd")
a2<-raster("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/mlh2008.grd")
a3<-raster("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/mlh2018.grd")
writeRaster(a1, filename=file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/", "mlh1998.tif"), 
                  format="GTiff", overwrite=TRUE)
writeRaster(a2, filename=file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/", "mlh2008.tif"), 
            format="GTiff", overwrite=TRUE)
writeRaster(a3, filename=file.path("C:/Users/like9/Documents/GitHub/2019-geo511-project-kli57/data/", "mlh2018.tif"), 
            format="GTiff", overwrite=TRUE)

