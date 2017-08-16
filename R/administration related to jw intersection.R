setwd("C:")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) ####intersect library


crswgs4326=CRS("+init=EPSG:4326")

##### upload adminsitrations-jurisdictional waters layer

org <- readShapePoly ("C:/R Fed/Viewer project/Results/jw_admins.shp",proj4string=crswgs4326,verbose=TRUE)
jw.laea <- spTransform(org,CRS("+init=EPSG:3035"))
### plot(org)

##### upload bluefin tuna dynamic zones

crswgs4326=CRS("+init=EPSG:4326")
dynamic_zones <- readShapePoly("C:\\Governance_test\\result_poly\\bf_06_2003.shp", proj4string=crswgs4326,verbose=TRUE) 
zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))

## make a 0 buffer to eliminate holes

zones.b <- gBuffer(zones.laea, byid=TRUE, width=0)
jw.b <- gBuffer(jw.laea, byid=TRUE, width=0)

## intersect poligon with jurisdictional waters layer
Results <- gIntersects(zones.b,jw.b,byid=TRUE)

## create a dtatframe with only selected administrations
Results <- as.data.frame(Results)

colnames(Results)<-"Select"

jw_infos <- jw.b@data 
Results_info <- cbind(Results, jw_infos) 

selected <- Results_info %>% 
  filter(Select == TRUE) # %>% 


#### 
#################
###### percentage intersect calculation ######################################################
library(PBSmapping)
library(raster)



j <- SpatialPolygons2PolySet(jw.laea)  #### very slow
pol <- SpatialPolygons2PolySet(zones.laea) #### very slow


union.jpol <- combinePolys(joinPolys(j, pol,"UNION"))
area.union.jpol <- sum(calcArea(union.jpol, rollup=1)$area)

detach("package:dplyr") ## dplyr in conflict with raster for intersect function



# intersect from raster package
pi <- intersect(zones.b, jw.b)
plot(zones.b, axes=T); plot(jw.b, add=T); plot(pi, add=T, col='red')

# Extract areas from polygon objects then attach as attribute
## 
## http://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) {slot(x, 'area')}))

row.names(areas) <- sapply(pi@polygons, FUN=function(x) {slot(x, 'ID')})
# Combine attributes info and areas 
attArea <- spCbind(pi, areas)

areas.poly <- data.frame(area=sapply(zones.b@polygons, FUN=function(x) {slot(x, 'area')}))

attArea$intersect <- paste(round((attArea$area.1/areas.poly$area)*100,digits=1),"%",sep="")

View(attArea@data)
