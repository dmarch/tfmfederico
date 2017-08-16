setwd("C:")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) ####intersect library


crswgs4326=CRS("+init=EPSG:4326")

##### in this case I should probably make a subset of the orgs with punctual reference not reletable with jw based on sector & org_type column

org <- readShapePoly ("C:/R Fed/Viewer project/Results/jw_admins.shp",proj4string=crswgs4326,verbose=TRUE)
jw.laea <- spTransform(org,CRS("+init=EPSG:3035"))
### plot(org)

### now I will do the intersection with the jurisdictional water poligons data frame jw.laea 
### for some reason the intersection doesn't work with the "admin_join" layer (something related with 
### the merge data changes: e.g. multipleGeometries=TRUE)
### so first I will do the intersection and afdter I will relate the intersected poligons with the corresponding admins


crswgs4326=CRS("+init=EPSG:4326")
dynamic_zones <- readShapePoly("C:\\Governance_test\\result_poly\\bf_06_2003.shp", proj4string=crswgs4326,verbose=TRUE) 
zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))

zones.b <- gBuffer(zones.laea, byid=TRUE, width=0)
jw.b <- gBuffer(jw.laea, byid=TRUE, width=0)
## intersect poligon with jurisdictional waters layer: I use the previously created layer (above*)
Results <- gIntersects(zones.b,jw.b,byid=TRUE)


Results <- as.data.frame(Results)
# rownames(Results)<-clean_jw@data$GeoName ### I can skip this
colnames(Results)<-"Select"
### clean_jw has a lot of columns in the dataframe so before the bind I should leave only the useful ones
jw_infos <- jw.b@data ## %>% select(1: 25)
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
