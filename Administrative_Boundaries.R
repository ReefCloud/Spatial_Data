AdminBoundaries<-function(SHP,admin_level=4){
  
  #install the osmdata, sf, tidyverse and ggmap package
  if(!require("osmdata")) install.packages("osmdata")
  if(!require("tidyverse")) install.packages("tidyverse")
  if(!require("sf")) install.packages("sf")
  if(!require("ggmap")) install.packages("ggmap")
  if(!require("mregions")) install.packages("mregions")
  if(!require("poorman")) install.packages("poorman")
  #load packages
  require(tidyverse)
  require(osmdata)
  require(sf)
  require(rgdal)
  require(mregions)
  require(rgeos)
  require(maptools)
  require(poorman)

  

  SHP=st_as_sf(SHP)
  SHP=st_wrap_dateline(SHP)
  SHP=st_as_sf(SHP)
  ### Detect those polygons that are crossed by the antimeridian
  antimeridian <- SpatialLines(list(Lines(slinelist=list(Line(coords=cbind(c(179,179), c(90,-90)))), ID="1"),
                                    Lines(slinelist=list(Line(coords=cbind(c(-179,-179), c(90,-90)))), ID="2")),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  antimeridian=st_as_sf(antimeridian)
  intrscts <- st_intersection(antimeridian, SHP)
  # any(intrscts[,1] & intrscts[,2])
  # intrscts <- which(intrscts[,1] & intrscts[,2])
  
  if (dim(intrscts)[1]>0){
    crds <- st_coordinates(SHP) # get coordinates
    crds.west <- subset(crds, crds[,1] <= 0)               # western half of the polygon
    crds.east<- subset(crds, crds[,1] >= 0)
    m=list()
    m[[1]]<-matrix(c(min(crds.east[,1]),max(crds.east[,1]),
                     min(crds.east[,2]),max(crds.east[,2])),
                   nrow = 2,ncol = 2,byrow = T,
                   dimnames = list(c("x", "y"),
                                   c("min", "max")))
    m[[2]]<-matrix(c(min(crds.west[,1]),max(crds.west[,1]),
                     min(crds.west[,2]),max(crds.west[,2])),
                   nrow = 2,ncol = 2,byrow = T,
                   dimnames = list(c("x", "y"),
                                   c("min", "max")))
    
    
    q.east <- m[[1]] %>% 
      opq (timeout = 25*100) %>%
      add_osm_feature(key="boundary", value="administrative")%>%
      add_osm_feature(key="admin_level", value=admin_level)
    
    q.west <- m[[2]] %>% 
      opq (timeout = 25*100) %>%
      add_osm_feature(key="boundary", value="administrative")%>%
      add_osm_feature(key="admin_level", value=admin_level)
    
    admin.b.east <- osmdata_sf(q.east)%>%
      unname_osmdata_sf()%>%
      poorman::pull(
        "osm_multipolygons"
      )
    admin.b.west <- osmdata_sf(q.west)%>%
      unname_osmdata_sf()%>%
      poorman::pull(
        "osm_multipolygons"
      )
    if (is.null(admin.b.east)){
      admin.b=admin.b.west
    }else if(is.null(admin.b.west)){
      admin.b=admin.b.east
    }else{
    comon.columns<-intersect(names(admin.b.east),names(admin.b.west))
    
    admin.b<-rbind(admin.b.east[,comon.columns], admin.b.west[,comon.columns])
    }
  } else {
    #bounding box for the country
    m <- st_bbox(SHP)
    
    #building the query in OSM
    q <- m %>% 
      opq (timeout = 25*100) %>%
      add_osm_feature(key="boundary", value="administrative")%>%
      add_osm_feature(key="admin_level", value=admin_level)
      
    
    #query
    admin.b <- osmdata_sf(q)%>%
      unname_osmdata_sf()%>%
      poorman::pull(
        "osm_multipolygons"
      )
    
  }
  
  return(admin.b)
  
  
}
