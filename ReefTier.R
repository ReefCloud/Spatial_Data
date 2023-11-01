### 

get_reef_area<-function(id, ACA){
  hex<-id%>%h3_to_geo_boundary_sf()
  r.area <- st_intersection(ACA, hex$geometry) %>% 
    st_area()
  return(r.area)
}


ReefTier<-function(tier2, d.folder="D:\\GIS_Datasets/"){
  require(h3)
  require(sf)
  require(tidyverse)

  url_base<-"https://allencoralatlas.org/geoserver/ows?service=wms&version=2.0.0&request=GetMap&layers=coral-atlas:geomorphic_data_verbose&crs=EPSG:4326&styles=polygon&bbox=%s,%s,%s,%s&width=2048&height=2048&format=geojson"
  
  t2_bbox<-st_bbox(tier2) %>% round(., 5)
  sf_use_s2(FALSE)
  ACA<-st_read(sprintf(url_base, t2_bbox$xmin, t2_bbox$ymin,t2_bbox$xmax, t2_bbox$ymax)) %>% 
    st_make_valid()%>%
    st_filter(y = tier2, .predicate = st_intersects)%>%
    st_intersection(x=tier2)
  
  
  ACA<-ACA%>%st_union()%>%st_sf %>% st_cast%>%st_collection_extract()%>%st_union(by_feature=F)
  
  
  h3.aca<-data.frame(reef_id=polyfill(ACA,res=7))%>%
    group_by(reef_id)%>%
    mutate(reef_area=get_reef_area(reef_id,ACA), source="ACA_Geomophic")%>%
    ungroup()
  
  
  ### Check against Millenium coral reef mapping project and add missing reef polygons
  mcr<-read_sf(file.path(d.folder,"Reefs/MCRMP_reef500/WCMC008_CoralReef2018_Py_v4_1.shp"))
  sf_use_s2(FALSE)

  mcr<-mcr%>%
    st_transform(crs = 4326)%>%
    st_intersection(tier2)
  
  mcr<-mcr%>%st_union()%>%st_sf %>% st_cast%>%st_collection_extract()%>%st_union(by_feature=F)
  
  
  h3.mcr<-data.frame(reef_id=polyfill(mcr[1],res=7))%>%
    dplyr::filter(!reef_id %in% c(h3.aca$reef_id))%>%
    # as.data.frame() %>% 
    # rename(reef_id=colnames(.)[1])%>%
    group_by(reef_id)%>%
    mutate(reef_area=get_reef_area(reef_id,mcr), source="MCRMP_Reef500poly")%>%
    ungroup()
  
  h3.poly<-h3.aca%>%
    bind_rows(h3.mcr)%>%
    as.data.frame() %>% 
    ungroup()%>%
    mutate(reef_id=as.factor(reef_id))
  
  
  
  tier5<-h3_to_geo_boundary_sf(h3.poly$reef_id)%>%
    rename(reef_id=h3_index)%>%
    mutate(reef_id=as.factor(reef_id)) %>% 
    merge(h3.poly, by="reef_id")
  
  return(tier5)
  
}


#Run Function Example###########################################
# tier2.file="D:\\GIS/ReefCloud_regions/tier2/VUT_tier2.geojson"
# ReefTier(tier2.file = tier2.file, overwrite = T)
#################################################################


