### REEFCLOUD BIOREGIONALISATION
# '''Objective: Define geographic scales for reporting on the coral reef monitoring across the globe'''


##SET UP ####
rm(list=ls())


d.folder<-"C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/"


makeTiers<-function(territoryName, d.folder="C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/"){
  require(tidyverse)
  require(osmdata)
  require(sf)
  require(rgdal)
  require(mregions)
  require(rgeos)
  require(maptools)
  require(sp)
  require(lwgeom)
  
  #Source custom functions
  source("Administrative_Boundaries.R")
  source("ReefTier.R")
  
  
  ##Country (EEZ)#### 
  #NOTE: Some countries are split in two IHO. So, we not cropping EEZ within IHO
  #We need to have all the tier from 2+ to be nested within the EEZ
  
  
  country=read_sf(file.path(d.folder,"MarineRegions/EEZ_land_union_v3_202003/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp"))%>%
    filter(grepl(territoryName,TERRITORY1), 
           is.na(TERRITORY2))%>% ##Added this to remove EEZ of joint regimes
    rename(name=TERRITORY1, source_id=MRGID_EEZ)%>%
    mutate(class="Geopolitical", tier=NA, source="EEZ_land_union_v3",
           ISO=ifelse(is.na(ISO_TER1), name, ISO_TER1),
           territory=name, sovereign=SOVEREIGN1)%>%
    dplyr::select(name,class,tier,source_id,source,ISO,territory, sovereign, geometry)
  
  dir.create(file.path(d.folder,"ReefCloud_regions/eez/", showWarnings = F))
  st_write(country, file.path(d.folder,sprintf("ReefCloud_regions/eez/%s_eez.geojson", territoryName)),
           delete_dsn=T,quiet = T,append = F)
  
  ##Define some anciliary info
  countryISO=country$ISO
  admin_level=dplyr::case_when(territoryName %in% c("Hawaii", "Bermuda") ~ 6,
                               territoryName %in% c("Jamaica","Grenada","Curaçao","Bonaire","Aruba","Anguilla", "Saint Vincent", "Turks and Caicos") ~ 2,
                               territoryName %in% c("American Samoa", "WSM") ~ 8,
                               .default=4)
  
  
  ## Tier 2: LME or Country #### 
  if (countryISO %in% c("AUS")){
    LME<-read_sf("MarineRegions/LME/LMEs66.shp")%>%
      st_filter(country, .pred = st_intersects)%>%
      st_shift_longitude()
    tier2<-read_sf("MarineRegions/EEZ_land_union_v3_202003/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")%>%
      filter(ISO_TER1 %in% countryISO)%>%
      st_intersection(y = LME)%>%
      st_shift_longitude()%>%
      rename(name=LME_NAME)%>% 
      mutate(tier_id=as.numeric(paste0(MRGID_EEZ,LME_NUMBER)), tier=2, tier_source="LMEs66", ISO=countryISO, territory=TERRITORY1, sovereign=SOVEREIGN1)%>%
      dplyr::select(name,tier,tier_id,tier_source,ISO,territory, sovereign, geometry)
  }else{
    
    tier2<-country%>%
      # st_shift_longitude()%>%
      rename( tier_id=source_id)%>%
      mutate(tier=2, tier_source="EEZ_land_union_v3")%>%
      dplyr::select(name,tier,tier_id,tier_source,ISO, territory,sovereign, geometry)
  }
  
  #Validate tier
  # valid=st_is_valid(tier2)
  # tier2=tier2 %>%st_set_precision(1000000) %>% st_make_valid()
  # tier2=st_buffer(tier2[!is.na(valid),], 0.0)
  # # dir.create("ReefCloud_regions/eez/", showWarnings = F)
  dir.create(file.path(d.folder,"ReefCloud_regions/tier2"))
  st_write(tier2, file.path(d.folder,sprintf("ReefCloud_regions/tier2/%s_tier2.geojson", territoryName)),
           delete_dsn=T,quiet = T,append = F)
  
  ###Generate Reef Polygons (tier 5)
  # ReefTier(tier2.file = sprintf("ReefCloud_regions/tier2/%s_tier2.geojson", countryISO))
  if (file.exists(file.path(d.folder,sprintf("ReefCloud_regions/tiers_databse/%s_tier5.geojson", countryISO)))){
    tier5<-st_read(file.path(d.folder,sprintf("ReefCloud_regions/tiers_databse/%s_tier5.geojson", countryISO)))
    t5.exist=T
  } else {
    tier5<-ReefTier(tier2, 
                    d.folder = d.folder)
    t5.exist=F
  }
  # tier5<-tier5%>%
  #   st_intersection(., y=tier2)
  # tier5<-tier5[,1:3]
  ## Tier 3:MEOW#### 
  #NOTE: for some countries tier3 will be the same as tier2...or very similar
  
  
  
  ##Tier 4: Administrative boundaries ########
  if (countryISO=="AUS"){
    tier4<-st_read(file.path(d.folder,"ReefCloud_regions//tiers//tier4//AUS_tier4.geojson"))
    tier3<-st_read(file.path(d.folder,"ReefCloud_regions//tiers//tier3//AUS_tier3.geojson"))
  } else {
    MEOW<-read_sf(file.path(d.folder,"MarineRegions/MEOW/meow_ecos.shp"))%>%
      st_filter(tier2, .predicate = st_intersects)%>%
      st_filter(tier5, .predicate = st_intersects)
    valid=st_is_valid(MEOW)
    MEOW=MEOW %>%st_set_precision(1000000) %>% st_make_valid()
    MEOW=st_buffer(MEOW[!is.na(valid),], 0.0)
    
    # only create tier 3 if there is more than one MEOW in tier 2
    
    tier3<-tier2%>%
      st_intersection(x=MEOW)%>%
      st_filter(tier5, .predicate = st_intersects)%>%
      dplyr::select(-c(name, tier,tier_id))%>%
      rename(name=ECOREGION, tier_id=ECO_CODE)%>%
      mutate(tier=3, tier_source="MEOW")%>%
      dplyr::select(name,tier,tier_id,tier_source, geometry)
    
    #admin_level ##Often admin_level=4 work for small nations. Change this For large countries (e.g., countries) to admin level=1 or 2as appropiate
    tier4=AdminBoundaries(tier2,admin_level =admin_level)  
    valid=st_is_valid(tier4)
    tier4=tier4 %>%st_set_precision(1000000) %>% st_make_valid()
    tier4=st_buffer(tier4[!is.na(valid),], 0.0)
    tier4<-st_difference(tier4)
    
    sf_use_s2(FALSE)
    
    tier4=tier4%>%
      st_collection_extract()%>%
      st_intersection(y=tier3)%>%
      dplyr::select(-tier_id)%>%
      rename(name=name, tier_id=osm_id)%>%
      # st_cast(., to="MULTIPOLYGON")
      mutate(tier=4, tier_source=sprintf("OSM_AdminitrationBoundary_level%s",admin_level))%>%
      dplyr::select(name,tier,tier_id,tier_source, geometry)%>%
      st_filter(tier5, .predicate = st_intersects)%>%
      dplyr::group_by(name, tier, tier_id, tier_source) %>%
      dplyr::summarise()%>%
      ungroup
    
  }
  
  
  #add tier IDs and save files
  dir.create(file.path(d.folder,"ReefCloud_regions/tiers_databse/", showWarnings = F))
  
  tier3<-tier3 %>%
    rename(source_id=tier_id)%>%
    st_join(tier2 %>% dplyr::select(tier_id, ISO, territory, sovereign, geometry))%>%
    tibble::rowid_to_column(., "ID")%>%
    mutate(tier_id=as.numeric(paste0(tier_id,ID)))%>%
    dplyr::select(-ID)
  tier4<-tier4 %>%
    ungroup%>%
    # select(-source_id)%>%
    rename(source_id=tier_id)%>%
    st_join(tier3 %>% dplyr::select(tier_id, ISO, territory, sovereign,geometry))%>%
    st_difference()%>%
    tibble::rowid_to_column(., "ID")%>%
    mutate(tier_id=as.numeric(paste0(tier_id,ID)))%>%
    dplyr::select(-ID)
  
  if (t5.exist==F){
    tier5<-tier5 %>%
      ungroup%>%
      # rename(source_id=tier_id)%>%
      st_join(tier4 %>% dplyr::select(tier_id,ISO, territory, sovereign, geometry))%>%
      tibble::rowid_to_column(., "ID")%>%
      mutate(tier_id=as.numeric(paste0(tier_id,ID)))%>%
      dplyr::select(-ID)%>%
      st_cast(to = "MULTIPOLYGON")
    st_write(tier5, file.path(d.folder,sprintf("ReefCloud_regions/tiers_databse/%s_tier5.geojson", countryISO)), 
             delete_dsn=T,quiet = T,append = F)
  }
  
  ##Join tiers
  t24=tier2%>%
    mutate(source_id=tier_id)%>%
    bind_rows(tier3, tier4)%>%
    st_cast(to = "MULTIPOLYGON")
  
  #Write File
  st_write(t24, file.path(d.folder,sprintf("ReefCloud_regions/tiers_databse/%s_tier2_4.geojson", countryISO)), 
           delete_dsn=T,quiet = T,append = F)
  
}

