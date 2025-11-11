# ReefTier.R
# 
# Purpose:
# This script contains functions to generate tier 5 reef polygons using data from the Allen Coral Atlas 
# and the Millennium Coral Reef Mapping Project. It calculates the area of the reef within a given H3 hexagon 
# and processes tier 2 geographic data to generate tier 5 reef polygons.
#
# Author: Manuel Gonzalez-Rivero
# Last Significant update: March 14, 2025
# 
# Dependencies:
# - h3
# - sf
# - tidyverse
# - progress
#
# Usage:
# - Source this script in your R environment.
# - Call the ReefTier function with appropriate arguments.
#
# Example:
# tier2.file="D:\\GIS/ReefCloud_regions/tier2/VUT_tier2.geojson"
# ReefTier(tier2.file = tier2.file, overwrite = T)
###

# Function to get reef area
# This function calculates the area of the reef within a given H3 hexagon.
# Args:
#   id: H3 index.
#   ACA: Spatial data of the Allen Coral Atlas.
# Returns:
#   The area of the reef within the H3 hexagon.
get_reef_area <- function(id, reef) {
  hex <- id %>% h3_to_geo_boundary_sf()
  r.area <- st_intersection(reef, hex$geometry) %>% 
    st_area()
  if (length(r.area) == 0) {
    return(units::set_units(0, "m^2"))
  }
  return(r.area)
}

# Function to generate tier 5 reef polygons
# This function generates tier 5 reef polygons using data from the Allen Coral Atlas and the Millennium Coral Reef Mapping Project.
# Args:
#   tier2: Spatial data of tier 2 regions.
#   d.folder: Directory containing GIS datasets.
# Returns:
#   Spatial data of tier 5 reef polygons.
ReefTier <- function(tier2, d.folder = "GIS") {
  library(h3)
  library(sf)
  library(tidyverse)
  library(progress)
  source("ReefTier_ACA.R") ##function to add more H3 hexagons from reefs mapped by the Allen Coral Atlas (ACA)
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Getting reef hexagons [:bar] :percent in :elapsed",
    total = 1, clear = FALSE, width = 60
  )
  pb$tick(0)
  
  # Load data from local files using the WCMC files
  reefs<-read_sf(
    file.path(d.folder,
              "Reefs/reefsWCMC.geojson")) 
  reefs<-reefs[st_filter(tier2, reefs,.predicate = st_contains), ]
  
  pb$tick()
  
  
  # Merge polygons nearby
  # reef.merged <- reefs %>%
  #   st_transform(3857) %>%
  #   st_buffer(dist = units::set_units(3, "m")) %>%
  #   summarise(geometry = st_union(geometry)) %>%
  #   st_cast("POLYGON") %>%
  #   st_transform(4326)
  
  # Process merged polygon to generate H3 grid where there are coral reefs
  h3.reefs <- data.frame(reef_id = character(), reef_area = units::set_units(numeric(), "m^2"), source = character())
  
  pb <- progress_bar$new(
    format = "  Processing reef data [:bar] :percent in :elapsed",
    total = dim(reefs)[1], clear = FALSE, width = 60
  )
  
  
  
  for (i in seq(1:dim(reefs)[1])) {
    pb$tick()
    reef.h3<-reefs[i,] |> st_transform( crs = 3857) |> st_buffer(dist = 5000) |> st_transform(crs=4326)
    
    if (st_area(reefs[i, ]) < units::set_units(5162000, "m^2")) {
      this.id <- data.frame(reef_id = geo_to_h3(st_centroid(reefs[i, ]), res = 7)) %>%
        mutate(reef_area = get_reef_area(reef_id, reefs[i, ]), source = "UNEP-WCMC, WorldFish Centre, WRI, TNC (2021)") %>%
        ungroup()
    } else {
      this.id <- data.frame(reef_id = polyfill(reef.h3, res = 7)) %>%
        group_by(reef_id) %>%
        mutate(reef_area = get_reef_area(reef_id, reefs[i, ]), source = "UNEP-WCMC, WorldFish Centre, WRI, TNC (2021)") %>%
        ungroup() |> 
        filter(reef_area > units::set_units(0, "m^2"))   
    }
    # Check for any missing reefs. Missing reefs are those that do not fit into the H3 hexagon at res 7
    id_list<-list()
    a=0
    for (x in this.id$reef_id) {
      r.check <- tibble(reef_id = k_ring(x, 1)) |> 
        filter(!(reef_id %in% x)) 
      if(dim(r.check)[1]>0){
        a=a+1
        r.check<-r.check |> 
          group_by(reef_id) |> 
          mutate(reef_area = get_reef_area(id = reef_id,reef= reefs[i,]), source =  "UNEP-WCMC, WorldFish Centre, WRI, TNC (2021)") |> 
          ungroup() |> 
          filter(reef_area > units::set_units(0, "m^2"))      
        id_list[[a]] <- r.check
      }
      
    }
    
    h3.reefs <- bind_rows(h3.reefs, this.id, bind_rows(id_list))
    
  }
  pb$terminate()
  gc()
  
  h3.reefs<-h3.reefs |> select(reef_id) |> unique() |>  
    group_by(reef_id)  |> 
    mutate(reef_area=sum(get_reef_area(reef_id, reefs)),
           source =  "UNEP-WCMC, WorldFish Centre, WRI, TNC (2021)") |> ungroup()
  
  # Generate tier 5 reef polygons
  tier5 <- h3_to_geo_boundary_sf(h3.reefs$reef_id) %>%
    rename(reef_id = h3_index) %>%
    mutate(reef_id = as.factor(reef_id)) %>%
    merge(h3.reefs, by = "reef_id")
  
  
  t5.aca<-ReefTier_filler(tier=tier2, tier5=tier5,d.folder = d.folder)
  
  tier5<-tier5 |> bind_rows(t5.aca)
  
  gc()
  return(tier5)
}

# DO NOT RUN: Function Example
# tier2.file="D:\\GIS/ReefCloud_regions/tier2/VUT_tier2.geojson"
# ReefTier(tier2.file = tier2.file, overwrite = T)
