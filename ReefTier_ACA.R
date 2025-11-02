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
get_reef_area <- function(id, ACA) {
  hex <- id %>% h3_to_geo_boundary_sf()
  r.area <- st_intersection(ACA, hex$geometry) %>% 
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
  
  url_base <- "https://allencoralatlas.org/geoserver/ows?service=wms&version=2.0.0&request=GetMap&layers=coral-atlas:geomorphic_data_verbose&crs=EPSG:4326&styles=polygon&bbox=%s,%s,%s,%s&width=2048&height=2048&format=geojson"
  
  t2_bbox <- st_bbox(tier2) %>% round(., 5)
  sf_use_s2(FALSE)
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Fetching data from Allen Coral Atlas [:bar] :percent in :elapsed",
    total = 1, clear = FALSE, width = 60
  )
  pb$tick(0)
  
  # Fetch data from Allen Coral Atlas
  ACA <- tryCatch({
    suppressMessages(suppressWarnings(
      st_read(sprintf(url_base, t2_bbox$xmin, t2_bbox$ymin, t2_bbox$xmax, t2_bbox$ymax)) %>% 
        st_make_valid() %>%
        st_filter(y = tier2, .predicate = st_intersects) %>%
        st_intersection(x = tier2)
    ))
  }, error = function(e) {
    message("Error fetching data from Allen Coral Atlas: ", e$message)
    return(NULL)
  })
  
  pb$tick()
  
  if (is.null(ACA)) return(NULL)
  
  # Merge polygons nearby
  ACA.merged <- ACA %>%
    st_transform(3857) %>%
    st_buffer(dist = units::set_units(3, "m")) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_cast("POLYGON") %>%
    st_transform(4326)
  
  # Process merged polygon to generate H3 grid where there are coral reefs
  h3.aca <- data.frame(reef_id = character(), reef_area = units::set_units(numeric(), "m^2"), source = character())
  
  pb <- progress_bar$new(
    format = "  Processing ACA data [:bar] :percent in :elapsed",
    total = dim(ACA.merged)[1], clear = FALSE, width = 60
  )
  
  for (i in seq(1:dim(ACA.merged)[1])) {
    pb$tick()
    if (st_area(ACA.merged[i, ]) < units::set_units(5162000, "m^2")) {
      this.id <- data.frame(reef_id = geo_to_h3(st_centroid(ACA.merged[i, ]), res = 7)) %>%
        mutate(reef_area = get_reef_area(reef_id, ACA.merged[i, ]), source = "ACA_Geomophic") %>%
        ungroup()
    } else {
      this.id <- data.frame(reef_id = polyfill(ACA.merged[i, ], res = 7)) %>%
        group_by(reef_id) %>%
        mutate(reef_area = get_reef_area(reef_id, ACA.merged[i, ]), source = "ACA_Geomophic") %>%
        ungroup()
    }
    # Check for any missing reefs. Missing reefs are those that do not fit into the H3 hexagon at res 7
    for (x in this.id$reef_id) {
      r.check <- data.frame(reef_id = k_ring(x, 1)) %>%
        filter(!(reef_id %in% this.id$reef_id)) %>%
        rowwise() %>%
        mutate(reef_area = get_reef_area(reef_id, ACA.merged[i, ]), source = "ACA_Geomophic") %>%
        ungroup() %>%
        filter(reef_area > units::set_units(0, "m^2"))
      this.id <- bind_rows(this.id, r.check)
    }
    h3.aca <- bind_rows(h3.aca, this.id)
  }
  
  gc()
  
  # Fetch reefs from the Millennium Coral Reef Mapping Project
  pb <- progress_bar$new(
    format = "  Fetching data from MCRMP [:bar] :percent in :elapsed",
    total = 1, clear = FALSE, width = 60
  )
  pb$tick(0)
  
  mcr <- tryCatch({
    suppressMessages(suppressWarnings(
      read_sf(file.path(d.folder, "Reefs/MCRMP_reef500/WCMC008_CoralReef2018_Py_v4_1.shp")) %>%
        st_transform(crs = 4326) %>%
        st_intersection(tier2)
    ))
  }, error = function(e) {
    message("Error reading MCRMP data: ", e$message)
    return(NULL)
  })
  
  pb$tick()
  
  if (!is.null(mcr)) {
    mcr.merged <- mcr %>%
      st_transform(3857) %>%
      st_buffer(dist = units::set_units(3, "m")) %>%
      st_union %>%
      # summarise(geometry = st_union(geometry)) %>%
      st_cast("POLYGON") %>%
      st_transform(4326) %>%
      st_as_sf()
    
    # Process merged polygon to generate H3 grid where there are coral reefs
    h3.mcr <- data.frame(reef_id = character(), reef_area = units::set_units(numeric(), "m^2"), source = character())
    
    pb <- progress_bar$new(
      format = "  Processing MCRMP data [:bar] :percent in :elapsed",
      total = dim(mcr.merged)[1], clear = FALSE, width = 60
    )
    
    for (i in seq(1:dim(mcr.merged)[1])) {
      pb$tick()
      if (st_area(mcr.merged[i, ]) < units::set_units(5162000, "m^2")) {
        this.id <- data.frame(reef_id = geo_to_h3(st_centroid(mcr.merged[i, ]), res = 7)) %>%
          mutate(reef_area = get_reef_area(reef_id, mcr.merged[i, ]), source = "MCRMP_Reef500poly") %>% 
          ungroup()
      } else {
        this.id <- data.frame(reef_id = polyfill(mcr.merged[i, ], res = 7)) %>%
          group_by(reef_id) %>%
          mutate(reef_area = get_reef_area(reef_id, mcr.merged[i, ]), source = "MCRMP_Reef500poly") %>%
          ungroup()
      }
      # Check for any missing reefs. Missing reefs are those that do not fit into the H3 hexagon at res 7
      for (x in this.id$reef_id) {
        r.check <- data.frame(reef_id = k_ring(x, 1)) %>%
          dplyr::filter(!(reef_id %in% this.id$reef_id)) %>%
          group_by(reef_id) %>%
          mutate(reef_area = get_reef_area(reef_id, mcr.merged[i, ]), source = "MCRMP_Reef500poly") %>%
          ungroup() %>%
          filter(reef_area > units::set_units(0, "m^2"))
        this.id <- bind_rows(this.id, r.check)
      }
      h3.mcr <- bind_rows(h3.mcr, this.id)
    }
  }
  
  gc()
  
  # Combine ACA and MCR data
  h3.poly <- h3.aca %>%
    bind_rows(h3.mcr %>% filter(!(reef_id %in% h3.aca$reef_id))) %>%
    as.data.frame() %>%
    ungroup() %>%
    mutate(reef_id = as.factor(reef_id))
  
  # Generate tier 5 reef polygons
  tier5 <- h3_to_geo_boundary_sf(h3.poly$reef_id) %>%
    rename(reef_id = h3_index) %>%
    mutate(reef_id = as.factor(reef_id)) %>%
    merge(h3.poly, by = "reef_id")
  
  gc()
  return(tier5)
}

# DO NOT RUN: Function Example
# tier2.file="D:\\GIS/ReefCloud_regions/tier2/VUT_tier2.geojson"
# ReefTier(tier2.file = tier2.file, overwrite = T)
