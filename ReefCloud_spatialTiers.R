### REEFCLOUD BIOREGIONALISATION
# Objective: Define geographic scales for reporting on the coral reef monitoring across the globe in ReefCloud.ai by creating tiered regions (tiers 2 to 5) using various spatial datasets.

# Purpose:
# This script is a wrapper for generating regional aggregations in ReefCloud. It defines geographic scales for reporting on coral reef monitoring across the globe by creating tiered regions (tiers 2 to 5) using various spatial datasets.

# Author: Manuel Gonzalez-Rivero
# Last Significant update: March 14, 2025

# Dependencies:
# - tidyverse
# - sf
# - mregions
# - progress

# Usage:
# - Source this script in your R environment.
# - Call the makeTiers function with appropriate arguments.

# Example:
# makeTiers("Vietnam", FALSE)

## SET UP ####

# Capture command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No territory name provided. Usage: Rscript ReefCloud_spatialTiers.R <territoryName>")
}
territoryName <- args[1]
exp.b <- as.logical(args[2])

makeTiers <- function(territoryName, exp.b = FALSE, d.folder = "GIS") {
  
  suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(mregions2)
  require(progress)
  require(nngeo)
  })
  
  # Source custom functions
  suppressPackageStartupMessages({
  source("Administrative_Boundaries.R")
  source("ReefTier.R")
  })
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent in :elapsed",
    clear = FALSE, width = 60
  )
  pb$tick(0)
  
  ## Country (EEZ) ####
  pb$tick()
  country <- suppressMessages(suppressWarnings(
    read_sf(file.path(d.folder, "MarineRegions/EEZ_land_union_v3_202003/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")) %>%
      filter(grepl(territoryName, TERRITORY1), is.na(TERRITORY2)) %>%
      rename(name = TERRITORY1, source_id = MRGID_EEZ) %>%
      mutate(class = "Geopolitical", tier = NA, source = "EEZ_land_union_v3",
             ISO = ifelse(is.na(ISO_TER1), name, ISO_TER1),
             territory = name, sovereign = SOVEREIGN1) %>%
      dplyr::select(name, class, tier, source_id, source, ISO, territory, sovereign, geometry)
  ))
  
  dir.create(file.path(d.folder, "ReefCloud_regions/eez/"), showWarnings = FALSE,recursive = TRUE)
  st_write(country, file.path(d.folder, sprintf("ReefCloud_regions/eez/%s_eez.geojson", territoryName)),
           delete_dsn = TRUE, quiet = TRUE, append = FALSE)
  
  ## Define some ancillary info
  countryISO <- country$ISO
  admin_level <- dplyr::case_when(
    territoryName %in% c("Hawaii", "Bermuda") ~ 6,
    territoryName %in% c("Jamaica", "Grenada", "Curaçao", "Bonaire", "Aruba", "Anguilla", "Saint Vincent", "Turks and Caicos") ~ 2,
    territoryName %in% c("American Samoa", "WSM") ~ 8,
    .default = 4
  )
  
  pb$tick()
  
  ## Tier 2: LME or Country ####
  pb$tick()
  if (countryISO %in% c("AUS")) {
    LME <- suppressMessages(suppressWarnings(
      read_sf("MarineRegions/LME/LMEs66.shp") %>%
        st_filter(country, .pred = st_intersects) %>%
        st_shift_longitude()
    ))
    tier2 <- suppressMessages(suppressWarnings(
      read_sf("MarineRegions/EEZ_land_union_v3_202003/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>%
        filter(ISO_TER1 %in% countryISO) %>%
        st_intersection(y = LME) %>%
        st_shift_longitude() %>%
        rename(name = LME_NAME) %>%
        mutate(tier_id = as.numeric(paste0(MRGID_EEZ, LME_NUMBER)), tier = 2, tier_source = "LMEs66", ISO = countryISO, territory = TERRITORY1, sovereign = SOVEREIGN1) %>%
        dplyr::select(name, tier, tier_id, tier_source, ISO, territory, sovereign, geometry)
    ))
  } else {
    tier2 <- country %>%
      rename(tier_id = source_id) %>%
      mutate(tier = 2, tier_source = "EEZ_land_union_v3") %>%
      dplyr::select(name, tier, tier_id, tier_source, ISO, territory, sovereign, geometry)
  }
  
  dir.create(file.path(d.folder, "ReefCloud_regions/tier2"), showWarnings = FALSE)
  st_write(tier2, file.path(d.folder, sprintf("ReefCloud_regions/tier2/%s_tier2.geojson", territoryName)),
           delete_dsn = TRUE, quiet = TRUE, append = FALSE)
  
  pb$tick()
  
  ### Generate Reef Polygons (tier 5)
  pb$tick()
  if (file.exists(file.path(d.folder, sprintf("ReefCloud_regions/tiers_databse/%s_tier5.geojson", countryISO)))) {
    tier5 <- suppressMessages(suppressWarnings(
      st_read(file.path(d.folder, sprintf("ReefCloud_regions/tiers_databse/%s_tier5.geojson", countryISO)))
    ))
    t5.exist <- TRUE
  } else {
    tier5 <- ReefTier(tier2, d.folder = d.folder)
    t5.exist <- FALSE
  }
  
  pb$tick()
  
  ## Tier 3: MEOW ####
  pb$tick()
  if (countryISO == "AUS") {
    tier4 <- suppressMessages(suppressWarnings(
      st_read(file.path(d.folder, "ReefCloud_regions/tiers/tier4/AUS_tier4.geojson"))
    ))
    tier3 <- suppressMessages(suppressWarnings(
      st_read(file.path(d.folder, "ReefCloud_regions/tiers/tier3/AUS_tier3.geojson"))
    ))
  } else {
    MEOW <- suppressMessages(suppressWarnings(
      read_sf(file.path(d.folder, "MarineRegions/MEOW/meow_ecos.shp")) %>%
        st_filter(tier2, .predicate = st_intersects) %>%
        st_filter(tier5, .predicate = st_intersects)
    ))
    # valid <- st_is_valid(MEOW)
    # MEOW <- MEOW %>% st_set_precision(1000000) %>% st_make_valid()
    # MEOW <- st_buffer(MEOW[!is.na(valid), ], 0.0)
    MEOW<- MEOW  |> st_make_valid()
    
    # Only create tier 3 if there is more than one MEOW in tier 2
    tier3 <- tier2 %>%
      st_intersection(x = MEOW) %>%
      st_filter(tier5, .predicate = st_intersects) %>%
      dplyr::select(-c(name, tier, tier_id)) %>%
      rename(name = ECOREGION, tier_id = ECO_CODE) %>%
      mutate(tier = 3, tier_source = "MEOW") %>%
      dplyr::select(name, tier, tier_id, tier_source, geometry)
    
    # Admin level: Often admin_level=4 works for small nations. Change this for large countries (e.g., countries) to admin level=1 or 2 as appropriate
    tier4 <- AdminBoundaries(tier2, admin_level = admin_level, expand.boundary = exp.b)
    tier4<- tier4  |> st_remove_holes() |> #use nngeo to remove holes inside from land union
      st_make_valid()
    
    sf_use_s2(FALSE)
    
    tier4 <- tier4 %>%
      st_collection_extract() %>%
      st_intersection(y = tier3) %>%
      dplyr::select(-tier_id) %>%
      rename(name = name, tier_id = osm_id) %>%
      mutate(tier = 4, tier_source = sprintf("OSM_AdminitrationBoundary_level%s", admin_level)) %>%
      dplyr::select(name, tier, tier_id, tier_source, geometry) %>%
      st_filter(tier5, .predicate = st_intersects) %>%
      dplyr::group_by(name, tier, tier_id, tier_source) %>%
      dplyr::summarise() %>%
      ungroup()
  }
  
  pb$tick()
  
  # Add tier IDs and save files
  pb$tick()
  dir.create(file.path(d.folder, "ReefCloud_regions/tiers_databse/"), showWarnings = FALSE)
  
  tier3 <- tier3 %>%
    rename(source_id = tier_id) %>%
    st_join(tier2 %>% dplyr::select(tier_id, ISO, territory, sovereign, geometry)) %>%
    tibble::rowid_to_column(., "ID") %>%
    mutate(tier_id = as.numeric(paste0(tier_id, ID))) %>%
    dplyr::select(-ID)
  tier4 <- tier4 %>%
    ungroup() %>%
    rename(source_id = tier_id) %>%
    st_join(tier3 %>% dplyr::select(tier_id, ISO, territory, sovereign, geometry)) %>%
    st_difference() %>%
    tibble::rowid_to_column(., "ID") %>%
    mutate(tier_id = as.numeric(paste0(tier_id, ID))) %>%
    dplyr::select(-ID)
  
  if (!t5.exist) {
    tier5 <- tier5 %>%
      ungroup() %>%
      st_join(tier4 %>% dplyr::select(tier_id, ISO, territory, sovereign, geometry)) %>%
      tibble::rowid_to_column(., "ID") %>%
      mutate(tier_id = as.numeric(paste0(tier_id, ID))) %>%
      dplyr::select(-ID) %>%
      st_cast(to = "MULTIPOLYGON")
    st_write(tier5, file.path(d.folder, sprintf("ReefCloud_regions/tiers_databse/%s_tier5.geojson", countryISO)),
             delete_dsn = TRUE, quiet = TRUE, append = FALSE)
  }
  
  pb$tick()
  
  ## Join tiers
  pb$tick()
  t24 <- tier2 %>%
    mutate(source_id = tier_id) %>%
    bind_rows(tier3, tier4) %>%
    st_cast(to = "MULTIPOLYGON")
  
  # Write File
  st_write(t24, file.path(d.folder, sprintf("ReefCloud_regions/tiers_databse/%s_tier2_4.geojson", countryISO)),
           delete_dsn = TRUE, quiet = TRUE, append = FALSE)
  
  pb$tick()
}

# Run the function with the provided territory name
suppressMessages(suppressWarnings(makeTiers(territoryName = territoryName, exp.b = exp.b)))
# makeTiers(territoryName = territoryName, exp.b = exp.b)