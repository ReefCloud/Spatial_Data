# Spatial_Tiers
Code to generate spatial polygons that define aggregation units for reporting monitoring data from ReefCloud into the public dashboard 

Functions:
 * Wrapper Function = ReefCloud_spatialTiers.R. This functions calls other ancilliary functions and generate the tiers based on a territory name( territoryName)
 * ReefTier.R = Function that generates tier 5 within a tier 2
 * AdministrativeBoundaries.R = Function that generates tier 4 within a tier 2. This function uses the administrative boundaries of the territory.
 * Mis.R => ancilliary functions to make this work. 

To run this:
modify territoryName(s) in run_Tiers.R and run the script. 
```



