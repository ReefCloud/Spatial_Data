# Spatial_Tiers
Code to generate spatial polygons that define aggregation units for analysing monitoring data from ReefCloud and reporting summaries on the status and trends of coral reefs into the public dashboard. 

## Functions:
 * **ReefCloud_spatialTiers.R** A wrapper function to geneter spatial polygons for data aggregation (Tiers). This function calls other functions and generate the tiers based on a territory name (territoryName)
 * **ReefTier.R** Function that generates tier 5 within a tier 2
 * **AdministrativeBoundaries.R** = Function that generates tier 4 within a tier 2. This function uses the administrative boundaries of the territory.
 * **Misc.R**  code tools to make this work. 

To run this:
modify territoryName(s) in run_Tiers.R and run the script. 




