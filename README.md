# Spatial_Tiers
Code to generate spatial polygons that define aggregation units for analysing monitoring data from ReefCloud and reporting summaries on the status and trends of coral reefs into the public dashboard. 

Tier 2 is the territory itself, Tier 3 define environmental provinces within a territory, Tier 4 is the administrative boundaries of the territory, and Tier 5 is a hexagonal grid within the territory where coral reefs are present.

For large territories, Tier 2 defines the Large Marine Ecoregions (LMEs) and Tier 3 defines the environmental provinces within the LMEs. For small territories, Tier 2 is defined by the Exclusive Economic Zone (EEZ) and Tier 3 defines the environmental provinces within the EEZ.

## Functions:
 * **ReefCloud_spatialTiers.R**: A wrapper function to generate spatial polygons for data aggregation (Tiers). This function calls other functions and generates the tiers based on a territory name (territoryName). 
 * **ReefTier.R**: Function that generates tier 5 within a tier 2.
 * **AdministrativeBoundaries.R**: Function that generates tier 4 within a tier 2. This function uses the administrative boundaries of the territory.
 * **Misc.R**: Code tools to make this work.

## Dependencies:
The following R packages are required to run the scripts:
 * tidyverse
 * osmdata
 * sf
 * rgdal
 * mregions
 * rgeos
 * maptools
 * sp
 * lwgeom
 * progress
 * tibble
 * dplyr
 * h3js
 * h3

 ### Data Dependencies:
 To run this code you need the following data:
  * Administrative boundaries of the territory (Tier 4)
  * Environmental provinces within the territory (Tier 3)
  * Hexagonal grid within the territory (Tier 5)
  * Large Marine Ecoregions (LMEs) or Exclusive Economic Zone (EEZ) for large and small territories, respectively (Tier 2)

### Installation of GDAL
To install the `rgdal` package, you need to have GDAL installed on your system. You can install GDAL using the following commands:
 1. **For Ubuntu/Debian**:
```bash
sudo apt-get install libgdal-dev libproj-dev
```
2. **For Windows**:
Install Anaconda and run the following commands in the Anaconda Prompt using conda forge:
```bash
conda install conda-forge::gdal
conda install conda-forge::proj
```

## Installation
To install the required packages, you can use the `install_dependencies.R` script provided in this repository. Run the following command in your R environment:
```r
source("install_dependencies.R")
```

# Usage:
This function needs two arguments:
 * **territoryName**: Name of the territory for which the tiers are to be generated.
 * **expand.boundaries**: A logical value to expand the boundaries of the territory. Default is `FALSE` because this it may not work well in all countries. It is designed for those administrative boundaries that do not include the marine area.

** From Terminal:** ``bash run_Tiers.R territoryName TRUE/ FALSE``
** From R:** Modify Territory Names and run ``run_Tiers.R``





