# List of required packages
required_packages <- c(
  "tidyverse",
  "osmdata",
  "sf",
  "rgdal",
  "mregions",
  "rgeos",
  "maptools",
  "sp",
  "lwgeom",
  "tibble",
  "dplyr",
  "h3js",
  "h3"
)


# Function to install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
      if (pkg %in% c("h3js","h3", "rgdal" )){
        devtools::install_github("saurfang/h3js")
        remotes::install_github("crazycapivara/h3-r") 
      }
    }
  }
}

# Install required packages
install_if_missing(required_packages)