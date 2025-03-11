## RUN ###

source("ReefCloud_spatialTiers.R")
for (territoryName in c( "Taiwan", "Palau")){
  tryCatch(makeTiers(territoryName = territoryName))
  gc()
}
