## RUN ###

source("ReefCloud_spatialTiers.R")
for (territoryName in c( "Vietnam")){
  tryCatch(makeTiers(territoryName = territoryName))
  gc()
}
