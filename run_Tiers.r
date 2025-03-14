## RUN ###

for (territoryName in c( "Taiwan", "New Caledonia", "East Timor")){
  tryCatch(eval("ReefCloud_spatialTiers.R",territoryName, TRUE))
  gc()
}
