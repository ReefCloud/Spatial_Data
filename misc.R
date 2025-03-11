library(sf)
library(leaflet)
new<-st_read("C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/ReefCloud_regions/tiers_databse/PLW_tier5.geojson")
old<- st_read("C://Users/mgonzale/Downloads/hexagons_PLW_TIER4_1874/hexagons_PLW_TIER4_1874.shp")


leaflet() |> 
  addPolygons(data=new, color="red", fillColor = "red", fillOpacity = 0.5) |> 
  addPolygons(data=old, color="grey", fillColor = "grey", opacity = 0.5) |> 
  addTiles()


##DEBUGGING 
leaflet() |> 
  addPolygons(data=ACA, color=NA, fillColor = "red", fillOpacity = 0.5) |> 
  addPolygons(data=mcr, color=NA, fillColor = "blue", fillOpacity = 0.5) |> 
  addPolygons(data=h3.poly, color="grey", fillColor = "grey", opacity = 0.5) |> 
  addPolygons(data=tier2, color="black", fillColor = NA) |> 
  addTiles()
