
library(sf)
library(leaflet)

tier2<-read_sf("GIS/ReefCloud_regions/tier2/Taiwan_tier2.geojson")
reefs<-read_sf(
  file.path(d.folder,
            "Reefs/WCMC008_CoralReefs2018_v4_1/14_001_WCMC008_CoralReefs2018_v4_1/01_Data/WCMC008_CoralReef2018_Py_v4_1.shp")) 

reefs <- reefs[st_within(reefs, tier2, sparse = FALSE), ]
i=4
reef.b<-st_buffer(reefs[i, ], 0.15)

tier5<-read_sf("GIS/ReefCloud_regions/tiers_databse/TWN_tier5.geojson")
tier24<-read_sf("GIS/ReefCloud_regions/tiers_databse/TWN_tier2_4.geojson")
leaflet() |> 
  addPolygons(data= tier2) |> 
  addTiles() |> 
  addPolygons(data=tier5, color="yellow",stroke = TRUE,fillOpacity = 0.5,
              popup = ~paste0("<b>", reef_id, "</b><br>")) |> 
  addPolygons(data=reefs, stroke = T, color = "red",
              popup = ~paste0("<b>", row.names(reefs), "</b><br>")) |> 
  addPolygons(data=ACA, stroke = T, color = "green",
              popup = ~paste0("<b>", row.names(ACA), "</b><br>")) 
  # addPolygons(data=test, stroke = T, color = "green")

leaflet() |> 
  addPolygons(data=t5 |> filter(is.na(tier_id)), color="yellow",stroke = TRUE,fillOpacity = 0.5,
              popup = ~paste0("<b>", reef_id, "</b><br>"))|> 
  addTiles() |> 
  addPolygons(data=tier4, stroke = T, color = "green",
              popup = ~paste0("<b>", row.names(reefs), "</b><br>")) |>
  addPolygons(data=admin.b, stroke = T, color = "red",
              popup = ~paste0("<b>", row.names(reefs), "</b><br>")) 

