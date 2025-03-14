### extend municipalities into maritime areas

require(sf)
require(tidyverse)
require(rnaturalearth)

tier4<-st_read('C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/ReefCloud_regions/tiers_databse/TWN_tier2_4.geojson') |> 
filter(tier==4) 

tier2<-st_read('C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/ReefCloud_regions/tiers_databse/TWN_tier2_4.geojson') |> 
filter(tier==2) 

coast <- ne_coastline() %>% 
    st_intersection(t2)

t4.ext<-tier4 %>% 
st_transform(3857) %>%
st_buffer(units::set_units(40, "km")) %>%
  st_transform(4326) |> 
  st_crop(coast)



