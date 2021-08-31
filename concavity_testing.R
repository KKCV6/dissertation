over_1 <- catchments %>% filter(over_id ==1)

plot(over_1)

points <- st_coordinates(over_1)

#assign to a cluster
  points_wcluster <- as.data.frame(points)

  points_wcluster_subset_coords <- points_wcluster[c("X", "Y")]
  
  concave_outline <- as.matrix(points_wcluster_subset_coords)  %>% concaveman(concavity = 1.5) #change concavity measure
  
  coords <- as.data.frame(concave_outline)
  
  polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 32737) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")

  plot(polygon)
  
  polygon <- st_buffer(polygon, 50,  endCapStyle = "FLAT", joinStyle = "MITRE") %>% st_union()
  
  polygon <- st_cast(polygon, "POLYGON")
  
  polygon <-  polygon %>% st_sf %>% st_cast()
  
  polygon <- smoothr::smooth(polygon, method = "chaikin")
  
  qtm(polygon)
  
  st_write(polygon, 'data/settlement_outlines/concavity15.shp', delete_layer = TRUE)
