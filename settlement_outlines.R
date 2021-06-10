# load our required libraries 
library(here)
library(tidyverse)
library(sf)
library(magrittr)
library(tmap)
library(dbscan)
library(tidygraph)
library(stplanr)
library(tidyverse)
library(spatstat)
library(maptools)
library(devtools)
library(foot)
library(concaveman)
library(rgeos)
tmap_mode("view")


#Step 0) Setting Up
  #footprints
  tnz_bf <- st_read('data/footprints/Tanzania_2019-09-16/Tanzania.geojson')

  #boundaries
  tnz_districts <- st_read('data/boundaries/tanzania_gadm/gadm36_TZA_2.shp')

  #filter
  monduli_district <- filter(tnz_districts, NAME_2 == "Monduli")
  monduli_bf <-  tnz_bf[monduli_district,]
  
  #monduli_bf <- st_read('data/footprints/monduli_bf.shp') #alternative


#Step 1) Create initial settlement outlines from building footprints 
    #Note the whole step must be run to achieve correct output
  
  #centroids
  centroids <- st_centroid(monduli_bf)
  
  #points
  points <- st_coordinates(centroids)
  
  #DBScan
  dbscan <- dbscan::dbscan(points, eps = 0.001, minPts = 5)
  plot(points, col=dbscan$cluster)
  plot(monduli_district$geometry, add=T)

  #assign building footprint to a cluster
  points_wcluster <- as.data.frame(points) %>% mutate(dbcluster=dbscan$cluster)

 #Create settlement outlines from Building Footprints and DB-Scan cluster
  geometry_list <- vector(mode = "list", length = max(points_wcluster$dbcluster))

  counter <-1

  for (cluster_index in seq(1, max(points_wcluster$dbcluster))) {
  
  points_wcluster_subset <- filter(points_wcluster, dbcluster==cluster_index)
  
  points_wcluster_subset_coords <- points_wcluster_subset[c("X", "Y")]
  
  concave_outline <- as.matrix(points_wcluster_subset_coords)  %>% concaveman()
  
  coords <- as.data.frame(concave_outline)
  
  polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
  
  geometry_list[counter] <- (polygon$geometry)
  
  counter <- counter + 1
}

  hulls <- st_sfc(geometry_list, crs = 4326)

  st_write(hulls, "data/settlement_outlines/bf_outlines.shp", delete_layer = TRUE)

  bf_outlines <- st_read('data/settlement_outlines/bf_outlines.shp')

  bf_outlines <- st_transform(bf_outlines, crs = 4326)

  tm_shape(bf_outlines) + tm_borders()
  
#Step 2) Create catchment areas from the building outline footprints
  #Note the whole step must be run to achieve correct output
  
  #centroids
  centroids <- st_point_on_surface(bf_outlines)
  tm_shape(bf_outlines) + tm_borders() + tm_shape(centroids)+tm_dots()

  #points
  points <- st_coordinates(centroids)

  plot(points)

  #dbscan
  dbscan <- dbscan::dbscan(points, eps = 0.0119, minPts = 3) 
  plot(points, col=dbscan$cluster)
  plot(monduli_district$geometry, add=T)

  #assign building footprint to a cluster
  points_wcluster <- as.data.frame(points) %>% mutate(dbcluster=dbscan$cluster)

  geometry_list <- vector(mode = "list", length = max(points_wcluster$dbcluster))

  counter <-1

  for (cluster_index in seq(1, max(points_wcluster$dbcluster))) {
  
    points_wcluster_subset <- filter(points_wcluster, dbcluster==cluster_index)
  
    points_wcluster_subset_coords <- points_wcluster_subset[c("X", "Y")]
  
    concave_outline <- as.matrix(points_wcluster_subset_coords)  %>% concaveman()
  
    coords <- as.data.frame(concave_outline)
  
    polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")

    geometry_list[counter] <- (polygon$geometry)
  
  counter <- counter + 1
}

  hulls <- st_sfc(geometry_list, crs = 4326)

  st_write(hulls, "data/settlement_outlines/catch_outlines.shp", delete_layer = TRUE)

  catch_outlines <- st_read('data/settlement_outlines/catch_outlines.shp')

tm_shape(catch_outlines) + tm_borders()+
  tm_shape(bf_outlines)+tm_polygons(col = "purple")

#Step 3) Asign the building footprint settlement outlines a catchment ID. Note: we will deal with NA values later

  mon_sp <- as(bf_outlines, 'Spatial')

  sett_sp <- as(catch_outlines, 'Spatial')

  over_ID <- over(mon_sp, sett_sp, fn=NULL)

  bf_outlines$over_id <- over_ID$FID

  tm_shape(bf_outlines)+
    tm_polygons(col = "over_id")+
  tm_shape(catch_outlines)+
    tm_borders()

#Step 4) Create the final settlement outlines, grouping smaller polygons (step 1) by their catchment (step 2)
  #Note the whole step must be run to achieve correct output
  
  catchments <- bf_outlines %>% group_by(over_id) %>% st_cast("POINT")  

  catchments <- catchments %>% drop_na(over_id) %>% st_as_sf() #drop NA points as these are standalone settlements

  tm_shape(catchments)+
    tm_dots()

  points <- st_coordinates(catchments)

  plot(points)

  #catchment clusters
  plot(points, col=catchments$over_id)
  plot(monduli_district$geometry, add=T)

  #assign to a cluster
  points_wcluster <- as.data.frame(points) %>% mutate(cluster=catchments$over_id)

  geometry_list <- vector(mode = "list", length = max(points_wcluster$cluster))

  counter <-1

  for (cluster_index in seq(1, max(points_wcluster$cluster))) {
  
    points_wcluster_subset <- filter(points_wcluster, cluster==cluster_index)
  
    points_wcluster_subset_coords <- points_wcluster_subset[c("X", "Y")]
  
    concave_outline <- as.matrix(points_wcluster_subset_coords)  %>% concaveman()
  
    coords <- as.data.frame(concave_outline)
  
    polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
  
    geometry_list[counter] <- (polygon$geometry)
  
    counter <- counter + 1
}

  hulls <- st_sfc(geometry_list, crs = 4326)

  st_write(hulls, "data/settlement_outlines/settlement_outlines.shp", delete_layer = TRUE)

  settlement_outlines <- st_read('data/settlement_outlines/settlement_outlines.shp')

  tm_shape(settlement_outlines)+
    tm_polygons(col = "purple", alpha = 0.5)+
  tm_shape(bf_outlines)+
    tm_borders()

  #Difference analysis to extract standalone settlements
  
  catch_na <- bf_outlines[is.na(bf_outlines$over_id),] %>% st_as_sf()

  catch_na <- st_transform(catch_na, crs = 4326)

  #merge the NA outlines with those within catchments to build a complete dataset

  settlement_outlines <- st_transform(settlement_outlines, crs = 4326)
  
  st_write(catch_na, "data/settlement_outlines/catch_na.shp", delete_layer = TRUE)

  catch_na <- st_read('data/settlement_outlines/catch_na.shp')

    #check for matching columns, we will create new settlement IDs
  catch_na$over_id=NULL 

  outlines <- rbind(settlement_outlines, catch_na)

  tm_shape(outlines)+tm_borders()

  outlines$ID <- 1:nrow(outlines)

  #buffer to create your final outlines in line with good practice

  outline_buffer <- st_transform(outlines, crs = 32737)
  
  outline_buffer<- st_buffer(outline_buffer, 150,  endCapStyle = "FLAT", joinStyle = "MITRE") %>% st_union()

  outline_buffer <- st_cast(outline_buffer, "POLYGON")

  final_outlines<-  outline_buffer %>% st_sf %>% st_cast()
  
  final_outlines$ID <- 1:nrow(final_outlines)

    #have a look at our final outlines
  
   tm_shape(final_outlines)+
    tm_polygons(col = "purple", alpha = 0.5)+
     tm_scale_bar(position = c("left", "bottom"))
   
   #export
   st_write(final_outlines, "data/settlement_outlines/final_outlines.shp", delete_layer = TRUE)
   
   final_outlines <- st_read('data/settlement_outlines/final_outlines.shp') %>% st_transform(crs = 32737)

#Step 5) Calculate areas
   
   final_outlines$area_sqm <- st_area(final_outlines)
   
   final_outlines$area_sqkm <- units::set_units(final_outlines$area_sqm, km^2) #add as_numeric to remove units
   
   tm_shape(final_outlines)+
     tm_polygons("area_sqkm", alpha = 0.5, palette = "viridis", style = "jenks")+
     tm_scale_bar(position = c("left", "bottom"))+
     tm_shape(monduli_district)+tm_borders(col = "gray")

#Step 6) Ground truth against GRID 3 data
   
   # List feature classes
   rgdal::ogrListLayers('data/ground-truth/GRID/GRID3_TZA_SettlementExtents_V01Alpha/GRID3_TZA_settlement_extents_20200425.gdb')
   
   ssa_extents <- rgdal::readOGR(dsn = "data/ground-truth/GRID/GRID3_TZA_SettlementExtents_V01Alpha/GRID3_TZA_settlement_extents_20200425.gdb", layer = "ssa_extents")
