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
library(dbscan)
tmap_mode("view")


#Step 0) Setting Up
  #Microsoft footprints
  tnz_bf <- st_read('data/footprints/Tanzania_2019-09-16/Tanzania.geojson') %>% st_transform(crs = 4326)
  
  #OSM footprints
  osm_footprints <- st_read('data/footprints/osm_footprints.shp')%>% st_transform(crs = 4326)

  #boundaries
  tnz_districts <- st_read('data/boundaries/tanzania_gadm/gadm36_TZA_2.shp')
  
  monduli_district <- filter(tnz_districts, NAME_2 == "Monduli") %>% st_transform(crs = 4326)
  
  #clip footprints
  
  MS_bf <-  tnz_bf[monduli_district,]
  
  osm_footprints <- osm_footprints[monduli_district,]
  
  osm_footprints <- osm_footprints[,-(1:35)]
  
  st_write(osm_footprints, 'data/footprints/osm_footprints_R.shp')
  
  #isolate non-intersecting footprints
  
  non_overlapping <-  MS_bf[lengths(st_intersects(MS_bf,osm_footprints))==0,] %>% st_cast('MULTIPOLYGON')
  
  tm_shape(non_overlapping)+tm_borders()+
    tm_shape(osm_footprints)+tm_polygons(col = "blue", alpha = 0.5)
  
  tm_shape(osm_footprints)+tm_borders()+
  tm_shape(monduli_district)+tm_polygons()
  
  #bind together
  
  footprints <- rbind(osm_footprints, non_overlapping)
  
  tm_shape(footprints)+tm_polygons(col = "purple")+tm_borders(col = "purple")+
    tm_shape(monduli_district)+tm_borders()
  
  anyDuplicated(footprints)
  
  st_write(footprints, 'data/footprints/footprints_combined.shp')
  
  footprints <- st_read('data/footprints/footprints_combined.shp') %>% st_transform(crs = 4326)

#Step 1) Create initial settlement outlines from building footprints 
    #Note the whole step must be run to achieve correct output
  
  #centroids
  
  footprints_proj <- st_transform(footprints, crs = 32737)
  
  centroids <- st_centroid(footprints_proj)
  
  #points
  points <- st_coordinates(centroids)

  #DBScan
  
  dbscan <- dbscan::dbscan(points, eps = 125, minPts = 5)
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
  
  concave_outline <- as.matrix(points_wcluster_subset_coords) %>% concaveman()
  
  coords <- as.data.frame(concave_outline)
  
  polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 32737) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
  
  geometry_list[counter] <- (polygon$geometry)
  
  counter <- counter + 1
}

  hulls <- st_sfc(geometry_list, crs = 32737)
  
  st_write(hulls, "data/final/bf_outlines.shp", delete_layer = TRUE)

  bf_outlines <- st_read('data/final/bf_outlines.shp')

  bf_outlines <- st_transform(bf_outlines, crs = 32737)

  bf_outlines <- bf_outlines %>% filter(!st_is_empty(.))
  
  multi_points <- st_cast(bf_outlines, "MULTIPOINT")
  border_points <- multi_points %>% group_by(FID) %>% st_cast("POINT")
  plot(border_points)
  
  st_write(border_points, 'data/final/border_points.shp')

#Step 2) Create catchment areas from the building outline footprints
  #Note the whole step must be run to achieve correct output
  
  #points
  points <- st_coordinates(border_points)
  
    #dbscan
  dbscan <- dbscan::dbscan(points, eps = 300, minPts = 10) 
  plot(points, col=dbscan$cluster)
  plot(monduli_district$geometry, add=T)

  #assign building footprint to a cluster
  points_wcluster <- as.data.frame(points) %>% mutate(dbcluster=dbscan$cluster)
  
  unique <- unique(points_wcluster$dbcluster)

  geometry_list <- vector(mode = "list", length = max(points_wcluster$dbcluster))

  counter <-1

  for (cluster_index in seq(1, max(points_wcluster$dbcluster))) {
  
    points_wcluster_subset <- filter(points_wcluster, dbcluster==cluster_index)
  
    points_wcluster_subset_coords <- points_wcluster_subset[c("X", "Y")]
  
    concave_outline <- as.matrix(points_wcluster_subset_coords)  %>% concaveman()
  
    coords <- as.data.frame(concave_outline)
  
    polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 32737) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")

    geometry_list[counter] <- (polygon$geometry)
  
  counter <- counter + 1
}

  hulls <- st_sfc(geometry_list, crs = 32737)
  
  st_write(hulls, "data/final/catch_outlines.shp", delete_layer = TRUE)

  catch_outlines <- st_read('data/final/catch_outlines.shp') %>% st_transform(crs = 32737)

tm_shape(catch_outlines) + tm_borders(lwd = 3)+
  tm_shape(bf_outlines)+tm_polygons(col = "purple", alpha = 0.5)

#Step 3) Assign the building footprint settlement outlines a catchment ID. Note: we will deal with NA values later

  mon_sp <- as(bf_outlines, 'Spatial')
  
  sett_sp <- as(catch_outlines, 'Spatial')
  
  over_ID <- over(mon_sp, sett_sp, fn = NULL)
  
  over_ID$ID <- over_ID$FID + 1 #subsequent calcs do not deal with 0 value

  bf_outlines$over_id <- over_ID$ID
  
  tmap_options(check.and.fix = TRUE)

  tm_shape(bf_outlines)+
    tm_polygons(col = "over_id")+
  tm_shape(catch_outlines)+
    tm_borders()

#Step 4) Create the final settlement outlines, grouping smaller polygons (step 1) by their catchment (step 2)
  #Note the whole step must be run to achieve correct output
  
  centroids <- st_centroid(footprints_proj)
  
  bf_overid <- bf_outlines %>% select(over_id) 

  bf_overid <- bf_overid %>% drop_na(over_id)
  
  bf_overid <- st_buffer(bf_overid, 1)
  
  bp_sf <- st_as_sf(border_points)
  
  catchments <- st_join(centroids, bf_overid) %>% st_transform(crs = 32737)
  
  catchments <- catchments %>% drop_na(over_id) #drop NA points as these are standalone settlements
  
  catchments <- catchments[,-(1)] #drop FID column
  
  st_write(catchments, 'data/final/catchments.shp')
  
  catchments$over_id[catchments$over_id == 436] <- 435

  #points
  points <- st_coordinates(catchments)

  #catchment clusters
  plot(points, col=catchments$over_id)
  plot(monduli_district$geometry, add=T)

  #assign to a cluster
  points_wcluster <- as.data.frame(points) %>% mutate(cluster=catchments$over_id)

  geometry_list <- vector(mode = "list", length = max(points_wcluster$cluster))
  
  seq <- unique(points_wcluster$cluster)
  
  counter < -1
  
  for (cluster_index in seq) {
    
    points_wcluster_subset <- filter(points_wcluster, cluster==cluster_index)
    
    points_wcluster_subset_coords <- points_wcluster_subset[c("X", "Y")]
    
    concave_outline <- as.matrix(points_wcluster_subset_coords)  %>% concaveman(concavity = 1.5)
    
    coords <- as.data.frame(concave_outline)
    
    polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 32737) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
    
    geometry_list[counter] <- (polygon$geometry)
    
    counter <- counter + 1
  }
  
  hulls <- st_sfc(geometry_list, crs = 32737)

  st_write(hulls, "data/final/settlement_outlines.shp", delete_layer = TRUE)

  settlement_outlines <- st_read('data/final/settlement_outlines.shp')
  
  tm_shape(settlement_outlines)+
    tm_polygons(col = "purple", alpha = 0.5)

  #Difference analysis to extract standalone settlements
  
  catch_na <- bf_outlines[is.na(bf_outlines$over_id),] %>% st_as_sf()

  catch_na <- st_transform(catch_na, crs = 32737)

  #merge the NA outlines with those within catchments to build a complete dataset

  settlement_outlines <- st_transform(settlement_outlines, crs = 32737)
  
  st_write(catch_na, "data/final/catch_na.shp", delete_layer = TRUE)

  catch_na <- st_read('data/final/catch_na.shp')

    #check for matching columns, we will create new settlement IDs
  catch_na$over_id=NULL 

  outlines <- rbind(settlement_outlines, catch_na)

  outlines$ID <- 1:nrow(outlines)

  #buffer to create your final outlines in line with good practice

  outline_buffer <- st_transform(outlines, crs = 32737)
  
  outline_buffer<- st_buffer(outline_buffer, 50,  endCapStyle = "FLAT", joinStyle = "MITRE") %>% st_union()

  outline_buffer <- st_cast(outline_buffer, "POLYGON")

  final_outlines<-  outline_buffer %>% st_sf %>% st_cast()
  
  final_outlines$ID <- 1:nrow(final_outlines)

    #have a look at our final outlines
  
   tm_shape(final_outlines)+
    tm_polygons(col = "purple", alpha = 0.5)+
     tm_scale_bar(position = c("left", "bottom"))

#Step 5) Calculate areas
   
   final_outlines$area_sqm <- st_area(final_outlines)
   
   final_outlines$area_sqkm <- units::set_units(final_outlines$area_sqm, km^2) #add as_numeric to remove units

   monduli_district$area_sqm <- st_area(monduli_district)
   
   monduli_district$area_sqkm <- units::set_units(monduli_district$area_sqm, km^2)

#Step 6) Does the data look right?
   
   #how many building footprints are within each settlement
    
   centroids <- st_centroid(footprints_proj)
   
   final_outlines$pt_count <- lengths(st_intersects(final_outlines, centroids))
   
   summary(final_outlines$pt_cont)

    tm_shape(final_outlines)+tm_polygons(col = "pt_count", alpha = 0.5, palette = "viridis")+
     tm_shape(monduli_district)+tm_borders()
   
  #which building footprints are outside the settlements?
   
   centroids$outside_outlines <- sapply(st_intersects(centroids, final_outlines),function(x){length(x)==0})
   
   outliers <- centroids %>% filter(outside_outlines == "TRUE")
   
   na_centroids <- st_transform(outliers, crs = 32737)
   
   na_buffer <- st_buffer(na_centroids, 125)
   
   #check if any are over 4
   
   na_buffer$pt_count <- lengths(st_intersects(na_buffer, na_centroids))
   
   summary(na_buffer$pt_count)
   
   #Step 6) Ground truth - GRID3
   
   # List feature classes
   
   dsn <- 'data/ground-truth/GRID/GRID3_TZA_SettlementExtents_V01Alpha/GRID3_TZA_settlement_extents_20200425.gdb'
   rgdal::ogrListLayers(dsn)
   
   ssa_extents <- st_read(dsn = dsn, layer = "ssa_extents") 
   
   ssa_extents <- st_transform(ssa_extents, crs = 32737) %>% st_cast("MULTIPOLYGON")
   
   bua_extents <- st_read(dsn = dsn, layer = "bua_extents")
   
   bua_extents <- st_transform(bua_extents, crs = 32737) %>% st_cast("MULTIPOLYGON")
   
   hamlet_extents <- st_read(dsn = dsn, layer = "hamlet_extents")
   
   hamlet_extents <- st_read('data/ground-truth/GRID/hamlets_monduli.shp') %>% st_transform(crs = 32737)
   
   hamlet_extents <- st_transform(hamlet_extents, crs = 32737) %>% st_cast("MULTIPOLYGON")
   
   monduli_proj <- st_transform(monduli_district, crs = 32737)
   
   ssa_extents <- ssa_extents[monduli_proj,]
   
   bua_extents <- bua_extents[monduli_proj,]
   
   hamlet_extents <- hamlet_extents[monduli_proj,]
   
   colnames(hamlet_extents)[4] <- "Shape_Length"
   
   st_geometry(hamlet_extents) <- "Shape"
   
   tm_shape(ssa_extents)+tm_borders()+
     tm_shape(bua_extents)+tm_borders()+
     tm_shape(final_outlines)+tm_borders(col = "blue")
   
   GRID_all <- rbind(ssa_extents, bua_extents, hamlet_extents)
   
   #precision and recall
   
    #GRID3

   intersect_tibble <- st_intersects(final_outlines, GRID_all, sparse = FALSE)
   
   final_outlines$count_true <-expss::count_row_if("TRUE", intersect_tibble) 
   
   final_outlines$FP <- final_outlines$count_true==0
   
   final_outlines$TP <- final_outlines$count_true>0
   
   GRID_all$count_true <-expss::count_col_if("TRUE", intersect_tibble)
   
   GRID_all$FN <- GRID_all$count_true == 0
   
   sum(GRID_all$FN =="TRUE")
   
   TP <- expss::count_if("TRUE", final_outlines$TP)
   TP
   
   FP <- expss::count_if("TRUE", final_outlines$FP)
   FP
   
   FN <- expss::count_if("TRUE", GRID_all$FN)
   FN
   
   precision <- sum(TP/(TP+FP))
   precision
   
   recall <- sum(TP/(TP+FN))
   recall
   
   #Step 7) Ground Truth - WUFL
   
   WSF <- raster::raster('data/ground-truth//wsf/WSF2015_v1_EPSG4326_e030_n00_e040_s10.tif')
   
   WSF <- raster::mask(WSF, monduli_district)
   
   WSF <- raster::projectRaster(WSF, crs = 32737)
   
   WSF <- raster::raster('data/ground-truth/wsf/monduli_wsf_projects.tif')
   
   outlines_sp <- as(final_outlines, 'Spatial')
   
   extract <- extract(WSF, outlines_sp)
   
   extract <- unlist(extract)
   
   extract <- as.matrix(extract)
   
   WSF_poly <- as(WSF,'SpatialPolygonsDataFrame')
   
   WSF_union <- unionSpatialPolygons(WSF_poly, rep(1, length(WSF_poly)))
   
   WSF_diss <- disaggregate(WSF_union)
   
   WSF_sf <- st_as_sf(WSF_diss)
   
   WSF_sf <- WSF_sf[monduli_proj,]
   
   qtm(WSF_sf)
   
   intersect_WSF <- st_intersects(final_outlines, WSF_sf, sparse = FALSE)
   
   final_outlines$count_true_wsf <-expss::count_row_if("TRUE", intersect_WSF) 
   
   final_outlines$FP_wsf <- final_outlines$count_true_wsf==0
   
   final_outlines$TP_wsf <- final_outlines$count_true_wsf>0
   
   WSF_sf$count_true <-expss::count_col_if("TRUE", intersect_WSF)
   
   WSF_sf$FN <- WSF_sf$count_true == 0
   
   TP_wsf <- expss::count_if("TRUE", final_outlines$TP_wsf)
   TP_wsf
   
   FP_wsf <- expss::count_if("TRUE", final_outlines$FP_wsf)
   FP_wsf
   
   FN_wsf <- expss::count_if("TRUE", WSF_sf$FN)
   FN_wsf
   
   precision_wsf <- sum(TP_wsf/(TP_wsf+FP_wsf))
   precision_wsf
   
   recall_wsf <- sum(TP_wsf/(TP_wsf+FN_wsf))
   recall_wsf

   #Step 7) Results Stats
   
   summary(final_outlines$pt_cont)
   summary(final_outlines$area_sqkm)
   
   sum(final_outlines$area_sqkm)
   sum(monduli_district$area_sqkm)
   
   box <- as.data.frame(final_outlines$pt_cont)
   
   boxplot(box, log = "x", col = "lightblue", border = "darkgreen",
           xlab = "Number of Buildings (Log Scale)",
           ylab = "Settlements Delineated",
           horizontal = TRUE,
           notch = TRUE)
   
   box2 <- as.data.frame(final_outlines$ar_sqkm)
   
   boxplot(box2, log = "x", col = "lightblue", border = "darkgreen",
           xlab = "Area of Settlement km^2 (Log Scale)",
           ylab = "Settlements Delineated",
           horizontal = TRUE,
           notch = TRUE)

   st_write(final_outlines, "data/final/final_outlines.shp", delete_layer = TRUE)
   
   final_outlines <- st_read('data/final/final_outlines.shp') %>% st_transform(crs = 32737)
   
   final_outlines <- smoothr::smooth(final_outlines, method = "chaikin")
   
   tm_shape(final_outlines)+tm_borders()
   
   st_write(final_outlines, "data/final/final_outlines_smooth.shp", delete_layer = TRUE)
   
   final_outlines <- st_read('data/final/final_outlines_smooth.shp') %>% st_transform(crs = 32737)
   
   #Step 8) calculate footprint stats
   
   #footprint_settlements <- footprints_proj[final_outlines,]
   
   #footprint_settlements$area_sqm <- st_area(footprint_settlements)
   
   #footprint_settlements$area_sqkm <- units::set_units(footprint_settlements$area_sqm, km^2)
   
   #sum(footprint_settlements$area_sqkm)
   


