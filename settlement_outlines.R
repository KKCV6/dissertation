# load our required libraries 
library(here)
library(tidyverse)
library(sf)
library(magrittr)
library(tmap)
library(fpc)
library(dbscan)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(spNetwork)
library(stplanr)
library(tidyverse)
library(spatstat)
library(maptools)
library(devtools)
library(foot)
library(concaveman)
library(rgeos)

#0) Setting Up - JW Ignore my edits up here!
#footprints
tnz_bf <- st_read('data/footprints/Tanzania_2019-09-16/Tanzania.geojson')

#boundaries
tnz_districts <- st_read('data/tanzania_gadm/gadm36_TZA_2.shp')

#filter
monduli_district <- filter(tnz_districts, NAME_2 == "Monduli")
monduli_bf <- st_read('data/footprints/monduli_building_footprints.shp')

#centroids
monduli_bf_centroids <- st_centroid(monduli_bf)

#points
monduli_points <- st_coordinates(monduli_bf_centroids)

#1) Settlement Outlines from Building Footprints
monduli_dbscan <- dbscan::dbscan(monduli_points, eps = 0.001, minPts = 5)
plot(monduli_points, col=monduli_dbscan$cluster)
plot(monduli_district$geometry, add=T)

  #assign building footprint to a cluster
monduli_points_wcluster <- as.data.frame(monduli_points) %>% mutate(dbcluster=monduli_dbscan$cluster)

 #Create settlement outlines from Building Footprints and DB-Scan cluster

geometry_list <- vector(mode = "list", length = max(monduli_points_wcluster$dbcluster))

counter <-1

for (cluster_index in seq(1, max(monduli_points_wcluster$dbcluster))) {
  
  monduli_points_wcluster_subset <- filter(monduli_points_wcluster, dbcluster==cluster_index)
  
  monduli_points_wcluster_subset_coords <- monduli_points_wcluster_subset[c("X", "Y")]
  
  concave_outline <- as.matrix(monduli_points_wcluster_subset_coords)  %>% concaveman()
  
  coords <- as.data.frame(concave_outline)
  
  settlement_polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
  
  geometry_list[counter] <- (settlement_polygon$geometry)
  
  counter <- counter + 1
}

settlement_hulls <- st_sfc(geometry_list, crs = 4326)

st_write(settlement_hulls, "data/settlement_outlines/monduli_settlements_v1.shp", delete_layer = TRUE)

monduli_settlements_v1 <- st_read('data/settlement_outlines/monduli_settlements_v1.shp')

monduli_settlements_v1 <- st_transform(monduli_settlements_v1, crs = 4326)

tmap_mode("view")

tm_shape(monduli_settlements_v1) + tm_borders()

  #buffer
# I would definitely look into adding a buffer

settlement_buffer <- st_transform(monduli_settlements_v1, crs = 32737)

settlement_buffer<- st_buffer(settlement_buffer, 100,  endCapStyle = "FLAT", joinStyle = "MITRE") %>% st_union()

settlement_buffer <- st_cast(settlement_buffer, "POLYGON")

settlement_buffer<-  settlement_buffer %>%
 st_sf %>%
st_cast()

tmap_mode("view")

tm_shape(settlement_buffer)+tm_borders()

#2) Create catchment areas from the building outline footprints

  #centroids
sett_centroids <- st_point_on_surface(monduli_settlements_v1)
tm_shape(monduli_settlements_v1) + tm_borders() + tm_shape(sett_centroids)+tm_dots()

  #points
sett_points <- st_coordinates(sett_centroids)

plot(sett_points)

  #dbscan
sett_dbscan <- dbscan::dbscan(sett_points, eps = 0.011, minPts = 3) 

plot(sett_points, col=sett_dbscan$cluster)
plot(monduli_district$geometry, add=T)

#assign building footprint to a cluster
# Keep as sett_points (do not use centroids version as we need the X and Y column)
sett_points_wcluster <- as.data.frame(sett_points) %>% mutate(dbcluster=sett_dbscan$cluster)

geometry_list <- vector(mode = "list", length = max(sett_points_wcluster$dbcluster))

counter <-1

for (cluster_index in seq(1, max(sett_points_wcluster$dbcluster))) {
  
  sett_points_wcluster_subset <- filter(sett_points_wcluster, dbcluster==cluster_index)
  
  sett_points_wcluster_subset_coords <- sett_points_wcluster_subset[c("X", "Y")]
  
  concave_outline <- as.matrix(sett_points_wcluster_subset_coords)  %>% concaveman()
  
  coords <- as.data.frame(concave_outline)
  
  settlement_polygon <- coords %>% st_as_sf(coords = c("V1", "V2"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")

  geometry_list[counter] <- (settlement_polygon$geometry)
  
  counter <- counter + 1
}

settlement_hulls <- st_sfc(geometry_list, crs = 4326)

st_write(settlement_hulls, "data/settlement_outlines/settlement_outlines_v2.shp", delete_layer = TRUE)

sett_outlines <- st_read('data/settlement_outlines/settlement_outlines_v2.shp')

tm_shape(sett_outlines)+tm_borders()

# Once plotted, I think there is obvious work in improving the settlement outline - I'd look into buffers as above and intersections to join outlines together.

tmap_mode("view")

tm_shape(sett_outlines) + tm_borders() +
  tm_shape(monduli_settlements_v1) + tm_borders()

#aggregate

mon_sp <- as(monduli_settlements_v1, 'Spatial')

sett_sp <- as(sett_outlines, 'Spatial')

over_ID <- over(mon_sp, sett_sp, fn=NULL)

monduli_settlements_v1$over_id <- over_ID$FID

tm_shape(monduli_settlements_v1)+tm_polygons(col = "over_id")+
  tm_shape(sett_outlines)+tm_borders()

mon_sp <- as(monduli_settlements_v1, 'Spatial')

zones.IDs <- as.numeric(monduli_settlements_v1[["over_id"]])

points <-  monduli_settlements_v1 %>%  group_by(over_id) %>% st_cast("MULTIPOINT")

tm_shape(points)+tm_dots()

poly_points <- st_coordinates(points)

#

polygons_sett <- map(unique(points$over_id),
                   ~ concaveman(points[points$over_id %in% .,], concavity = 3)
) %>%
  map2(unique(points$over_id), ~ mutate(.x, k = .y)) %>% reduce(rbind)

polygons_sett <- st_zm(polygons_sett, drop=T, what='ZM')

class(polygons_sett)

tmap_mode("view")

tm_shape(polygons_sett)+tm_fill("over_id",colorNA = NULL)+
  tm_shape(monduli_settlements_v1)+tm_borders()

polygons_sett <- polygons_sett %>% drop_na(over_id) %>% st_as_sf()

# Difference
  
mon_na <- monduli_settlements_v1[is.na(monduli_settlements_v1$over_id),] %>% st_

mon_na <- st_transform(mon_na, crs = 4326)

polygons_sett <- st_transform(polygons_sett, crs = 4326)

#merge

st_write(polygons_sett, "data/settlement_outlines/polygons_sett.shp", delete_layer = TRUE)

polygons_sett <- st_read('data/settlement_outlines/polygons_sett.shp')

st_write(mon_na, "data/settlement_outlines/mon_na.shp", delete_layer = TRUE)

mon_na <- st_read('data/settlement_outlines/mon_na.shp')

polygons_sett$k=NULL
mon_na$FID=NULL

outlines <- rbind(polygons_sett, mon_na)

tm_shape(outlines)+tm_borders()

outlines$ID <- 1:nrow(outlines)


