# Delineating Settlement Outlines in Monduli, Tanzania #
This research project focuses on delineating settlement outlines from openly accessible building footprint data, contributing to the Data for Good Movement by outputting vector polygons fully integratable with OSM.

## Methodology ##
Following emerging research on delineating urban areas from building footprints (Jochem et al. 2018; Arribas-Bel et al. 2019), this research aims to implement a bottom-up approach to define settlements, using the Density Based Spatial Clustering of Application with Noise (DBSCAN) clustering algorithm to group buildings based on density and distance parameters. 

It advances on previous research in the field and shows that:
* The application of a concave hull, rather than the comparable, convex hull is more advanced though advantagous with the ability to capture more accurately on-the-ground settlement geometries
* A second iteration clustering creates more cohesive settlements and recognises multi-density clusters across the dataset
* Simple point representations of buildings can be used to delineate settlement extent
* Accurate and spatially contiguous data can be created from open data sources

## Data Sources ##
Two sources of building footprints are used in this analysis:

Dataset       | Download Location
------------- | -------------
Microsoft     | [Link](https://github.com/microsoft/Uganda-Tanzania-Building-Footprints, "Link")
HOT OSM       | [Link](https://data.humdata.org/dataset/hotosm_tza_buildings, "Link")

## Final Outlines ##

The final settlement outlines delinated as part of this analysis can be seen below:

![image](https://github.com/KKCV6/dissertation/blob/main/images/settlements_github.PNG)

## References ##
Arribas-Bel, D., M. Garcia-López and E. Viladecans-Marsal (2019) 'Building(s) and cities: Delineating urban areas with a machine learning algorithm', Journal of Urban Economics, October, 103217.
Jochem, W. C., T. J. Bird and A. J. Tatem (2018) 'Identifying residential neighbourhood types from settlement points in a machine learning approach', Computers, Environment and Urban Systems, 69, 104–113
