# Delineating Settlement Outlines in Monduli, Tanzania #

[![Open Source Love svg1](https://badges.frapsoft.com/os/v1/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badges/)
[![GitHub commits](https://img.shields.io/github/commits-since/Naereen/StrapDown.js/v1.0.0.svg)](https://GitHub.com/Naereen/StrapDown.js/commit/)

This research project focuses on delineating settlement outlines from openly accessible building footprint data, contributing to the Data for Good Movement by outputting vector polygons fully integratable with OSM.

## Methodology ##
Following emerging research on delineating urban areas from building footprints (Jochem et al. 2018; Arribas-Bel et al. 2019; de Bellefon et al. 2019), this research aims to implement a bottom-up approach to define settlements, using the Density Based Spatial Clustering of Application with Noise (DBSCAN) clustering algorithm to group buildings based on density and distance parameters. 

It advances on previous research in the field and shows that:
* The application of a concave hull, rather than the comparable convex hull, is more advanced though advantagous with the ability to capture more accurately on-the-ground settlement geometries
* A second iteration clustering creates more cohesive settlements and recognises multi-density clusters across the dataset
* Simple point representations of buildings can be used to delineate settlement extent
* Accurate and spatially contiguous data can be created from open data sources

## Data Sources ##
The sources of data used in this analysis are:

Dataset              | Download Location
---------------------| ------------------
Microsoft Buildings  | [Link](https://github.com/microsoft/Uganda-Tanzania-Building-Footprints, "Link")
HOT OSM Buildings    | [Link](https://data.humdata.org/dataset/hotosm_tza_buildings, "Link")
Tanzania Boundaries  | [Link](https://gadm.org/download_country_v3.html, "Link")

The ground-truth data we used to compare with our final delineations can be found here:

Dataset                                 | Download Location
----------------------------------------| ------------------
GRID3 Settlement Extent Version 01 (SE) | [Link](https://data.grid3.org/search?tags=all(Settlement%20Extents,TZA), "Link")
World Settlement Footprint (WSF)        | [Link](https://figshare.com/articles/dataset/World_Settlement_Footprint_WSF_2015/10048412, "Link")


## Final Outlines ##

The final settlement outlines delinated as part of this analysis can be seen below:

![image](https://github.com/KKCV6/dissertation/blob/main/images/settlements_github.PNG)

## References ##
* Arribas-Bel, D., M. Garcia-López and E. Viladecans-Marsal (2019) 'Building(s) and cities: Delineating urban areas with a machine learning algorithm', Journal of Urban Economics, October, 103217.
* Jochem, W. C., T. J. Bird and A. J. Tatem (2018) 'Identifying residential neighbourhood types from settlement points in a machine learning approach', Computers, Environment and Urban Systems, 69, 104–113
* de Bellefon, M. P., P. P. Combes., G. Duranton., L . Gobillon and C. Gorin (2019) 'Delineating urban areas using building density', Journal of Urban Economics, October 2018.
