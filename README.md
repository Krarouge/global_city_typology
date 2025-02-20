# global_city_typology
This repo is divided into several subfolders:
### clustering
- clustering_final.ipynb: code for DEC
- data_combination.ipynb: code to combine all data sources into clustering_variables.csv
- clustering_variables.csv: all variables used for DEC
### co2
- co2_combination.ipynb: code for extracting emissions per Urban Centre for every ODIAC file (2000-2022)
- odiac: ODIAC geotiffs found at download link: 
### plots
- maps.ipynb: code for plotting fig 1 
### road_master: 
- osm_extract.ipynb: code for extracting OSM data trough Overpass, clean the features and extract lengths (osm_extract.ipynb)
- clean_roads: files for every UC, found at https://filesender.switch.ch/filesender2/?s=download&token=84edaef0-a2b1-4b26-b3c4-17cd005fbb3d
### topics
- 02_anayze_clusters.R: R code to plot fig 2 and and fig 3
- dataverse_files: topic files and data_prep.R (extracting the main topic for each publication) found under https://filesender.switch.ch/filesender2/?s=download&token=965abe9e-d32e-4c0e-a870-893b1e08604d
### urban_centres
- GHSL database. GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv found at https://human-settlement.emergency.copernicus.eu/ghs_stat_ucdb2015mt_r2019a.php

Additionally, results_clusters_small_medium.csv contains the clusters ID per small and medium city under column y_pre_dec (0,1,2,3 and 4 meaning clusters 1,2,3,4 and 5)


Please note that downloadable files are available until 12/04/2025