Author: Brecken Robb

Projected started: July 2025

-------------

This repository was used to produce the files needed to create a template ArcGIS dashboard of CCRP's CMIP5 climate futures: https://geospatial.nps.gov/portal/apps/dashboards/b1b53badc7834d8083ff7d7f761b35de
It includes scripts to create .csv and .gpkg files that can be read into ArcGIS as feature layers that are associated with dashboard elements.

For dashboard v1 (https://geospatial.nps.gov/portal/apps/dashboards/bccd3bfbf0114c2ab2580f0d3747a449), Alaska parks (CAKR, KOVA) were piloted because we had just run those parks' climate futures and the R scripts / outputs were in an easily usable state. These were used to produce bar charts across multiple climate variables using script time-series.R.

For dashboard v2 (https://geospatial.nps.gov/portal/apps/dashboards/b1b53badc7834d8083ff7d7f761b35de), where we incorporated temporal and spatial data, only CONUS parks (EVER, GRCA, ISRO, OLYM) were used so that they matched the MACA spatial climate variable data we had, which we did not have for the AK parks. 
- The bar chart and times series outputs came from the 2024 Climate Future Summaries folders and were produced using script time-series-v2.R.
- The spatial outputs came from MACA climate variable data found at https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/ and were produced using spatial.R, spatial_v2.R, and spatial_ISRO.R scripts. These were different attempts I had made. The only fully successful one I ran was ISRO using script spatial_ISRO.R for the variable 'tasmax'.

The MACA climate variable data I downloaded were:
- macav2metdata_tasmax_ANN_19712000_historical_CCSM4.tif
- macav2metdata_tasmax_ANN_20402069_rcp45_CCSM4.tif
- macav2metdata_tasmax_ANN_20402069_rcp85_CCSM4.tif

Other spatial data needed to run the scripts include:
- MACA_grid.zip
- nps_boundary.zip
These are available from CCRP, NPS, or through web searches.

The new_RICA_scatterplots.R script produces a list of interpretable climate future variable names.

The scripts are very early versions and would benefit from a lot of cleanup, particularly on an automation front for multiple parks. I attempted to do this but wasn't ever successful (time-series_iterations.R, spatial_iterations.R).

Because I cannot push large files to GitHub, I would recommend saving your own .RData files as you go because the imported MACA data files are very large. That way you do not need to rerun those lines of code every time. The MACA files are of the entirety of CONUS, so you can simply clip to your location(s) of interest from the .RData files.
