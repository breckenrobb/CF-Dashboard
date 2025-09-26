library(terra)
library(sf)
library(dplyr)
library(here)
library(geojsonio)

## Will need to rewrite code to call the variable and loop that through
## This is currently only for tasmax

##### MACA grid ----

# Load MACA grid polygons
grid <- st_read(here(paste0("MACA_grid/MACA_grid.shp")))

# List of raster files and climate labels
rasters <- list(
  Historical = "MACA_grid/macav2metdata_tasmax_ANN_19712000_historical_CCSM4.tif",
  `Climate Future 1` = "MACA_grid/macav2metdata_tasmax_ANN_20402069_rcp45_CCSM4.tif",
  `Climate Future 2` = "MACA_grid/macav2metdata_tasmax_ANN_20402069_rcp85_CCSM4.tif"
)

# Initialize empty list to store extracted data
all_results <- list()

# Loop through each raster and extract mean value per grid cell
for (label in names(rasters)) {
  r <- rast(rasters[[label]])
  
  # Match CRS
  grid_proj <- st_transform(grid, crs(r))
  
  # Extract mean tasmax for each polygon
  extracted <- terra::extract(r, vect(grid_proj), fun = mean, na.rm = TRUE)
  
  # Combine with geometry and label
  df <- cbind(grid_proj, tasmax = extracted[[2]])  # [[2]] skips ID column
  df$ClimateFuture <- label
  
  all_results[[label]] <- df
}

# Combine all into one spatial dataframe
tasmax <- do.call(rbind, all_results)

# Export to load later so don't have to re-run each time
st_write(tasmax, "tasmax_grid.gpkg", delete_dsn = TRUE)


##### Clip to park ----

# Load your GeoPackage with tasmax data
tasmax <- st_read("tasmax_grid.gpkg")

# Load park boundary shapefile - see climate futures repository
parks <- st_read("nps_boundary/nps_boundary.shp")

# Change from being a list
parks$UNIT_CODE <- as.character(parks$UNIT_CODE)

# Filter to park
park <- parks[parks$UNIT_CODE == "ISRO", ] # Change park code here

# Match CRS
park <- st_transform(park, st_crs(tasmax))

# Clip to park
tasmax_park <- st_intersection(tasmax, park)

# Ensure all geometries are valid and cast to POLYGON
tasmax_park <- st_make_valid(tasmax_park)
tasmax_park <- st_cast(tasmax_park, "POLYGON", warn = FALSE)

# Average tasmax per geometry and climate future
tasmax_park_avg <- tasmax_park %>%
  group_by(UNIT_CODE, ClimateFuture, geom) %>%   # after clipping, some parks cut off "geometry" to "geom", unsure why
  summarize(tasmax = mean(tasmax, na.rm = TRUE), .groups = "drop")

# Add row identifier for PowerBI association
tasmax_park_avg$Index <- seq_len(nrow(tasmax_park_avg))

# Pivot longer climate variable and rename columns
tasmax_park_avg <- tasmax_park_avg %>%
  mutate(
    Variable = "tasmax",  # Add variable name
  ) %>%
  rename(
    Park = UNIT_CODE,
    `Climate Future` = ClimateFuture,
    Geometry = geom,
    Value = tasmax
  ) %>%
  select(
    Index, Park, 'Climate Future', Variable, Value, Geometry
  )

# Export as csv
write.csv(tasmax_park_avg, "tasmax_park_attribute_geometry.csv", row.names = FALSE, 
          fileEncoding = "UTF-8", quote = TRUE)

# Export as geopackage (doesn't work with PowerBI)
st_write(tasmax_park_avg, "tasmax_park_avg.gpkg", delete_dsn = TRUE)

# First export as geojson
st_write(tasmax_park_avg, "tasmax_park_avg.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# Then as topojson (works with PowerBI)
topojson_write(
  input = tasmax_park_avg,
  file = "tasmax_park_avg.topojson"
)

# Export the attribute table for PowerBI association (no geometry)
attribute_table <- tasmax_park_avg %>%
  st_drop_geometry()  # Removes the geometry column

write.csv(attribute_table, "tasmax_park_attribute.csv", row.names = FALSE)


# Read in spatial file, as needed
# tasmax_park_avg <- st_read(here(paste0("./tasmax_park.gpkg")))

