# ======================= CF Dashboard Spatial Build ===========================
# Extract MACA ANN climatologies (tasmax °F, precip inches) to MACA grid,
# clip/average by park, and export combined layers for Dashboards.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(here)
  library(lwgeom)   # st_make_valid
})

# --------- EDIT THESE PATHS / PARKS IF NEEDED --------------------------------
parks_wanted <- c("EVER","GRCA","ISRO","OLYM")

grid_path   <- here("MACA_grid/MACA_grid.shp")           # MACA polygon grid
parks_path  <- here("nps_boundary/nps_boundary.shp")     # has UNIT_CODE
raw_dir     <- here("spatial/raw_data")                  # folder with GeoTIFFs

# MACA ANN climatology rasters (CCSM4)
tasmax_files <- c(
  "Historical"       = "macav2metdata_tasmax_ANN_19712000_historical_CCSM4.tif",
  "Climate Future 1" = "macav2metdata_tasmax_ANN_20402069_rcp45_CCSM4.tif",
  "Climate Future 2" = "macav2metdata_tasmax_ANN_20402069_rcp85_CCSM4.tif"
)

pr_files <- c(
  "Historical"       = "macav2metdata_pr_ANN_19712000_historical_CCSM4.tif",
  "Climate Future 1" = "macav2metdata_pr_ANN_20402069_rcp45_CCSM4.tif",
  "Climate Future 2" = "macav2metdata_pr_ANN_20402069_rcp85_CCSM4.tif"
)

# Output locations
gpkg_out    <- here("spatial/outputs/CF_dashboard_vars.gpkg")
geojson_out <- here("spatial/outputs/CF_dashboard_vars.geojson")
attr_out    <- here("spatial/outputs/CF_dashboard_vars_attributes.csv")
dir.create(dirname(gpkg_out), recursive = TRUE, showWarnings = FALSE)
# -----------------------------------------------------------------------------


# ------------------------ Helpers --------------------------------------------

# Precip units: convert to inches if values look like mm (simple, robust gate)
as_inches <- function(r) {
  rng <- try(terra::global(r, range, na.rm = TRUE), silent = TRUE)
  # if range failed, just return as-is
  if (inherits(rng, "try-error")) return(r)
  # Annual totals in inches rarely > 200 in CONUS; mm commonly > 200
  if (!is.na(rng[1, 2]) && rng[1, 2] > 200) r <- r / 25.4
  r
}

# Extract mean raster value per MACA grid polygon for a named list of rasters
extract_to_grid <- function(grid_sf, files_named, var_label, transform_fun = NULL){
  out <- vector("list", length(files_named)); names(out) <- names(files_named)
  for (lab in names(files_named)) {
    r <- terra::rast(file.path(raw_dir, files_named[[lab]]))
    g <- sf::st_transform(grid_sf, terra::crs(r))
    if (!is.null(transform_fun)) r <- transform_fun(r)
    vals <- terra::extract(r, terra::vect(g), fun = mean, na.rm = TRUE)
    df <- cbind(g, value = vals[[2]])
    df$ClimateFuture <- lab
    df$Variable      <- var_label
    out[[lab]] <- df
  }
  do.call(rbind, out)
}

# Safe intersection: validate, try s2; if fails, retry with GEOS + buffer(0)
safe_intersection <- function(a, b) {
  a <- st_make_valid(a); b <- st_make_valid(b)
  
  sf_use_s2(TRUE)
  out <- try(suppressWarnings(st_intersection(a, b)), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  
  old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old), add = TRUE)
  a2 <- suppressWarnings(st_buffer(a, 0))
  b2 <- suppressWarnings(st_buffer(b, 0))
  suppressWarnings(st_intersection(a2, b2))
}

# Clip to a park and average per polygon piece, per CF, per variable
summarize_for_park <- function(sf_with_vals, parks_sf, park_code){
  park <- parks_sf[parks_sf$UNIT_CODE == park_code, ]
  if (nrow(park) == 0) return(sf_with_vals[0,])
  
  park <- st_transform(park, st_crs(sf_with_vals))
  
  # pre-filter to those MACA polygons that touch the park
  idx <- st_intersects(sf_with_vals, park, sparse = TRUE)[[1]]
  if (length(idx) == 0) return(sf_with_vals[0,])
  
  clipped <- safe_intersection(sf_with_vals[idx, ], park)
  clipped <- st_make_valid(clipped)
  suppressWarnings(clipped <- st_cast(clipped, "POLYGON"))
  
  clipped |>
    group_by(UNIT_CODE, ClimateFuture, Variable, geometry) |>
    summarize(Value = mean(value, na.rm = TRUE), .groups = "drop") |>
    mutate(Park = UNIT_CODE) |>
    select(Park, `Climate Future` = ClimateFuture, Variable, Value, geometry)
}
# -----------------------------------------------------------------------------


# ------------------------ Load static layers ---------------------------------
grid  <- st_read(grid_path, quiet = TRUE)  |> st_make_valid()
parks <- st_read(parks_path, quiet = TRUE) |> st_make_valid() |>
  mutate(UNIT_CODE = as.character(UNIT_CODE))

# ------------------------ Build grid-level values -----------------------------
# tasmax: your ANN rasters are already °F; keep as °F and label accordingly
tasmax_grid <- extract_to_grid(
  grid, tasmax_files,
  var_label = "Maximum Temperature (°F)"
)

# precip: auto-convert to inches if needed
pr_grid <- extract_to_grid(
  grid, pr_files,
  var_label = "Average Precipitation (in)",
  transform_fun = as_inches
)

saveRDS(list(tasmax=tasmax_grid, pr=pr_grid), "grid_extracted.Rds")

grid_both <- bind_rows(tasmax_grid, pr_grid)

# Run if you already have the extracted grid in the environment
grids <- readRDS("grid_extracted.Rds")
tasmax_grid <- grids$tasmax
pr_grid     <- grids$pr
grid_both   <- bind_rows(tasmax_grid, pr_grid)

# ------------------------ Per-park clips & summaries --------------------------
res_list <- vector("list", length(parks_wanted)); names(res_list) <- parks_wanted
for (pk in parks_wanted){
  message("Processing ", pk, " ...")
  res_list[[pk]] <- summarize_for_park(grid_both, parks, pk)
}
res_all <- bind_rows(res_list)

# Index for joins; tidy columns
res_all <- res_all |>
  mutate(Index = row_number()) |>
  select(Index, Park, `Climate Future`, Variable, Value, geometry)

# ------------------------ Write outputs ---------------------------------------
st_write(res_all, gpkg_out, delete_dsn = TRUE, quiet = TRUE)
st_write(res_all, geojson_out, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
write.csv(st_drop_geometry(res_all), attr_out, row.names = FALSE)

message(
  "Wrote:\n  ", gpkg_out,
  "\n  ", geojson_out,
  "\n  ", attr_out
)
