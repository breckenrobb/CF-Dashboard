suppressPackageStartupMessages({
  library(sf); library(dplyr); library(lwgeom); library(readr); library(here)
})

# ------------------ Inputs ------------------
parks_path  <- here("spatial/nps_boundary/nps_boundary.shp")   # boundary layer
gpkg_out    <- here("spatial/outputs/ISRO_dashboard_vars.gpkg")
geojson_out <- here("spatial/outputs/ISRO_dashboard_vars.geojson")
attr_out    <- here("spatial/outputs/ISRO_dashboard_vars_attributes.csv")
dir.create(dirname(gpkg_out), recursive = TRUE, showWarnings = FALSE)

# Combine tasmax & pr (assumes you already ran extract_to_grid earlier)
grid_both <- bind_rows(tasmax_grid, pr_grid)

# Force GEOS, reproject to meters CRS
sf::sf_use_s2(FALSE)
target_crs <- 5070  # NAD83 / Conus Albers
grid_both <- st_transform(grid_both, target_crs) |>
  st_make_valid() |>
  st_buffer(0)

parks <- st_read(parks_path, quiet = TRUE) |>
  mutate(UNIT_CODE = as.character(UNIT_CODE)) |>
  st_transform(target_crs) |>
  st_make_valid()

isro <- parks[parks$UNIT_CODE == "ISRO", ]

# ------------------ Clip + summarize ------------------
cand <- st_filter(grid_both, isro, .pred = st_intersects)

clipped <- suppressWarnings(st_intersection(cand, isro))
clipped <- clipped[!st_is_empty(clipped), ]
clipped <- suppressWarnings(st_collection_extract(clipped, "POLYGON"))

res_isro <- clipped |>
  group_by(UNIT_CODE, ClimateFuture, Variable, geometry) |>
  summarize(Value = mean(value, na.rm = TRUE), .groups = "drop") |>
  rename(Park = UNIT_CODE, `Climate Future` = ClimateFuture) |>
  select(Park, `Climate Future`, Variable, Value, geometry) |>
  mutate(Index = row_number()) |>
  select(Index, Park, `Climate Future`, Variable, Value, geometry)

# ------------------ Write outputs ------------------
st_write(res_isro, gpkg_out, delete_dsn = TRUE, quiet = TRUE)
st_write(res_isro, geojson_out, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
write_csv(st_drop_geometry(res_isro), attr_out)

message("ISRO outputs written:\n  ", gpkg_out, "\n  ", geojson_out, "\n  ", attr_out)
