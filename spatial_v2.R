library(sf); library(dplyr); library(readr); library(lwgeom)

# assumes:
# - grid_both already exists (bind_rows of tasmax_grid & pr_grid)
# - parks_path points to a layer with UNIT_CODE and geometry

sf::sf_use_s2(FALSE)
target_crs <- 5070

# Clean & reproject inputs
grid_both <- st_transform(grid_both, target_crs) |>
  st_make_valid() |>
  st_set_precision(1) |>
  lwgeom::st_snap_to_grid(1) |>
  suppressWarnings(st_buffer(0))

parks <- st_read(parks_path, quiet = TRUE) |>
  mutate(UNIT_CODE = as.character(UNIT_CODE)) |>
  st_transform(target_crs) |>
  st_make_valid() |>
  st_set_precision(1) |>
  lwgeom::st_snap_to_grid(1) |>
  suppressWarnings(st_buffer(0)) |>
  group_by(UNIT_CODE) |>
  summarize(geometry = st_union(geometry), .groups = "drop")

# helper: construct an empty return with the final schema
empty_out <- function(crs) {
  st_sf(
    Park = character(0),
    `Climate Future` = character(0),
    Variable = character(0),
    Value = numeric(0),
    geometry = st_sfc(crs = crs)
  )
}

summarize_for_park <- function(sf_with_vals, parks_sf, park_code){
  park <- parks_sf[parks_sf$UNIT_CODE == park_code, ]
  if (nrow(park) == 0) return(empty_out(st_crs(sf_with_vals)))
  
  # prefilter candidates
  idx <- st_intersects(sf_with_vals, park, sparse = TRUE)[[1]]
  if (length(idx) == 0) return(empty_out(st_crs(sf_with_vals)))
  
  clipped <- suppressWarnings(st_intersection(sf_with_vals[idx, ], park))
  if (nrow(clipped) == 0) return(empty_out(st_crs(sf_with_vals)))
  
  clipped <- clipped[!st_is_empty(clipped), ]
  if (nrow(clipped) == 0) return(empty_out(st_crs(sf_with_vals)))
  
  suppressWarnings(clipped <- st_collection_extract(clipped, "POLYGON"))
  
  clipped |>
    group_by(UNIT_CODE, ClimateFuture, Variable, geometry) |>
    summarize(Value = mean(value, na.rm = TRUE), .groups = "drop") |>
    rename(Park = UNIT_CODE, `Climate Future` = ClimateFuture) |>
    select(Park, `Climate Future`, Variable, Value, geometry)
}

# run for your list of parks
parks_wanted <- c("EVER","GRCA","ISRO","OLYM")
res_list <- lapply(parks_wanted, function(pk){
  message("Processing ", pk, " ...")
  summarize_for_park(grid_both, parks, pk)
})
names(res_list) <- parks_wanted

# bind + finalize
res_all <- bind_rows(res_list)

# sanity check: do we have the expected columns?
# print(names(res_all))

res_all <- res_all |>
  mutate(Index = row_number()) |>
  select(Index, Park, `Climate Future`, Variable, Value, geometry)

# write (adjust paths if needed)
gpkg_out    <- here::here("spatial/outputs/CF_dashboard_vars.gpkg")
geojson_out <- here::here("spatial/outputs/CF_dashboard_vars.geojson")
attr_out    <- here::here("spatial/outputs/CF_dashboard_vars_attributes.csv")
dir.create(dirname(gpkg_out), recursive = TRUE, showWarnings = FALSE)

st_write(res_all, gpkg_out, delete_dsn = TRUE, quiet = TRUE)
st_write(res_all, geojson_out, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
readr::write_csv(st_drop_geometry(res_all), attr_out)

message("Wrote:\n  ", gpkg_out, "\n  ", geojson_out, "\n  ", attr_out)
