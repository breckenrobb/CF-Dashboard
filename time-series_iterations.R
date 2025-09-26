# VERSION 1 TEST ----
## Test version - Did not convert GCM names to CF names

# ---- Setup ----
# Packages
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
})

# Parks & directories
parks <- c("EVER", "GRCA", "ISRO", "OLYM")
base_dir <- "C:/Users/brobb/R Projects/CF-Dashboard/CF-Dashboard/time-series/parks"
# ^ adjust if your folder depth differs (needs .../parks/[PARK]/[PARK]/WarmWet_HotDry/tables/WB-Annual.csv)

# Variable keep/rename map
var_map <- c(
  "sum_p.mm"   = "Total Precipitation (mm)",
  "avg_t.C"    = "Average Temperature (°C)",
  "sum_rain.mm"= "Total Rainfall (mm)",
  "sum_snow.mm"= "Total Snowpack (mm)",
  "runoff.mm"  = "Runoff (mm)",
  "sum_d.mm"   = "Climatic Water Deficit (mm)"
)

# ---- Build combined long table ----
rows_list <- list()

for (pk in parks) {
  csv_path <- file.path(base_dir, pk, pk, "WarmWet_HotDry", "tables", "WB-Annual.csv")
  if (!file.exists(csv_path)) {
    warning(glue::glue("Missing WB-Annual for {pk}: {csv_path}")); next
  }
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  # keep only year, GCM, and the six variables
  need_cols <- c("year", "GCM", names(var_map))
  missing_cols <- setdiff(need_cols, names(df))
  if (length(missing_cols)) {
    warning(glue::glue("Skipping {pk}: missing columns {paste(missing_cols, collapse=', ')}"))
    next
  }
  
  df_long <- df %>%
    select(all_of(need_cols)) %>%
    pivot_longer(cols = all_of(names(var_map)),
                 names_to = "Variable_raw",
                 values_to = "Value") %>%
    mutate(
      Park = pk,
      `Climate Future` = GCM,          # keep GCM names as-is, header = Climate Future
      Year = year,
      Variable = unname(var_map[Variable_raw])
    ) %>%
    select(Park, `Climate Future`, Year, Variable, Value) %>%
    arrange(Park, `Climate Future`, Year, Variable)
  
  rows_list[[pk]] <- df_long
}

out <- bind_rows(rows_list)

# ---- Write CSV (matches your template’s columns) ----
out_path <- file.path(dirname(base_dir), "annual_climate_data_long.csv")
write_csv(out, out_path)
message("Wrote: ", out_path, "  (rows=", nrow(out), ")")


# VERSION 2 TEST ----
## Converted GCM names to CF names (using sessionInfo.txt files)
## Ordered CF in final data set (historical, warm wet, hot dry)

# ---- Setup ----
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
})

parks <- c("EVER", "GRCA", "ISRO", "OLYM")
root <- "C:/Users/brobb/R Projects/CF-Dashboard/time-series/parks"

var_map <- c(
  "sum_p.mm"    = "Total Precipitation (mm)",
  "avg_t.C"     = "Average Temperature (°C)",
  "sum_rain.mm" = "Total Rainfall (mm)",
  "sum_snow.mm" = "Total Snowpack (mm)",
  "runoff.mm"   = "Runoff (mm)",
  "sum_d.mm"    = "Climatic Water Deficit (mm)"
)

# ---- Helper: parse CF map from sessionInfo.txt ----
parse_cf_map <- function(session_file) {
  if (!file.exists(session_file)) return(NULL)
  L <- readLines(session_file, warn = FALSE)
  line <- L[grepl("Models used in Water Balance analysis:", L)][1]
  if (is.na(line)) return(NULL)
  
  # extract both c(...) blocks
  parts <- regmatches(line, gregexpr('c\\([^\\)]*\\)', line))[[1]]
  if (length(parts) < 2) return(NULL)
  
  extract_vec <- function(x) {
    s <- sub('^c\\(|\\)$', '', x)
    toks <- strsplit(s, ',\\s*')[[1]]
    gsub('^"|"$', '', trimws(toks))
  }
  
  models <- extract_vec(parts[1])
  labels <- extract_vec(parts[2])
  setNames(labels, models)
}

# ---- Build combined dataset ----
out_list <- list()

for (pk in parks) {
  csv_path <- file.path(root, pk, pk, "WarmWet_HotDry", "tables", "WB-Annual.csv")
  session_path <- file.path(root, pk, pk, "sessionInfo.txt")
  if (!file.exists(csv_path)) { warning("Missing WB-Annual for ", pk); next }
  
  cf_map <- parse_cf_map(session_path)
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  # keep needed
  need_cols <- c("year", "GCM", names(var_map))
  if (!all(need_cols %in% names(df))) next
  
  df_long <- df %>%
    select(all_of(need_cols)) %>%
    pivot_longer(cols = names(var_map),
                 names_to = "Variable_raw",
                 values_to = "Value") %>%
    mutate(
      Park = pk,
      `Climate Future` = case_when(
        GCM == "gridmet.historical" ~ "Historical",
        !is.null(cf_map) ~ recode(GCM, !!!cf_map, .default = GCM),
        TRUE ~ GCM
      ),
      Year = year,
      Variable = unname(var_map[Variable_raw])
    ) %>%
    select(Park, `Climate Future`, Year, Variable, Value)
  
  # Order Climate Futures per park
  df_long$`Climate Future` <- factor(
    df_long$`Climate Future`,
    levels = c("Historical", "Warm Wet", "Hot Dry"),
    ordered = TRUE
  )
  
  out_list[[pk]] <- arrange(df_long, Park, `Climate Future`, Year, Variable)
}

out <- bind_rows(out_list)

# ---- Write CSV ----
out_file <- file.path(dirname(root), "annual_climate_data_long.csv")
write_csv(out, out_file)
message("Wrote: ", out_file, "  rows=", nrow(out))


# VERSION 3 TEST ----
## Added a CF_order column to see if I can order the bar chart bars and ts legend
## as "Historical, Warm Wet, Hot Dry" - it ultimately did not work

# ---- Setup ----
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(purrr)
})

parks <- c("EVER","GRCA","ISRO","OLYM")
root  <- "C:/Users/brobb/R Projects/CF-Dashboard/time-series/parks"

var_map <- c(
  "sum_p.mm"    = "Total Precipitation (mm)",
  "avg_t.C"     = "Average Temperature (°C)",
  "sum_rain.mm" = "Total Rainfall (mm)",
  "sum_snow.mm" = "Total Snowpack (mm)",
  "runoff.mm"   = "Runoff (mm)",
  "sum_d.mm"    = "Climatic Water Deficit (mm)"
)

# ---- Helper: parse model -> CF mapping from sessionInfo.txt (robust) ----
parse_cf_map <- function(session_file) {
  if (!file.exists(session_file)) return(NULL)
  L <- readLines(session_file, warn = FALSE)
  line <- L[grepl("Models used in Water Balance analysis:", L)][1]
  if (is.na(line)) return(NULL)
  
  parts <- regmatches(line, gregexpr('c\\([^\\)]*\\)', line))[[1]]
  if (length(parts) < 2) return(NULL)
  
  extract_vec <- function(x) {
    s <- sub('^c\\(|\\)$', '', x)
    toks <- strsplit(s, ',\\s*')[[1]]
    # drop quotes, trim
    toks <- gsub('^"|"$', '', toks)
    trimws(toks)
  }
  
  models_raw <- extract_vec(parts[1])
  labels_raw <- extract_vec(parts[2])
  
  # normalize key for matching (lowercase, trim)
  models_key <- tolower(trimws(models_raw))
  names(labels_raw) <- models_key
  labels_raw
}

# ---- Build combined table, add CF_order, report unmatched ----
unmatched_log <- list()
out_list <- list()

for (pk in parks) {
  csv_path     <- file.path(root, pk, pk, "WarmWet_HotDry", "tables", "WB-Annual.csv")
  session_path <- file.path(root, pk, pk, "sessionInfo.txt")
  
  if (!file.exists(csv_path)) { warning("Missing WB-Annual for ", pk); next }
  
  cf_map <- parse_cf_map(session_path)  # named vector: names=normalized GCMs, values=CF labels
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  need_cols <- c("year","GCM", names(var_map))
  miss <- setdiff(need_cols, names(df))
  if (length(miss)) { warning(pk, " missing cols: ", paste(miss, collapse=", ")); next }
  
  df_long <- df %>%
    select(all_of(need_cols)) %>%
    pivot_longer(all_of(names(var_map)), names_to = "Variable_raw", values_to = "Value") %>%
    # normalize a matching key for GCM
    mutate(
      Park = pk,
      gcm_key = tolower(trimws(GCM)),
      `Climate Future` = case_when(
        gcm_key == "gridmet.historical" ~ "Historical",
        !is.null(cf_map) & gcm_key %in% names(cf_map) ~ cf_map[gcm_key],
        TRUE ~ GCM  # fall back to the raw name so we can see it later
      ),
      Year = year,
      Variable = unname(var_map[Variable_raw])
    ) %>%
    select(Park, `Climate Future`, Year, Variable, Value, GCM, gcm_key)
  
  # track any that failed to map (excluding Historical)
  unmatched <- df_long %>%
    filter(`Climate Future` == GCM & gcm_key != "gridmet.historical") %>%
    distinct(Park, GCM, gcm_key)
  if (nrow(unmatched)) unmatched_log[[pk]] <- unmatched
  
  # keep only the three CFs you want
  df_long <- df_long %>%
    mutate(
      # Standardize CF labels if any have inconsistent spacing/case
      `Climate Future` = case_when(
        `Climate Future` %in% c("Historical","historical") ~ "Historical",
        str_to_lower(`Climate Future`) == "warm wet" ~ "Warm Wet",
        str_to_lower(`Climate Future`) == "hot dry"  ~ "Hot Dry",
        TRUE ~ `Climate Future`
      )
    ) %>%
    filter(`Climate Future` %in% c("Historical","Warm Wet","Hot Dry")) %>%
    mutate(
      CF_order = case_when(
        `Climate Future` == "Historical" ~ 1L,
        `Climate Future` == "Warm Wet"   ~ 2L,
        `Climate Future` == "Hot Dry"    ~ 3L
      )
    ) %>%
    arrange(Park, CF_order, Year, Variable) %>%
    select(Park, `Climate Future`, CF_order, Year, Variable, Value)
  
  out_list[[pk]] <- df_long
}

out <- bind_rows(out_list)

# ---- Write CSV for dashboard time series/bar charts ----
out_file <- file.path(dirname(root), "annual_climate_data_long.csv")
write_csv(out, out_file)
message("Wrote: ", out_file, "  rows=", nrow(out))

# ---- Print any GCMs that did not map to a CF (so you can fix quickly) ----
if (length(unmatched_log)) {
  message("\nGCMs that did not map to a Climate Future label (check sessionInfo vs WB-Annual):")
  print(bind_rows(unmatched_log, .id = "ParkGroup"))
} else {
  message("\nAll GCMs mapped to CF labels successfully.")
}

# VERSION 4 TEST ----
## Still not pulling CF names from GCMs correctly, so tried more robust mapping

# ---- Setup ----
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(purrr)
})

parks <- c("EVER","GRCA","ISRO","OLYM")
root  <- "C:/Users/brobb/R Projects/CF-Dashboard/time-series/parks"

var_map <- c(
  "sum_p.mm"    = "Total Precipitation (mm)",
  "avg_t.C"     = "Average Temperature (°C)",
  "sum_rain.mm" = "Total Rainfall (mm)",
  "sum_snow.mm" = "Total Snowpack (mm)",
  "runoff.mm"   = "Runoff (mm)",
  "sum_d.mm"    = "Climatic Water Deficit (mm)"
)

# ---- Helper: parse model -> CF mapping from sessionInfo.txt (robust) ----
parse_cf_map <- function(session_file) {
  if (!file.exists(session_file)) return(NULL)
  L <- readLines(session_file, warn = FALSE)
  line <- L[grepl("Models used in Water Balance analysis:", L)][1]
  if (is.na(line)) return(NULL)
  
  parts <- regmatches(line, gregexpr('c\\([^\\)]*\\)', line))[[1]]
  if (length(parts) < 2) return(NULL)
  
  extract_vec <- function(x) {
    s <- sub('^c\\(|\\)$', '', x)
    toks <- strsplit(s, ',\\s*')[[1]]
    # drop quotes, trim
    toks <- gsub('^"|"$', '', toks)
    trimws(toks)
  }
  
  models_raw <- extract_vec(parts[1])
  labels_raw <- extract_vec(parts[2])
  
  # normalize key for matching (lowercase, trim)
  models_key <- tolower(trimws(models_raw))
  names(labels_raw) <- models_key
  labels_raw
}

# ---- Build combined table, add CF_order, report unmatched ----
unmatched_log <- list()
out_list <- list()

for (pk in parks) {
  csv_path     <- file.path(root, pk, pk, "WarmWet_HotDry", "tables", "WB-Annual.csv")
  session_path <- file.path(root, pk, pk, "sessionInfo.txt")
  
  if (!file.exists(csv_path)) { warning("Missing WB-Annual for ", pk); next }
  
  cf_map <- parse_cf_map(session_path)  # named vector: names=normalized GCMs, values=CF labels
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  need_cols <- c("year","GCM", names(var_map))
  miss <- setdiff(need_cols, names(df))
  if (length(miss)) { warning(pk, " missing cols: ", paste(miss, collapse=", ")); next }
  
  df_long <- df %>%
    select(all_of(need_cols)) %>%
    pivot_longer(all_of(names(var_map)), names_to = "Variable_raw", values_to = "Value") %>%
    # normalize a matching key for GCM
    mutate(
      Park = pk,
      gcm_key = tolower(trimws(GCM)),
      `Climate Future` = case_when(
        gcm_key == "gridmet.historical" ~ "Historical",
        !is.null(cf_map) & gcm_key %in% names(cf_map) ~ cf_map[gcm_key],
        TRUE ~ GCM  # fall back to the raw name so we can see it later
      ),
      Year = year,
      Variable = unname(var_map[Variable_raw])
    ) %>%
    select(Park, `Climate Future`, Year, Variable, Value, GCM, gcm_key)
  
  # track any that failed to map (excluding Historical)
  unmatched <- df_long %>%
    filter(`Climate Future` == GCM & gcm_key != "gridmet.historical") %>%
    distinct(Park, GCM, gcm_key)
  if (nrow(unmatched)) unmatched_log[[pk]] <- unmatched
  
  # keep only the three CFs you want
  df_long <- df_long %>%
    mutate(
      # Standardize CF labels if any have inconsistent spacing/case
      `Climate Future` = case_when(
        `Climate Future` %in% c("Historical","historical") ~ "Historical",
        str_to_lower(`Climate Future`) == "warm wet" ~ "Warm Wet",
        str_to_lower(`Climate Future`) == "hot dry"  ~ "Hot Dry",
        TRUE ~ `Climate Future`
      )
    ) %>%
    filter(`Climate Future` %in% c("Historical","Warm Wet","Hot Dry")) %>%
    mutate(
      CF_order = case_when(
        `Climate Future` == "Historical" ~ 1L,
        `Climate Future` == "Warm Wet"   ~ 2L,
        `Climate Future` == "Hot Dry"    ~ 3L
      )
    ) %>%
    arrange(Park, CF_order, Year, Variable) %>%
    select(Park, `Climate Future`, CF_order, Year, Variable, Value)
  
  out_list[[pk]] <- df_long
}

out <- bind_rows(out_list)

# ---- Write CSV for dashboard time series/bar charts ----
out_file <- file.path(dirname(root), "annual_climate_data_long.csv")
write_csv(out, out_file)
message("Wrote: ", out_file, "  rows=", nrow(out))

# ---- Print any GCMs that did not map to a CF (so you can fix quickly) ----
if (length(unmatched_log)) {
  message("\nGCMs that did not map to a Climate Future label (check sessionInfo vs WB-Annual):")
  print(bind_rows(unmatched_log, .id = "ParkGroup"))
} else {
  message("\nAll GCMs mapped to CF labels successfully.")
}


# VERSION 5 TEST ----
## Couldn't figure it out so manually forced names per park

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(purrr)
})

parks <- c("EVER","GRCA","ISRO","OLYM")
root  <- "C:/Users/brobb/R Projects/CF-Dashboard/time-series/parks"

var_map <- c(
  "sum_p.mm"    = "Total Precipitation (mm)",
  "avg_t.C"     = "Average Temperature (°C)",
  "sum_rain.mm" = "Total Rainfall (mm)",
  "sum_snow.mm" = "Total Snowpack (mm)",
  "runoff.mm"   = "Runoff (mm)",
  "sum_d.mm"    = "Climatic Water Deficit (mm)"
)

# ---- Robust parser: grab the first two c(...) vectors after the first mention ----
parse_cf_map <- function(session_file) {
  if (!file.exists(session_file)) return(NULL)
  L <- readLines(session_file, warn = FALSE)
  
  # index of the first line that mentions the phrase
  ix <- which(grepl("Models used in Water Balance analysis:", L))
  if (!length(ix)) return(NULL)
  
  # concatenate from that first line onward (handles line wraps / repeats)
  tail_text <- paste(L[min(ix):length(L)], collapse = " ")
  
  # collect ALL c(...) blocks after the first mention; take the first two
  parts <- regmatches(tail_text, gregexpr('c\\([^\\)]*\\)', tail_text))[[1]]
  if (length(parts) < 2) return(NULL)
  
  extract_vec <- function(x) {
    s <- sub('^c\\(|\\)$', '', x)
    toks <- strsplit(s, ',\\s*')[[1]]
    toks <- gsub('^"|"$', '', toks)
    trimws(toks)
  }
  
  models_raw <- extract_vec(parts[1])
  labels_raw <- extract_vec(parts[2])
  
  # normalize keys
  models_key <- tolower(trimws(models_raw))
  names(labels_raw) <- models_key
  labels_raw
}

# ---- Park-specific overrides (from your messages) ----
override_maps_raw <- list(
  EVER = c(
    "bcc-csm1-1.rcp45"      = "Warm Dry",
    "GFDL-ESM2G.rcp85"      = "Hot Dry",
    "HadGEM2-CC365.rcp85"   = "Hot Wet",
    "MRI-CGCM3.rcp45"       = "Warm Wet"
  ),
  GRCA = c(
    "bcc-csm1-1-m.rcp45"    = "Warm Dry",
    "CanESM2.rcp85"         = "Hot Wet",
    "inmcm4.rcp45"          = "Warm Wet",
    "IPSL-CM5A-MR.rcp85"    = "Hot Dry"
  ),
  ISRO = c(
    "bcc-csm1-1-m.rcp45"    = "Warm Dry",
    "GFDL-ESM2G.rcp85"      = "Warm Wet",
    "HadGEM2-ES365.rcp85"   = "Hot Wet",
    "MIROC-ESM.rcp85"       = "Hot Dry"
  ),
  OLYM = c(
    "bcc-csm1-1.rcp85"      = "Hot Wet",
    "CNRM-CM5.rcp45"        = "Warm Dry",
    "HadGEM2-CC365.rcp85"   = "Hot Dry",
    "MIROC5.rcp45"          = "Warm Wet"
  )
)

# normalize override keys to lower
override_maps <- lapply(override_maps_raw, function(v) { names(v) <- tolower(names(v)); v })

unmatched_log <- list()
out_list <- list()

for (pk in parks) {
  csv_path     <- file.path(root, pk, pk, "WarmWet_HotDry", "tables", "WB-Annual.csv")
  session_path <- file.path(root, pk, pk, "sessionInfo.txt")
  if (!file.exists(csv_path)) { warning("Missing WB-Annual for ", pk); next }
  
  # parsed mapping + park override (override wins)
  parsed <- parse_cf_map(session_path)
  if (!is.null(parsed)) names(parsed) <- tolower(names(parsed))
  cf_map <- c(parsed, override_maps[[pk]])
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  need_cols <- c("year","GCM", names(var_map))
  miss <- setdiff(need_cols, names(df))
  if (length(miss)) { warning(pk, " missing cols: ", paste(miss, collapse=", ")); next }
  
  df_long <- df %>%
    select(all_of(need_cols)) %>%
    pivot_longer(all_of(names(var_map)), names_to = "Variable_raw", values_to = "Value") %>%
    mutate(
      Park    = pk,
      gcm_key = tolower(trimws(GCM)),
      `Climate Future` = case_when(
        gcm_key == "gridmet.historical" ~ "Historical",
        !is.null(cf_map) & gcm_key %in% names(cf_map) ~ cf_map[gcm_key],
        TRUE ~ GCM
      ),
      Year    = year,
      Variable = unname(var_map[Variable_raw])
    ) %>%
    select(Park, `Climate Future`, Year, Variable, Value, GCM, gcm_key)
  
  # log any still-unmatched (non-historical)
  unmatched <- df_long %>%
    filter(`Climate Future` == GCM & gcm_key != "gridmet.historical") %>%
    distinct(Park, GCM, gcm_key)
  if (nrow(unmatched)) unmatched_log[[pk]] <- unmatched
  
  # keep only Historical / Warm Wet / Hot Dry and add CF_order
  df_long <- df_long %>%
    mutate(
      `Climate Future` = case_when(
        `Climate Future` %in% c("Historical","historical") ~ "Historical",
        str_to_lower(`Climate Future`) == "warm wet" ~ "Warm Wet",
        str_to_lower(`Climate Future`) == "hot dry"  ~ "Hot Dry",
        TRUE ~ `Climate Future`
      )
    ) %>%
    filter(`Climate Future` %in% c("Historical","Warm Wet","Hot Dry")) %>%
    mutate(
      CF_order = case_when(
        `Climate Future` == "Historical" ~ 1L,
        `Climate Future` == "Warm Wet"   ~ 2L,
        `Climate Future` == "Hot Dry"    ~ 3L
      )
    ) %>%
    arrange(Park, CF_order, Year, Variable) %>%
    select(Park, `Climate Future`, CF_order, Year, Variable, Value)
  
  out_list[[pk]] <- df_long
}

out <- bind_rows(out_list)

# write for dashboard
out_file <- file.path(dirname(root), "annual_climate_data_long.csv")
write_csv(out, out_file)
message("Wrote: ", out_file, " rows=", nrow(out))

# any stragglers?
if (length(unmatched_log)) {
  message("\nStill-unmapped GCMs (should be none now):")
  print(bind_rows(unmatched_log, .id = "ParkGroup"))
} else {
  message("\nAll GCMs mapped to CF labels successfully.")
}

