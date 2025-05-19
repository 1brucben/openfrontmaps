pkgs <- c("terra", "sf", "elevatr", "png")

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE, type = "binary")
  }
}

invisible(lapply(pkgs, install_if_missing))
invisible(lapply(pkgs, library, character.only = TRUE))
start_time <- proc.time()
coord_str <- "5.5,44.0,15.0,48"
coords <- as.numeric(strsplit(coord_str, ",")[[1]])

xmin <- coords[1]
ymin <- coords[2]
xmax <- coords[3]
ymax <- coords[4]

total_pixels <- 4000000 # e.g. 4 million
zoom <- 8 # 7 should be fine for most regions... raise if region is really small
output_file <- "terrain_map.png"

# Define bounding box
bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = st_crs(4326))

# Convert to polygon
region_geom <- st_as_sfc(bbox)

# Wrap in an sf data frame
region_sf <- st_sf(geometry = region_geom)



elev_raster <- get_elev_raster(locations = region_sf, z = zoom, clip = "bbox")

writeRaster(rast(elev_raster), "elevation_raw.tif", overwrite = TRUE)
elev_terra <- rast(elev_raster)
# switch comments to load instead from file
# elev_terra <- rast("elevation_raw.tif")

# Mean latitude (for scaling longitude degrees)
mean_lat <- (ymin + ymax) / 2
lat_km <- 111 # 1 degree latitude ~111 km
lon_km <- 111 * cos(mean_lat * pi / 180) # scaled for latitude

# Real-world width and height in km
width_km <- (xmax - xmin) * lon_km
height_km <- (ymax - ymin) * lat_km

# Aspect ratio in km (height / width)
aspect_ratio <- height_km / width_km

# Solve for width and height in pixels
pixel_width <- sqrt(total_pixels / aspect_ratio)
pixel_height <- pixel_width * aspect_ratio

# Round
pixel_width <- round(pixel_width)
pixel_height <- round(pixel_height)

cat("Computed size:", pixel_width, "x", pixel_height, "\n")


# Create resampling target
target_rast <- rast(nrows = pixel_height, ncols = pixel_width, extent = ext(elev_terra), crs = crs(elev_terra))

# Resample
elev_resampled <- resample(elev_terra, target_rast, method = "bilinear")

# slope calc
slope_rast <- terrain(elev_resampled, v = "slope", unit = "degrees")


## --- starting point ----------------------------------------------------
terrain_initial <- classify(
  elev_resampled,
  rbind(
    c(-Inf, 120, 1),
    c(120, 1900, 2),
    c(1900, Inf, 3)
  )
)

init_freq <- as.data.frame(freq(terrain_initial, digits = 0), useNA = "no")
total_land <- sum(init_freq$count) # all non-NA cells
init_freq$percent <- round(100 * init_freq$count / total_land, 2)

cat("Initial elevation-based classes:\n")
print(init_freq)

ti <- terrain_initial

rule1 <- slope_rast < .55 & elev_resampled < 1000
rule2 <- !rule1 & (slope_rast < 4 & elev_resampled > 2000)
rule3 <- !rule1 & !rule2 & (slope_rast > 12)
rule4 <- !rule1 & !rule2 & !rule3 &
  (slope_rast > 1.3 & elev_resampled < 200)

chg1 <- rule1 & ti != 1
chg2 <- rule2 & ti != 2
chg3 <- rule3 & ti != 3
chg4 <- rule4 & ti != 2


reclass_counts <- sapply(
  list(chg1, chg2, chg3, chg4),
  function(m) global(m, "sum", na.rm = TRUE)[[1]]
)
names(reclass_counts) <- paste0("rule", 1:4)

reclass_perc <- round(100 * reclass_counts /
  global(!is.na(ti), "sum", na.rm = TRUE)[[1]], 2)

cat("Tiles that *changed* because of each slope rule:\n")
print(data.frame(
  Rule           = names(reclass_counts),
  ChangedTiles   = reclass_counts,
  PercentOfTotal = reclass_perc
))


## --- combined reclassification (fast, one pass) ------------------------
terrain_class <- ifel(
  rule1, 1,
  ifel(
    rule2, 2,
    ifel(
      rule3, 3,
      ifel(
        rule4, 2,
        terrain_initial
      )
    )
  )
)


cat("Reclassifications based on slope rules:\n")
print(data.frame(
  Rule           = names(reclass_counts),
  ChangedTiles   = as.vector(reclass_counts),
  PercentOfTotal = as.vector(reclass_percents)
))

# Set water where elevation ≤ 0
# terrain_class[elev_resampled <= 137] <- 0

# Get raw elevation values
elev_vals <- values(elev_resampled)
tc_vals <- values(terrain_class)

# Initialize magnitude and blue
mag <- numeric(ncell(terrain_class))
blue <- numeric(ncell(terrain_class))
alpha <- numeric(ncell(terrain_class))

# helper to clamp a raster at an upper bound (a tad faster than pmin)
clamp_upper <- function(x, upper) terra::clamp(x, lower = -Inf, upper = upper)

# start with an empty (all-NA) raster that shares the grid
mag <- setValues(elev_resampled, NA_real_)

# plains
mag <- ifel(
  terrain_class == 1,
  1 + 7 * clamp_upper(elev_resampled / 120, 1),
  mag
)

# highlands
mag <- ifel(
  terrain_class == 2,
  11 + 7 * clamp_upper((elev_resampled - 120) / 880, 1),
  mag
)

# mountains
mag <- ifel(
  terrain_class == 3,
  21 + 7 * clamp_upper((elev_resampled - 1000) / 2000, 1),
  mag
)

blue <- 140 + 2 * mag
alpha <- ifel(is.na(terrain_class), 0, 255)

rgb_stack <- c(
  setValues(terrain_class, 0), # red   (all zero)
  setValues(terrain_class, 0), # green (all zero)
  blue,
  alpha
)
names(rgb_stack) <- c("red", "green", "blue", "alpha")



blue_base <- blue
alpha_base <- alpha

source("loadrivers.R")
source("generatepreview.R")
end_time <- proc.time()
elapsed <- end_time - start_time
cat("Elapsed time (seconds):", elapsed["elapsed"], "\n")
