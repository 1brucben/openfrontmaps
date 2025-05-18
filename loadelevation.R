pkgs <- c("terra", "sf", "elevatr", "png")

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p,dependencies=TRUE,type="binary")
  }
}

invisible(lapply(pkgs, install_if_missing))
invisible(lapply(pkgs, library, character.only = TRUE))
start_time <- proc.time()
coord_str <- "-91.7,-56.6,-31.1,24.1"
coords <- as.numeric(strsplit(coord_str, ",")[[1]])

xmin <- coords[1]
ymin <- coords[2]
xmax <- coords[3]
ymax <- coords[4]

total_pixels <- 4000000 # e.g. 4 million
zoom <- 6 # 7 should be fine for most regions... raise if region is really small
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
#switch comments to load instead from file
#elev_terra <- rast("elevation_raw.tif")

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

# Define the projection string for Plate Carrée (Equirectangular) projection
plate_carre_proj <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Project the elevation raster to Plate Carrée
elev_projected <- project(elev_terra, plate_carre_proj)

# Now slope calculation will be accurate
slope_rast <- terrain(elev_projected, v = "slope", unit = "degrees")


# start with terrain based on elevation
terrain_class <- classify(elev_resampled, matrix(c(
  -Inf, 180, 1, # Plains
  180, 2600, 2, # Highlands
  2600, Inf, 3 # Mountains
), ncol = 3, byrow = TRUE))
# Count initial classification from elevation
tc_vals_initial <- values(terrain_class)
total_land <- sum(!is.na(tc_vals_initial))

initial_counts <- table(tc_vals_initial, useNA = "ifany")
initial_percents <- round(100 * initial_counts / total_land, 2)

cat("Initial classification (elevation-based):\n")
print(data.frame(Class = names(initial_counts),
                 Count = as.vector(initial_counts),
                 Percent = as.vector(initial_percents)))


# Project slope back to match terrain_class grid
slope_back <- project(slope_rast, terrain_class)
# Now safe to use in logical assignment
# Track reclassifications
reclass_counts <- c(rule1 = 0, rule2 = 0, rule3 = 0, rule4 = 0)

# Rule 1
idx <- which(slope_back[] < .5 & elev_resampled[] < 1000)
reclass_counts["rule1"] <- sum(terrain_class[][idx] != 1, na.rm = TRUE)
terrain_class[idx] <- 1

# Rule 2
idx <- which(slope_back[] < 2 & elev_resampled[] > 2400)
reclass_counts["rule2"] <- sum(terrain_class[][idx] != 2, na.rm = TRUE)
terrain_class[idx] <- 2

# Rule 3
idx <- which(slope_back[] > 4)
reclass_counts["rule3"] <- sum(terrain_class[][idx] != 3, na.rm = TRUE)
terrain_class[idx] <- 3

# Rule 4
idx <- which(slope_back[] > 1.7 & elev_resampled[] < 200)
reclass_counts["rule4"] <- sum(terrain_class[][idx] != 2, na.rm = TRUE)
terrain_class[idx] <- 2

# Total % of land tiles affected by each slope rule
reclass_percents <- round(100 * reclass_counts / total_land, 2)

cat("Reclassifications based on slope rules:\n")
print(data.frame(Rule = names(reclass_counts),
                 ChangedTiles = as.vector(reclass_counts),
                 PercentOfTotal = as.vector(reclass_percents)))



# Set water where elevation ≤ 0
#terrain_class[elev_resampled <= 137] <- 0

# Get raw elevation values
elev_vals <- values(elev_resampled)
tc_vals <- values(terrain_class)

# Initialize magnitude and blue
mag <- numeric(ncell(terrain_class))
blue <- numeric(ncell(terrain_class))
alpha <- numeric(ncell(terrain_class))

for (i in seq_along(tc_vals)) {
  t <- tc_vals[i]
  elev <- elev_vals[i]

  if (is.na(t) || is.na(elev)) {
    mag[i] <- 0
    blue[i] <- 0
    alpha[i] <- 0
    next
  }

  if (t == 0) {
    # Water
    blue[i] <- 106
    alpha[i] <- 0
    next
  }

  alpha[i] <- 255

  if (t == 1) {
    # Plains: 0–200 m → mag 1–9
    mag[i] <- 1 + 7 * min(1, elev / 200)
  } else if (t == 2) {
    # Highlands: 200–1000 m → mag 11–19
    mag[i] <- 11 + 7 * min(1, (elev - 200) / 800)
  } else if (t == 3) {
    # Mountains: 1000–3000 m → mag 21–29
    mag[i] <- 21 + 7 * min(1, (elev - 1000) / 2000)
  }

  blue[i] <- 140 + 2 * mag[i]
  #blue[i] <- max(0, blue[i])

}

r <- rep(0, length(blue))
g <- rep(0, length(blue))

r_rast <- setValues(rast(terrain_class), r)
g_rast <- setValues(rast(terrain_class), g)
b_rast <- setValues(rast(terrain_class), blue)
a_rast <- setValues(rast(terrain_class), alpha)

rgba_stack <- rast(list(r_rast, g_rast, b_rast, a_rast))
names(rgba_stack) <- c("red", "green", "blue", "alpha")

blue_base <- blue
alpha_base <- alpha

source("loadrivers.R")
source("generatepreview.R")
end_time <- proc.time()
elapsed <- end_time - start_time
cat("Elapsed time (seconds):", elapsed["elapsed"], "\n")