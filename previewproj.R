library(terra)

# ---- PARAMETERS ----
display_crs <- "+proj=moll +datum=WGS84 +units=m +no_defs"

preview_file <- sub("\\.png$", "_preview_moll.png", output_file)

# ---- Extract blue and alpha from rgba_stack ----
blue_vals <- values(rgba_stack$blue)
alpha_vals <- values(rgba_stack$alpha)

# ---- Recolor based on blue+alpha logic ----
n <- length(blue_vals)
r <- numeric(n)
g <- numeric(n)
b <- numeric(n)
a <- numeric(n)

for (i in seq_len(n)) {
  blue <- blue_vals[i]
  alpha <- alpha_vals[i]

  if (is.na(alpha) || is.na(blue)) {
    r[i] <- g[i] <- b[i] <- a[i] <- 0
    next
  }

  if (alpha < 20 || blue == 106) {
    # Water 
    r[i] <- 100
    g[i] <- 143
    b[i] <- 255
    a[i] <- 255 
  } else {
    # Land: decode terrain magnitude from blue
    mag_raw <- pmin(200, pmax(140, blue))
    mag <- (mag_raw - 140) / 2

    if (mag < 10) {
      r_col <- 190
      g_col <- 220 - 2 * mag
      b_col <- 138
    } else if (mag < 20) {
      adj <- 2 * mag
      r_col <- 200 + adj
      g_col <- 183 + adj
      b_col <- 138 + adj
    } else {
      adj <- floor(230 + mag / 2)
      r_col <- adj
      g_col <- adj
      b_col <- adj
    }

    r[i] <- r_col
    g[i] <- g_col
    b[i] <- b_col
    a[i] <- 255  # opaque land
  }
}

# ---- Build new RGBA raster with same geometry ----
base <- rast(rgba_stack[[1]])  # template layer
r_rast <- setValues(base, r)
g_rast <- setValues(base, g)
b_rast <- setValues(base, b)
a_rast <- setValues(base, a)

rgba_colored <- rast(list(r_rast, g_rast, b_rast, a_rast))
names(rgba_colored) <- c("red", "green", "blue", "alpha")

# ---- Project to Mollweide projection ----
rgba_projected <- project(rgba_colored, display_crs, method = "bilinear")

# ---- Clamp and round values to 0–255 ----
rgba_projected[] <- round(pmin(255, pmax(0, rgba_projected[])))

# ---- Write projected PNG ----
writeRaster(
  rgba_projected,
  filename = preview_file,
  datatype = "INT1U",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE")
)
