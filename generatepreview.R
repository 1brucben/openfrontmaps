# Extract channels (0-255)
r_chan <- values(rgba_stack$red)
g_chan <- values(rgba_stack$green)
b_chan <- values(rgba_stack$blue)
a_chan <- values(rgba_stack$alpha)

n <- length(r_chan)

# Initialize RGB preview vectors (0-1 scale)
preview_r <- numeric(n)
preview_g <- numeric(n)
preview_b <- numeric(n)

for (i in seq_len(n)) {
  alpha <- a_chan[i]
  blue <- b_chan[i]
  
  if (is.na(alpha) || is.na(blue)) {
    preview_r[i] <- 0
    preview_g[i] <- 0
    preview_b[i] <- 0
    next
  }
  
  if (alpha < 20 || blue == 106) {
    # Water color (shoreline and ocean)
    preview_r[i] <- 100 / 255
    preview_g[i] <- 143 / 255
    preview_b[i] <- 255 / 255
  } else {
    # Land: decode magnitude and color
    mag_raw <- pmin(200, pmax(140, blue))
    mag <- (mag_raw - 140) / 2
    
    # Apply OpenFront color logic from TS code:
    if (mag < 10) {  # Plains
      r_col <- 190
      g_col <- 220 - 2 * mag
      b_col <- 138
    } else if (mag < 20) {  # Highlands
      adj <- 2 * mag
      r_col <- 200 + adj
      g_col <- 183 + adj
      b_col <- 138 + adj
    } else {  # Mountains
      adj <- floor(230 + mag / 2)
      r_col <- adj
      g_col <- adj
      b_col <- adj
    }
    
    preview_r[i] <- r_col / 255
    preview_g[i] <- g_col / 255
    preview_b[i] <- b_col / 255
  }
}

dims <- dim(rgba_stack)[1:2]

# Reshape to raster dimensions (rows = height, cols = width)
preview_r_mat <- matrix(preview_r, nrow = dims[1], ncol = dims[2], byrow = TRUE)
preview_g_mat <- matrix(preview_g, nrow = dims[1], ncol = dims[2], byrow = TRUE)
preview_b_mat <- matrix(preview_b, nrow = dims[1], ncol = dims[2], byrow = TRUE)

# Combine into array for PNG: height x width x 3 channels
preview_array <- array(0, dim = c(dims[1], dims[2], 3))
preview_array[,,1] <- preview_r_mat
preview_array[,,2] <- preview_g_mat
preview_array[,,3] <- preview_b_mat

# Write PNG preview
preview_file <- sub("\\.png$", "_preview.png", output_file)
writePNG(preview_array, target = preview_file)

