## ── 0. shorthand handles ────────────────────────────────────────────────
blue  <- rgba_stack$blue          # 0–255
alpha <- rgba_stack$alpha

## ── 1. masks ------------------------------------------------------------
water  <- (alpha < 20) | (blue == 106)            # TRUE / FALSE raster
mag    <- (clamp(blue, 140, 200) - 140) / 2       # 0-30   ──> numeric raster

## ── 2. R, G, B rasters (0-255)  — one pass each via nested ifel() -------
r_rast <- ifel(water, 100,
           ifel(mag < 10,                    190,
           ifel(mag < 20,               200 + 2*mag,
                                  floor(230 + mag/2))))

g_rast <- ifel(water, 143,
           ifel(mag < 10,           220 - 2*mag,
           ifel(mag < 20,           183 + 2*mag,
                                  floor(230 + mag/2))))

b_rast <- ifel(water, 255,
           ifel(mag < 10,                    138,
           ifel(mag < 20,               138 + 2*mag,
                                  floor(230 + mag/2))))

## ── 3. stack & scale to 0-1 for PNG -------------------------------------
rgb_stack <- c(r_rast/255, g_rast/255, b_rast/255)

## ── 4. convert once to array (rows × cols × 3) and save -----------------
preview_array <- as.array(rgb_stack)              # fast C++ copy

preview_file  <- sub("\\.png$", "_preview.png", output_file)
png::writePNG(preview_array, target = preview_file)
