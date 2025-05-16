

library(sf)
library(terra)


rivers <- st_read("./ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
rivers_proj <- st_transform(rivers, crs(terrain_class))

# 2. Filter to line geometries only
rivers_proj <- rivers_proj[st_geometry_type(rivers_proj) %in% c("LINESTRING", "MULTILINESTRING"), ]
# 3. Buffer very lightly
rivers_buffered <- st_buffer(rivers_proj, dist = .0001)  # tweak this value if needed

# 4. Rasterize buffered result
river_rast <- rasterize(vect(rivers_buffered), terrain_class, field = 1, background = 0, touches = TRUE)

# 5. Create river mask
river_mask <- river_rast == 1

blue <- blue_base
alpha <- alpha_base

blue[river_mask[]] <- 106
alpha[river_mask[]] <- 0

r_rast <- setValues(rast(terrain_class), 0)
g_rast <- setValues(rast(terrain_class), 0)
b_rast <- setValues(rast(terrain_class), blue)
a_rast <- setValues(rast(terrain_class), alpha)

rgba_stack <- rast(list(r_rast, g_rast, b_rast, a_rast))
names(rgba_stack) <- c("red", "green", "blue", "alpha")

writeRaster(rgba_stack, "terrain_map.png", datatype = "INT1U", overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))

