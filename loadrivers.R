# Read rivers shapefile and project to match elevation raster CRS (in meters)
rivers <- st_read("./ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
rivers_proj <- st_transform(rivers, crs(elev_projected))

# Clip all rivers to the projected terrain extent
terrain_extent <- as.polygons(ext(elev_projected), crs = crs(elev_projected))
rivers_clipped <- st_intersection(rivers_proj, st_as_sf(terrain_extent))

# Separate by geometry type
geom_type <- st_geometry_type(rivers_clipped)
lines_only <- rivers_clipped[geom_type %in% c("LINESTRING", "MULTILINESTRING"), ]
others <- rivers_clipped[geom_type %in% c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT"), ]

# Compute buffer distances based on river importance (scalerank)
scalerank <- as.numeric(lines_only$scalerank)
is_major <- !is.na(scalerank) & scalerank <= 4
base_res <- mean(res(elev_projected))
buffer_distances <- ifelse(is_major, base_res * 0.5, base_res * 0.01)

# Buffer each line with its assigned width
lines_buffered <- mapply(
  function(geom, dist) st_buffer(geom, dist = dist, endCapStyle = "FLAT"),
  st_geometry(lines_only),
  buffer_distances,
  SIMPLIFY = FALSE
)

# Rewrap into sf with original attributes (may trigger harmless warning)
lines_buffered <- st_sf(geometry = st_sfc(lines_buffered, crs = crs(lines_only)))

# Combine buffered lines + unmodified polygons/points
rivers_combined <- rbind(lines_buffered, others)

# Reproject to lat/lon for rasterization
rivers_ll <- st_transform(rivers_combined, crs(terrain_class))
rivers_vect <- vect(rivers_ll)

# Rasterize to match terrain_class resolution
river_rast <- rasterize(rivers_vect, terrain_class, field = 1, background = 0, touches = TRUE)
river_mask <- river_rast == 1

# Paint river areas into blue channel and set transparency
blue <- blue_base
alpha <- alpha_base
blue[river_mask[]] <- 106
alpha[river_mask[]] <- 0 # transparent rivers (use 255 if you want them opaque)

# Assemble RGBA image stack
r_rast <- setValues(rast(terrain_class), 0)
g_rast <- setValues(rast(terrain_class), 0)
b_rast <- setValues(rast(terrain_class), blue)
a_rast <- setValues(rast(terrain_class), alpha)
rgba_stack <- rast(list(r_rast, g_rast, b_rast, a_rast))
names(rgba_stack) <- c("red", "green", "blue", "alpha")

# Export to PNG with compression
writeRaster(rgba_stack, output_file, datatype = "INT1U", overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))
