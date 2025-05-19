# Load and project rivers (lines + other geom) to elevation CRS
riversmore <- st_read("./ne_10m_rivers_lake_centerlines_scale_rank/ne_10m_rivers_lake_centerlines_scale_rank.shp")
#Eliminate smaller rivers
#riversmore <- riversmore[as.numeric(riversmore$scalerank) < 6, ]
rivers_proj <- st_transform(riversmore, crs(elev_projected))
# Load and project lakes polygons
lakes <- st_read("./ne_10m_lakes/ne_10m_lakes.shp")
lakes_proj <- st_transform(lakes, crs(elev_projected))

# Load and project ocean polygons
oceans <- st_read("./ne_10m_ocean/ne_10m_ocean.shp")
oceans_proj <- st_transform(oceans, crs(elev_projected))
# Load and project glaciated areas
glaciers <- st_read("./ne_10m_glaciated_areas/ne_10m_glaciated_areas.shp")
glaciers_proj <- st_transform(glaciers, crs(elev_projected))

# Define terrain extent polygon for clipping
terrain_extent <- as.polygons(ext(elev_projected), crs = crs(elev_projected))

# Clip all water-related features to terrain extent
rivers_clipped <- st_intersection(rivers_proj, st_as_sf(terrain_extent))
lakes_clipped <- st_intersection(lakes_proj, st_as_sf(terrain_extent))
oceans_clipped <- st_intersection(oceans_proj, st_as_sf(terrain_extent))

# Clip glaciers to terrain extent
glaciers_clipped <- st_intersection(glaciers_proj, st_as_sf(terrain_extent))

# Separate rivers lines from other geometries
geom_type <- st_geometry_type(rivers_clipped)
lines_only <- rivers_clipped[geom_type %in% c("LINESTRING", "MULTILINESTRING"), ]
others <- rivers_clipped[geom_type %in% c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT"), ]

# Compute buffer distances based on stroke weight
strokeweig <- as.numeric(lines_only$strokeweig)
base_res <- mean(res(elev_projected))
buffer_distances <- base_res * (strokeweig / max(strokeweig, na.rm = TRUE)) * 1

# Buffer only line geometries (rivers)
lines_buffered <- mapply(
  function(geom, dist) st_buffer(geom, dist = dist, endCapStyle = "FLAT"),
  st_geometry(lines_only),
  buffer_distances,
  SIMPLIFY = FALSE
)
lines_buffered <- st_sf(geometry = st_sfc(lines_buffered, crs = crs(lines_only)))

# Combine buffered rivers lines + unmodified other river geometries
rivers_combined <- rbind(lines_buffered, others)

# Extract only geometry from each sf object
lakes_geom <- st_geometry(lakes_clipped)
oceans_geom <- st_geometry(oceans_clipped)
river_polygons_geom <- st_geometry(rivers_combined)[st_geometry_type(rivers_combined) %in% c("POLYGON", "MULTIPOLYGON")]

# Combine geometries into a single sfc geometry list
all_water_geom <- c(lakes_geom, oceans_geom, river_polygons_geom)

# Wrap combined geometries into a single sf object without attributes
water_polygons <- st_sf(geometry = all_water_geom, crs = st_crs(lakes_clipped))

# Combine buffered river lines with all water polygons
water_and_rivers <- rbind(water_polygons, lines_buffered)

# Reproject combined vector to terrain_class CRS (likely lat/lon)
water_and_rivers_ll <- st_transform(water_and_rivers, crs(terrain_class))

# Convert to terra vector and rasterize
water_vect <- vect(water_and_rivers_ll)
river_rast <- rasterize(water_vect, terrain_class, field = 1, background = 0, touches = TRUE)
river_mask <- river_rast == 1

# Paint water (rivers/lakes/ocean) on blue channel with transparency
blue <- blue_base
alpha <- alpha_base
blue[river_mask[]] <- 106
alpha[river_mask[]] <- 0  

# Add glaciers as mountains (e.g. darker blue and opaque)
glacier_vect <- vect(st_transform(glaciers_clipped, crs(terrain_class)))
glacier_rast <- rasterize(glacier_vect, terrain_class, field = 1, background = 0, touches = TRUE)
glacier_mask <- glacier_rast == 1

blue[glacier_mask[]] <- 198
alpha[glacier_mask[]] <- 255


# ── RGBA stack ──────────────────────────────────────────────────────────────
r_rast <- setValues(rast(terrain_class), 0)   # red   – all zero
g_rast <- r_rast                              # green – all zero
b_rast <- blue                                # blue  – the raster we computed
a_rast <- alpha                               # alpha – 0 or 255

rgba_stack <- c(r_rast, g_rast, b_rast, a_rast)
names(rgba_stack) <- c("red", "green", "blue", "alpha")

# Export final terrain + water PNG with compression
writeRaster(rgba_stack, output_file, datatype = "INT1U", overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))
