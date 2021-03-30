library(sf)

leo_coords = c(403873.8,	5759162)

val_at_coords = function(coord_pair, image)
{
  coord_extent = c(coord_pair, coord_pair)
  coord_extent = coord_extent[order(c(1,3,2,4))]
  buffer = c(-1, 1, -1, 1)
  coord_extent = coord_extent + buffer
  e = extent(coord_extent)
  
  values(crop(image, e))
}

# leo_extents = c(leo_coords, leo_coords)
# leo_extents = leo_extents[order(c(1,3,2,4))]
# e = extent(leo_extents)

leo_val = val_at_coords(image = ET.24, coord_pair = leo_coords)
# Measured value on ground = 4.46 mm/day

vpts = read_sf("results/pts.gpkg")
pts_geom = st_geometry(vpts)
ET_pts = lapply(X = pts_geom, FUN = function(x, img) val_at_coords(as.numeric(x), img), ET.24)
val_results = as.data.frame(cbind(vpts$LULC, as.numeric(unlist(ET_pts))))
val_results$V2 = as.numeric(val_results$V2)


