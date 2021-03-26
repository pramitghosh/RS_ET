leo_coords = c(403873.8,	5759162)

val_at_coords = function(image, coord_pair)
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

leo_val = val_at_coords(ET.24, leo_coords)

# Measured value on ground = 4.46 mm/day