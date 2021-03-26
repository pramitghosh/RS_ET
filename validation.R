leo_coords = c(403873.8,	5759162)
leo_extents = c(leo_coords, leo_coords)
leo_extents = leo_extents[order(c(1,3,2,4))]

e = extent(leo_extents)
leo_val = values(crop(ET.24, e))

# Measured value on ground = 4.46 mm/day