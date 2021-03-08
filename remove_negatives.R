set_to_NA = function(rasterlayer)
{
  rasterlayer[rasterlayer < 0] = NA
  return(rasterlayer)
}

remove_negatives = function(brick)
{
  bands = lapply(bands, set_to_NA)
  brick(bands)
}
