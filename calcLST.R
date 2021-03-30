library(jsonlite)

calcLST = function(L8.ST)
{
  L2_MTL_JSON_path = file("data/L8_C2/SR/LC08_L2SP_197024_20170614_20200903_02_T1_MTL.json")
  L2_MTL = fromJSON(L2_MTL_JSON_path)
  
  st_params = t(as.data.frame(L2_MTL$LANDSAT_METADATA_FILE$LEVEL2_SURFACE_TEMPERATURE_PARAMETERS))
  st_mult = as.numeric(st_params[5])
  st_add = as.numeric(st_params[6])
  
  LST = st_add + (L8.ST * st_mult)
  return(LST)
}
