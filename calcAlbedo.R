library(jsonlite)

albedo_coeffs_grass_mult = c(2.153642, -2.242688, -0.520669,  0.622670,  0.129979, -0.047970,  0.152228)
albedo_coeffs_grass_add = 0.058674

L8_SR = function(L8.SR)
{
  L2_MTL_JSON_path = file("data/L8_C2/SR/LC08_L2SP_197024_20170614_20200903_02_T1_MTL.json")
  L2_MTL = fromJSON(L2_MTL_JSON_path)
  
  sr_params = t(as.data.frame(L2_MTL$LANDSAT_METADATA_FILE$LEVEL2_SURFACE_REFLECTANCE_PARAMETERS))
  
  sr_coeffs_mult = as.numeric(sr_params[29:35, ])
  sr_coeffs_add = as.numeric(sr_params[36:42, ])
  
  sr_scaled = L8.SR
  for(i in 1:7)
    sr_scaled[[i]] = (sr_scaled[[i]] * sr_coeffs_mult[i]) + sr_coeffs_add[i]
  
  sr_scaled
}

albedo.daSilva = function(sr_scaled)
{
  albedo_coeffs_daSilva = c(0.300, 0.277, 0.233, 0.143, 0.036, 0.012)
  
  albedo_daSilva = sr_scaled[[2]] * albedo_coeffs_daSilva[1]
  for(i in 3:7)
    albedo_daSilva = albedo_daSilva + (sr_scaled[[i]] * albedo_coeffs_daSilva[i-1])
  
  albedo_daSilva
}  
