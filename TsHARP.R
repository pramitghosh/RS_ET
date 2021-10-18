sentinel_ndvi = rast("../../../../pramit/Pictures/S2A_MSIL1C_20211009T103941_N0301_R008_T32UMC_20211009T124809_resampled_ndvi.tif")
sentinel_ndvi = sentinel_ndvi$S2A_1

sentinel_ndvi[sentinel_ndvi < 0] = NA
plot(sentinel_ndvi)
