library(mapview)

sentinel_img = rast("../../../../pramit/Pictures/S2A_MSIL1C_20211009T103941_N0301_R008_T32UMC_20211009T124809_resampled_ndvi.tif")
sentinel_ndvi = sentinel_img$S2A_1

sentinel_ndvi = mask(sentinel_ndvi, sentinel_img$S2A_2, maskvalues = 0, inverse = TRUE)

sentinel_ndvi[sentinel_ndvi <= 0] = NA
plot(sentinel_ndvi)

sentinel_fC = (1 - sentinel_ndvi)^0.625
plot(sentinel_fC)

low_lst = project(modis_day_lst, "epsg:32632")
low_lst = crop(low_lst, ext(sentinel_fC))
plot(low_lst)
low_fC = resample(sentinel_fC, low_lst)
plot(low_fC)

low_lst_vals = values(low_lst)
hist(low_lst_vals)
low_fC_vals = values(low_fC)
hist(low_fC_vals)
plot(low_lst_vals ~ low_fC_vals)

lst_fC_reg = lm(low_lst_vals ~ low_fC_vals)
summary(lst_fC_reg)

low_lst_hat = coef(lst_fC_reg)[1] + coef(lst_fC_reg)[2] * low_fC
low_lst_residuals = low_lst - low_lst_hat
plot(low_lst_residuals)

high_lst_hat = coef(lst_fC_reg)[1] + coef(lst_fC_reg)[2] * sentinel_fC
plot(high_lst_hat)

lst_residuals_resampled = resample(low_lst_residuals, high_lst_hat)
high_lst = high_lst_hat + lst_residuals_resampled
plot(high_lst)


