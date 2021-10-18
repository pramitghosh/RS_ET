library(terra)
library(luna)
library(remotes)
remotes::install_github("rspatial/luna")
library(luna)


data_dir = "data/modis"
modis_refl = file.path(data_dir, "MOD09GA.A2021282.h18v03.061.2021284032618.hdf")

modis_qa = get_subdatasets(modis_refl)[2]
gdal_translate(modis_qa, "data/modis/modis_qa.tif")
modis_qa = rast("data/modis/modis_qa.tif")

r = rast(modis_refl)

from <- c(1,3,11,14,4)
to   <- c(2,3,11,14,6)
reject <- c("01,10", "1", "1", "1","000,101,110,111")
qa_bits <- cbind(from, to, reject)
qa_bits

quality_mask = modis_mask(modis_qa, 16, qa_bits)
plot(quality_mask)

quality_mask_resampled = resample(quality_mask, r[[2]])

rmask = mask(r, quality_mask_resampled, inverse = TRUE)
plotRGB(rmask, r = 3, g = 2, b = 5, stretch = "hist")

rmask = clamp(rmask, 0, 1)
modis_ndvi = (rmask[[3]] - rmask[[2]])/(rmask[[3]] + rmask[[2]])
plot(modis_ndvi)

modis_lst = rast("data/modis/MOD11A1.A2021282.h18v03.006.2021283094336.hdf")
modis_day_lst = modis_lst[[1]]
plot(modis_day_lst)

modis_fc = (1 - modis_ndvi)^0.625

