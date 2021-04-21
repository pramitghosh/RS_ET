# devtools::install_github("16EAGLE/getSpatialData")
library("getSpatialData")

set_archive(dir_archive = "data/scratch/", create = FALSE)

set_aoi(aoi)
view_aoi()

login_USGS(username = "pramitghsh")
services()
get_products()

records = get_records(time_range = c("2017-10-01", "2017-12-31"),
                       products = c("LANDSAT_8_C1"))
l1 = records[records$level == "l1" & records$cloudcov_land < 60,]
bt = records[records$level == "bt" & records$cloudcov_land < 60,]
sr = records[records$level == "sr" & records$cloudcov_land < 60,]
# view_records(records)
# plot_records(records)

download_img = function(records)
{
  records = calc_cloudcov(records) 
  # records = select_bitemporal(records)
  records <- check_availability(records)
  records <- order_data(records, wait_to_complete = TRUE)
  records <- get_data(records)
  records
}

download_img(l1[3,])
download_img(bt)
download_img(sr)

