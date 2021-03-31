devtools::install_github("16EAGLE/getSpatialData")
library("getSpatialData")

set_archive(dir_archive = "data/scratch")

set_aoi(aoi)
view_aoi()

login_USGS(username = "pramitghsh")
services()
get_products()

records = get_records(time_range = c("2017-04-01", "2017-05-31"),
                       products = c("LANDSAT_8_C1"))
records = records[records$level == "l1",]
view_records(records)
plot_records(records)

records = calc_cloudcov(records) 
# records = select_bitemporal(records)
records <- check_availability(records)
records <- order_data(records)
records <- get_data(records)

