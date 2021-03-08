library(water)
library(readr)

# Data source:
# https://timeseries.fh-muenster.de:3000/d/lcdHXyKmk/climateatmuenster?orgId=18&from=1532642400000&to=1532728799000
# Lat: 51.968791 N
# Long: 7.59513 E
# Elevation: 60 m
# Location Easting: 403490.150	m
# Location Northing: 5758499.100 m
# Height: 2 m (assumed)
# Date: 27-July-2018

# Create AOI (625 sq.km. centered on Muenster FHZ weather station)
utm_x = 403490.150
utm_y = 5758499.100
aoi = createAoi(topleft = c(utm_x - 25000, utm_y + 25000), bottomright = c(utm_x + 25000, utm_y - 25000), EPSG = 32632)

# Read weather station data
rain = read_csv("data/weather_14-06-2017/Precipitation-data-2021-03-08 12_15_07.csv")
radiation = read_csv("data/weather_14-06-2017/Radiation-data-2021-03-08 12_17_32.csv")
humidity = read_csv("data/weather_14-06-2017/Relative humidity-data-2021-03-08 12_16_26.csv")
temperature = read_csv("data/weather_14-06-2017/Temperature-data-2021-03-08 12_15_39.csv")
wind = read_csv("data/weather_14-06-2017/Wind velocity-data-2021-03-08 12_16_05.csv")

# Prepare Weather Station data
weather_list = list(rain, radiation, humidity, temperature, wind)
weather = Reduce(function(x, y, ...) merge(x, y, ...), weather_list)

weather_time = format(weather$Time, format = "%H:%M:%S")
weather$Time = format(weather$Time, format = "%d/%m/%Y")
weather = cbind(weather_time, weather)
colnames(weather) = c("time", "date", "precipitation", "radiation", "rel_humidity", "temperature", "wind_vel")

# Plot weather data at satellite overpass
MTLfile = file("data/L8_C2/LC08_L1TP_197024_20170614_20200903_02_T1_MTL.txt")
WeatherStation = read.WSdata(WSdata = weather, date.format = "%d/%m/%Y", lat = 51.968791, long = 7.59513, elev = 60, height = 2, columns = c("date" = 2, "time" = 1, "radiation" = 4, "wind" = 7, "RH" = 5, "temp" = 6, "rain" = 3), MTL = MTLfile)
print(WeatherStation)
plot(WeatherStation, hourly = TRUE)

# Prepare satellite data
# 
# L7_B = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B1.TIF")
# L7_G = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B2.TIF")
# L7_R = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B3.TIF")
# L7_NIR = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B4.TIF")
# L7_SWIR1 = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B5.TIF")
# L7_Thermal = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B6_VCID_1.TIF")
# L7_SWIR2 = raster("data/C1/LE07_L1TP_197024_20180727_20180822_01_T1_B7.TIF")

# L7 = brick(L7_B, L7_G, L7_R, L7_NIR, L7_SWIR1, L7_Thermal, L7_SWIR2)

# Modified water::loadImageSR() with correct filename pattern
loadSR = function (path = getwd(), aoi) 
{
  files <- list.files(path = path, pattern = "_SR_B+[2-7].TIF$", 
                      full.names = T)
  stack1 <- list()
  for (i in 1:6) {
    stack1[i] <- raster(files[i])
  }
  image_SR <- do.call(stack, stack1)
  image_SR <- water:::aoiCrop(image_SR, aoi)
  image_SR <- image_SR/10000
  bandnames <- c("B", "G", "R", "NIR", 
                 "SWIR1", "SWIR2")
  image_SR <- water:::saveLoadClean(imagestack = image_SR, stack.names = bandnames, 
                            file = "image_SR", overwrite = TRUE)
  return(image_SR)
}

L8 = loadImage(path = "data/L8_C2/", sat = "L8", aoi = aoi)
L8.SR = loadSR(path = "data/L8_C2/SR/", aoi = aoi)
DEM = prepareSRTMdata(path = "data/SRTM_DEM/", extent = L8)

# Calculate solar angles
surface.model = METRICtopo(DEM)
solar.angles.r = solarAngles(surface.model = surface.model, WeatherStation = WeatherStation, MTL = MTLfile)
plot(solar.angles.r)

# Calculate incident SW radiation, TOA reflectance, Surface reflectance and albedo
Rs.inc = incSWradiation(surface.model = surface.model, solar.angles = solar.angles.r, WeatherStation = WeatherStation)
image.TOAr = calcTOAr(image.DN = L8, sat = "L8", MTL = MTLfile, incidence.rel = solar.angles.r$incidence.rel, aoi = aoi)
# image.SR = calcSR(image.TOAr = image.TOAr, sat = "L7", surface.model = surface.model, incidence.hor = solar.angles.r$incidence.hor, WeatherStation = WeatherStation)
albedo = albedo(image.SR = L8.SR, coeff = "Olmedo", sat = "L8")

# Calculate LAI
LAI = LAI(method = "metric2010", image = image.TOAr, L = 0.1)
plot(LAI)
 
# Calculate Surface temperature, Incident and Outgoing LW radiation
Ts = surfaceTemperature(image.DN = L8, LAI = LAI, sat = "L8", WeatherStation = WeatherStation, aoi = aoi, method = "SW")
Rl.out = outLWradiation(LAI = LAI, Ts = Ts)
Rl.inc = incLWradiation(WeatherStation = WeatherStation, DEM = surface.model$DEM, solar.angles = solar.angles.r, Ts = Ts)

# Calculate Net radiation
Rn = netRadiation(LAI, albedo, Rs.inc, Rl.inc, Rl.out)
plot(Rn)

# Calculate Soil Heat flux
G = soilHeatFlux(image = L8.SR, Ts = Ts, albedo = albedo, Rn = Rn, LAI = LAI)
plot(G)

# Calculate Sensible Heat flux
Z.om = momentumRoughnessLength(LAI = LAI, mountainous = FALSE, method = "short.crops", surface.model = surface.model)
hot.and.cold = calculate_anchors(image = image.TOAr, Ts, LAI, Rn = Rn, G = G, plots = TRUE, albedo = albedo, Z.om = Z.om, n = 1, anchors.method = "flexible", WeatherStation = WeatherStation, verbose = TRUE)
H = calcH(anchors = hot.and.cold, mountainous = FALSE, Ts = Ts, Z.om = Z.om, WeatherStation = WeatherStation, ETp.coef = 1.05, Z.om.ws = 0.03, DEM = DEM, Rn = Rn, G = G, verbose = TRUE)

# Calculate 24h evapotranspiration
ET_WS = dailyET(WeatherStation = WeatherStation, lat = 51.968791, long = 7.59513, elev = 60, height = 2)
ET.24 = ET24h(Rn, G, H$H, Ts, WeatherStation = WeatherStation, ETr.daily = ET_WS)

# 
# # # For simple method
# # Energy.Balance = METRIC.EB(image.DN = L7, plain = TRUE, WeatherStation = WeatherStation, ETp.coef = 1.2, MTL = MTLfile, n = 5, sat = "L7", thermalband = L7$Thermal1, aoi = aoi)
# # plot(Energy.Balance$EB)
# # 
# # ET_WS = dailyET(WeatherStation = WeatherStation, lat = 51.968791, long = 7.59513, elev = 60, height = 2)
# # ET.24s = ET24h(Rn = Energy.Balance$EB$NetRadiation, G = Energy.Balance$EB$SoilHeat, H = Energy.Balance$EB$SensibleHeat, Ts = Energy.Balance$EB$surfaceTemperature, WeatherStation = WeatherStation, ETr.daily = ET_WS)

# Energy.Balance = METRIC.EB(image.DN = L8, image.SR = L8.SR, WeatherStation = WeatherStation, MTL = MTLfile, sat = "L8", DEM = DEM, aoi = aoi, alb.coeff = "Olmedo", LST.method = "SW", LAI.method = "metric2010", n = 5, ETp.coef = 1.2, Z.om.ws = 0.03, verbose = TRUE)
