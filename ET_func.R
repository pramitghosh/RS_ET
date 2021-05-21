library(water)
library(readr)

source("remove_negatives.R")
source("loadSR.R")
source("calcAlbedo.R")
source("calcLST.R")
source("calcAnchors.R")


ET_wrap = function(utm_x = 403490.150, utm_y = 5758499.100, dist = 25000, epsg = 32632,
                   rain = "data/weather_14-06-2017/Precipitation-data-2021-03-08 12_15_07.csv",
                   radiation = "data/weather_14-06-2017/Radiation-data-2021-03-08 12_17_32.csv",
                   humidity = "data/weather_14-06-2017/Relative humidity-data-2021-03-08 12_16_26.csv",
                   temperature = "data/weather_14-06-2017/Temperature-data-2021-03-08 12_15_39.csv",
                   wind = "data/weather_14-06-2017/Wind velocity-data-2021-03-08 12_16_05.csv",
                   MTL = "data/L8_C2/LC08_L1TP_197024_20170614_20200903_02_T1_MTL.txt",
                   lat = 51.968791, long = 7.59513, elev = 60, height = 2,
                   L8_path = "data/L8/", L8.SR_path = paste(L8_path, "SR/", sep = ""), DEM_path = "data/SRTM_DEM/",
                   LAI_method = "metric2010", LAI_L = 0.1,
                   mountainous = FALSE, mom_rough_method = "short.crops",
                   anchors_method = "flexible", anchors_n = 5, ETp.coef = 1.05, Z.om.ws = 0.03)
{
  aoi = createAoi(topleft = c(utm_x - dist, utm_y + dist), bottomright = c(utm_x + dist, utm_y - dist), EPSG = epsg)
  
  weather_list = lapply(list(rain, radiation, humidity, temperature, wind), read_csv)
  weather = Reduce(function(x, y, ...) merge(x, y, ...), weather_list)
  weather_time = format(weather$Time, format = "%H:%M:%S")
  weather$Time = format(weather$Time, format = "%d/%m/%Y")
  weather = cbind(weather_time, weather)
  colnames(weather) = c("time", "date", "precipitation", "radiation", "rel_humidity", "temperature", "wind_vel")
  
  MTLfile = file(MTL)
  WeatherStation = read.WSdata(WSdata = weather, date.format = "%d/%m/%Y", lat = lat, long = long, elev = elev, height = height, columns = c("date" = 2, "time" = 1, "radiation" = 4, "wind" = 7, "RH" = 5, "temp" = 6, "rain" = 3), MTL = MTLfile)
  print(WeatherStation)
  plot(WeatherStation)
  
  L8 = loadImage(path = L8_path, sat = "L8", aoi = aoi)
  L8 = remove_negatives(L8)
  plot(L8)
  
  L8.SR = loadSR(path = L8.SR_path, aoi = aoi)
  L8.SR = remove_negatives(L8.SR)
  L8.SR = L8_SR(L8.SR)
  plot(L8.SR)
  
  DEM = prepareSRTMdata(path = DEM_path, extent = L8)
  plot(DEM)
  
  surface.model = METRICtopo(DEM)
  solar.angles.r = solarAngles(surface.model = surface.model, WeatherStation = WeatherStation, MTL = MTLfile)
  
  Rs.inc = incSWradiation(surface.model = surface.model, solar.angles = solar.angles.r, WeatherStation = WeatherStation)
  image.TOAr = calcTOAr(image.DN = L8, sat = "L8", MTL = MTLfile, incidence.rel = solar.angles.r$incidence.rel, aoi = aoi)
  plot(Rs.inc)
  
  albedo = albedo.daSilva(L8.SR)
  plot(albedo)
  
  LAI = LAI(method = LAI_method, image = image.TOAr, L = 0.1)
  plot(LAI)
  
  L8.ST = loadST(path = L8.SR_path, aoi = aoi)
  Ts = calcLST(L8.ST)
  plot(Ts)
  
  Rl.out = outLWradiation(LAI = LAI, Ts = Ts)
  Rl.inc = incLWradiation(WeatherStation = WeatherStation, DEM = surface.model$DEM, solar.angles = solar.angles.r, Ts = Ts)
  plot(Rl.out)
  plot(Rl.inc)
  
  Rn = netRadiation(LAI, albedo, Rs.inc, Rl.inc, Rl.out)
  plot(Rn)
  
  G = soilHeatFlux(image = L8.SR, Ts = Ts, albedo = albedo, Rn = Rn, LAI = LAI)
  plot(G)
  
  Z.om = momentumRoughnessLength(LAI = LAI, mountainous = mountainous, method = mom_rough_method, surface.model = surface.model)
  hot.and.cold = calculate_anchors(image = image.TOAr, Ts, LAI, Rn = Rn, G = G, plots = TRUE, albedo = albedo, Z.om = Z.om, n = anchors_n, anchors.method = anchors_method, WeatherStation = WeatherStation, verbose = TRUE)
  H = calcH(anchors = hot.and.cold, mountainous = mountainous, Ts = Ts, Z.om = Z.om, WeatherStation = WeatherStation, ETp.coef = ETp.coef, Z.om.ws = Z.om.ws, DEM = DEM, Rn = Rn, G = G, verbose = TRUE)
  
  ET_WS = dailyET(WeatherStation = WeatherStation, lat = lat, long = long, elev = elev, height = height, ET = "ETr", MTL = MTL)
  ET.24 = ET24h(Rn, G, H$H, Ts, WeatherStation = WeatherStation, ETr.daily = ET_WS)
  
  ET_date = WeatherStation$at.sat$date
  return(list(ET_date, ET.24))
}

# ptm = proc.time()
# t2 = ET_wrap()
# proc.time() - ptm

utm_x = 411014.92
utm_y = 5776294.31

rain = "data/weather_FMO/rain.csv"
radiation = "data/weather_FMO/radiation.csv"
humidity = "data/weather_FMO/humidity.csv"
temperature = "data/weather_FMO/temperature.csv"
wind = "data/weather_FMO/wind.csv"

lat = 52.13
long = 7.7
elev = 47.8
height = 2

L8_path = paste(list.files("data/L8", full.names = TRUE), "/", sep = "")
L8_MTL_path = paste(L8_path, "MTL.txt", sep = "/")
L8.SR_path = paste(L8_path, "SR/", sep = "")


t2 = ET_wrap(utm_x = utm_x, utm_y = utm_y,
             rain = rain, radiation = radiation, humidity = humidity, temperature = temperature, wind = wind,
             MTL = L8_MTL_path[3], lat = lat, long = long, elev = elev, height = height,
             L8_path = L8_path[3])

