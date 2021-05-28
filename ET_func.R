library(water)
library(readr)

source("remove_negatives.R")
source("loadSR.R")
source("calcAlbedo.R")
source("calcLST.R")
source("calcAnchors.R")
source("validation.R")

ET_wrap = function(L8_path = "data/L8/",
                   utm_x = 403490.150, utm_y = 5758499.100, dist = 25000, epsg = 32632,
                   rain = "data/weather_14-06-2017/Precipitation-data-2021-03-08 12_15_07.csv",
                   radiation = "data/weather_14-06-2017/Radiation-data-2021-03-08 12_17_32.csv",
                   humidity = "data/weather_14-06-2017/Relative humidity-data-2021-03-08 12_16_26.csv",
                   temperature = "data/weather_14-06-2017/Temperature-data-2021-03-08 12_15_39.csv",
                   wind = "data/weather_14-06-2017/Wind velocity-data-2021-03-08 12_16_05.csv",
                   MTL = paste(L8_path, "MTL.txt", sep = "/"),
                   lat = 51.968791, long = 7.59513, elev = 60, height = 2,
                   L8.SR_path = paste(L8_path, "SR/", sep = ""), DEM_path = "data/SRTM_DEM/",
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
  
  ET_WS = dailyET(WeatherStation = WeatherStation, lat = lat, long = long, elev = elev, height = height, ET = "ETo", MTL = MTL)
  ET.24 = ET24h(Rn, G, H$H, Ts, WeatherStation = WeatherStation, ETr.daily = ET_WS)
  
  ET_date = WeatherStation$at.sat$date
  return(list(ET_date, ET_WS, ET.24))
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
# L8_MTL_path = paste(L8_path, "MTL.txt", sep = "/")
# L8.SR_path = paste(L8_path, "SR/", sep = "")

# t2 = ET_wrap(L8_path = L8_path[1], utm_x = utm_x, utm_y = utm_y,
#              rain = rain, radiation = radiation, humidity = humidity, temperature = temperature, wind = wind,
#              lat = lat, long = long, elev = elev, height = height)

ET_results = lapply(as.list(L8_path), ET_wrap,
                    utm_x = utm_x, utm_y = utm_y,
                    rain = rain, radiation = radiation, humidity = humidity, temperature = temperature, wind = wind,
                    lat = lat, long = long, elev = elev, height = height)

fmo_coords = c(409569.18, 5776377.16)
ET_FMO = data.frame("Date" = sapply(ET_results, function(img_list){as.character(img_list[[1]])}),
                    "PET_RS" = sapply(ET_results, function(img_list){img_list[[2]]}),
                    "ET_RS" = sapply(ET_results, function(img_list){val_at_coords(img_list[[3]], fmo_coords)}))

FMO_DWD = read_csv("data/weather_FMO/klima_1989-2020_MünsterOsnabrück.csv",
                   col_types = cols(STATIONS_ID = col_skip(),
                                    MESS_DATUM = col_datetime(format = "%Y-%m-%d %H:%M:%S UTC"),
                                    QN_3 = col_skip(), FX.Windspitze = col_skip(),
                                    FM.Windgeschwindigkeit = col_skip(),
                                    QN_4 = col_skip(), RSK.Niederschlagshoehe = col_skip(),
                                    RSKF.Niederschlagsform = col_skip(),
                                    SDK.Sonnenscheindauer = col_skip(),
                                    SHK_TAG.Schneehoehe = col_skip(),
                                    NM.Bedeckungsgrad = col_skip(), VPM.Dampfdruck = col_skip(),
                                    PM.Luftdruck = col_skip(), TMK.Lufttemperatur = col_skip(),
                                    UPM.Relative_Feuchte = col_skip(),
                                    TXK.Lufttemperatur_Max = col_skip(),
                                    TNK.Lufttemperatur_Min = col_skip(),
                                    TGK.Lufttemperatur_5cm_min = col_skip(),
                                    eor = col_skip()))
colnames(FMO_DWD) = c("Date", "ET_DWD")
FMO_DWD$Date = as.character(FMO_DWD$Date)

ET_comparison = merge(x = FMO_DWD, y = ET_FMO, all.y = TRUE)
ET_cor = cor(ET_comparison$ET_DWD, y = ET_comparison$ET_RS)

max_ET = max(ET_comparison$ET_DWD, ET_comparison$PET_RS, ET_comparison$ET_RS)
plot(ET_comparison$ET_DWD, ET_comparison$ET_RS,
     xlab = "Daily ET from Climate station (mm/d)",
     ylab = "Daily ET from Remote Sensing (mm/d)",
     main = "Comparison of daily ET values with climate station data", sub = paste("Coefficient of correlation:", round(ET_cor, 3)),
     xlim = c(0, max_ET), ylim = c(0, max_ET))
abline(0, 1, col = "Blue", lty = 3)
points(x = ET_comparison$ET_DWD, y = ET_comparison$PET_RS, pch = 4, col = "darkgreen")

# plot(ET_comparison$ET_DWD ~ as.POSIXct(ET_comparison$Date, format = "%Y-%m-%d"), xaxt = "none", ylab = "ET (mm/d)", xlab = "", type = "b", lty = 2, col = "darkgreen", main = "Daily Evapotranspiration at FMO")
# axis(1, at = as.POSIXct(ET_FMO$Date, format = "%Y-%m-%d"), labels = format(as.POSIXct(ET_FMO$Date, format = "%Y-%m-%d"), format = "%m/%Y"), las = 2, cex.axis = 0.8)
# title(xlab = "Time", line = 4)
# par(new = TRUE)
# plot(ET_comparison$ET_RS ~ as.POSIXct(ET_comparison$Date, format = "%Y-%m-%d"), xlab = "", ylab = "", axes = FALSE, type = "b", lty = 2, col = "blue")
# legend(x = "bottomleft", legend = c("Remote Sensing", "DWD Climate Station"), col = c("blue", "darkgreen"), lty = 2, cex = 0.8, inset = 0.01)

plot(ET_comparison$ET_DWD ~ as.POSIXct(ET_comparison$Date, format = "%Y-%m-%d"), xaxt = "none", ylab = "Evapotranspiration (mm/d)", xlab = "", type = "b", lty = 2, col = "blue", main = "Daily Evapotranspiration at Flughafen Münster/Osnabrück", ylim = c(0, max_ET))
axis(1, at = as.POSIXct(ET_FMO$Date, format = "%Y-%m-%d"), labels = format(as.POSIXct(ET_FMO$Date, format = "%Y-%m-%d"), format = "%m/%Y"), las = 2, cex.axis = 0.8)
title(xlab = "Time", line = 4)
par(new = TRUE)
plot(ET_comparison$ET_RS ~ as.POSIXct(ET_comparison$Date, format = "%Y-%m-%d"), xlab = "", ylab = "", axes = FALSE, type = "b", lty = 2, col = "black")
par(new = TRUE)
plot(ET_comparison$PET_RS ~ as.POSIXct(ET_comparison$Date, format = "%Y-%m-%d"), xlab = "", ylab = "", axes = FALSE, type = "b", lty = 2, col = "darkgreen", pch = 4)
legend(title = "ET calculated using", x = "bottomleft", legend = c("Remote Sensing data (actual ET)", "Remote Sensing data (potential ET)", "DWD Climate Station data (potential ET)"), col = c("black", "darkgreen", "blue"), lty = 2, cex = 0.8, inset = 0.01, pch = c(1,4,1), title.adj = 0.1, seg.len = 3)
