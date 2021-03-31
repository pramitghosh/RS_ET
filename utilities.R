read_WS = function(rain, radiation, humidity, temperature, wind)
{
  weather_list = list(rain, radiation, humidity, temperature, wind)
  weather_list = lapply(weather_list, read_csv)
  weather = Reduce(function(x, y, ...) merge(x, y, ...), weather_list)
  weather
}

sat_overpass = function(L8_L2_MTL, weather, lat, long, elev, height)
{
  MTLfile = file(L8_L2_MTL)
  WeatherStation = read.WSdata(WSdata = weather, date.format = "%d/%m/%Y", lat = lat, long = long, elev = elev, height = height, columns = c("date" = 2, "time" = 1, "radiation" = 4, "wind" = 7, "RH" = 5, "temp" = 6, "rain" = 3), MTL = MTLfile)
  print(WeatherStation)
  plot(WeatherStation, hourly = TRUE)
  WeatherStation
}
