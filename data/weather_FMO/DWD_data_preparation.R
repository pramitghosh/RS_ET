library(readr)

humid_temp_2019 <- read_delim("humidity-pressure-temperature/temperature_20100101_20191231_hist/produkt_zehn_min_tu_20100101_20191231_01766.txt",
";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
QN = col_skip(), TM5_10 = col_skip(), PP_10 = col_skip(),
TD_10 = col_skip(),
eor = col_skip()), trim_ws = TRUE)

humid_temp_2020 = read_delim("humidity-pressure-temperature/temperature_20200101_20201231_hist/produkt_zehn_min_tu_20200101_20201231_01766.txt",
                                ";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
                                                                             MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
                                                                             QN = col_skip(), TM5_10 = col_skip(),
                                                                             PP_10 = col_skip(), TD_10 = col_skip(),
                                                                             eor = col_skip()), trim_ws = TRUE)
humid_temp = rbind(pressure_temp_2019, pressure_temp_2020)

humidity = humid_temp[,c(1,3)]
colnames(humidity) = c("Time", "Relative_Humidity")
temperature = humid_temp[,c(1,2)]
colnames(temperature) = c("Time", "Temperature")

temperature$Temperature[temperature$Temperature == -999] = NA
humidity$Relative_Humidity[humidity$Relative_Humidity == -999] = NA

radiation_2019 <- read_delim("radiation/10minutenwerte_SOLAR_01766_20100101_20191231_hist/produkt_zehn_min_sd_20100101_20191231_01766.txt",
";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
QN = col_skip(), DS_10 = col_skip(),
SD_10 = col_skip(), LS_10 = col_skip(),
eor = col_skip()), trim_ws = TRUE)

radiation_2020 <- read_delim("radiation/10minutenwerte_SOLAR_01766_20200101_20201231_hist/produkt_zehn_min_sd_20200101_20201231_01766.txt",
                             ";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
                                                                          MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
                                                                          QN = col_skip(), DS_10 = col_skip(),
                                                                          SD_10 = col_skip(), LS_10 = col_skip(),
                                                                          eor = col_skip()), trim_ws = TRUE)
radiation = rbind(radiation_2019, radiation_2020)
colnames(radiation) = c("Time", "Radiation")
radiation$Radiation[radiation$Radiation == -999] = NA

#Convert radiation in J/cm^2 (for 10 min.) to W/m^2
cf = 100*100/(10*60)
radiation$Radiation = cf * radiation$Radiation

rain_2019 <- read_delim("rain/10minutenwerte_nieder_01766_20100101_20191231_hist/produkt_zehn_min_rr_20100101_20191231_01766.txt",
";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
QN = col_skip(), RWS_DAU_10 = col_skip(),
RWS_IND_10 = col_skip(), eor = col_skip()),
trim_ws = TRUE)

rain_2020 = read_delim("rain/10minutenwerte_nieder_01766_20200101_20201231_hist/produkt_zehn_min_rr_20200101_20201231_01766.txt",
                                    ";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
                                                                                 MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
                                                                                 QN = col_skip(), RWS_DAU_10 = col_skip(),
                                                                                 RWS_IND_10 = col_skip(), eor = col_skip()),
                                    trim_ws = TRUE)
rain = rbind(rain_2019, rain_2020)
colnames(rain) = c("Time", "Precipitation")
rain$Precipitation[rain$Precipitation == -999] = NA

wind_2019 = read_delim("wind/10minutenwerte_wind_01766_20100101_20191231_hist/produkt_zehn_min_ff_20100101_20191231_01766.txt",
";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
QN = col_skip(), DD_10 = col_skip(),
eor = col_skip()), trim_ws = TRUE)

wind_2020 = read_delim("wind/10minutenwerte_wind_01766_20200101_20201231_hist/produkt_zehn_min_ff_20200101_20201231_01766.txt",
           ";", escape_double = FALSE, col_types = cols(STATIONS_ID = col_skip(),
                                                        MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
                                                        QN = col_skip(), DD_10 = col_skip(),
                                                        eor = col_skip()), trim_ws = TRUE)
wind = rbind(wind_2019, wind_2020)
colnames(wind) = c("Time", "Windspeed")
wind$Windspeed[wind$Windspeed == -999] = NA

write_csv(humidity, file = "humidity.csv")
write_csv(radiation, file = "radiation.csv")
write_csv(rain, file = "rain.csv")
write_csv(temperature, file = "temperature.csv")
write_csv(wind, file = "wind.csv")
