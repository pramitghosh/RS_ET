Calculating evapotranspiration using remotely sensed data
================
Pramit Ghosh

## Loading required packages

``` r
library(water)
```

    ## Loading required package: raster

    ## Loading required package: sp

    ## Loading required package: rgdal

    ## rgdal: version: 1.5-21, (SVN revision 1105)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.0.4, released 2020/01/28
    ## Path to GDAL shared files: C:/Users/Pramit/Documents/R/win-library/4.0/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 6.3.1, February 10th, 2020, [PJ_VERSION: 631]
    ## Path to PROJ shared files: C:\Program Files (x86)\MapWindow\PROJ_NAD
    ## Linking to sp version:1.4-5
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading rgdal.
    ## Overwritten PROJ_LIB was C:\Program Files (x86)\MapWindow\PROJ_NAD

``` r
library(readr)
```

## Define Area of Interest (AOI)

``` r
# Create AOI (625 sq.km. centered on Muenster FHZ weather station)
utm_x = 403490.150
utm_y = 5758499.100
aoi = createAoi(topleft = c(utm_x - 25000, utm_y + 25000), bottomright = c(utm_x + 25000, utm_y - 25000), EPSG = 32632)
```

## Read Weather Station data

``` r
source("utilities.R")

rain = "data/weather_14-06-2017/Precipitation-data-2021-03-08 12_15_07.csv"
radiation = "data/weather_14-06-2017/Radiation-data-2021-03-08 12_17_32.csv"
humidity = "data/weather_14-06-2017/Relative humidity-data-2021-03-08 12_16_26.csv"
temperature = "data/weather_14-06-2017/Temperature-data-2021-03-08 12_15_39.csv"
wind = "data/weather_14-06-2017/Wind velocity-data-2021-03-08 12_16_05.csv"

weather = read_WS(rain, radiation, humidity, temperature, wind)
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Time = col_datetime(format = ""),
    ##   precipitation.mean = col_double()
    ## )

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Time = col_datetime(format = ""),
    ##   radiation.mean = col_double()
    ## )

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Time = col_datetime(format = ""),
    ##   relative_humidity.mean = col_double()
    ## )

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Time = col_datetime(format = ""),
    ##   `temperature.mean {measurement_interpolationtype: Continuous_Instantaneous, sensor_device: WS301}` = col_double()
    ## )

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Time = col_datetime(format = ""),
    ##   wind_velocity.mean = col_double()
    ## )

``` r
weather_time = format(weather$Time, format = "%H:%M:%S")
weather$Time = format(weather$Time, format = "%d/%m/%Y")
weather = cbind(weather_time, weather)
colnames(weather) = c("time", "date", "precipitation", "radiation", "rel_humidity", "temperature", "wind_vel")
```

## Calculate data values at satellite overpass

``` r
MTLfile = "data/L8_C2/LC08_L1TP_197024_20170614_20200903_02_T1_MTL.txt"
WeatherStation = sat_overpass(L8_L2_MTL = MTLfile, weather = weather, lat = 51.968791, long = 7.59513, elev = 60, height = 2)
```

    ## Warning in read.WSdata(WSdata = weather, date.format = "%d/%m/%Y", lat = lat, :
    ## As tz = "", assuming the weather station time zone is Europe/Berlin

    ## Weather Station @ lat: 51.97 long: 7.6 elev: 60 
    ## Summary:
    ##    radiation          wind             RH              ea             temp     
    ##  Min.   :  0.0   Min.   :0.667   Min.   :34.40   Min.   :1.023   Min.   : 8.3  
    ##  1st Qu.:  0.0   1st Qu.:1.500   1st Qu.:40.88   1st Qu.:1.118   1st Qu.:11.9  
    ##  Median :235.5   Median :2.000   Median :53.65   Median :1.152   Median :18.8  
    ##  Mean   :327.0   Mean   :2.202   Mean   :61.76   Mean   :1.155   Mean   :17.6  
    ##  3rd Qu.:681.0   3rd Qu.:2.900   3rd Qu.:85.70   3rd Qu.:1.193   3rd Qu.:23.0  
    ##  Max.   :894.0   Max.   :4.800   Max.   :97.10   Max.   :1.315   Max.   :25.1  
    ##       rain  
    ##  Min.   :0  
    ##  1st Qu.:0  
    ##  Median :0  
    ##  Mean   :0  
    ##  3rd Qu.:0  
    ##  Max.   :0  
    ## 
    ##  Conditions at satellite flyby:
    ##                datetime radiation wind    RH  ea  temp rain       date
    ## 434 2017-06-14 12:27:22    789.38 3.19 41.39 1.2 23.47    0 2017-06-14

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Load Landsat 8 Level-2 data

``` r
source("remove_negatives.R")
L8 = loadImage(path = "data/L8_C2/", sat = "L8", aoi = aoi)
```

    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(list.files(path = path, pattern =
    ## paste0(image_pattern, : implicit list embedding of S4 objects is deprecated

``` r
L8 = remove_negatives(L8)
plot(L8)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Load Surface Reflectance data

``` r
source("loadSR.R")
source("calcAlbedo.R")
L8.SR = loadSR(path = "data/L8_C2/SR/", aoi = aoi)
```

    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated
    
    ## Warning in `[<-`(`*tmp*`, i, value = raster(files[i])): implicit list embedding
    ## of S4 objects is deprecated

``` r
L8.SR = remove_negatives(L8.SR)
L8.SR = L8_SR(L8.SR)
plot(L8.SR)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Load Digital Elevation Model

``` r
checkSRTMgrids(L8)
```

    ## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj
    ## = prefer_proj): Discarded datum Unknown based on WGS84 ellipsoid in Proj4
    ## definition

    ## [1] "You need 2 1deg x 1deg SRTM grids"
    ## [1] "You can get them here:"

    ## [1] "http://earthexplorer.usgs.gov/download/options/8360/SRTM1N51E07V3/"
    ## [2] "http://earthexplorer.usgs.gov/download/options/8360/SRTM1N52E07V3/"

``` r
DEM = prepareSRTMdata(path = "data/SRTM_DEM/", extent = L8)
plot(DEM)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Calculate solar angles

``` r
surface.model = METRICtopo(DEM)
solar.angles.r = solarAngles(surface.model = surface.model, WeatherStation = WeatherStation, MTLfile)
plot(solar.angles.r)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Calculate intermediate results

### Calculate incident short-wave radiation

``` r
Rs.inc = incSWradiation(surface.model = surface.model, solar.angles = solar.angles.r, WeatherStation = WeatherStation)
plot(Rs.inc)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Calculate Top-of-Atmosphere reflectance

``` r
image.TOAr = calcTOAr(image.DN = L8, sat = "L8", MTL = MTLfile, incidence.rel = solar.angles.r$incidence.rel, aoi = aoi)
plot(image.TOAr)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# image.SR = calcSR(image.TOAr = image.TOAr, sat = "L7", surface.model = surface.model, incidence.hor = solar.angles.r$incidence.hor, WeatherStation = WeatherStation)
```

### Calculate Albedo

``` r
# albedo = albedo(image.SR = L8.SR, coeff = "Olmedo", sat = "L8")
# albedo = albedo - 1 #To compensate for too high albedo! (needs more investigation)
albedo = albedo.daSilva(L8.SR)
plot(albedo)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Calculate Leaf Area Index

``` r
LAI = LAI(method = "metric2010", image = image.TOAr, L = 0.1)
plot(LAI)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Calculate further intermediate results

``` r
source("calcLST.R")
```

### Calculate Surface Temperature

``` r
L8.ST = loadST(path = "data/L8_C2/SR/", aoi = aoi)
Ts = calcLST(L8.ST) 
# Ts = surfaceTemperature(image.DN = L8, LAI = LAI, sat = "L8", WeatherStation = WeatherStation, aoi = aoi, method = "SW")
plot(Ts)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Calculate incident and outgoing long-wave radiation

``` r
Rl.out = outLWradiation(LAI = LAI, Ts = Ts)
Rl.inc = incLWradiation(WeatherStation = WeatherStation, DEM = surface.model$DEM, solar.angles = solar.angles.r, Ts = Ts)
plot(Rl.out)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(Rl.inc)
```

![](README_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

## Calculate Net radiation

``` r
Rn = netRadiation(LAI, albedo, Rs.inc, Rl.inc, Rl.out)
plot(Rn)
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Calculate Soil Heat Flux

``` r
G = soilHeatFlux(image = L8.SR, Ts = Ts, albedo = albedo, Rn = Rn, LAI = LAI)
plot(G)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Calculate Sensible Heat flux

``` r
source("calcAnchors.R")

Z.om = momentumRoughnessLength(LAI = LAI, mountainous = FALSE, method = "short.crops", surface.model = surface.model)
hot.and.cold = calculate_anchors(image = image.TOAr, Ts, LAI, Rn = Rn, G = G, plots = TRUE, albedo = albedo, Z.om = Z.om, n = 5, anchors.method = "flexible", WeatherStation = WeatherStation, verbose = TRUE)
```

    ## [1] "relaxing criteria for cold pixels: 0.1%"
    ## [1] "relaxing criteria for cold pixels: 0.2%"
    ## [1] "relaxing criteria for cold pixels: 0.3%"
    ## [1] "relaxing criteria for cold pixels: 0.4%"

    ## Warning in calculate_anchors(image = image.TOAr, Ts, LAI, Rn = Rn, G = G, : Criteria used for cold pixels was:
    ##     LAI: 1.8 to 8.4 
    ##     albedo: 0.108 to 0.35 
    ##     Z.om: 0.018 to 0.112 
    ##     and buffer == TRUE

    ## Warning in calculate_anchors(image = image.TOAr, Ts, LAI, Rn = Rn, G = G, : I
    ## can only find 1 anchors with cold pixel conditions

    ## [1] "Cold pixels"
    ##     pixel      LAI      NDVI    albedo      Z.om       Ts
    ## 1 1433829 5.880326 0.8265637 0.1084798 0.1058459 289.5251
    ## [1] "Hot pixels"
    ##     pixel        LAI      NDVI    albedo         Z.om       Ts
    ## 1 2662068 0.01109450 0.1078400 0.1330022 0.0001997011 319.2242
    ## 2 2161842 0.01396457 0.1123579 0.1474046 0.0002513622 318.9986
    ## 3 2600944 0.02056578 0.1342561 0.1315451 0.0003701841 316.9923
    ## 4 2138884 0.01001867 0.1039880 0.1428028 0.0001803361 315.9190
    ## 5 1604596 0.19731384 0.2798715 0.1324220 0.0035516491 315.6456

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# hot.and.cold = calcAnchors(image = image.TOAr, Ts, LAI, plots = TRUE, albedo = albedo, Z.om = Z.om, n = 5, anchors.method = "flexible", WeatherStation = WeatherStation, verbose = TRUE)
H = calcH(anchors = hot.and.cold, mountainous = FALSE, Ts = Ts, Z.om = Z.om, WeatherStation = WeatherStation, ETp.coef = 1.05, Z.om.ws = 0.03, DEM = DEM, Rn = Rn, G = G, verbose = TRUE)
```

    ## [1] "starting conditions"
    ## [1] "Cold"
    ##         Ts Ts_datum       Rn        G      Z.om     u200       u.
    ## 1 289.5251 288.9248 653.5944 38.17583 0.1058459 6.687982 0.363473
    ## [1] "Hot"
    ##         Ts Ts_datum       Rn        G         Z.om     u200        u.
    ## 1 317.3559 317.2847 596.2919 129.6412 0.0009106465 6.687982 0.2111376
    ## 2 317.3559 317.2847 596.2919 129.6412 0.0009106465 6.687982 0.2111376
    ## 3 317.3559 317.2847 596.2919 129.6412 0.0009106465 6.687982 0.2111376
    ## 4 317.3559 317.2847 596.2919 129.6412 0.0009106465 6.687982 0.2111376
    ## 5 317.3559 317.2847 596.2919 129.6412 0.0009106465 6.687982 0.2111376

![](README_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

    ## [1] "iteraction # 1"
    ## [1] "a 0.403483736598569"
    ## [1] "b -113.172050349543"
    ## [1] "r.ah cold 20.1023563952467"
    ## [1] "r.ah hot 34.8863627040433"
    ## [1] "dT cold 3.40442661202874"
    ## [1] "dT hot 14.8471601492061"
    ## [1] "##############"
    ## [1] "delta rah hot -239.070392167543"
    ## [1] "delta rah cold -79.9043804436735"
    ## [1] "### -------"
    ## [1] "iteraction # 2"
    ## [1] "a 0.0876743590084544"
    ## [1] "b -23.4389474103077"
    ## [1] "r.ah cold 11.1739115777343"
    ## [1] "r.ah hot 10.2888260107373"
    ## [1] "dT cold 1.89235337329357"
    ## [1] "dT hot 4.37878402012456"
    ## [1] "##############"
    ## [1] "delta rah hot 39.4311626069338"
    ## [1] "delta rah cold 23.3393127543599"
    ## [1] "### -------"
    ## [1] "iteraction # 3"
    ## [1] "a 0.167876637465405"
    ## [1] "b -46.03525230387"
    ## [1] "r.ah cold 14.5758040779497"
    ## [1] "r.ah hot 16.9869960421515"
    ## [1] "dT cold 2.46847953140571"
    ## [1] "dT hot 7.22943382866689"
    ## [1] "##############"
    ## [1] "delta rah hot -12.7255412080877"
    ## [1] "delta rah cold -8.60580208870868"
    ## [1] "### -------"
    ## [1] "iteraction # 4"
    ## [1] "a 0.145996105912187"
    ## [1] "b -39.9090226199546"
    ## [1] "r.ah cold 13.4208336917803"
    ## [1] "r.ah hot 15.0693408610867"
    ## [1] "dT cold 2.27287997872293"
    ## [1] "dT hot 6.41330593864401"
    ## [1] "##############"
    ## [1] "delta rah hot 3.14028281364026"
    ## [1] "delta rah cold 2.92615763884815"
    ## [1] "### -------"
    ## [1] "iteraction # 5"
    ## [1] "a 0.150911950032248"
    ## [1] "b -41.2608192864452"
    ## [1] "r.ah cold 13.8253862887694"
    ## [1] "r.ah hot 15.55790301565"
    ## [1] "dT cold 2.34139282368874"
    ## [1] "dT hot 6.62123132809143"
    ## [1] "##############"
    ## [1] "delta rah hot -0.830322743544705"
    ## [1] "delta rah cold -1.02075946348918"
    ## [1] "### -------"
    ## [1] "iteraction # 6"
    ## [1] "a 0.149823567071469"
    ## [1] "b -40.9700168982847"
    ## [1] "r.ah cold 13.6856883299973"
    ## [1] "r.ah hot 15.4297859932676"
    ## [1] "dT cold 2.31773433116484"
    ## [1] "dT hot 6.56670647076286"
    ## [1] "##############"
    ## [1] "delta rah hot 0.21542568085174"
    ## [1] "delta rah cold 0.352861365626294"
    ## [1] "### -------"
    ## [1] "iteraction # 7"
    ## [1] "a 0.150034060628878"
    ## [1] "b -41.0226263676264"
    ## [1] "r.ah cold 13.7341508422164"
    ## [1] "r.ah hot 15.4630974762867"
    ## [1] "dT cold 2.32594168074318"
    ## [1] "dT hot 6.58088338359804"
    ## [1] "##############"
    ## [1] "convergence reached at iteration #8"
    ## [1] "delta rah hot -0.0562136371471853"
    ## [1] "delta rah cold -0.122359807707384"
    ## [1] "### -------"

## Calculate 24h evapotranspiration

``` r
# Calculate 24h evapotranspiration
ET_WS = dailyET(WeatherStation = WeatherStation, lat = 51.968791, long = 7.59513, elev = 60, height = 2)
ET.24 = ET24h(Rn, G, H$H, Ts, WeatherStation = WeatherStation, ETr.daily = ET_WS)
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
