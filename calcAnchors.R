calculate_anchors = function (image, Ts, LAI, albedo, Z.om, n = 1, aoi, anchors.method = "flexible", 
          WeatherStation, plots = TRUE, deltaTemp = 5, minDist = 500, 
          WSbuffer = 30000, verbose = FALSE) 
{
  if (anchors.method %in% c("CITRA-MCB", "CITRA-MCBbc", 
                            "CITRA-MCBr")) {
    warning("anchor method names has changed. Old names (CITRA-MCBx) are \n            deprecated. Options now include 'best', 'random' and 'flexible'")
  }
  if (anchors.method %in% c("CITRA-MCB", "CITRA-MCBbc")) {
    anchors.method <- "best"
  }
  if (anchors.method %in% c("CITRA-MCBr")) {
    anchors.method <- "random"
  }
  NDVI <- (image$NIR - image$R)/(image$NIR + image$R)
  NDVI[NDVI < -1] <- -1
  NDVI[NDVI > 1] <- 1
  if (!missing(WeatherStation)) {
    WSloc <- WeatherStation$location
    coordinates(WSloc) <- ~long + lat
    WSloc@proj4string <- sp::CRS("+init=epsg:4326")
    WSloc <- sp::spTransform(WSloc, Ts@crs)
    WScell <- extract(Ts, WSloc, cellnumbers = T)[1]
    WS.buffer <- raster(Ts)
    values(WS.buffer)[WScell] <- 1
    WS.buffer <- buffer(WS.buffer, width = WSbuffer)
  }
  else {
    WS.buffer <- raster(Ts)
    values(WS.buffer) <- 1
  }
  if (anchors.method == "random") {
    minT <- quantile(Ts[LAI >= 3 & LAI <= 6 & albedo >= 0.18 & 
                          albedo <= 0.25 & Z.om >= 0.03 & Z.om <= 0.08], 0.05, 
                     na.rm = TRUE)
    if (minT + deltaTemp < 288 | is.na(minT)) {
      minT = 288 + deltaTemp
    }
    maxT <- max(Ts[albedo >= 0.13 & albedo <= 0.15 & NDVI >= 
                     0.1 & NDVI <= 0.28 & Z.om <= 0.005], na.rm = TRUE)
    cold.candidates <- values(LAI >= 3) & values(LAI <= 6) & 
      values(albedo >= 0.18) & values(albedo <= 0.25) & 
      values(NDVI >= max(values(NDVI), na.rm = T) - 0.15) & 
      values(Z.om >= 0.03) & values(Z.om <= 0.08) & values(Ts < 
                                                             (minT + deltaTemp)) & values(WS.buffer == 1)
    hot.candidates <- values(albedo >= 0.13) & values(albedo <= 
                                                        0.15) & values(NDVI >= 0.1) & values(NDVI <= 0.28) & 
      values(Z.om <= 0.005) & values(Ts > (maxT - deltaTemp)) & 
      values(WS.buffer == 1)
    cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
    hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
    if (cold.n < 1 | hot.n < 1) {
      stop(paste("Not enough pixels with the conditions for anchor pixels. I \n                 found", 
                 cold.n, "cold pixels and", hot.n, "hot pixels."))
    }
    try(cold <- sample(which(cold.candidates), 1), silent = TRUE)
    if (n > 1) {
      for (nsample in 1:(n - 1)) {
        distbuffer <- raster(Ts)
        values(distbuffer)[cold] <- 1
        distbuffer <- buffer(distbuffer, width = minDist)
        distbuffer <- is.na(distbuffer)
        newAnchor <- NA
        cold.candidates <- values(LAI >= 3) & values(LAI <= 
                                                       6) & values(albedo >= 0.18) & values(albedo <= 
                                                                                              0.25) & values(NDVI >= max(values(NDVI), na.rm = T) - 
                                                                                                               0.15) & values(Z.om >= 0.03) & values(Z.om <= 
                                                                                                                                                       0.08) & values(Ts < (minT + deltaTemp)) & values(distbuffer == 
                                                                                                                                                                                                          1) & values(WS.buffer == 1)
        if (length(which(cold.candidates)) < 2) {
          warning(paste("I can only find ", nsample, 
                        " anchors with cold pixel conditions"))
          break
        }
        try(newAnchor <- sample(which(cold.candidates), 
                                1), silent = FALSE)
        if (!is.na(newAnchor)) {
          cold <- c(cold, newAnchor)
        }
      }
    }
    try(hot <- sample(which(hot.candidates & values(Ts > 
                                                      quantile(Ts[hot.candidates], 0.75))), 1), silent = TRUE)
    if (n > 1) {
      for (nsample in 1:(n - 1)) {
        distbuffer <- raster(Ts)
        values(distbuffer)[hot] <- 1
        distbuffer <- buffer(distbuffer, width = minDist)
        distbuffer <- is.na(distbuffer)
        newAnchor <- NA
        hot.candidates <- values(albedo >= 0.13) & values(albedo <= 
                                                            0.15) & values(NDVI >= 0.1) & values(NDVI <= 
                                                                                                   0.28) & values(distbuffer == 1) & values(Z.om <= 
                                                                                                                                              0.005) & values(Ts > (maxT - deltaTemp)) & 
          values(WS.buffer == 1)
        if (length(which(hot.candidates)) < 2) {
          warning(paste("I can only find ", nsample, 
                        " anchors with hot pixel conditions"))
          break
        }
        try(newAnchor <- sample(which(hot.candidates), 
                                1), silent = FALSE)
        if (!is.na(newAnchor)) {
          hot <- c(hot, newAnchor)
        }
      }
    }
  }
  if (anchors.method == "best") {
    minT <- quantile(Ts[LAI >= 2.8 & LAI <= 6 & albedo >= 
                          0.15 & albedo <= 0.25 & Z.om >= 0.03 & Z.om <= 0.08], 
                     0.05, na.rm = TRUE)
    if (minT + deltaTemp < 288 | is.na(minT)) {
      minT = 288 + deltaTemp
    }
    maxT <- max(Ts[albedo >= 0.13 & albedo <= 0.15 & NDVI >= 
                     0.1 & NDVI <= 0.28 & Z.om <= 0.005], na.rm = TRUE)
    cold.candidates <- values(LAI >= 3) & values(LAI <= 6) & 
      values(albedo >= 0.18) & values(albedo <= 0.25) & 
      values(NDVI >= max(values(NDVI), na.rm = T) - 0.15) & 
      values(Z.om >= 0.03) & values(Z.om <= 0.08) & values(Ts < 
                                                             (minT + deltaTemp)) & values(WS.buffer == 1)
    hot.candidates <- values(albedo >= 0.13) & values(albedo <= 
                                                        0.15) & values(NDVI >= 0.1) & values(NDVI <= 0.28) & 
      values(Z.om <= 0.005) & values(Ts > (maxT - deltaTemp)) & 
      values(WS.buffer == 1)
    cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
    hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
    if (cold.n < 1 | hot.n < 1) {
      stop(paste("Not enough pixels with the conditions for anchor pixels. I \n                 found", 
                 cold.n, "cold pixels and", hot.n, "hot pixels."))
    }
    Ts.cold <- Ts
    values(Ts.cold)[!cold.candidates] <- NA
    cold <- raster::which.min(Ts.cold)[1]
    if (n > 1) {
      for (nsample in 1:(n - 1)) {
        distbuffer <- raster(Ts)
        values(distbuffer)[cold] <- 1
        distbuffer <- buffer(distbuffer, width = minDist)
        distbuffer <- is.na(distbuffer)
        newAnchor <- NA
        cold.candidates <- values(LAI >= 3) & values(LAI <= 
                                                       6) & values(albedo >= 0.18) & values(albedo <= 
                                                                                              0.25) & values(NDVI >= max(values(NDVI), na.rm = T) - 
                                                                                                               0.15) & values(Z.om >= 0.03) & values(Z.om <= 
                                                                                                                                                       0.08) & values(Ts < (minT + deltaTemp)) & values(distbuffer == 
                                                                                                                                                                                                          1) & values(WS.buffer == 1)
        values(Ts.cold)[!cold.candidates] <- NA
        if (length(which(cold.candidates)) < 2) {
          warning(paste("I can only find ", nsample, 
                        " anchors with cold pixel conditions"))
          break
        }
        try(newAnchor <- raster::which.min(Ts.cold)[1], 
            silent = FALSE)
        if (!is.na(newAnchor)) {
          cold <- c(cold, newAnchor)
        }
      }
    }
    Ts.hot <- Ts
    values(Ts.hot)[!hot.candidates] <- NA
    hot <- raster::which.max(Ts.hot)
    if (n > 1) {
      for (nsample in 1:(n - 1)) {
        distbuffer <- raster(Ts)
        values(distbuffer)[hot] <- 1
        distbuffer <- buffer(distbuffer, width = minDist)
        distbuffer <- is.na(distbuffer)
        newAnchor <- NA
        hot.candidates <- values(albedo >= 0.13) & values(albedo <= 
                                                            0.15) & values(NDVI >= 0.1) & values(NDVI <= 
                                                                                                   0.28) & values(distbuffer == 1) & values(Z.om <= 
                                                                                                                                              0.005) & values(Ts > (maxT - deltaTemp)) & 
          values(WS.buffer == 1)
        values(Ts.hot)[!hot.candidates] <- NA
        if (length(which(hot.candidates)) < 2) {
          warning(paste("I can only find ", nsample, 
                        " anchors with hot pixel conditions"))
          break
        }
        try(newAnchor <- raster::which.max(Ts.hot)[1], 
            silent = FALSE)
        if (!is.na(newAnchor)) {
          hot <- c(hot, newAnchor)
        }
      }
    }
  }
  if (anchors.method == "flexible") {
    minT <- quantile(Ts[LAI >= 2.8 & LAI <= 6 & albedo >= 
                          0.15 & albedo <= 0.25 & Z.om >= 0.03 & Z.om <= 0.08], 
                     0.05, na.rm = TRUE)
    if (minT + deltaTemp < 288 | is.na(minT)) {
      minT = 288 + deltaTemp
    }
    maxT <- max(Ts[albedo >= 0.13 & albedo <= 0.15 & NDVI >= 
                     0.1 & NDVI <= 0.28 & Z.om <= 0.005], na.rm = TRUE)
    if (is.na(maxT) | maxT == -Inf) {
      maxT <- quantile(Ts, 0.95, na.rm = T)
    }
    optValCold <- data.frame(LAI = c(3, 6), albedo = c(0.18, 
                                                       0.25), Z.om = c(0.03, 0.08))
    optValHot <- data.frame(albedo = c(0.13, 0.15), NDVI = c(0.1, 
                                                             0.28), Z.om = c(NA, 0.005), Ts = c(maxT - deltaTemp, 
                                                                                                NA))
    cold.candidates <- values(LAI >= optValCold$LAI[1]) & 
      values(LAI <= optValCold$LAI[2]) & values(albedo >= 
                                                  optValCold$albedo[1]) & values(albedo <= optValCold$albedo[2]) & 
      values(NDVI >= max(values(NDVI), na.rm = T) - 0.15) & 
      values(Z.om >= optValCold$Z.om[1]) & values(Z.om <= 
                                                    optValCold$Z.om[2]) & values(Ts < (minT + deltaTemp)) & 
      values(WS.buffer == 1)
    cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
    useBuffer <- TRUE
    flex <- 0
    optValColdBck <- optValCold
    while (cold.n < 1) {
      useBuffer <- !useBuffer
      cold.candidates <- values(LAI >= optValCold$LAI[1]) & 
        values(LAI <= optValCold$LAI[2]) & values(albedo >= 
                                                    optValCold$albedo[1]) & values(albedo <= optValCold$albedo[2]) & 
        values(NDVI >= max(values(NDVI), na.rm = T) - 
                 0.15) & values(Z.om >= optValCold$Z.om[1]) & 
        values(Z.om <= optValCold$Z.om[2]) & values(Ts < 
                                                      (minT + deltaTemp)) & values(WS.buffer == as.numeric(useBuffer))
      cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
      useBuffer <- !useBuffer
      flex <- flex + 0.1
      print(paste0("relaxing criteria for cold pixels: ", 
                   flex, "%"))
      optValCold[1, ] <- optValColdBck[1, ] * c((1 - flex), 
                                                1, 1)
      optValCold[2, ] <- optValColdBck[2, ] * c((1 + flex), 
                                                1, 1)
      cold.candidates <- values(LAI >= optValCold$LAI[1]) & 
        values(LAI <= optValCold$LAI[2]) & values(albedo >= 
                                                    optValCold$albedo[1]) & values(albedo <= optValCold$albedo[2]) & 
        values(NDVI >= max(values(NDVI), na.rm = T) - 
                 0.15) & values(Z.om >= optValCold$Z.om[1]) & 
        values(Z.om <= optValCold$Z.om[2]) & values(Ts < 
                                                      (minT + deltaTemp)) & values(WS.buffer == as.numeric(useBuffer))
      cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
      optValCold[1, ] <- optValColdBck[1, ] * c(1, (1 - 
                                                      flex), 1)
      optValCold[2, ] <- optValColdBck[2, ] * c(1, (1 + 
                                                      flex), 1)
      cold.candidates <- values(LAI >= optValCold$LAI[1]) & 
        values(LAI <= optValCold$LAI[2]) & values(albedo >= 
                                                    optValCold$albedo[1]) & values(albedo <= optValCold$albedo[2]) & 
        values(NDVI >= max(values(NDVI), na.rm = T) - 
                 0.15) & values(Z.om >= optValCold$Z.om[1]) & 
        values(Z.om <= optValCold$Z.om[2]) & values(Ts < 
                                                      (minT + deltaTemp)) & values(WS.buffer == as.numeric(useBuffer))
      cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
      optValCold[1, ] <- optValColdBck[1, ] * c(1, 1, (1 - 
                                                         flex))
      optValCold[2, ] <- optValColdBck[2, ] * c(1, 1, (1 + 
                                                         flex))
      cold.candidates <- values(LAI >= optValCold$LAI[1]) & 
        values(LAI <= optValCold$LAI[2]) & values(albedo >= 
                                                    optValCold$albedo[1]) & values(albedo <= optValCold$albedo[2]) & 
        values(NDVI >= max(values(NDVI), na.rm = T) - 
                 0.15) & values(Z.om >= optValCold$Z.om[1]) & 
        values(Z.om <= optValCold$Z.om[2]) & values(Ts < 
                                                      (minT + deltaTemp)) & values(WS.buffer == as.numeric(useBuffer))
      cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
      optValCold[1, ] <- optValColdBck[1, ] * (1 - flex)
      optValCold[2, ] <- optValColdBck[2, ] * (1 + flex)
      cold.candidates <- values(LAI >= optValCold$LAI[1]) & 
        values(LAI <= optValCold$LAI[2]) & values(albedo >= 
                                                    optValCold$albedo[1]) & values(albedo <= optValCold$albedo[2]) & 
        values(NDVI >= max(values(NDVI), na.rm = T) - 
                 0.15) & values(Z.om >= optValCold$Z.om[1]) & 
        values(Z.om <= optValCold$Z.om[2]) & values(Ts < 
                                                      (minT + deltaTemp)) & values(WS.buffer == as.numeric(useBuffer))
      cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
      if (flex >= 10) {
        stop("Automatic selection of cold anchors FAILED")
      }
    }
    if (flex != 0 | useBuffer != TRUE) {
      warning(paste("Criteria used for cold pixels was:\n    LAI:", 
                    optValCold[1, 1], "to", optValCold[2, 1], 
                    "\n    albedo:", optValCold[1, 2], "to", 
                    optValCold[2, 2], "\n    Z.om:", optValCold[1, 
                                                                3], "to", optValCold[2, 3], "\n    and buffer ==", 
                    useBuffer))
    }
    flex.cold <- flex
    hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
      values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                       optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
      values(Z.om <= optValHot$Z.om[2]) & values(Ts > (optValHot$Ts[1])) & 
      values(WS.buffer == 1)
    hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
    useBuffer <- TRUE
    flex <- 0
    optValHotBck <- optValHot
    while (hot.n < 1) {
      useBuffer <- !useBuffer
      hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
        values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                         optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
        values(Z.om <= optValHot$Z.om[2]) & values(Ts > 
                                                     (optValHot$Ts[1])) & values(WS.buffer == 1)
      hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
      useBuffer <- !useBuffer
      flex <- flex + 0.1
      print(paste0("relaxing criteria for hot pixels: ", 
                   flex, "%"))
      optValHot[1, ] <- optValHotBck[1, ] * c((1 - flex), 
                                              1, 1, 1)
      optValHot[2, ] <- optValHotBck[2, ] * c((1 + flex), 
                                              1, 1, 1)
      hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
        values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                         optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
        values(Z.om <= optValHot$Z.om[2]) & values(Ts > 
                                                     (optValHot$Ts[1])) & values(WS.buffer == 1)
      hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
      optValHot[1, ] <- optValHotBck[1, ] * c(1, (1 - flex), 
                                              1, 1)
      optValHot[2, ] <- optValHotBck[2, ] * c(1, (1 + flex), 
                                              1, 1)
      hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
        values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                         optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
        values(Z.om <= optValHot$Z.om[2]) & values(Ts > 
                                                     (optValHot$Ts[1])) & values(WS.buffer == 1)
      hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
      optValHot[1, ] <- optValHotBck[1, ] * c(1, 1, (1 - 
                                                       flex), 1)
      optValHot[2, ] <- optValHotBck[2, ] * c(1, 1, (1 + 
                                                       flex), 1)
      hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
        values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                         optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
        values(Z.om <= optValHot$Z.om[2]) & values(Ts > 
                                                     (optValHot$Ts[1])) & values(WS.buffer == 1)
      hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
      optValHot[1, ] <- optValHotBck[1, ] * c(1, 1, 1, 
                                              (1 - flex))
      optValHot[2, ] <- optValHotBck[2, ] * c(1, 1, 1, 
                                              (1 + flex))
      hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
        values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                         optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
        values(Z.om <= optValHot$Z.om[2]) & values(Ts > 
                                                     (optValHot$Ts[1])) & values(WS.buffer == 1)
      hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
      optValHot[1, ] <- optValHotBck[1, ] * (1 - flex)
      optValHot[2, ] <- optValHotBck[2, ] * (1 + flex)
      hot.candidates <- values(albedo >= optValHot$albedo[1]) & 
        values(albedo <= optValHot$albedo[2]) & values(NDVI >= 
                                                         optValHot$NDVI[1]) & values(NDVI <= optValHot$NDVI[2]) & 
        values(Z.om <= optValHot$Z.om[2]) & values(Ts > 
                                                     (optValHot$Ts[1])) & values(WS.buffer == 1)
      hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
      if (flex >= 10) {
        stop("Automatic selection of hot anchors FAILED")
      }
    }
    if (flex != 0 | useBuffer != TRUE) {
      warning(paste("Criteria used for hot pixels was:\n    albedo:", 
                    optValHot[1, 1], "to", optValHot[2, 1], 
                    "\n    NDVI:", optValHot[1, 2], "to", 
                    optValHot[2, 2], "\n    max Z.om:", optValHot[2, 
                                                                  3], "\n    min Ts:", optValHot[1, 4], 
                    "\n    and buffer ==", useBuffer))
    }
    flex.hot <- flex
    cold.n <- sum(as.numeric(cold.candidates), na.rm = T)
    hot.n <- sum(as.numeric(hot.candidates), na.rm = T)
    if (cold.n < 1 | hot.n < 1) {
      stop(paste("Not enough pixels with the conditions for anchor pixels. I \n                 found", 
                 cold.n, "cold pixels and", hot.n, "hot pixels."))
    }
    Ts.cold <- Ts
    values(Ts.cold)[!cold.candidates] <- NA
    cold <- raster::which.min(Ts.cold)[1]
    if (n > 1) {
      for (nsample in 1:(n - 1)) {
        distbuffer <- raster(Ts)
        values(distbuffer)[cold] <- 1
        distbuffer <- buffer(distbuffer, width = minDist)
        distbuffer <- is.na(distbuffer)
        newAnchor <- NA
        cold.candidates <- values(LAI >= 3) & values(LAI <= 
                                                       6) & values(albedo >= 0.18) & values(albedo <= 
                                                                                              0.25) & values(NDVI >= max(values(NDVI), na.rm = T) - 
                                                                                                               0.15) & values(Z.om >= 0.03) & values(Z.om <= 
                                                                                                                                                       0.08) & values(Ts < (minT + deltaTemp)) & values(distbuffer == 
                                                                                                                                                                                                          1) & values(WS.buffer == 1)
        values(Ts.cold)[!cold.candidates] <- NA
        if (length(which(cold.candidates)) < 2) {
          warning(paste("I can only find ", nsample, 
                        " anchors with cold pixel conditions"))
          break
        }
        try(newAnchor <- raster::which.min(Ts.cold)[1], 
            silent = FALSE)
        if (!is.na(newAnchor)) {
          cold <- c(cold, newAnchor)
        }
      }
    }
    Ts.hot <- Ts
    values(Ts.hot)[!hot.candidates] <- NA
    hot <- raster::which.max(Ts.hot)
    if (n > 1) {
      for (nsample in 1:(n - 1)) {
        distbuffer <- raster(Ts)
        values(distbuffer)[hot] <- 1
        distbuffer <- buffer(distbuffer, width = minDist)
        distbuffer <- is.na(distbuffer)
        newAnchor <- NA
        hot.candidates <- values(albedo >= 0.13) & values(albedo <= 
                                                            0.15) & values(NDVI >= 0.1) & values(NDVI <= 
                                                                                                   0.28) & values(distbuffer == 1) & values(Z.om <= 
                                                                                                                                              0.005) & values(Ts > (maxT - deltaTemp)) & 
          values(WS.buffer == 1)
        values(Ts.hot)[!hot.candidates] <- NA
        if (length(which(hot.candidates)) < 2) {
          warning(paste("I can only find ", nsample, 
                        " anchors with hot pixel conditions"))
          break
        }
        try(newAnchor <- raster::which.max(Ts.hot)[1], 
            silent = FALSE)
        if (!is.na(newAnchor)) {
          hot <- c(hot, newAnchor)
        }
      }
    }
  }
  if (verbose == TRUE) {
    print("Cold pixels")
    print(data.frame(cbind(pixel = cold, LAI = LAI[cold], 
                           NDVI = NDVI[cold], albedo = albedo[cold], Z.om = Z.om[cold]), 
                     Ts = Ts[cold]))
    print("Hot pixels")
    print(data.frame(cbind(pixel = hot, LAI = LAI[hot], NDVI = NDVI[hot], 
                           albedo = albedo[hot], Z.om = Z.om[hot], Ts = Ts[hot])))
  }
  if (plots == TRUE) {
    plot(LAI, main = "LAI and hot and cold pixels")
    graphics::points(xyFromCell(LAI, hot), col = "red", 
                     pch = 3)
    graphics::points(xyFromCell(LAI, cold), col = "blue", 
                     pch = 4)
    graphics::points(WSloc, pch = 13)
  }
  hot.and.cold <- data.frame(pixel = integer(), X = integer(), 
                             Y = integer(), Ts = double(), LAI = double(), NDVI = double(), 
                             type = factor(levels = c("hot", "cold")))
  for (i in 1:length(hot)) {
    hot.and.cold[i, ] <- c(hot[i], xyFromCell(LAI, hot[i]), 
                           Ts[hot][i], round(LAI[hot][i], 2), round(NDVI[hot][i], 
                                                                    2), "hot")
  }
  for (i in 1:length(cold)) {
    hot.and.cold[i + length(hot), ] <- c(cold[i], xyFromCell(LAI, 
                                                             cold[i]), Ts[cold][i], round(LAI[cold][i], 2), round(NDVI[cold][i], 
                                                                                                                  2), "cold")
  }
  for (i in 1:5) {
    hot.and.cold[, i] <- as.numeric(hot.and.cold[, i])
  }
  if (anchors.method == "flexible") {
    hot.and.cold$flex <- flex.cold
  }
  return(hot.and.cold)
}
