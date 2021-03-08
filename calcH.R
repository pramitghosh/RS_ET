calculate_sensibleHeat = function (anchors, method = "mean", Ts, Z.om, WeatherStation, 
          ETp.coef = 1.05, Z.om.ws = 0.03, mountainous = FALSE, DEM, 
          Rn, G, verbose = FALSE, maxit = 20) 
{
  if (class(WeatherStation) == "waterWeatherStation") {
    WeatherStation <- getDataWS(WeatherStation)
  }
  if (class(anchors) != "SpatialPointsDataFrame") {
    coordinates(anchors) <- ~X + Y
  }
  hot <- as.numeric(extract(Ts, anchors[anchors@data$type == 
                                          "hot", ], cellnumbers = T)[, 1])
  cold <- as.numeric(extract(Ts, anchors[anchors@data$type == 
                                           "cold", ], cellnumbers = T)[, 1])
  ETo.hourly <- hourlyET(WeatherStation, hours = WeatherStation$hours, 
                         DOY = WeatherStation$DOY, ET.instantaneous = TRUE, ET = "ETor")
  Ts.datum <- Ts - (DEM - WeatherStation$elev) * 6.49/1000
  P <- 101.3 * ((293 - 0.0065 * DEM)/293)^5.26
  air.density <- 1000 * P/(1.01 * (Ts) * 287)
  latent.heat.vaporization <- (2.501 - 0.00236 * (Ts - 273.15))
  u.ws <- WeatherStation$wind * 0.41/log(WeatherStation$height/Z.om.ws)
  u200.v <- u.ws/0.41 * log(200/Z.om.ws)
  if (u200.v < 1) {
    warning(paste0("u200 less than threshold value = ", 
                   round(u200.v, 4), "m/s. using u200 = 4m/s"))
    u200.v <- 4
  }
  u200 <- raster(DEM)
  values(u200) <- u200.v
  if (mountainous == TRUE) {
    u200 <- u200 * (1 + 0.1 * ((DEM - WeatherStation$elev)/1000))
  }
  friction.velocity <- 0.41 * u200/log(200/Z.om)
  friction.velocity[friction.velocity == 0] <- 0.1
  r.ah <- log(2/0.1)/(friction.velocity * 0.41)
  LE.cold <- ETo.hourly * ETp.coef * (2.501 - 0.002361 * (mean(Ts[cold]) - 
                                                            273.15)) * (1e+06)/3600
  H.cold <- mean(Rn[cold], na.rm = T) - mean(G[cold], na.rm = T) - 
    LE.cold
  result <- list()
  if (verbose == TRUE) {
    print("starting conditions")
    print("Cold")
    print(data.frame(cbind(Ts = mean(Ts[cold], na.rm = T), 
                           Ts_datum = mean(Ts.datum[cold], na.rm = T), Rn = mean(Rn[cold], 
                                                                                 na.rm = T), G = mean(G[cold], na.rm = T), Z.om = mean(Z.om[cold], 
                                                                                                                                       na.rm = T), u200 = u200[cold], `u*` = mean(friction.velocity[cold], 
                                                                                                                                                                                  na.rm = T))))
    print("Hot")
    print(data.frame(cbind(Ts = mean(Ts[hot], na.rm = T), 
                           Ts_datum = mean(Ts.datum[hot], na.rm = T), Rn = mean(Rn[hot], 
                                                                                na.rm = T), G = mean(G[hot], na.rm = T), Z.om = mean(Z.om[hot], 
                                                                                                                                     na.rm = T), u200 = u200[hot], `u*` = mean(friction.velocity[hot], 
                                                                                                                                                                               na.rm = T))))
  }
  plot(1, mean(r.ah[hot], na.rm = T), xlim = c(0, 15), ylim = c(0, 
                                                                mean(r.ah[hot], na.rm = T)), col = "red", ylab = "aerodynamic resistance s m-1", 
       xlab = "iteration", pch = 20)
  graphics::points(1, mean(r.ah[cold], na.rm = T), col = "blue", 
                   pch = 20)
  converge <- FALSE
  last.loop <- FALSE
  i <- 1
  if (method == "mean") {
    while (!converge) {
      if (verbose == TRUE) {
        print(paste("iteraction #", i))
      }
      i <- i + 1
      dT.cold <- H.cold * mean(r.ah[cold], na.rm = T)/(mean(air.density[cold], 
                                                            na.rm = T) * 1004)
      dT.hot <- (mean(Rn[hot], na.rm = T) - mean(G[hot], 
                                                 na.rm = T)) * mean(r.ah[hot], na.rm = T)/(mean(air.density[hot], 
                                                                                                na.rm = T) * 1004)
      a <- (dT.hot - dT.cold)/(mean(Ts.datum[hot], na.rm = T) - 
                                 mean(Ts.datum[cold], na.rm = T))
      b <- -a * mean(Ts.datum[cold], na.rm = T) + dT.cold
      if (verbose == TRUE) {
        print(paste("a", a))
        print(paste("b", b))
      }
      dT <- as.numeric(a) * Ts.datum + as.numeric(b)
      rho <- 349.467 * ((((Ts - dT) - 0.0065 * DEM)/(Ts - 
                                                       dT))^5.26)/Ts
      H <- rho * 1004 * dT/r.ah
      Monin.Obukhov.L <- (air.density * -1004 * friction.velocity^3 * 
                            Ts)/(0.41 * 9.807 * H)
      phi.200 <- raster(Monin.Obukhov.L)
      phi.2 <- raster(Monin.Obukhov.L)
      phi.01 <- raster(Monin.Obukhov.L)
      phi.200[Monin.Obukhov.L > 0] <- -5 * (2/Monin.Obukhov.L)[Monin.Obukhov.L > 
                                                                 0]
      phi.2[Monin.Obukhov.L > 0] <- -5 * (2/Monin.Obukhov.L)[Monin.Obukhov.L > 
                                                               0]
      phi.01[Monin.Obukhov.L > 0] <- -5 * (0.1/Monin.Obukhov.L)[Monin.Obukhov.L > 
                                                                  0]
      x.200 <- (1 - 16 * (200/Monin.Obukhov.L))^0.25
      x.2 <- (1 - 16 * (2/Monin.Obukhov.L))^0.25
      x.01 <- (1 - 16 * (0.1/Monin.Obukhov.L))^0.25
      phi.200[Monin.Obukhov.L < 0] <- (2 * log((1 + x.200)/2) + 
                                         log((1 + x.200^2)/2) - 2 * atan(x.200) + 0.5 * 
                                         pi)[Monin.Obukhov.L < 0]
      phi.2[Monin.Obukhov.L < 0] <- (2 * log((1 + x.2^2)/2))[Monin.Obukhov.L < 
                                                               0]
      phi.01[Monin.Obukhov.L < 0] <- (2 * log((1 + x.01^2)/2))[Monin.Obukhov.L < 
                                                                 0]
      if (verbose == TRUE) {
        print(paste("r.ah cold", mean(r.ah[cold], 
                                      na.rm = T)))
        print(paste("r.ah hot", mean(r.ah[hot], 
                                     na.rm = T)))
        print(paste("dT cold", mean(dT[cold], na.rm = T)))
        print(paste("dT hot", mean(dT[hot], na.rm = T)))
        print("##############")
      }
      friction.velocity <- 0.41 * u200/(log(200/Z.om) - 
                                          phi.200)
      friction.velocity[friction.velocity == 0] <- 0.1
      r.ah.hot.previous <- mean(r.ah[hot], na.rm = T)
      r.ah.cold.previous <- mean(r.ah[cold], na.rm = T)
      r.ah <- (log(2/0.1) - phi.2 + phi.01)/(friction.velocity * 
                                               0.41)
      graphics::points(i, mean(r.ah[hot], na.rm = T), col = "red", 
                       pch = 20)
      graphics::points(i, mean(r.ah[cold], na.rm = T), 
                       col = "blue", pch = 20)
      lines(c(i, i - 1), c(mean(r.ah[hot], na.rm = T), 
                           r.ah.hot.previous), col = "red")
      lines(c(i, i - 1), c(mean(r.ah[cold], na.rm = T), 
                           r.ah.cold.previous), col = "blue")
      if (last.loop == TRUE) {
        converge <- TRUE
        if (verbose == TRUE) {
          print(paste0("convergence reached at iteration #", 
                       i))
        }
      }
      delta.r.ah.hot <- (mean(r.ah[hot], na.rm = T) - r.ah.hot.previous)/mean(r.ah[hot], 
                                                                              na.rm = T) * 100
      delta.r.ah.cold <- (mean(r.ah[cold], na.rm = T) - 
                            r.ah.cold.previous)/mean(r.ah[cold], na.rm = T) * 
        100
      if (verbose == TRUE) {
        print(paste("delta rah hot", delta.r.ah.hot))
        print(paste("delta rah cold", delta.r.ah.cold))
        print("### -------")
      }
      if (abs(delta.r.ah.hot) < 1 & abs(delta.r.ah.cold) < 
          1) {
        last.loop <- TRUE
      }
      if (i == maxit) {
        warning(paste0("No convergence after ", 
                       i, " iterations: try different anchor values?"))
        break
      }
    }
  }
  else if (method == "lm") {
    npairs <- min(c(length(cold), length(hot)))
    for (pair in 1:npairs) {
      while (!converge) {
        i <- i + 1
        if (verbose == TRUE) {
          print(paste("iteraction #", i))
        }
        dT.cold <- H.cold * r.ah[cold[pair]]/(air.density[cold[pair]] * 
                                                1004)
        dT.hot <- (Rn[hot[pair]] - G[hot[pair]]) * r.ah[hot[pair]]/(air.density[hot[pair]] * 
                                                                      1004)
        a <- (dT.hot - dT.cold)/(Ts.datum[hot[pair]] - 
                                   Ts.datum[cold[pair]])
        b <- -a * Ts.datum[cold[pair]] + dT.cold
        if (verbose == TRUE) {
          print(paste("a", a))
          print(paste("b", b))
        }
        dT <- as.numeric(a) * Ts.datum + as.numeric(b)
        rho <- 349.467 * ((((Ts - dT) - 0.0065 * DEM)/(Ts - 
                                                         dT))^5.26)/Ts
        H <- rho * 1004 * dT/r.ah
        Monin.Obukhov.L <- (air.density * -1004 * friction.velocity^3 * 
                              Ts)/(0.41 * 9.807 * H)
        phi.200 <- raster(Monin.Obukhov.L)
        phi.2 <- raster(Monin.Obukhov.L)
        phi.01 <- raster(Monin.Obukhov.L)
        phi.200[Monin.Obukhov.L > 0] <- -5 * (2/Monin.Obukhov.L)[Monin.Obukhov.L > 
                                                                   0]
        phi.2[Monin.Obukhov.L > 0] <- -5 * (2/Monin.Obukhov.L)[Monin.Obukhov.L > 
                                                                 0]
        phi.01[Monin.Obukhov.L > 0] <- -5 * (0.1/Monin.Obukhov.L)[Monin.Obukhov.L > 
                                                                    0]
        x.200 <- (1 - 16 * (200/Monin.Obukhov.L))^0.25
        x.2 <- (1 - 16 * (2/Monin.Obukhov.L))^0.25
        x.01 <- (1 - 16 * (0.1/Monin.Obukhov.L))^0.25
        phi.200[Monin.Obukhov.L < 0] <- (2 * log((1 + 
                                                    x.200)/2) + log((1 + x.200^2)/2) - 2 * atan(x.200) + 
                                           0.5 * pi)[Monin.Obukhov.L < 0]
        phi.2[Monin.Obukhov.L < 0] <- (2 * log((1 + x.2^2)/2))[Monin.Obukhov.L < 
                                                                 0]
        phi.01[Monin.Obukhov.L < 0] <- (2 * log((1 + 
                                                   x.01^2)/2))[Monin.Obukhov.L < 0]
        if (verbose == TRUE) {
          print(paste("r.ah cold", r.ah[cold[pair]]))
          print(paste("r.ah hot", r.ah[hot[pair]]))
          print(paste("dT cold", dT[cold[pair]]))
          print(paste("dT hot", dT[hot[pair]]))
          print("##############")
        }
        friction.velocity <- 0.41 * u200/(log(200/Z.om) - 
                                            phi.200)
        r.ah.hot.previous <- r.ah[hot[pair]]
        r.ah.cold.previous <- r.ah[cold[pair]]
        r.ah <- (log(2/0.1) - phi.2 + phi.01)/(friction.velocity * 
                                                 0.41)
        graphics::points(i, r.ah[hot[pair]], col = "red", 
                         pch = 20)
        graphics::points(i, r.ah[cold[pair]], col = "blue", 
                         pch = 20)
        lines(c(i, i - 1), c(r.ah[hot[pair]], r.ah.hot.previous), 
              col = "red")
        lines(c(i, i - 1), c(r.ah[cold[pair]], r.ah.cold.previous), 
              col = "blue")
        if (last.loop == TRUE) {
          converge <- TRUE
          if (verbose == TRUE) {
            print(paste0("convergence reached at iteration #", 
                         i))
          }
        }
        delta.r.ah.hot <- (r.ah[hot[pair]] - r.ah.hot.previous)/r.ah[hot[pair]] * 
          100
        delta.r.ah.cold <- (r.ah[cold[pair]] - r.ah.cold.previous)/r.ah[cold[pair]] * 
          100
        if (verbose == TRUE) {
          print(paste("delta rah hot", delta.r.ah.hot))
          print(paste("delta rah cold", delta.r.ah.cold))
          print("### -------")
        }
        if (abs(delta.r.ah.hot) < 1 & abs(delta.r.ah.cold) < 
            1) {
          last.loop <- TRUE
        }
      }
    }
  }
  dT <- water:::saveLoadClean(imagestack = dT, file = "dT", overwrite = TRUE)
  H <- water:::saveLoadClean(imagestack = H, file = "H", overwrite = TRUE)
  result$a <- a
  result$b <- b
  result$dT <- dT
  result$H <- H
  return(result)
}